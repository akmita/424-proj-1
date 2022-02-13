library(shiny)
library(ggplot2)
library(lubridate)

# global vars
graphChoices <- c("Total Rides Yearly", "Total Rides Daily", "Total Rides Monthly", "Total Rides By Day of the Week")
viewTypes <- c("bar", "table")


#
# add UI elements to the app
#
ui <- fluidPage(
  column(6, 
     column(4,sliderInput(
       inputId = "UIC_year",
        label = "Choose a year",
        value = 2021, min = 2001, max = 2021, ticks = FALSE)
     ),
     column(4,radioButtons(
       inputId = "UIC_graphType",
       label = "Choose the bar graph to show",
       choices = graphChoices)
     ),
     column(4,radioButtons(
       inputId = "UIC_tableOrBar",
       label = "Choose raw table view or bar graph",
       choices = viewTypes)
     ),
     column(12, wellPanel(
       conditionalPanel(
         condition = "input.UIC_tableOrBar == 'bar'",
         plotOutput("UICbar"),
       ),
       conditionalPanel(
         condition = "input.UIC_tableOrBar == 'table'",
         tableOutput("UICtable"),
       ),
     )
   ),
   style = "height: 90vh; overflow: scroll;"
  ),
  column(6, 
   column(4,sliderInput(
     inputId = "ohare_year",
     label = "Choose a year",
     value = 2021, min = 2001, max = 2021, ticks = FALSE)
   ),
   column(4,radioButtons(
     inputId = "ohare_graphType",
     label = "Choose the bar graph to show",
     choices = graphChoices)
   ),
   column(4,radioButtons(
     inputId = "ohare_tableOrBar",
     label = "Choose raw table view or bar graph",
     choices = viewTypes)
   ),
   column(12, wellPanel(
     conditionalPanel(
       condition = "input.ohare_tableOrBar == 'bar'",
       plotOutput("oharebar"),
     ),
     conditionalPanel(
       condition = "input.ohare_tableOrBar == 'table'",
       tableOutput("oharetable"),
     ),
   )
   ),
   style = "height: 90vh; overflow: scroll;"   
  ),
  style="padding: 50px;"
)


#
# send inputs into outputs
#
server <- function(input, output) {
  # both input and output must be in the server func args
  
  print(head(iris))
  
  # UIC data
  output$UICbar <- renderPlot({
    createBarGraph("UIC-Halsted", input$UIC_graphType, input$UIC_year, D_main)  # TODO change to global dataset
  })

  # ohare data
  output$oharebar <- renderPlot({
    createBarGraph("ohare", input$ohare_graphType, input$ohare_year, D_main)  # TODO change to global dataset
  })
 
 
  output$UICtable <- renderTable(getTable(input$UIC_graphType, input$UIC_year, "UIC-Halsted", D_main))



  output$oharetable <- renderTable(getTable(input$ohare_graphType, input$ohare_year, "O'hare", D_main))
  
}



#
# creates a bar graph based on user criteria 
# 
# @location - location of stations
# @barGraphSelect - type of bar graph user selected
# @yearSelected - year the user selected
# @D - station and ride count dataset 
# 
createBarGraph <- function(location, barGraphSelect, yearSelected, D) {
  print(head(D))
  
  # yearly view
  if (barGraphSelect == graphChoices[1]) {
      (ggplot(
        data=parseByYear(D, location), 
        aes(x=year, y=rides)) 
       + geom_bar(stat="identity") 
       + labs(title = paste(barGraphSelect, "at", location))
      )
  }
  # daily view
  else if (barGraphSelect == graphChoices[2]) {
    (ggplot(
      data=parseByDay(D, yearSelected, location), 
      aes(x=newDate, y=rides))
      + geom_bar(stat="identity")
      + labs(title = paste(barGraphSelect, "at", location), x = "Date")
    )
  }
  # monthly view
  else if (barGraphSelect == graphChoices[3]) {
    # TODO
    (ggplot(data=parseByMonth(D, yearSelected, location), aes(x=month, y=rides))
     + geom_bar(stat="identity")
     + labs(title = paste(barGraphSelect, "at", location), x = "Month")
    )
  }
  # day of the week view
  else if (barGraphSelect == graphChoices[4]) {
# TODO
    
    (ggplot(data=parseByWeekday(D, yearSelected, location), aes(x=dayOfWeek, y=rides))
      + geom_bar(stat="identity")
      + labs(title = paste(barGraphSelect, "at", location), x = "Day of the Week")
    )
  }
  else {
    print("failed to load graph")
  }
}



#
# get appropriate graph data for table
#
getTable = function(barGraphSelect, yearSelected, location, D) {
  
  print(paste(barGraphSelect, yearSelected, location))
  
  # yearly view
  if (barGraphSelect == graphChoices[1]) {
    return(parseByYear(D, location))
  }
  # daily view
  else if (barGraphSelect == graphChoices[2]) {
    return(parseByDay(D, yearSelected, location))
  }
  # monthly view  
  else if (barGraphSelect == graphChoices[3]) {
    return(parseByMonth(D, yearSelected, location))
  }
  # day of the week view
  else if (barGraphSelect == graphChoices[4]) {
    return(parseByWeekday(D, yearSelected, location))
  }
  else {
    print("failed to load table")
  }
}


#
# parse dataset to show rides per year
#
parseByYear = function(D, location) {
  D <- filterByLocation(D, location)
  
  D <- aggregate(rides~year,D,sum)        # group by year
  return(D);
}


#
# parse dataset to show rides per day, for specific year
#
parseByDay = function(D, yearSelected, location) {
  D <- filterByLocation(D, location)
  D <- subset(D, format(D$newDate, format="%Y") == yearSelected) # filter by year
  return(D)
}


#
# parse dataset to show rides per month, specific year
#
parseByMonth = function(D, yearSelected, location) {
  D <- filterByLocation(D, location)
  D <- subset(D, format(D$newDate, format="%Y") == yearSelected) # get subset only selected year
  D <- aggregate(rides~month,D,sum) # aggregate per month
  return(D)
}


#
#  parse dataset to show rides per day of week, given year
#
parseByWeekday = function(D, yearSelected, location) {
  D <- filterByLocation(D, location)
  D <- subset(D, format(D$newDate, format="%Y") == yearSelected)  # get subset only selected year
  D$dayOfWeek <- weekdays(as.Date(D$newDate))   # get weekdays 
  D <- aggregate(rides~dayOfWeek,D,sum)         # group by weekday
  # D <- D[c(4,5,6,7,1,2,3),] # try to rearrange days in better order
  return(D)
}


filterByLocation = function(D, location) {
  print(head(D))
  
  if (location == "UIC-Halsted") {
    D <- subset(D, stationname == "UIC-Halsted") # filter only selected station
  }
  else if (grepl( "o'hare", location, fixed = TRUE)) {
    D <- subset(D, 
                (stationname == "Irving Park-O'Hare" 
                | stationname ==  "Harlem-O'Hare" 
                | stationname ==  "Addison-O'Hare" 
                | stationname ==  "O'Hare Airport" 
                | stationname ==  "Montrose-O'Hare" 
                | stationname ==  "Belmont-O'Hare"))
  }

  return(D)
    
}



# 
#  reads data from already split tsv file and formats date using lubridate
# 
readAndParseData = function() {
  # load first frame
  DF <- read.table(
    file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_1.tsv", 
    sep = "\t", header = TRUE)
  names(DF)<-c("station_id","stationname", "date", "daytype", "rides")
  
  # load and concatenate following frames
  i <- 2
  while (i <= 11) {
    DF <- rbind(DF, read.table(
      file = paste0("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_", i, ".tsv"), 
      sep = "\t", header = TRUE)
    )
    i <- i+1
  }

  # convert dates to newDate
  DF$date <- mdy(DF$date)
  DF$newDate <- as.Date(DF$date, "%Y/%m/%d")
  
  # split date into components
  DF$year <- format(DF$newDate, format="%Y")
  DF$month <- format(DF$newDate, format="%m")
  DF$day <- format(DF$newDate, format="%d")
  
  return(DF)
}


#
# main dataset, reformatted dates
#
if (!exists("D_main",  mode="environment")) {
  D_main <- readAndParseData()
}

shinyApp(ui = ui, server = server)



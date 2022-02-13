

# 1. load and concatenate data into single frame
d1 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_1.tsv", sep = "\t", header = TRUE)
d2 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_2.tsv", sep = "\t", header = TRUE)
d3 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_3.tsv", sep = "\t", header = TRUE)
d4 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_4.tsv", sep = "\t", header = TRUE)
d5 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_5.tsv", sep = "\t", header = TRUE)
d6 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_6.tsv", sep = "\t", header = TRUE)
d7 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_7.tsv", sep = "\t", header = TRUE)
d8 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_8.tsv", sep = "\t", header = TRUE)
d9 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_9.tsv", sep = "\t", header = TRUE)
d10 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_10.tsv", sep = "\t", header = TRUE)
d11 <- read.table(file = "CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals_11.tsv", sep = "\t", header = TRUE)
			


	
# 2. fix messed up coluymn names
names(d1)<-c("station_id","stationname", "date", "daytype", "rides")

# 3. concatenate all sets
D <- rbind(D, d11)

# 4. convert dates
library(lubridate)
D_date_formatted <- D
D_date_formatted$date <- mdy(D$date)

# 5. get only UIC data
D_UIC <- subset(D_date_formatted, station_id == 40350)

# 6. put years into separate column should be 7643 rows
D_UIC$year <- format(D_UIC$date, format="%Y")
D_UIC$month <- format(D_UIC$date, format="%m")
D_UIC$day <- format(D_UIC$date, format="%d")
D_UIC$newDate <- as.Date(D_UIC$date, "%Y/%m/%d")

# 7. aggregate and count
# library(sqldf)
# test <- sqldf("SELECT COUNT(*) FROM D_UIC AS count GROUP BY years")
# by year
UIC_agg <- aggregate(rides~year,D_UIC,sum)
# daily, we don't need agregationm, we need to elimitate anythign but years



# 
# 8. start graphin'
# 
library(ggplot2)
ggplot(data=UIC_agg, aes(x=year, y=rides)) + geom_bar(stat="identity") 

ggplot(data=subset(D_UIC, year == 2021), aes(x=newDate, y=rides)) + geom_bar(stat="identity") 

test <- subset(D_UIC, year == 2021)
test$dayOfWeek <- weekdays(as.Date(test$newDate))
test <- aggregate(rides~dayOfWeek,test,sum)


ggplot()


save.image("~/FALL 2022 offline/CS 424 offline/project 1/proj_1_workspace.RData")


# 
# playing around
# remove years from UIC dates, then use years as aggegates below
# 
D_u_aggr <- aggregate(D_UIC, by=list(D_UIC$year), FUN=length)

D_fac <- factor(D_UIC)


options(max.print = 1000)
getOption("max.print")
print(UIC_agg)
print(D_UIC)
typeof(D_UIC$years)

data("midwest", package = "ggplot2")
ggplot(midwest, aes(x=area, y=poptotal)) + geom_point()

test <- subset(D, stationname = "")



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
D_main <- readAndParseData()



test <- subset(D_main, any(stationname == "ohare", "o'hare")) # filter only selected station


test <- grepl("o'hare", D_main$stationname)


test <- grepl("o'hare", D_main$stationname)



D_ <- subset(D, any(grepl("o'hare", D_main$stationname, fixed = TRUE))

             
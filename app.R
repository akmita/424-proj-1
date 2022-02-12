library(shiny)

# add UI elements to the app
ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 25, min = 1, max = 100),
  plotOutput("hist")
)


# send inputs into outputs
server <- function(input, output) {
  # both input and output must be in the server func args
  
  # our plot named "hist" above
  # save all objects you want to display inside output object
  # everything saved into output needs a render function
  output$hist <- renderPlot({ 
    # can declare variables, treat it like an inline function
    ggplot(data=UIC_agg, aes(x=year, y=rides)) + geom_bar(stat="identity") 
  }) 
  
}

shinyApp(ui = ui, server = server)



library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- fluidPage(
  fluidRow(
    column(2, selectInput("yard", "Patio", choices = c('All',unique(resumeData$yardId)))),
    column(2, selectInput("operation", "Tipo Op", choices = c('All',unique(resumeData$operationType)))),
    column(2, selectInput("line", "Linea", choices = c('All',unique(resumeData$linerCode)))),
    column(2, selectInput("month", "Mes", choices = c('All',unique(resumeData$month)))),
    column(2, selectInput("day", "Día", choices = c('All',unique(resumeData$day)))),
    column(2, selectInput("container", "Tipo Contenedor", choices = c('All',unique(resumeData$c_type))))
  ),
  #selectInput("filterSelection", )
  plotOutput("data")
)

server <- function(input, output, session){
  values <- reactiveValues(df = NULL)
  
  histData <- resumeData %>%
    group_by(appointment_date, yardId, operationType, linerCode, c_type, month, day) %>%
    summarise(total_quantity = sum(quantity, na.rm = T))
  
  observe({
    if(input$yard != 'All'){histData <- histData %>% filter(yardId == input$yard)}
    if(input$operation != 'All'){histData <- histData %>% filter(operationType == input$operation)}
    if(input$line != 'All'){histData <- histData %>% filter(linerCode == input$line)}
    if(input$month != 'All'){histData <- histData %>% filter(month == input$month)}
    if(input$day != 'All'){histData <- histData %>% filter(day == input$day)}
    if(input$container != 'All'){histData <- histData %>% filter(c_type == input$container)}
    values$df <- histData
  })
  
  # observeEvent(input$selection,{
  #   userSelection <- input$selection
  #   selectedVars <- lapply(userSelection, as.symbol)
    # if(userSelection == "month"){
    #   filterSelection <- 10
    #   histData <- resumeData %>%
    #     filter(!!as.symbol(userSelection) == filterSelection) %>% 
    #     group_by(appointment_date, .dots = selectedVars) %>% 
    #     summarise(total_quantity = sum(quantity, na.rm = T)) 
    # }else if(userSelection == "day"){
    #   filterSelection <- "Friday"
    #   histData <- resumeData %>%
    #     filter(!!as.symbol(userSelection) == filterSelection) %>% 
    #     group_by(appointment_date, .dots = selectedVars) %>% 
    #     summarise(total_quantity = sum(quantity, na.rm = T)) 
    # }else if(userSelection == "linerCode"){
    #   filterSelection <- "SUD"
    #   histData <- resumeData %>%
    #     filter(!!as.symbol(userSelection) == filterSelection) %>% 
    #     group_by(appointment_date, .dots = selectedVars) %>% 
    #     summarise(total_quantity = sum(quantity, na.rm = T)) 
  # })
  
  output$data <- renderPlot({
    ggplot(values$df, aes(x = total_quantity)) + 
      geom_histogram(binwidth = 50, color = "black", fill = "#71E1FF") + theme_minimal()
  })
}

shinyApp(ui, server)
library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- fluidPage(
  selectInput("selection", "Seleccionar filtro", 
              choices = c("Dia" = "day",
                          "Mes" = "month",
                          "Linea" = "linerCode",
                          "Tipo de contenedor" = "c_type",
                          "Operacion" = "operationType")),
  selectInput("filterSelection", )
  plotOutput("data")
)

server <- function(input, output, session){
  
  observeEvent(input$selection,{
    userSelection <- input$selection
    selectedVars <- lapply(userSelection, as.symbol)
    if(userSelection == "month"){
      filterSelection <- 10
      histData <- resumeData %>%
        filter(!!as.symbol(userSelection) == filterSelection) %>% 
        group_by(appointment_date, .dots = selectedVars) %>% 
        summarise(total_quantity = sum(quantity, na.rm = T)) 
    }else if(userSelection == "day"){
      filterSelection <- "Friday"
      histData <- resumeData %>%
        filter(!!as.symbol(userSelection) == filterSelection) %>% 
        group_by(appointment_date, .dots = selectedVars) %>% 
        summarise(total_quantity = sum(quantity, na.rm = T)) 
    }else if(userSelection == "linerCode"){
      filterSelection <- "SUD"
      histData <- resumeData %>%
        filter(!!as.symbol(userSelection) == filterSelection) %>% 
        group_by(appointment_date, .dots = selectedVars) %>% 
        summarise(total_quantity = sum(quantity, na.rm = T)) 
    }
    output$data <- renderPlot({
      ggplot(histData, aes(x = total_quantity)) + 
        geom_histogram(binwidth = 50, color = "black", fill = "#71E1FF") + theme_minimal()
    })
    
  })
}

shinyApp(ui, server)
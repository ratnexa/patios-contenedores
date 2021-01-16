library(shiny)
library(shinydashboard)
library(ggplot2)
get_query <- function(query){
  mydb <- RMySQL::dbConnect(MySQL(), 
                            user=patiosName, 
                            password=patiosPw, 
                            dbname=patiosDbName,
                            host=patiosHost)
  result <- dbGetQuery(mydb, query)
  dbDisconnect(mydb)
  return(result)
}

resumeData <- get_query({
  "select
	date(yo.appointment_dateTime) as appointment_date
	,count(yo.id) as quantity
	,yo.yardId
	,yo.operationType
	,yo.linerCode
	,ct.nombre as c_type
	,if(yo.operationType like 'IMPO', c2.cSize, yo.requiredContainer_size) as c_size
from yard_operation yo
left join container c2 on c2.id = yo.containerId
inner join contenedores_tipos ct on ct.codigo = if(yo.operationType like 'IMPO', c2.cType, yo.requiredContainer_type)
where
	date(yo.appointment_dateTime) > '2020-01-01'
	AND date(yo.appointment_dateTime) <= date(now())
	#and yo.operationType like 'IMPO'
group by
	date(yo.appointment_dateTime)
	,operationType
	#,linerCode
	,c_type
	,c_size"
})

resumeData$appointment_date <- as.Date(resumeData$appointment_date)
resumeData$day <- weekdays(as.Date(resumeData$appointment_date))
resumeData$month <- month(as.Date(resumeData$appointment_date))
resumeData$year <- year(as.Date(resumeData$appointment_date))
ui <- dashboardPage(skin = "black", title = "Resumen estadisticas",
                    dashboardHeader(title = tags$a(class = "logo",href='http://www.google.com',
                                                   tags$img(src='https://static.wixstatic.com/media/147158_1a7f50efa656423caa43a0b22efe77b9~mv2.png/v1/fill/w_344,h_84,al_c,q_85,usm_0.66_1.00_0.01/Imagen1.webp', height='90%', width='80%'),
                                                   'NS', style="background-color:white")),
                    dashboardSidebar(sidebarUserPanel("NS",
                                                      subtitle = a(href = "#", icon("circle", class = "text-success"), "Online")
                                                      # Image file should be in www/ subdir
                                                      #image = "userimage.png"
                    ),
                    sidebarMenu(id = "sideBarMenu",
                                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                                menuItem("Estadisticas", tabName = "stats",
                                         menuSubItem("Distribucion de Contenedores", tabName = "stats_1"),
                                         menuSubItem("Comportamiento historico", tabName = "stats_2")))),
                    dashboardBody(tabItems(
                      tabItem(tabName = "stats_1",
                              fluidPage(
                                fluidRow(
                                  column(2, selectInput("yard", "Patio", choices = c('All',unique(resumeData$yardId)))),
                                  column(2, selectInput("operation", "Tipo Op", choices = c('All',unique(resumeData$operationType)))),
                                  column(2, selectInput("line", "Linea", choices = c('All',unique(resumeData$linerCode)))),
                                  column(2, selectInput("month", "Mes", choices = c('All',unique(resumeData$month)))),
                                  column(2, selectInput("day", "Día", choices = c('All',unique(resumeData$day)))),
                                  column(2, selectInput("container", "Tipo Contenedor", choices = c('All',unique(resumeData$c_type))))
                                ),
                                #selectInput("filterSelection", )
                                plotOutput("dataHist")
                              )),
                      tabItem(tabName = "stats_2",
                              fluidPage(
                                fluidRow(
                                  column(2, selectInput("period", "Patio", choices = c('All', 'lastYear', 'lastMonth', 'lastWeek'))),
                                  column(2, selectInput("yard_ts", "Patio", choices = c('All',unique(resumeData$yardId)))),
                                  column(2, selectInput("operation_ts", "Tipo Op", choices = c('All',unique(resumeData$operationType)))),
                                  column(2, selectInput("line_ts", "Linea", choices = c('All',unique(resumeData$linerCode))))
                                ),
                                plotOutput("dataTs")
                              ))
                    )))

# ui <- fluidPage(
#   fluidRow(
#     column(2, selectInput("yard", "Patio", choices = c('All',unique(resumeData$yardId)))),
#     column(2, selectInput("operation", "Tipo Op", choices = c('All',unique(resumeData$operationType)))),
#     column(2, selectInput("line", "Linea", choices = c('All',unique(resumeData$linerCode)))),
#     column(2, selectInput("month", "Mes", choices = c('All',unique(resumeData$month)))),
#     column(2, selectInput("day", "Día", choices = c('All',unique(resumeData$day)))),
#     column(2, selectInput("container", "Tipo Contenedor", choices = c('All',unique(resumeData$c_type))))
#   ),
#   #selectInput("filterSelection", )
#   plotOutput("dataHist")
# )

# ui <- fluidPage(
#   fluidRow(
#     column(2, selectInput("period", "Patio", choices = c('All', 'lastYear', 'lastMonth', 'lastWeek'))),
#     column(2, selectInput("yard_ts", "Patio", choices = c('All',unique(resumeData$yardId)))),
#     column(2, selectInput("operation_ts", "Tipo Op", choices = c('All',unique(resumeData$operationType)))),
#     column(2, selectInput("line_ts", "Linea", choices = c('All',unique(resumeData$linerCode))))
#   ),
#   plotOutput("dataTs")
# )

server <- function(input, output, session){
  values <- reactiveValues(df = NULL)
  valuesTs <- reactiveValues(df = NULL)
  histData <- resumeData %>%
    group_by(appointment_date, yardId, operationType, linerCode, c_type, month, day) %>%
    summarise(total_quantity = sum(quantity, na.rm = T))
  
  tsData <- resumeData %>% group_by(appointment_date) %>% summarise(total_quantity = sum(quantity, na.rm = T))
  
  today <- Sys.Date()
  lastYear <- today - 365
  organizedDates <- data.frame(
    date = seq(lastYear, today, by = "days")
  )
  
  displayData <- organizedDates %>% left_join(tsData, by = c("date" = "appointment_date"))
  displayData$selected_filter <- 'All'
  
  observe({
    if(input$yard != 'All'){histData <- histData %>% filter(yardId == input$yard)}
    if(input$operation != 'All'){histData <- histData %>% filter(operationType == input$operation)}
    if(input$line != 'All'){histData <- histData %>% filter(linerCode == input$line)}
    if(input$month != 'All'){histData <- histData %>% filter(month == input$month)}
    if(input$day != 'All'){histData <- histData %>% filter(day == input$day)}
    if(input$container != 'All'){histData <- histData %>% filter(c_type == input$container)}
    values$df <- histData
  })

  observe({
    tsData <- resumeData %>% group_by(appointment_date) %>% summarise(total_quantity = sum(quantity, na.rm = T))
    
    today <- Sys.Date()
    lastYear <- today - 365
    organizedDates <- data.frame(
      date = seq(lastYear, today, by = "days")
    )
    
    displayData <- organizedDates %>% left_join(tsData, by = c("date" = "appointment_date"))
    displayData$selected_filter <- 'All'
    
    if(input$period == "lastYear" | input$period == "All"){
      organizedDatesTemp <- organizedDates %>% filter(date >= (Sys.Date() - 365))
      displayData <- organizedDatesTemp %>% left_join(tsData, by = c("date" = "appointment_date"))
      displayData$selected_filter <- 'All'
    }else if(input$period == "lastMonth"){
      organizedDatesTemp <- organizedDates %>% filter(date >= (Sys.Date() - 30))
      displayData <- organizedDatesTemp %>% left_join(tsData, by = c("date" = "appointment_date"))
      displayData$selected_filter <- 'All'
    }else{
      organizedDatesTemp <- organizedDates %>% filter(date >= (Sys.Date() - 7))
      displayData <- organizedDatesTemp %>% left_join(tsData, by = c("date" = "appointment_date"))
      displayData$selected_filter <- 'All'
    }
    
    if(input$yard_ts != "All"){
      tsData <- resumeData %>% filter(yardId == input$yard_ts) %>% group_by(appointment_date) %>% summarise(total_quantity = sum(quantity, na.rm = T))
      displayData <- organizedDatesTemp %>% left_join(tsData, by = c("date" = "appointment_date"))
      displayData$selected_filter <- as.character(input$yard_ts)
    }
    if(input$operation_ts != "All"){
      tsData <- resumeData %>% filter(operationType == input$operation_ts) %>% group_by(appointment_date) %>% summarise(total_quantity = sum(quantity, na.rm = T))
      displayData <- organizedDatesTemp %>% left_join(tsData, by = c("date" = "appointment_date"))
      displayData$selected_filter <- as.character(input$yard_ts)
    }
    
    if(input$line_ts != "All"){
      tsData <- resumeData %>% filter(linerCode == input$line_ts) %>% group_by(appointment_date) %>% summarise(total_quantity = sum(quantity, na.rm = T))
      displayData <- organizedDatesTemp %>% left_join(tsData, by = c("date" = "appointment_date"))
      displayData$selected_filter <- as.character(input$yard_ts)
    }
    
    valuesTs$df <- displayData
     
  })
  
  output$dataTs <- renderPlot({
    ggplot(valuesTs$df, aes(x = date,
                            y = total_quantity)) +
      geom_line(aes(color = selected_filter), size = 1) +
      geom_point(color = "blue")+
      scale_color_manual(values = c("blue")) +
      theme_minimal()
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
  
  output$dataHist <- renderPlot({
    ggplot(values$df, aes(x = total_quantity)) + 
      geom_histogram(binwidth = 50, color = "black", fill = "#71E1FF") + theme_minimal() +
      labs(x = "Total de contenedores", y = "Frecuencia")
  })
}

shinyApp(ui, server)
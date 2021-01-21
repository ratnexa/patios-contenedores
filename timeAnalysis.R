library(shiny)
library(shinydashboard)
library(ggplot2)
library(RMySQL)
library(lubridate)
library(dplyr)
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

timeData <- get_query({
  'select 
	yo.id
	, yo.truckerId 
	, t2.phone as trucker_phone
	, yo.transportadorId 
	, t3.name as transporter_name
	, yo.linerCode 
	, yo.containerId 
	, yo.yardId 
	, yo.appointmentRequested_dateTime #When the appointment is requested
	, yo.appointment_dateTime 
	, yo.operationType 
	, yo.containerInspectionStartDateTime 
	, yo.yardEntryAuthorizedDateTime 
	, yo.dateTimeStatus3 #Enters the yard
	, yo.dateTimeStatus4 #Gets out of the yard
	, yo.unload_containerDateTime 
	, yo.load_containerDateTime 
	, yo.load_containerInspectedDateTime 
	, yo.cita_anulada_dateTime 
from yard_operation yo 
left join trucker t2 on t2.id = yo.truckerId 
left join transportador t3 on t3.transportadorId = yo.transportadorId 
where yo.appointmentRequested_dateTime >= DATE_SUB(DATE(NOW()), interval 3 month)'
})

timeData$appointmentRequested_dateTime <- as.POSIXct(timeData$appointmentRequested_dateTime)
timeData$appointment_dateTime <- astimeData$appointment_dateTime 

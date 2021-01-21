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
  "select 
	yo.id
	, yo.truckerId 
	, t2.phone as trucker_phone
	, yo.transportadorId 
	, t3.name as transporter_name
	, yo.linerCode 
	, yo.containerId
	, ct.nombre as c_type
	, if(yo.operationType like 'IMPO', c2.cSize, yo.requiredContainer_size) as c_size
	, yo.yardId
	, yo.operationType 
	, yo.appointmentRequested_dateTime #When the appointment is requested
	, count(d.id) as docQuantity
	, MIN(d.`dateTime`) as firstDocDate
	, MAX(d.`dateTime`) as lastDocDate
	, yo.appointment_dateTime
	, min(yodc.checkedDateTime) as firstRevDocDate
	, max(yodc.checkedDateTime) as lastRevDocDate
	, yo.dateTimeStatus2 #Arrived the yard
	#, yo.containerInspectionStartDateTime 
	, yo.yardEntryAuthorizedDateTime 
	, yo.yardEntryAuthorizedUserId
	, yo.dateTimeStatus3 #Enters the yard
	, yo.containerInspectionEndDateTime #Inspection Finish
	, if(yo.operationType like 'IMPO', yo.unload_containerDateTime, yo.load_containerDateTime) as containerTime
	, yo.unload_containerDateTime 
	, yo.load_containerDateTime 
	, yo.load_containerInspectedDateTime 
	, yo.cita_anulada_dateTime 
	, yo.dateTimeStatus4 #Gets out of the yard
from yard_operation yo 
left join trucker t2 on t2.id = yo.truckerId 
left join transportador t3 on t3.transportadorId = yo.transportadorId 
left join container c2 on c2.id = yo.containerId
left join document d on d.yardOperationId = yo.id
left join yard_operation_document_checklist yodc on yodc.operationId = yo.id
inner join contenedores_tipos ct on ct.codigo = if(yo.operationType like 'IMPO', c2.cType, yo.requiredContainer_type)
where 
	yo.appointment_dateTime >= DATE_SUB(DATE(NOW()), interval 1 month)
	and yo.cancelled_appointment != 1
group by yo.id"
})

timeData$appointmentRequested_dateTime <- as.POSIXct(timeData$appointmentRequested_dateTime)
timeData$appointment_dateTime <- astimeData$appointment_dateTime 

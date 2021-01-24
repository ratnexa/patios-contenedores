library(shiny)
library(shinydashboard)
library(ggplot2)
library(RMySQL)
library(lubridate)
library(dplyr)
source("config.R")
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
	, yo.containerInspectionStartDateTime 
	, yo.yardEntryAuthorizedDateTime 
	, yo.yardEntryAuthorizedUserId
	, yo.dateTimeStatus3 #Enters the yard
	, if(yo.operationType like 'IMPO', yo.unload_containerDateTime, IFNULL(yo.load_containerDateTime, yo.load_containerInspectedDateTime)) as operationContainerTime
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


# Transform columns into dates --------------------------------------------
{
  timeData$appointmentRequested_dateTime <- as.POSIXct(timeData$appointmentRequested_dateTime)
  timeData$appointment_dateTime <- as.POSIXct(timeData$appointment_dateTime)
  timeData$firstDocDate <- as.POSIXct(timeData$firstDocDate)
  timeData$lastDocDate <- as.POSIXct(timeData$lastDocDate)
  timeData$firstRevDocDate <- as.POSIXct(timeData$firstRevDocDate)
  timeData$lastRevDocDate <- as.POSIXct(timeData$lastRevDocDate)
  timeData$dateTimeStatus2 <- ifelse(
    is.na(timeData$dateTimeStatus2),
    as.character(timeData$appointment_dateTime),
    timeData$dateTimeStatus2
  )
  timeData$dateTimeStatus2 <- as.POSIXct(timeData$dateTimeStatus2)
  timeData$containerInspectionStartDateTime <- as.POSIXct(timeData$containerInspectionStartDateTime)
  timeData$yardEntryAuthorizedDateTime <- as.POSIXct(timeData$yardEntryAuthorizedDateTime)
  timeData$dateTimeStatus3 <- as.POSIXct(timeData$dateTimeStatus3)
  timeData$operationContainerTime <- as.POSIXct(timeData$operationContainerTime)
  timeData$cita_anulada_dateTime <- as.POSIXct(timeData$cita_anulada_dateTime)
  timeData$dateTimeStatus4 <- as.POSIXct(timeData$dateTimeStatus4)
  
}

timeResumeIMPO <- {timeData %>% filter(operationType == "IMPO") %>%
  mutate(
    wait_time_inspection = difftime(containerInspectionStartDateTime, dateTimeStatus2, units = "mins"),
    wait_time_yard_entry = difftime(yardEntryAuthorizedDateTime, containerInspectionStartDateTime, units = "mins"),
    wait_time_begin_op = difftime(dateTimeStatus3, yardEntryAuthorizedDateTime, units = "mins"),
    service_time_operation = difftime(operationContainerTime, dateTimeStatus3, units = "mins"),
    leave_yard = difftime(dateTimeStatus4, operationContainerTime, units = "mins"),
    upload_doc_time = difftime(lastDocDate, appointmentRequested_dateTime, units = "mins"),
    time_to_appointment = difftime(appointment_dateTime, dateTimeStatus2, units = "mins")
  ) %>%
  select(
    trucker_phone,
    transporter_name,
    linerCode,
    containerId,
    c_type,
    c_size,
    yardId,
    docQuantity,
    wait_time_inspection,
    wait_time_yard_entry,
    wait_time_begin_op,
    service_time_operation,
    leave_yard,
    upload_doc_time,
    time_to_appointment
  )
}

timeResumeREST <- timeData %>% filter(operationType %in% c("EXPO", "REPO")) %>%
  mutate(
    wait_time_yard_entry = difftime(yardEntryAuthorizedDateTime, dateTimeStatus2, units = "mins"),
    wait_time_begin_op = difftime(dateTimeStatus3, yardEntryAuthorizedDateTime, units = "mins"),
    service_time_operation = difftime(operationContainerTime, dateTimeStatus3, units = "mins"),
    leave_yard = difftime(dateTimeStatus4, operationContainerTime, units = "mins"),
    upload_doc_time = difftime(lastDocDate, appointmentRequested_dateTime, units = "mins"),
    time_to_appointment = difftime(appointment_dateTime, dateTimeStatus2, units = "mins")
  ) %>%
  select(
    trucker_phone,
    transporter_name,
    linerCode,
    containerId,
    c_type,
    c_size,
    yardId,
    docQuantity,
    wait_time_yard_entry,
    wait_time_begin_op,
    service_time_operation,
    leave_yard,
    upload_doc_time,
    time_to_appointment
  )

unique(timeResumeIMPO$service_time_inspection)

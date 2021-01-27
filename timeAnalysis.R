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
	, yo.containerInspectionEndDateTime
	, yo.approvedInYardDateTime #Queue
	, yo.yardEntryAuthorizedDateTime
	, yo.yardEntryAuthorizedUserId
	, yo.dateTimeStatus3 #Enters the yard
	, if(yo.operationType like 'IMPO', yo.unload_containerDateTime, IFNULL(yo.load_containerDateTime, yo.load_containerInspectedDateTime)) as operationContainerTime
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
  # timeData$dateTimeStatus2 <- ifelse(
  #   is.na(timeData$dateTimeStatus2),
  #   as.character(timeData$appointment_dateTime),
  #   timeData$dateTimeStatus2
  # )
  timeData$dateTimeStatus2 <- as.POSIXct(timeData$dateTimeStatus2)
  timeData$containerInspectionStartDateTime <- as.POSIXct(timeData$containerInspectionStartDateTime)
  timeData$approvedInYardDateTime <- as.POSIXct(timeData$approvedInYardDateTime)
  timeData$yardEntryAuthorizedDateTime <- as.POSIXct(timeData$yardEntryAuthorizedDateTime)
  timeData$dateTimeStatus3 <- as.POSIXct(timeData$dateTimeStatus3)
  timeData$operationContainerTime <- as.POSIXct(timeData$operationContainerTime)
  timeData$dateTimeStatus4 <- as.POSIXct(timeData$dateTimeStatus4)
  timeData$load_containerInspectedDateTime <- as.POSIXct(timeData$load_containerInspectedDateTime)
  timeData$dateTimeStatus4 <- as.POSIXct(timeData$dateTimeStatus4)
}

timeResumeIMPO <- {timeData %>% filter(operationType == "IMPO", yardId != "3", is.na(dateTimeStatus2) == FALSE) %>%
  mutate(
    upload_doc_time = difftime(lastDocDate, appointmentRequested_dateTime, units = "mins"),
    diff_upload_doc_arrival = difftime(dateTimeStatus2, lastDocDate, units = "mins"),
    diff_time_to_appointment = difftime(appointment_dateTime, appointmentRequested_dateTime, units = "mins"),
    diff_time_to_arrival = difftime(dateTimeStatus2, appointment_dateTime, units = "mins"),
    wait_time_in_queue = difftime(approvedInYardDateTime, dateTimeStatus2, units = "mins"),
    wait_time_inspection = difftime(containerInspectionEndDateTime, approvedInYardDateTime, units = "mins"),
    wait_time_yard_entry = difftime(yardEntryAuthorizedDateTime, containerInspectionEndDateTime, units = "mins"),
    wait_time_begin_op = difftime(dateTimeStatus3, yardEntryAuthorizedDateTime, units = "mins"),
    service_time_operation = difftime(operationContainerTime, dateTimeStatus3, units = "mins"),
    leave_yard = difftime(dateTimeStatus4, operationContainerTime, units = "mins")
  ) %>%
  select(
    id,
    trucker_phone,
    transporter_name,
    linerCode,
    containerId,
    c_type,
    c_size,
    yardId,
    docQuantity,
    upload_doc_time,
    diff_upload_doc_arrival,
    diff_time_to_appointment,
    diff_time_to_arrival,
    wait_time_in_queue,
    wait_time_inspection,
    wait_time_yard_entry,
    wait_time_begin_op,
    service_time_operation,
    leave_yard
  )
}

timeResumeEXPO <- {timeData %>% filter(operationType == "EXPO", yardId != "3", is.na(dateTimeStatus2) == FALSE) %>%
  mutate(
    operation_type = operationType,
    upload_doc_time = difftime(lastDocDate, appointmentRequested_dateTime, units = "mins"),
    diff_upload_doc_arrival = difftime(dateTimeStatus2, lastDocDate, units = "mins"),
    diff_time_to_appointment = difftime(appointment_dateTime, appointmentRequested_dateTime, units = "mins"),
    diff_time_to_arrival = difftime(dateTimeStatus2, appointment_dateTime, units = "mins"),
    wait_time_in_queue = difftime(approvedInYardDateTime, dateTimeStatus2, units = "mins"),
    wait_time_yard_entry = difftime(yardEntryAuthorizedDateTime, approvedInYardDateTime, units = "mins"),
    wait_time_begin_op = difftime(dateTimeStatus3, yardEntryAuthorizedDateTime, units = "mins"),
    service_time_operation = difftime(operationContainerTime, dateTimeStatus3, units = "mins"),
    wait_time_inspection = difftime(load_containerInspectedDateTime, operationContainerTime, units = "mins"),
    leave_yard = difftime(dateTimeStatus4, load_containerInspectedDateTime, units = "mins")
  ) %>%
    select(
      operation_type,
      trucker_phone,
      transporter_name,
      linerCode,
      containerId,
      c_type,
      c_size,
      yardId,
      docQuantity,
      upload_doc_time,
      diff_upload_doc_arrival,
      diff_time_to_appointment,
      diff_time_to_arrival,
      wait_time_in_queue,
      wait_time_yard_entry,
      wait_time_begin_op,
      service_time_operation,
      wait_time_inspection,
      leave_yard
    )
}

timeResumeREPO <- {timeData %>% filter(operationType == "REPO", yardId != "3", is.na(dateTimeStatus2) == FALSE) %>%
  mutate(
    operation_type = operationType,
    upload_doc_time = difftime(lastDocDate, appointmentRequested_dateTime, units = "mins"),
    diff_upload_doc_arrival = difftime(dateTimeStatus2, lastDocDate, units = "mins"),
    diff_time_to_appointment = difftime(appointment_dateTime, appointmentRequested_dateTime, units = "mins"),
    diff_time_to_arrival = difftime(dateTimeStatus2, appointment_dateTime, units = "mins"),
    wait_time_in_queue = difftime(approvedInYardDateTime, dateTimeStatus2, units = "mins"),
    wait_time_yard_entry = difftime(yardEntryAuthorizedDateTime, approvedInYardDateTime, units = "mins"),
    wait_time_begin_op = difftime(dateTimeStatus3, yardEntryAuthorizedDateTime, units = "mins"),
    service_time_operation = difftime(operationContainerTime, dateTimeStatus3, units = "mins"),
    wait_time_inspection = difftime(load_containerInspectedDateTime, operationContainerTime, units = "mins"),
    leave_yard = difftime(dateTimeStatus4, load_containerInspectedDateTime, units = "mins")
  ) %>%
    select(
      operation_type,
      trucker_phone,
      transporter_name,
      linerCode,
      containerId,
      c_type,
      c_size,
      yardId,
      docQuantity,
      upload_doc_time,
      diff_upload_doc_arrival,
      diff_time_to_appointment,
      diff_time_to_arrival,
      wait_time_in_queue,
      wait_time_yard_entry,
      wait_time_begin_op,
      service_time_operation,
      wait_time_inspection,
      leave_yard
    )
}

timeResumeCancelledIMPO <- {timeData %>% filter(operationType == "IMPO", yardId != "3", is.na(dateTimeStatus2)) %>%
    mutate(
      upload_doc_time = difftime(lastDocDate, appointmentRequested_dateTime, units = "mins"),
      diff_upload_doc_arrival = difftime(dateTimeStatus2, lastDocDate, units = "mins"),
      diff_time_to_appointment = difftime(appointment_dateTime, appointmentRequested_dateTime, units = "mins")
    ) %>%
    select(
      id,
      trucker_phone,
      transporter_name,
      linerCode,
      containerId,
      c_type,
      c_size,
      yardId,
      docQuantity,
      upload_doc_time,
      diff_upload_doc_arrival,
      diff_time_to_appointment
    )
}

timeResumeCancelledEXPO <- {timeData %>% filter(operationType == "EXPO", yardId != "3", is.na(dateTimeStatus2)) %>%
  mutate(
    upload_doc_time = difftime(lastDocDate, appointmentRequested_dateTime, units = "mins"),
    diff_upload_doc_arrival = difftime(dateTimeStatus2, lastDocDate, units = "mins"),
    diff_time_to_appointment = difftime(appointment_dateTime, appointmentRequested_dateTime, units = "mins")
  ) %>%
    select(
      id,
      trucker_phone,
      transporter_name,
      linerCode,
      containerId,
      c_type,
      c_size,
      yardId,
      docQuantity,
      upload_doc_time,
      diff_upload_doc_arrival,
      diff_time_to_appointment
    )
}

timeResumeCancelledREPO <- {timeData %>% filter(operationType == "REPO", yardId != "3", is.na(dateTimeStatus2)) %>%
  mutate(
    upload_doc_time = difftime(lastDocDate, appointmentRequested_dateTime, units = "mins"),
    diff_upload_doc_arrival = difftime(dateTimeStatus2, lastDocDate, units = "mins"),
    diff_time_to_appointment = difftime(appointment_dateTime, appointmentRequested_dateTime, units = "mins")
  ) %>%
    select(
      id,
      trucker_phone,
      transporter_name,
      linerCode,
      containerId,
      c_type,
      c_size,
      yardId,
      docQuantity,
      upload_doc_time,
      diff_upload_doc_arrival,
      diff_time_to_appointment
    )
}

timeResumeIMPOErrors <- timeResumeIMPO %>% filter(wait_time_inspection < 0)

temp <- 100
timeResumeIMPOErrors[temp,]
timeData %>% filter(id == timeResumeIMPOErrors[temp,1])
#hist(as.numeric(timeResumeIMPO$wait_time_inspection))
#boxplot(as.numeric(timeResumeIMPO$wait_time_inspection))
# hist(as.numeric(timeResumeIMPO$wait_time_yard_entry))
# boxplot(as.numeric(timeResumeIMPO$wait_time_yard_entry))
# hist(as.numeric(timeResumeIMPO$wait_time_begin_op))
# boxplot(as.numeric(timeResumeIMPO$wait_time_begin_op))
# hist(as.numeric(timeResumeIMPO$service_time_operation))
# boxplot(as.numeric(timeResumeIMPO$service_time_operation))
# hist(as.numeric(timeResumeIMPO$leave_yard))
# boxplot(as.numeric(timeResumeIMPO$leave_yard))
# hist(as.numeric(timeResumeIMPO$upload_doc_time))
# boxplot(as.numeric(timeResumeIMPO$upload_doc_time))

# hist(as.numeric(timeResumeREST$wait_time_yard_entry))
# boxplot(as.numeric(timeResumeREST$wait_time_yard_entry))
# hist(as.numeric(timeResumeREST$wait_time_begin_op))
# boxplot(as.numeric(timeResumeREST$wait_time_begin_op))
# hist(as.numeric(timeResumeREST$service_time_operation))
# boxplot(as.numeric(timeResumeREST$service_time_operation))
# hist(as.numeric(timeResumeREST$leave_yard))
# boxplot(as.numeric(timeResumeREST$leave_yard))
# hist(as.numeric(timeResumeREST$upload_doc_time))
# boxplot(as.numeric(timeResumeREST$upload_doc_time))

# Review avgs -------------------------------------------------------------
resumeIMPO <- list()
resumeIMPO[[1]] <- timeResumeIMPO %>% group_by(c_type) %>% 
  summarise(
    avg_wait_time_inspection = mean(abs(wait_time_inspection), na.rm = T),
    avg_wait_time_yard_entry = mean(abs(wait_time_yard_entry), na.rm = T),
    avg_wait_time_begin_op = mean(abs(wait_time_begin_op), na.rm = T),
    avg_service_time_operation = mean(abs(service_time_operation), na.rm = T),
    avg_leave_yard = mean(abs(leave_yard), na.rm = T),
  )

resumeIMPO[[2]] <- timeResumeIMPO %>% group_by(linerCode) %>% 
  summarise(
    avg_wait_time_inspection = mean(abs(wait_time_inspection), na.rm = T),
    avg_wait_time_yard_entry = mean(abs(wait_time_yard_entry), na.rm = T),
    avg_wait_time_begin_op = mean(abs(wait_time_begin_op), na.rm = T),
    avg_service_time_operation = mean(abs(service_time_operation), na.rm = T),
    avg_leave_yard = mean(abs(leave_yard), na.rm = T),
  )

resumeIMPO[[3]] <- timeResumeIMPO %>% group_by(yardId) %>% 
  summarise(
    avg_wait_time_inspection = mean(abs(wait_time_inspection), na.rm = T),
    avg_wait_time_yard_entry = mean(abs(wait_time_yard_entry), na.rm = T),
    avg_wait_time_begin_op = mean(abs(wait_time_begin_op), na.rm = T),
    avg_service_time_operation = mean(abs(service_time_operation), na.rm = T),
    avg_leave_yard = mean(abs(leave_yard), na.rm = T),
  )

resumeIMPO[[4]] <- timeResumeIMPO %>% group_by(transporter_name) %>% 
  summarise(
    avg_wait_time_inspection = mean(abs(wait_time_inspection), na.rm = T),
    avg_wait_time_yard_entry = mean(abs(wait_time_yard_entry), na.rm = T),
    avg_wait_time_begin_op = mean(abs(wait_time_begin_op), na.rm = T),
    avg_service_time_operation = mean(abs(service_time_operation), na.rm = T),
    avg_leave_yard = mean(abs(leave_yard), na.rm = T),
    obs = n()
  )

resumeIMPO[[5]] <- timeResumeIMPO %>% group_by(trucker_phone) %>% 
  summarise(
    avg_wait_time_inspection = mean(abs(wait_time_inspection), na.rm = T),
    avg_wait_time_yard_entry = mean(abs(wait_time_yard_entry), na.rm = T),
    avg_wait_time_begin_op = mean(abs(wait_time_begin_op), na.rm = T),
    avg_service_time_operation = mean(abs(service_time_operation), na.rm = T),
    avg_leave_yard = mean(abs(leave_yard), na.rm = T),
    obs = n()
  )

resumeIMPO[[6]] <- timeResumeIMPO %>% group_by(c_size) %>% 
  summarise(
    avg_wait_time_inspection = mean(abs(wait_time_inspection), na.rm = T),
    avg_wait_time_yard_entry = mean(abs(wait_time_yard_entry), na.rm = T),
    avg_wait_time_begin_op = mean(abs(wait_time_begin_op), na.rm = T),
    avg_service_time_operation = mean(abs(service_time_operation), na.rm = T),
    avg_leave_yard = mean(abs(leave_yard), na.rm = T)
  )

resumeEXPO <- list()
resumeEXPO[[1]] <- timeResumeREST %>% filter(operation_type == "EXPO") %>% group_by(c_type) %>% 
  summarise(
    avg_wait_time_yard_entry = mean(abs(wait_time_yard_entry), na.rm = T),
    avg_wait_time_begin_op = mean(abs(wait_time_begin_op), na.rm = T),
    avg_service_time_operation = mean(abs(service_time_operation), na.rm = T),
    avg_leave_yard = mean(abs(leave_yard), na.rm = T),
  )

unique(timeResumeIMPO$service_time_inspection)

library(RMySQL)
library(dplyr)
library(lubridate)
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
mydb <- RMySQL::dbConnect(MySQL(), 
                          user=patiosName, 
                          password=patiosPw, 
                          dbname=patiosDbName,
                          host=patiosHost)

totalTables <- dbListTables(mydb)

dbDisconnect(mydb)

dataset <- get_query(
 'select * from operation_list where yardId = 1'
)

unique(dataset$transportadorName)
unique(dataset$linerName)
unique(dataset$containerStatusId)
unique(dataset$statusId)
unique(dataset$statusName)

sort(unique(dataset$clientName))

sort(unique(dataset$licensePlate))

unique(dataset$containerNumber) #Container id
unique(dataset$containerType)
unique(dataset$reposicion_id)
unique(dataset$reposicion_status)
unique(dataset$reposicion_destino)
unique(dataset$reposicion_tamano) #Datos contenedor
unique(dataset$reposicion_oferta)
unique(dataset$reposicion_transportista)




outliers <- boxplot(dataset$kpi2_tiempoEnPatio)$out
dataset2 <- dataset[-which(dataset$kpi2_tiempoEnPatio %in% outliers),]

hist(dataset2$kpi2_tiempoEnPatio)
boxplot(dataset2$kpi2_tiempoEnPatio)d
boxplot(dataset$kpi2_tiempoEfectivo)

yardOp <- get_query({
  'select * from yard_operation yo where stopwatch_seconds is not NULL and date(appointment_dateTime) >= "2020-11-01"'
})

yardOp$validation <- as.POSIXlt(yardOp$dateTimeStatus4) - seconds(yardOp$stopwatch_seconds)
yardOp$kpi2test <- difftime(as.POSIXlt(yardOp$dateTimeStatus4), as.POSIXlt(yardOp$approvedInYardDateTime), units = "mins")

fullData <- dataset %>% left_join(yardOp %>% select(id, kpi2test),
                                  by = c("yardOperationId" = "id"))
fullData$kpi2test <- ifelse(is.na(fullData$kpi2test), 0, fullData$kpi2test)


x <- fullData %>% group_by(licensePlate) %>% summarize(meanTime = mean(kpi2test, na.rm = T),
                                                       count = n())

hist(as.numeric(yardOp$kpi2test))

xfilt <- yardOp %>% select(id, kpi2test)


yardOp$load_containerId
yardOp$load_containerDateTime

yardOp$statusId
table(yardOp$operationType)
yardOp$appointment_dateTime[5]
yardOp$dateTimeStatus2[5]
yardOp$approvedInYardDateTime[5]
yardOp$yardEntryAuthorizedDateTime[5]
yardOp$dateTimeStatus3[5]

yardOp$unload_containerDateTime[5]
yardOp$dateTimeStatus4[5]


yardOp$load_containerDateTime[4]
yardOp$dateTimeStatus4[4]
yardOp$codigoTurno
yardOp$load_containerInspectedDateTime[4]

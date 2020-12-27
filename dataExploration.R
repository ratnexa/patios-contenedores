library(RMySQL)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
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

clients <- get_query(
  'select * from client'
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
  'select * from yard_operation yo where stopwatch_seconds is not NULL and date(appointment_dateTime) >= "2020-10-01"'
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
yardOp$dateTimeStatus2[5] #Waiting in line
yardOp$approvedInYardDateTime[5]
yardOp$yardEntryAuthorizedDateTime[5]
yardOp$dateTimeStatus3[5]

yardOp$unload_containerDateTime[5]
yardOp$dateTimeStatus4[5]

yardOp$load_containerDateTime[4]
yardOp$dateTimeStatus4[4]
yardOp$codigoTurno
yardOp$load_containerInspectedDateTime[4]

yardOp$totalTime <- ifelse(
  yardOp$operationType == "EXPO",
  difftime(as.POSIXlt(yardOp$dateTimeStatus3), as.POSIXlt(yardOp$dateTimeStatus2), units = "mins") + 
    abs(difftime(as.POSIXlt(yardOp$load_containerInspectedDateTime), as.POSIXlt(yardOp$dateTimeStatus4), units = "mins")),
  difftime(as.POSIXlt(yardOp$dateTimeStatus3), as.POSIXlt(yardOp$dateTimeStatus2), units = "mins") + 
    difftime(as.POSIXlt(yardOp$dateTimeStatus4), as.POSIXlt(yardOp$unload_containerDateTime), units = "mins")
)

yardOp$waitingTime <- as.numeric(abs(difftime(
  yardOp$appointment_dateTime,
  yardOp$dateTimeStatus2,
  units = "mins"
)))



expo <- yardOp %>% filter(operationType == "EXPO")
outExpo <- boxplot(expo$totalTime)$out
expo <- expo[-which(expo$totalTime %in% outExpo),]
impo <- yardOp %>% filter(operationType == "IMPO")

#Unload time
expoPlot <- expo %>% 
  ggplot(aes(totalTime)) + geom_histogram(fill = "#33FFF6",
                                          col = I("black"))+ 
  labs(title="EXPO")

impoPlot <- impo %>% 
  ggplot(aes(totalTime)) + geom_histogram(fill = "#B8FF33",
                                          col = I("black"))+ 
  labs(title="IMPO")
hist(impo$totalTime)


impo %>% filter(linerCode == "SUD") %>% 
  ggplot(aes(totalTime)) + geom_histogram(fill = I("blue"),
                                          col = I("red"))

hist(impo$waitingTime)

hist(expo$totalTime)

a <- impo %>% filter(linerCode == "SEA") %>% 
  ggplot(aes(totalTime)) + geom_histogram(fill = "#B8FF33",
                                          col = I("black"))+ 
  labs(title="SEA")

b <- impo %>% filter(linerCode == "MSK") %>% 
  ggplot(aes(totalTime)) + geom_histogram(fill = "#B8FF33",
                                          col = I("black")) + 
  labs(title="MSK")

c <- impo %>% filter(linerCode == "PIL") %>% 
  ggplot(aes(totalTime)) + geom_histogram(fill = "#B8FF33",
                                          col = I("black"))+ 
  labs(title="PIL")

d <- impo %>% filter(linerCode == "SUD") %>% 
  ggplot(aes(totalTime)) + geom_histogram(fill = "#B8FF33",
                                          col = I("black")) + 
  labs(title="SUD")

grid.arrange(a,b,c,d, nrow = 2, ncol = 2)

hist(expo$waitingTime)

table(expo$linerCode)


table(yardOp$appointmentRequestedName)
sort(table(yardOp$truckerId))
sort(table(yardOp$truckId))

yardOp <- yardOp %>% left_join(clients, 
                     by = c("clientId" = "id"))

sort(table(yardOp$name))


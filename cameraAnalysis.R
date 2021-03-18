library(RMySQL)
library(lubridate)
library(dplyr)
library(reshape2)
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
	, t.licensePlate
	, yo.operationType
	, date_sub(yo.dateTimeStatus4, interval 5 hour) as time #Gets out of the yard
from yard_operation yo 
left join truck t on t.id = yo.truckId
where 
	yo.dateTimeStatus4 >= '2021-03-13'
	and yo.cancelled_appointment != 1
  and yo.dateTimeStatus2 is not null
  and yo.yardId = 1
group by yo.id"
})

data <- read.csv('bd_test.csv', sep = ';')

check_plate <- adist(data$plate, timeData$licensePlate)
colnames(check_plate) <- timeData$licensePlate
rownames(check_plate) <- data$plate

check_plate <- melt(check_plate, varnames = c('camera', 'reinaldix'))
check_plate <- check_plate %>% filter(value <= 2)

data <- data %>% left_join(check_plate %>% select(camera, reinaldix), by = c('plate' = 'camera'))

x <- timeData %>% inner_join(data %>% select(reinaldix, register_date, plate), by = c('licensePlate' = 'reinaldix'))

dummy <- x[which(duplicated(x$licensePlate) == FALSE),]
dummy$diffTime <- difftime(dummy$time, dummy$register_date, units = 'mins')

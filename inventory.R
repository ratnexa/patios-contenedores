library(shiny)
library(shinydashboard)
library(ggplot2)
library(RMySQL)
library(lubridate)
library(dplyr)
library(tidyr)
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

dbInv <- get_query({
  "select 
	yo.id
	, yo.yardId
	, yo.appointment_dateTime 
	, DATE(yo.appointment_dateTime) as appointment_date
	, t3.name as transporter_name
	, t2.phone as trucker_phone
	, CONCAT(t2.firstName, ' ', t2.lastName) as trucker_name
	, yo.linerCode
	, c2.id as container_id
	, c2.cSize as c_size
	, yo.operationType
from yard_operation yo 
left join trucker t2 on t2.id = yo.truckerId 
left join transportador t3 on t3.transportadorId = yo.transportadorId 
left join container c2 on c2.id = if(yo.operationType like 'IMPO', yo.containerId, yo.load_containerId)
inner join contenedores_tipos ct on ct.codigo = c2.cType
where 
	#yo.appointment_dateTime >= DATE_SUB(DATE(NOW()), interval 5 month)
	yo.cancelled_appointment != 1
group by yo.id"
})

selectedYardId <- 1
baseInv <- dbInv %>% filter(yardId == selectedYardId) %>% 
  arrange(container_id) %>% 
  select(id, appointment_dateTime, container_id, operationType)

kardexIn <- baseInv %>% 
  spread(operationType, appointment_dateTime, convert = T) %>% 
  arrange(container_id) %>% select(container_id, IMPO) %>% filter(!is.na(IMPO))

kardexRepo <- baseInv %>% 
  spread(operationType, appointment_dateTime, convert = T) %>% 
  arrange(container_id) %>% select(container_id, REPO) %>% filter(!is.na(REPO))

kardexExpo <- baseInv %>% 
  spread(operationType, appointment_dateTime, convert = T) %>% 
  arrange(container_id) %>% select(container_id, EXPO) %>% filter(!is.na(EXPO))

kardexTest <- kardexIn %>% 
  left_join(kardexRepo, 
            by = c("container_id")) %>%
  left_join(kardexExpo,
            by = c("container_id")) 

kardexTest <- unique(kardexTest) %>%
  mutate(total_time_in_yard = difftime(ifelse(is.na(REPO), ifelse(is.na(EXPO), NA, EXPO), REPO), IMPO, units = "days"),
         total_time_in_yard = ifelse(is.na(total_time_in_yard), difftime(Sys.time(), IMPO, units = "days"), total_time_in_yard))

table(kardexTest$container_id)

boxplot(kardexTest$total_time_in_yard)
hist(kardexTest$total_time_in_yard)



inMove <- baseInv %>% filter(operationType == "IMPO")
totalIn <- inMove %>% group_by()
outMove <- baseInv %>% filter(operationType != "IMPO")

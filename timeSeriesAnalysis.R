library(RMySQL)
library(dplyr)
library(tidyr)
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

resumeData$day <- weekdays(as.Date(resumeData$appointment_date))
resumeData$month <- month(as.Date(resumeData$appointment_date))
resumeData$year <- year(as.Date(resumeData$appointment_date))
#selectedLinerCode <- "EVE"
#resumeData <- resumeData %>% filter(linerCode == selectedLinerCode)
generalTs <- resumeData %>% group_by(appointment_date, day, yardId) %>% 
  summarise(total_quantity = sum(quantity, na.rm = T),
            count = n())
generalTs$yardId_temp <- as.character(generalTs$yardId)


selectedYardId <- 1
filterTs <- generalTs %>% filter(yardId == selectedYardId)
dayResume <- generalTs %>% filter(yardId == selectedYardId) %>% 
  group_by(day) %>% 
  summarise(avg_quantity = mean(total_quantity, na.rm = T),
            count = n())

plot(filterTs$total_quantity, type = "o")


# Time series plots -------------------------------------------------------
#This looks hideous
ggplot(generalTs, aes(x = as.Date(appointment_date),
                     y = total_quantity)) +
  geom_line(aes(color = yardId_temp), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "blue", "green", "black"))


#Total last year
ggplot(filterTs, aes(x = as.Date(appointment_date),
                      y = total_quantity)) +
  geom_line(aes(color = yardId_temp), size = 1) +
  geom_point(color = "blue")+
  scale_color_manual(values = c("blue")) +
  theme_minimal() + 
  stat_smooth(color = "red", fill = "red", method = "loess")

#Total last week
lastWeekTs <- filterTs %>% filter(appointment_date >= (Sys.Date()-7))
ggplot(lastWeekTs, aes(x = as.Date(appointment_date),
                     y = total_quantity)) +
  geom_line(aes(color = yardId_temp), size = 1) +
  geom_point(color = "blue")+
  scale_color_manual(values = c("blue")) +
  theme_minimal()

#Total last month
lastMonthTs <- filterTs %>% filter(appointment_date >= (Sys.Date()-30))
ggplot(lastMonthTs, aes(x = as.Date(appointment_date),
                       y = total_quantity)) +
  geom_line(aes(color = yardId_temp), size = 1) +
  geom_point(color = "blue")+
  scale_color_manual(values = c("blue")) +
  theme_minimal() 


# Histograms --------------------------------------------------------------

#By Total
ggplot(resumeData, aes(x = quantity)) + geom_histogram()

#By days
selectedDay <- "Friday"
histByDayData <- resumeData %>% 
  filter(day == selectedDay) %>%
  group_by(appointment_date, day) %>% 
  summarise(total_quantity = sum(quantity, na.rm = T)) 

ggplot(histByDayData, aes(x = total_quantity)) +
  geom_histogram(binwidth = 50, color = "black", fill = "#71E1FF")+
  theme_minimal() 

#By month
selectedMonth <- 10
eval(as.symbol("selectedMonth"))
histByMonthData <- resumeData %>%
  filter(month == selectedMonth)%>%
  group_by(appointment_date, month) %>% 
  summarise(total_quantity = sum(quantity, na.rm = T)) 

ggplot(histByMonthData, aes(x = total_quantity)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "#71E1FF") + theme_minimal()

#By liner
selectedLiner <- "SUD"
histByLinerData <- resumeData %>% 
  filter(linerCode == selectedLiner) %>%
  group_by(appointment_date, linerCode) %>% 
  summarise(total_quantity = sum(quantity, na.rm = T)) 

ggplot(histByLinerData, aes(x = total_quantity)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "#71E1FF")+ theme_minimal()

#By Container type
selectedContainerType <- "Dry"
histByContainerTypeData <- resumeData %>% 
  filter(c_type == selectedContainerType) %>%
  group_by(appointment_date, c_type) %>% 
  summarise(total_quantity = sum(quantity, na.rm = T)) 

ggplot(histByContainerTypeData, aes(x = total_quantity)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "#71E1FF")+ theme_minimal()

#By operation type
selectedOperation <- "REPO"
histByOperationData <- resumeData %>% 
  filter(operationType == selectedOperation) %>%
  group_by(appointment_date, operationType) %>% 
  summarise(total_quantity = sum(quantity, na.rm = T)) 

ggplot(histByOperationData, aes(x = total_quantity)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "#71E1FF")+ theme_minimal()


# Auto --------------------------------------------------------------------

userSelection <- c("month")
selectedVars <- lapply(userSelection, as.symbol)
filterSelection <- 10
histData <- resumeData %>%
  filter(!!as.symbol(userSelection) == filterSelection) %>% 
  group_by(appointment_date, .dots = selectedVars) %>% 
  summarise(total_quantity = sum(quantity, na.rm = T)) 

ggplot(histData, aes(x = total_quantity)) + 
  geom_histogram(binwidth = 50, color = "black", fill = "#71E1FF") + theme_minimal()


x <- hist((filterTs %>% filter(day == "Friday"))$total_quantity)

dayResume <- resumeData %>% group_by(day) %>% summarise(total_quantity = sum(quantity, na.rm = T),
                                                        count = n(),
                                                        avg_quantity = mean(quantity, na.rm = T))
impoResume <- resumeData %>% 
  filter(operationType == "IMPO") %>% 
  group_by(day) %>% summarise(avg_quantity = mean(quantity, na.rm = T))

expoResume <- resumeData %>% 
  filter(operationType == "EXPO") %>% 
  group_by(day) %>% summarise(avg_quantity = mean(quantity, na.rm = T))

repoResume <- resumeData %>% 
  filter(operationType == "REPO") %>% 
  group_by(day) %>% summarise(avg_quantity = mean(quantity, na.rm = T))

test2 <- resumeData %>% filter(yardId == 1) %>% 
  group_by(year, month, operationType) %>% 
  summarise(total = sum(quantity, na.rm = T))

test2 <- test2 %>% spread(operationType, total) %>% replace(is.na(.), 0)
test2$total <- cumsum(test2$IMPO - test2$EXPO - test2$REPO)
test2$delta <- test2$IMPO - test2$EXPO - test2$REPO

sum((resumeData %>% filter(operationType == "IMPO", yardId == 1))$quantity)



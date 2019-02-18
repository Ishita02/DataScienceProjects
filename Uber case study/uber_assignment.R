setwd("C:/Users/Admin/Desktop/DATA_ANALYTICS/Module2/UberAssignment")
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)
# Loading data 
Uber_Request <- read.csv("Uber Request Data.csv",stringsAsFactors = F)
#------------------------Data Cleaning----------------------------------------------------------
#-----------------------Checking number of N.A values in requested and Drop Timestamp-----------
sum(is.na(Uber_Request$Request.timestamp))
sum(is.na(Uber_Request$Drop.timestamp))
#---------------Replacing '/' in date with '-'  ----------------------------------------------
Uber_Request$Request.timestamp<-str_replace_all(Uber_Request$Request.timestamp, "[/]", "-")
Uber_Request$Drop.timestamp<-str_replace_all(Uber_Request$Drop.timestamp, "[/]", "-")
#-------------To check whether there are any duplicate requests or not-----------------
n_distinct(Uber_Request$Request.id)
#Ans:No
#--------Adding Seconds to Request_timestamp where not present-----------------
for(i in 1:nrow(Uber_Request)){
  if(nchar(Uber_Request$Request.timestamp[i])==14|nchar(Uber_Request$Request.timestamp[i])==15)
  {
    
    Uber_Request$Request.timestamp[i]<-paste0(Uber_Request$Request.timestamp[i],":00")
    
  }
}

#----------Adding Seconds to Drop_timestamp where not present--------------------
for(i in 1:nrow(Uber_Request)){
  if(!is.na(Uber_Request$Drop.timestamp[i])){
  if(nchar(Uber_Request$Drop.timestamp[i])==14|nchar(Uber_Request$Drop.timestamp[i])==15)
  {
    
    Uber_Request$Drop.timestamp[i]<-paste0(Uber_Request$Drop.timestamp[i],":00")
    
  }
}
}
#----------Converting Requested time to Date format-------------------------------------------------------
Uber_Request$Request.timestamp = as.POSIXlt(Uber_Request$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
#d = unclass(Uber_Request$Request.timestamp)
#View(d)
#---------------Converting Drop time to Date format-------------------------------------------------------
Uber_Request$Drop.timestamp= as.POSIXlt(Uber_Request$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")
str(Uber_Request$Drop.timestamp)
#v = unclass(Uber_Request$Drop.timestamp)
#View(v)
#----------Checking number of N.A values in requested and Drop Timestamp after formating----------
sum(is.na(Uber_Request$Request.timestamp))
sum(is.na(Uber_Request$Drop.timestamp))
#As number of NA's are same for both the timmings,hence no new N.A values have been introduced due to formatting
#----------------Retriving time out of Requested time---------------------------------------
submission<-str_split(Uber_Request$Request.timestamp," ",simplify = T)
Uber_Request$Request_time<-submission[,2]
str(Uber_Request$Request_time)
#----------------Retriving time out of Drop time------------------------------------------------
submission2<-str_split(Uber_Request$Drop.timestamp," ",simplify = T)
Uber_Request$Drop_time<-submission2[,2]

#-----------------Finding Requested Hour--------------------------------
Uber_Request$ReqHour <- format(Uber_Request$Request.timestamp, "%H")

#-----------------------Converting Requested Hour to integer-----------------------
Uber_Request$ReqHour<-as.integer(Uber_Request$ReqHour)
str(Uber_Request$ReqHour)
#-----------------Finding Drop Hour--------------------------------
Uber_Request$DropHour <- format(Uber_Request$Drop.timestamp, "%H")

#-----------------------Converting Drop Hour to integer-----------------------
Uber_Request$DropHour<-as.integer(Uber_Request$DropHour)
str(Uber_Request$DropHour)

sum(is.na(Uber_Request$DropHour))/nrow(Uber_Request)*100
#58% of the requests are either cancelled or there is no availability of cars

#----------------Plotting Bar graph to know the count of each status on Hourly Basis--------------
ggplot(Uber_Request, aes(x=factor(ReqHour), fill=Status )) +geom_bar()+ggtitle("Hourly based Status")+xlab("Requested Hour")
#stacked Bar graph is plotted to know the variation in number of requests made based on the requested hour
#factor of requested hour is done to show each hour as a discrete value
#status is used for fill to get an idea of number of each type of status for every hour
#By plotting the bar graph 4-10 is conssidered as one slot where the request count is greater than 400 for most of the hours-Consisdered as Early morning slot
#Between 17-23 again there is a spike so that is considered as Late Evening slot
#Requested hours where count is less than 100 i.e 0-4 is considered as late night slot
#The left over hours i.e 10-16 requested hours are considered as day time slot
#--------------------Adding Time slots based on requested Hour---------------------------
for(i in 1:nrow(Uber_Request)){
  if(Uber_Request$ReqHour[i]>=17&& Uber_Request$ReqHour[i]<=23){
    Uber_Request[i,"Time_Slot"]<-"Late Evening"
  }
  else if(Uber_Request$ReqHour[i]>9 && Uber_Request$ReqHour[i]<=16){
    Uber_Request[i,"Time_Slot"]<-"Day Time"
  }
  else if(Uber_Request$ReqHour[i]>=04 && Uber_Request$ReqHour[i]<=9){
    Uber_Request[i,"Time_Slot"]<-"Early Morning"}
  else{
    Uber_Request[i,"Time_Slot"]<-"Late Night"}
}


#-------------------Count of each status for every slot------------------------------------
summary(factor(Uber_Request$Time_Slot))
ggplot(Uber_Request, aes(x= Time_Slot,fill=Status )) +geom_bar(colour="black",position=position_dodge())+ggtitle("Status based on Time Slots")+xlab("Time Slot")
#Grouped bar chart is created to show information about different status for each time slot
# ----------------------------Count of each status-------------------------------------
z<-summary(factor(Uber_Request$Status))

# Percentage of total number of requests raised vs cancelled by drivers.
(z[1]/nrow(Uber_Request))*100
# 18.73981% of requests getting cancelled
#Percentage of total number of requests raised vs cars not available.
(z[2]/nrow(Uber_Request))*100
# 39.28836% of requests remain incomplete due unavailabilty of cars
# Percentage of total number of requests raised vs trip completed
(z[3]/nrow(Uber_Request))*100
# 41.97183 % of requests are getting completed

ggplot(Uber_Request, aes(x=Status, fill=Status )) +geom_bar()+ggtitle("Count of each Status")
#stacked bar graph is created to know the count of each status and fill is also taken as status to add the color element to the graph
#The graph shows the frequecy of requests getting cancelled,trips completed and no cars available
ggplot(Uber_Request, aes(x = ReqHour,col=Status)) + geom_freqpoly(binwidth=2,size=1)+ggtitle("Frequency of each Status-Hourly Basis")+xlab("Requested Hour")
#Frequency polygons are made to know the frequency of each status on the basis of dependent variable i.e requested hour

#--------------------Analyzing the dataset based on Pickup Point-----------------------------------
summary(factor(Uber_Request$Pickup.point))
ggplot(Uber_Request, aes(x = Pickup.point, fill = Status)) + geom_bar(stat = "count")+ggtitle("Count of each Status-At Airport & City")
#stacked bar graph is plotted with x as Pickup point to analyze the number of requests at the airport and status is used as fill to know the status for each variable
#the graph shows that there are slightly higher number of requests in the city than in the airport
#It also shows that problemetic areas in the airport is unavalibility of cars
#and in the city the problem is due to cancellation of cars

ggplot(Uber_Request,aes(x=factor(ReqHour),fill=Pickup.point))+geom_bar(stat="count")+xlab("Requested Hour")+ggtitle("Demand At Airport & City-Hourly Basis")
#stacked Bar graph is plotted to know the variation in number of requests made based on the requested hour
#factor of requested hour is done to show each hour as a discrete value
#Pick-up point is used for fill to get an idea of number of requests at the airport and city for every hour 
#-----------------------Demand Supply--------------------------------------------------------------------
uber_master<-Uber_Request[,c(2,4,9,10,11)]
#only the required columns such as pickup point,time slot,requested hour,drop hour and status are considered to create demand and supply dataframe
for(j in 1:nrow(uber_master)){
  if(uber_master$Status[j]=="Trip Completed"){
    uber_master[j,"Status_Numeric"]<-"Supply"
  }
  else if (uber_master$Status[j]=="No Cars Available" ||uber_master$Status[j]=="Cancelled"){
    uber_master[j,"Status_Numeric"]<-"Gap"
  }
}
#If the staus of trip is completed than it is considered as supply otherwise gap(as the demands are not met)
#Demand column will be the number of requests in each hour
Requested_hour_frequency <- uber_master %>% group_by(ReqHour) %>% summarise(Demand=n())
#Calculating the occurances of Supply and gap for each Requested hour
Uber_data <- uber_master %>% group_by(ReqHour,Status_Numeric) %>% summarise(frequency=n())
demand_supply<-spread(Uber_data , Status_Numeric, frequency)
#Merging the demand column with above data set
demand_supply <- merge(demand_supply,Requested_hour_frequency, by  = "ReqHour")

for(i in 1:nrow(demand_supply)){
  if(demand_supply$ReqHour[i]>=17&& demand_supply$ReqHour[i]<=23){
    demand_supply[i,"Time_Slot"]<-"Late Evening"
  }
  else if(demand_supply$ReqHour[i]>9 && demand_supply$ReqHour[i]<=16){
    demand_supply[i,"Time_Slot"]<-"Day Time"
  }
  else if(demand_supply$ReqHour[i]>=04 && demand_supply$ReqHour[i]<=9){
    demand_supply[i,"Time_Slot"]<-"Early Morning"}
  else{
    demand_supply[i,"Time_Slot"]<-"Late Night"}
}
#Total Gap
sum(demand_supply$Gap)
#3914
#total supply
sum(demand_supply$Supply)
#Ans 2813
#Total Demand
sum(demand_supply$Demand)
#6745
#time slots when the highest gap exists
ggplot(demand_supply,aes(ReqHour,Gap,col=Time_Slot))+geom_point(size=4)+ggtitle("Shortage of cars-Hourly Basis")+xlab("Requested Hour")
#Scatter plot is created to see how the gap varies with the requested hour and time slot is used a colour to see for which cluster gap is the highest
#As blue dots are there in the upper right corner,it signifies that gap is highest in the late evening time slot
#---------------------------------only Airport to city Data-----------------------------------------
Uber_Request_Airport <- subset(Uber_Request,Uber_Request$Pickup.point=="Airport")
summary(factor(Uber_Request_Airport$Status))
#Analysing the time slot when the problem is maximum at the airport
ggplot(Uber_Request_Airport, aes(x=ReqHour, fill=Status )) + geom_histogram(stat = "count")+ggtitle("Airport to City-Hourly Basis")
#A histogram is plotted to see for which interval of requested hour the problem is highest and status is used as fill to see what kind of problem exists in that interval
ggplot(Uber_Request_Airport, aes(x=Time_Slot, fill=Status )) + geom_histogram(stat = "count")+ggtitle("Airport to City-Based on Time Slot")
#A stacked histogram is plotted to know the request in each time slot and status as fill to know the problemetic area in each slot
#Though the number of trips getting cancelled from airport to city are less but 
#maximum demand of cars is between 5-11 P.M and at the same time there are less number of cars available
#Percentage of total number of requests raised vs cars not available or cancelled at the airport
sum(is.na(Uber_Request_Airport$DropHour))/nrow(Uber_Request_Airport)*100
1911/3238*100
#Ans.59.01791%
#Total Supply at the airport
sum(Uber_Request_Airport$Status=="Trip Completed")
#Ans.1327
#Demand at the airport
nrow(Uber_Request_Airport)
#3238
#For airport-city request the gap is the most severe in the Late Evening time slot
#------------------------------only  city to Airport Data-------------------------------------
Uber_Request_City <- subset(Uber_Request,Uber_Request$Pickup.point=="City")
summary(factor(Uber_Request_City$Status))
#Analysing the time slot when the problem is maximum in the city
ggplot(Uber_Request_City, aes(x=ReqHour, fill=Status )) + geom_histogram(stat = "count")+ggtitle("City to Airport")
#A histogram is plotted to see for which interval of requested hour the problem is highest and status is used as fill to see what kind of problem exists in that interval
ggplot(Uber_Request_City, aes(x=Time_Slot, fill=Status )) + geom_histogram(stat = "count")+ggtitle("City to Airport-Based on Time Slot")
#A stacked histogram is plotted to know the request in each time slot and status as fill to know the problemetic area in each slot
#The graph shows that most of the trips from city to airport are cancelled between 5-10 A.M
#Percentage of total number of requests raised vs cars not available or cancelled in the city
sum(is.na(Uber_Request_City$DropHour))
2003/3507*100
#Ans.57.11434%

#Total Supply in the city
sum(Uber_Request_City$Status=="Trip Completed")
#Ans.1504
#Demand in the city
nrow(Uber_Request_City)
#3507

#write file for tableau 
write.csv(Uber_Request,file="Uber_Request.csv")
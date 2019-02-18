############import all the neccessary libraries##########
library(ggplot2)
library(caTools)
library("MASS")
library(car)
library(e1071)
library(caret)
library(ROCR)
require(dplyr)
library(cowplot)
library(stringr)
library(lubridate)
library(corrplot)

setwd("E:/DATA SCIENCE/Predictive_Analytics/HR_Analytics_Case_Study/PA-I_Case_Study_HR_Analytics")
##################load the data##################################
general_data <- read.csv('general_data.csv',stringsAsFactors = FALSE)
employee_survey_data <-read.csv('employee_survey_data.csv',stringsAsFactors = FALSE)
manager_survey_data <- read.csv('manager_survey_data.csv',stringsAsFactors = FALSE)
in_time <- read.csv('in_time.csv',stringsAsFactors = FALSE)
out_time <- read.csv('out_time.csv',stringsAsFactors = FALSE)

#nrow(general_data)
#Info: all the data frames have  4410 no of rows

#------------------------------Collate the data together in one single file-------------------------------
length(unique(tolower(general_data$EmployeeID)))    # 4410, confirming EmployeeID is key 
length(unique(tolower(employee_survey_data$EmployeeID))) # 4410, confirming EmployeeID is key
length(unique(tolower(manager_survey_data$EmployeeID))) # 4410, confirming EmployeeID is key

setdiff(general_data$EmployeeID,employee_survey_data$EmployeeID) # Identical EmployeeID across these datasets
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID) # Identical EmployeeID across these datasets

#merge the data frames
combined <- merge(x= general_data,y=employee_survey_data,by ="EmployeeID",all = TRUE)
combined <- merge(x= combined,y=manager_survey_data,by="EmployeeID",all = TRUE)

nrow(combined) #check row count after merge


#str(combined)
#View(names(combined))


########################clean the data####################
which(is.na(combined))


colSums(is.na(combined))

#NumCompaniesWorked,TotalWorkingYears(considered number of companies worked and years at a COMPANY)
#if No of comapnies in 1 or 0 then yearsAt campany else 11 as its the average)
#NUmCompaniesWorked with Median value i.e 2
#-------------------------------------NumCompaniesWorked has 19 NA Values-----------------------------------------------------------------------------
combined$NumCompaniesWorked <- ifelse(is.na(combined$NumCompaniesWorked), median(combined$NumCompaniesWorked,na.rm=TRUE),
                                      combined$NumCompaniesWorked)

#---------------------------------------TotalWorkingYears has 9 NA Values----------------------------------------------------------------------------
combined$TotalWorkingYears <- ifelse(is.na(combined$TotalWorkingYears), ifelse(combined$NumCompaniesWorked == 0|combined$NumCompaniesWorked == 1,
                                                                               combined$YearsAtCompany,ifelse(is.na(combined$TotalWorkingYears),11,combined$TotalWorkingYears))
                                     ,combined$TotalWorkingYears)

#---------------------------------------WorkLifeBalance has 38 NA Values---------------------------------------------------------------------------------------
combined$WorkLifeBalance <-ifelse(is.na(combined$WorkLifeBalance),median(combined$WorkLifeBalance,na.rm=TRUE),combined$WorkLifeBalance)

#-------------------------------------JobSatisfaction has 20 NA Values--------------------------------------------------------------------------------------------
combined$JobSatisfaction <-ifelse(is.na(combined$JobSatisfaction),median(combined$JobSatisfaction,na.rm=TRUE),combined$JobSatisfaction)

#-----------------------------------------EnvironmentSatisfaction has 25 NA Values------------------------------------------------------------------------------------
combined$EnvironmentSatisfaction <-ifelse(is.na(combined$EnvironmentSatisfaction),median(combined$EnvironmentSatisfaction,na.rm=TRUE),combined$EnvironmentSatisfaction)


#todo: move it to after na removal
combined$Attrition <- as.factor(combined$Attrition)
combined$BusinessTravel <- as.factor(combined$BusinessTravel)
combined$Department <-as.factor(combined$Department)
combined$Education <-as.factor(combined$Education)
combined$EducationField <-as.factor(combined$EducationField)
combined$Gender<-as.factor(combined$Gender)
combined$JobLevel <-as.factor(combined$JobLevel)
combined$JobRole <-as.factor(combined$JobRole)
combined$MaritalStatus <-as.factor(combined$MaritalStatus)
combined$StockOptionLevel <-as.factor(combined$StockOptionLevel)
combined$EnvironmentSatisfaction <-as.factor(combined$EnvironmentSatisfaction)
combined$JobSatisfaction <-as.factor(combined$JobSatisfaction)
combined$WorkLifeBalance <-as.factor(combined$WorkLifeBalance)
combined$JobInvolvement <-as.factor(combined$JobInvolvement)
combined$PerformanceRating <-as.factor(combined$PerformanceRating)


#EmployeeCount
sum(is.na(combined$EmployeeCount)) 
sum((combined$EmployeeCount>1)) 

#Over18
sum( is.na(combined$Over18))
sum((combined$Over18!='Y'))
#standard hours
sum(is.na(combined$StandardHours!=8))

#not necessary can remove these columns
combined <- combined[,-c(9,16,18)]

############################################################

##Merging the derived varibales from in & out time to combined data frame

names(in_time)[1]<-"EmployeeID"
names(out_time)[1] <- "EmployeeID"
names(in_time) <- str_replace(names(in_time),"X","in_")
names(out_time) <- str_replace(names(out_time), "X", "out_")


in_time_df <- subset(in_time, select = -which(names(in_time)%in%colnames(in_time)[colSums(is.na(in_time)) == nrow(in_time)]))
out_time_df <- subset(out_time, select = -which(names(out_time)%in%colnames(out_time)[colSums(is.na(out_time)) == nrow(out_time)]))

####There are 12 holidays in a year..
### Finding the leave  day of employee
ofc_hrs_metrics <- setNames(data.frame(matrix(ncol = 5, nrow = nrow(in_time_df))), c("EmployeeID", "leave_cnt", "Avg_work_hrs_year", "Avg_work_hrs_mnth","Avg_work_hrs_week"))
ofc_hrs_metrics$EmployeeID <- in_time_df$EmployeeID
ofc_hrs_metrics$leave_cnt <- apply(in_time_df[,2:250],1, function(x) {sum(is.na(x))})


in_time_df[,-1] <- lapply(in_time_df[,-1], function(x) ymd_hms(x))
out_time_df[,-1] <- lapply(out_time_df[,-1], function(y) ymd_hms(y))
work_hrs <-(out_time_df[,-1]-in_time_df[,-1])
work_hrs <- sapply(work_hrs,as.numeric)
work_hrs <- round(work_hrs,2)

#converting work_hrs into a data frame,
work_hrs_df <- as.data.frame(work_hrs)

#######Dervied business metrics based on in & out time
ofc_hrs_metrics$Avg_work_hrs_year <- apply(work_hrs_df,1,function(x) {round(mean(x,na.rm = T),2)})
ofc_hrs_metrics$Avg_work_hrs_mnth <- apply(work_hrs_df,1,function(x) {round(sum(x,na.rm = T)/12,2)})
ofc_hrs_metrics$Avg_work_hrs_week <- apply(work_hrs_df,1,function(x) {round(sum(x,na.rm=T)/(250/5),2)})

combined <- merge(x= combined,y=ofc_hrs_metrics,by ="EmployeeID",all = TRUE)



#############Outlier Treatement##############################
#TODO: Test  with removing outliers and without removing outliers to see if it has positive effect or negative effect
#---------------------------- Histogram and Boxplots for numeric variables and checking outliers in numeric values---------------------
 box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                   axis.ticks=element_blank(), axis.text=element_blank())

# box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
#                     axis.ticks.y=element_blank(), axis.text.y=element_blank(),
#                     legend.position="none")

outlier_treatment <- function(df,col,lv,hv){
  quantiles<-quantile(df[,col], c(lv, hv), na.rm =TRUE)
  for(i in 1:nrow(df)){
    if(df[i,col]< quantiles[[1]])
    {
      df[i,col] <-quantiles[[1]]
    }
    if(df[i,col]> quantiles[[2]]){
      df[i,col] <-quantiles[[2]]
    }
    if(df[i,col]>= quantiles[[1]] &&  df[i,col]<= quantiles[[2]]){
      df[i,col] <- df[i,col]
    }
  }
  return(df[,col])
}

plot_grid(ggplot(combined, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
           ggplot(combined, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1)
#------------------------------------------------Removing outliers
combined$YearsAtCompany<-outlier_treatment(combined,"YearsAtCompany",0.05,0.92)
plot_grid(ggplot(combined, aes(YearsAtCompany))+ geom_histogram(binwidth = 10),
           ggplot(combined, aes(x="",y=YearsAtCompany))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1)

 plot_grid(ggplot(combined, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 20),
           ggplot(combined, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1)
#-------------------------------------------------Removing outliers
combined$YearsWithCurrManager<-outlier_treatment(combined,"YearsWithCurrManager",0.00,0.98)
plot_grid(ggplot(combined, aes(YearsWithCurrManager))+ geom_histogram(binwidth = 10),
           ggplot(combined, aes(x="",y=YearsWithCurrManager))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1)

plot_grid(ggplot(combined, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 20),
         ggplot(combined, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1) 
#---------------------------------------Removing outliers
combined$YearsSinceLastPromotion<-outlier_treatment(combined,"YearsSinceLastPromotion",0.001,0.92)
plot_grid(ggplot(combined, aes(YearsSinceLastPromotion))+ geom_histogram(binwidth = 10),
           ggplot(combined, aes(x="",y=YearsSinceLastPromotion))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1)

plot_grid(ggplot(combined, aes(TotalWorkingYears))+ geom_histogram(binwidth = 20),
           ggplot(combined, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1) 
#----------------------------------------Removing outliers
combined$TotalWorkingYears<-outlier_treatment(combined,"TotalWorkingYears",0.05,0.92)
plot_grid(ggplot(combined, aes(TotalWorkingYears))+ geom_histogram(binwidth = 10),
           ggplot(combined, aes(x="",y=TotalWorkingYears))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1)

plot_grid(ggplot(combined, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 20),
           ggplot(combined, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1) 
#--------------------------------------------Removing outliers
combined$TrainingTimesLastYear<-outlier_treatment(combined,"TrainingTimesLastYear",0.05,0.97)
plot_grid(ggplot(combined, aes(TrainingTimesLastYear))+ geom_histogram(binwidth = 10),
           ggplot(combined, aes(x="",y=TrainingTimesLastYear))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
           align = "v",ncol = 1)

#Boxplot showed no outlier, Nevertheless confirming it also with percentiles
sapply(combined[,c("YearsAtCompany","YearsWithCurrManager","YearsSinceLastPromotion","TotalWorkingYears","TrainingTimesLastYear")], 
       function(x) quantile(x,seq(0,1,.01),na.rm = T)) #no outlier




##############univariate analysis############################
#NOTE: The graphs are commented out to remove the over head while re - running code all time
# #age
# mean(combined$Age)
# max(combined$Age)



# min(combined$Age)
# sum( is.na(combined$Age))
# ggplot(combined,aes(combined$Age)) +geom_histogram()
# 
# #age data is found to be consistent between 18 to 60 with an average of 36
# 
# #attrition
# 
# sum( is.na(combined$Attrition))
# ggplot(combined,aes(combined$Attrition)) +geom_bar(aes(fill=combined$Attrition))
# 
# #business travel
# sum( is.na(combined$BusinessTravel))
# ggplot(combined,aes(combined$BusinessTravel)) +geom_bar(aes(fill=combined$BusinessTravel))
# 
# #department
# sum( is.na(combined$Department))
# ggplot(combined,aes(combined$Department)) +geom_bar(aes(fill=combined$Department))
# 
# 
# #DistanceFromHome
# 
# mean(combined$DistanceFromHome)
# max(combined$DistanceFromHome)
# min(combined$DistanceFromHome)
# 
# sum( is.na(combined$DistanceFromHome))
# ggplot(combined,aes(combined$DistanceFromHome)) +geom_histogram(binwidth = 4)
# 
# 
# #Education
# sum(is.na(combined$Education)) 
# ggplot(combined,aes(combined$Education)) +geom_bar(aes(fill=combined$Education))+scale_fill_discrete(labels=c("Below college","College","Bachelors","Masters","Doctor"))
# #most of them are bachelor degree holders and masters 
# 
# #EducationField
# sum(is.na(combined$EducationField)) 
# ggplot(combined,aes(combined$EducationField)) +geom_bar(aes(fill=combined$EducationField))
# 
# 
# #Gender
#  sum(is.na(combined$Gender)) 
#  ggplot(combined,aes(combined$Gender)) +geom_bar(aes(fill=combined$Gender))
# 
#  #JobLevel
#  
#  sum(is.na(combined$JobLevel)) 
#  ggplot(combined,aes(combined$JobLevel)) +geom_bar(aes(fill=combined$JobLevel))
#  
#  #JobRole
#  
#  sum(is.na(combined$JobRole)) 
#  ggplot(combined,aes(combined$JobRole)) +geom_bar(aes(fill=combined$JobRole))
#  
#  #MaritalStatus
# 
#  sum(is.na(combined$MaritalStatus)) 
#  ggplot(combined,aes(combined$MaritalStatus)) +geom_bar(aes(fill=combined$MaritalStatus))
#  
# #quite a number of divorced people 
# 
#  #MonthlyIncome
#  mean(combined$MonthlyIncome)
#  max(combined$MonthlyIncome)
#  min(combined$MonthlyIncome)
#  
#  
#  sum( is.na(combined$MonthlyIncome))
#  ggplot(combined,aes(combined$MonthlyIncome)) +geom_histogram()
# 
#  #NumCompaniesWorked
#  sum( is.na(combined$NumCompaniesWorked))
#ggplot(combined,aes(combined$NumCompaniesWorked)) +geom_bar()
#  #TODO: remove na values. set to 0 ?
# 
#  
#  #PercentSalaryHike
#  mean(combined$PercentSalaryHike)
#  max(combined$PercentSalaryHike)
#  min(combined$PercentSalaryHike)
#  
#  #TODO get percentile info for percentage hike
#  sum( is.na(combined$PercentSalaryHike))
#  ggplot(combined,aes(combined$PercentSalaryHike)) +geom_histogram(binwidth = 1)
#  
#  #StockOptionLevel
#  str(combined)
#  sum(is.na(combined$StockOptionLevel)) 
#  ggplot(combined,aes(combined$StockOptionLevel)) +geom_bar(aes(fill=combined$StockOptionLevel))
#  
#  #TotalWorkingYears
# 
#  sum(is.na(combined$TotalWorkingYears)) 
#  #TODO see why total working years is na
#  ggplot(combined,aes(combined$TotalWorkingYears)) +geom_histogram()
#  
#  
#  #TrainingTimesLastYear
#  sum(is.na(combined$TrainingTimesLastYear)) 
#  ggplot(combined,aes(combined$TrainingTimesLastYear)) +geom_histogram(binwidth = 1)
#  
#  #YearsAtCompany
#  sum(is.na(combined$YearsAtCompany)) 
#  ggplot(combined,aes(combined$YearsAtCompany)) +geom_histogram()
#  
#  #YearsSinceLastPromotion
# 
#  sum(is.na(combined$YearsSinceLastPromotion)) 
#  ggplot(combined,aes(combined$YearsSinceLastPromotion)) +geom_histogram(binwidth = 1)
#  
#  #YearsWithCurrManager
#  sum(is.na(combined$YearsWithCurrManager)) 
#  ggplot(combined,aes(combined$YearsWithCurrManager)) +geom_histogram()
# 
#  
#  #EnvironmentSatisfaction
# 
#  sum(is.na(combined$EnvironmentSatisfaction)) 
#  ggplot(combined,aes(combined$EnvironmentSatisfaction)) +geom_bar(aes(fill=combined$EnvironmentSatisfaction))
#  #TODO remove na here and check for NA's
#  
#  #JobSatisfaction
# 
#  sum(is.na(combined$JobSatisfaction)) 
#  ggplot(combined,aes(combined$JobSatisfaction)) +geom_bar(aes(fill=combined$JobSatisfaction))
#  #TODO remove or check NA here
#  
#  #WorkLifeBalance
# 
#  sum(is.na(combined$WorkLifeBalance)) 
#  ggplot(combined,aes(combined$WorkLifeBalance)) +geom_bar(aes(fill=combined$WorkLifeBalance))
#  #TODO remove NA here 
#  
#  #JobInvolvement
# 
#  sum(is.na(combined$JobInvolvement)) 
#  ggplot(combined,aes(combined$JobInvolvement)) +geom_bar(aes(fill=combined$JobInvolvement))
#  
#  #PerformanceRating
#     
#  sum(is.na(combined$PerformanceRating)) 
#  ggplot(combined,aes(combined$PerformanceRating)) +geom_bar(aes(fill=combined$PerformanceRating))
 
 
 #TODO check if conversion of factors for ratings is required prior ?
 #because it might be required to convert again to numeric for glm function


##############################Uni variate analysis########################
# Barcharts for categorical features with stacked employee information
 bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                    legend.position="top")
 
  
 plot_grid(ggplot(combined, aes(x=BusinessTravel,fill=Attrition))+ geom_bar()+bar_theme1, 
           ggplot(combined, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
           ggplot(combined, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
           ggplot(combined, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
           ggplot(combined, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
           ggplot(combined, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
           align = "h") 
 
 
 plot_grid(ggplot(combined, aes(x=JobInvolvement,fill=Attrition))+ geom_bar()+bar_theme1, 
           ggplot(combined, aes(x=PerformanceRating,fill=Attrition))+ geom_bar()+bar_theme1,
           ggplot(combined, aes(x=EnvironmentSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
           ggplot(combined, aes(x=JobSatisfaction,fill=Attrition))+ geom_bar()+bar_theme1,
           ggplot(combined, aes(x=WorkLifeBalance,fill=Attrition))+ geom_bar()+bar_theme1,
           align = "h") 


 
 ############################################################
 
 #############Multi variate analysis#############################

#hike vs manager and self rating
#years in company with attrition

g <- ggplot(combined,aes(x=JobSatisfaction,..count..,fill=Attrition))+geom_bar(position = "stack")
g + facet_wrap(~combined$JobRole)+labs(title="Job Satisfaction across Dfferent Job Roles", x="Job Satisfaction")
# 

h <- ggplot(combined,aes(x=WorkLifeBalance,..count..,fill=Attrition))+geom_bar(position = "dodge")
h + facet_wrap(~combined$JobRole)+labs(title="Work-Life Balance With Different Job Roles", x="Work-Life balance")
 
h + facet_wrap(~combined$Gender)+labs(title="Work-Life Balance in Gender", x="Work-Life balance")
#ggsave("WorkLifeBal.PNG")
 
e <- ggplot(combined,aes(x=MonthlyIncome,..count..,fill=Attrition))+geom_histogram(position="stack")
e + facet_wrap(~combined$JobRole)+labs(title="Monthly Income across Different Job Roles", x="Monthly Income")
 
i <- ggplot(combined,aes(x=BusinessTravel,..count..,fill=Attrition))+geom_bar(position = "stack")
i + facet_wrap(~combined$JobRole)+labs(title="Business Travels for Different Job Roles", x="Business Travel")
 
k <- ggplot(combined,aes(x=TotalWorkingYears,..count..,fill=Attrition))+geom_bar(position = "stack")
k + facet_wrap(~combined$Education)+labs(title="Education and Work Experience on Attrition", x="Working Experience")
 
l <- ggplot(combined,aes(x=Education,..count..,fill=Attrition))+geom_bar(position = "dodge")
l + facet_wrap(~combined$JobRole)+labs(title="Education level and Job Role", x="Education Level")
 
m <- ggplot(combined,aes(x=PercentSalaryHike,..count..,fill=Attrition))+geom_bar(position = "stack")
m + facet_wrap(~combined$PerformanceRating)+labs(title="Hike per Rating Against Attrition", x="Performamce Rating")
# ggsave("Hike Per Rating.PNG")

#TODO: leave count vs attrition analysis

################Correlation plots####################
#1.Age
#2.Attrition_num
#3.DistanceFromHome
#4.Gender_num
#5.Education -- factor variables can not be considered in correlation matrix
#6.JobLevel --- factor
#7.MonthlyIncome
#8.Numcompaniesworked
#9.PercentSalaryHike
#10.StockOptionLevel
#11.TotalWorkingYears --> It contains some NA values which has to be imputed
#12.TrainingTimesLastYear --> It contains
#13.YearsAtCompany
#14.YearsSinceLastPromtion
#15.YearWithCurrManager
#16.EnvironmentSatisfaction
#17.JobSatisfaction
#18.WorkLifeBalance
#19.JobInvolvement
#20.PerformcancRating
#21.Avg_work_hrs_years
#22.Leave_cnt
#23.Avg_wrk_hrs_mnth
#24.Avg_wrk_hrs_week
#class(combined$JobSatisfaction)

#TotalWorkingYears_new = combined$TotalWorkingYears,
#NumCompaniesWorked = combined$NumCompaniesWorked,

attr_corr<-data.frame(Age=combined$Age,Attr_val = as.numeric( combined$Attrition),
                     MonthlyInc = as.numeric(combined$MonthlyIncome),
                     PercSalHike = combined$PercentSalaryHike,
                       YearsAtCompany = combined$YearsAtCompany,
                       YearsSinceLastPromotion = combined$YearsSinceLastPromotion,
                       YearsWithCurrManager=combined$YearsWithCurrManager,
                       Avg_work_hrs_years = combined$Avg_work_hrs_year,
                       leave_cnt = combined$leave_cnt,
                       Avg_work_hrs_mnth = combined$Avg_work_hrs_mnth,
                       Avg_work_hrs_week = combined$Avg_work_hrs_week,
                       NumCompaniesWorked = combined$NumCompaniesWorked,
                       TotalWorkingYears = combined$TotalWorkingYears
                       )
 
 
str(attr_corr)
 
 M<- cor(attr_corr)
 corrplot(M,method = "color")
 
 col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
 corrplot(M, method="color", col=col(200), 
          type="lower", order="hclust", 
          addCoef.col = "black", 
          tl.col="black", tl.srt=45, 
          # Combine with significance
          insig = "blank", 
          # hide correlation coefficient on the principal diagonal
          number.cex = 0.6, tl.cex = 0.9,
          diag=FALSE)


 
 str(combined)
############################################################
 
 
########derive from categorical variables and feature scaling#####################
 #TODO: create new variables based on in time and out time
 
 #derived variable
 
 #TODO: new variables either from binning or some other intution from data
 
 #data variables for all trabnsformations. Combined will remain as master data for future references

 data <-  combined[,-1] 
#remove employee id at last Just in case if it is used to identify records

#########feature scaling
data$Age<-scale(data$Age)
data$DistanceFromHome<-scale(data$DistanceFromHome)
data$MonthlyIncome<-scale(data$MonthlyIncome)
data$NumCompaniesWorked<-scale(data$NumCompaniesWorked)
data$PercentSalaryHike<-scale(data$PercentSalaryHike)
data$TotalWorkingYears<-scale(data$TotalWorkingYears)
data$TrainingTimesLastYear<-scale(data$TrainingTimesLastYear)
data$YearsAtCompany<-scale(data$YearsAtCompany)
data$YearsSinceLastPromotion<-scale(data$YearsSinceLastPromotion)
data$YearsWithCurrManager<-scale(data$YearsWithCurrManager)
data$leave_cnt<-scale(data$leave_cnt)
data$Avg_work_hrs_mnth<-scale(data$Avg_work_hrs_mnth)
data$Avg_work_hrs_year<-scale(data$Avg_work_hrs_year)
data$Avg_work_hrs_week<-scale(data$Avg_work_hrs_week)

# categorical variables transform

data$Attrition <- ifelse(data$Attrition=="Yes",1,0)
data$Gender <- ifelse(data$Gender=="Male",1,0)

data$PerformanceRating <- ifelse(data$PerformanceRating=="4",1,0)

str(data)
categorical_variables_index <- c(3,4,6,7,9,10,11,15,21,22,23,24)
names(data[,categorical_variables_index])
data_numeric <- data[,-categorical_variables_index]
data_categorical <- data[,categorical_variables_index]
str(data_numeric)
str(data_categorical)


str(data_categorical)
dummies <- data.frame(sapply(data_categorical, function(x) data.frame(model.matrix(~x-1,data =data_categorical))[,-1]))
str(dummies)
summary(dummies$JobInvolvement.x4)
data <- cbind(data_numeric,dummies)
############################################################


##################model building##############################
# splitting the data between train and test
set.seed(100)

indices = sample.split(data$Attrition, SplitRatio = 0.7)
train = data[indices,]
test = data[!(indices),]

########################################################################
# Logistic Regression: 

model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) 

#intial model from step AIC function
model_2<- stepAIC(model_1, direction="both")

#model 2
# glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
#       NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear + 
#       YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year + 
#       BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
#       Department.xResearch...Development + Department.xSales + 
#       Education.x3 + Education.x4 + Education.x5 + EducationField.xOther + 
#       JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director + 
#       JobRole.xResearch.Scientist + JobRole.xSales.Executive + 
#       MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 + 
#       EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 + 
#       JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 + 
#       WorkLifeBalance.x2 + WorkLifeBalance.x3 + WorkLifeBalance.x4 + 
#       JobInvolvement.x3, family = "binomial", data = train)
summary(model_2)
vif(model_2)

#remove WorkLifeBalance.x4

model_3 <- glm(formula = Attrition ~ Age + DistanceFromHome + MonthlyIncome + 
                     NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                     YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                     BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                     Department.xResearch...Development + Department.xSales +
                     Education.x3 + Education.x4 + Education.x5 + EducationField.xOther +
                     JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                     JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                     MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                     EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                     JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                     WorkLifeBalance.x2 + WorkLifeBalance.x3+
                     JobInvolvement.x3, family = "binomial", data = train)
summary(model_3)
vif(model_3)
#remove monthly income
model_4 <- glm(formula = Attrition ~ Age + DistanceFromHome +  
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                 YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 Department.xResearch...Development + Department.xSales +
                 Education.x3 + Education.x4 + Education.x5 + EducationField.xOther +
                 JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3+
                 JobInvolvement.x3, family = "binomial", data = train)
summary(model_4)
vif(model_4)

#remove x other

model_5 <- glm(formula = Attrition ~ Age + DistanceFromHome +  
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                 YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 Department.xResearch...Development + Department.xSales +
                 Education.x3 + Education.x4 + Education.x5 + 
                 JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3+
                 JobInvolvement.x3, family = "binomial", data = train)
summary(model_5)
vif(model_5)

#remove Education.x5
model_6 <- glm(formula = Attrition ~ Age + DistanceFromHome +  
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                 YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 Department.xResearch...Development + Department.xSales +
                 Education.x3 + Education.x4 + 
                 JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3+
                 JobInvolvement.x3, family = "binomial", data = train)
summary(model_6)
vif(model_6)
#remove Education.x3
model_7 <- glm(formula = Attrition ~ Age + DistanceFromHome +  
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                 YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 Department.xResearch...Development + Department.xSales  + Education.x4 + 
                 JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3+
                 JobInvolvement.x3, family = "binomial", data = train)
summary(model_7)
vif(model_7)
#remove Education.x4
model_8 <- glm(formula = Attrition ~ Age + DistanceFromHome +  
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                 YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 Department.xResearch...Development + Department.xSales  +  
                 JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle + StockOptionLevel.x1 + EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3+
                 JobInvolvement.x3, family = "binomial", data = train)
summary(model_8)
vif(model_8)

#remove stockoption x 
model_9 <- glm(formula = Attrition ~ Age + DistanceFromHome +  
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                 YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 Department.xResearch...Development + Department.xSales  +  
                 JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3+
                 JobInvolvement.x3, family = "binomial", data = train)
summary(model_9)
vif(model_9)
# remove distance from home
model_10 <- glm(formula = Attrition ~ Age +   
                 NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                 YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                 BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                 Department.xResearch...Development + Department.xSales  +  
                 JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                 JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                 MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                 EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                 JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                 WorkLifeBalance.x2 + WorkLifeBalance.x3+
                 JobInvolvement.x3, family = "binomial", data = train)

summary(model_10)
vif(model_10)
#remove job involvement

model_11 <- glm(formula = Attrition ~ Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                  Department.xResearch...Development + Department.xSales  +  
                  JobLevel.x2 + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                  MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_11)
vif(model_11)


#remove job level
model_12 <- glm(formula = Attrition ~ Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                  Department.xResearch...Development + Department.xSales   + JobRole.xLaboratory.Technician + JobRole.xResearch.Director +
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                  MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_12)
vif(model_12)
# remove technician

model_13 <- glm(formula = Attrition ~ Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                  Department.xResearch...Development + Department.xSales   +  JobRole.xResearch.Director +
                  JobRole.xResearch.Scientist + JobRole.xSales.Executive +
                  MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_13)
vif(model_13)

# remove scientist
model_14 <- glm(formula = Attrition ~ Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                  Department.xResearch...Development + Department.xSales   +  JobRole.xResearch.Director +
               JobRole.xSales.Executive +
                  MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  WorkLifeBalance.x2 + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_14)
vif(model_14)

# remove WorkLifeBalance.x2
model_15 <- glm(formula = Attrition ~ Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                  Department.xResearch...Development + Department.xSales   +  JobRole.xResearch.Director +
                  JobRole.xSales.Executive +
                  MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                   + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_15)
vif(model_15)

# remove JobRole.xResearch.Director
model_16 <- glm(formula = Attrition ~ Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                  Department.xResearch...Development + Department.xSales   +   
                  JobRole.xSales.Executive +
                  MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_16)
vif(model_16)

# remove JobRole.xSales.Executive
model_17 <- glm(formula = Attrition ~ Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                  Department.xResearch...Development + Department.xSales   +   
                   +
                  MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 + JobSatisfaction.x3 + JobSatisfaction.x4 +
                  + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_17)
vif(model_17)

# remove JobSatisfaction.x3
model_18 <- glm(formula = Attrition ~ Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                  Department.xResearch...Development + Department.xSales   +   
                  +
                  MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                  JobSatisfaction.x2 +JobSatisfaction.x4 +
                  + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_18)
vif(model_18)

# remove JobSatisfaction.x2
model_19 <- glm(formula = Attrition ~ Age +   
                  NumCompaniesWorked + TotalWorkingYears + TrainingTimesLastYear +
                  YearsSinceLastPromotion + YearsWithCurrManager + Avg_work_hrs_year +
                  BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely +
                  Department.xResearch...Development + Department.xSales   +   
                  +
                  MaritalStatus.xSingle +  EnvironmentSatisfaction.x2 +
                  EnvironmentSatisfaction.x3 + EnvironmentSatisfaction.x4 +
                   +JobSatisfaction.x4 +
                  + WorkLifeBalance.x3, family = "binomial", data = train)

summary(model_19)
vif(model_19)





final_model <-model_19
############################################################



###################################model evaluation##########

# check parameters like Accuracy , sensitivity, specificity, Gain and lift charts , ks statistics etc

### Test Data ####

#predicted probabilities of Attrition for test data

test_pred = predict(final_model, type = "response",newdata = test)

summary(test_pred)
test$prob <- test_pred
#View(test)

detach("package:InformationValue", unload=TRUE)
test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition==1,"Yes","No"))
table(test_actual_attrition,test_pred_attrition)


test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))



test_conf <- confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf

# Let's Choose the cutoff value. 
# 

# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.003575 to 0.812100 for plotting and initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

detach("package:InformationValue", unload=TRUE)
cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]


# Let's choose a cutoff value of 0.161 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=cutoff, "Yes", "No"))

conf_final <- confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")
conf_final
acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

acc

sens

spec

#View(test)
##################################################################################################
### KS -statistic - Test Data ######

test_cutoff_attrition <- ifelse(test_cutoff_attrition=="Yes",1,0)
test_actual_attrition <- ifelse(test_actual_attrition=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_attrition, test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)
View(ks_table_test)

############################################################

####################################################################
# Lift & Gain Chart 

# plotting the lift chart

# Loading dplyr package

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

attrition_decile = lift(test_actual_attrition ,test_pred, groups = 10)
attrition_decile

#KS static bases on Gain and Lift

#Gain chart
ggplot(attrition_decile,aes(x = bucket,y=Gain))+geom_line(color="brown")+scale_x_discrete(limits=c(1:10)) +
scale_y_continuous(breaks = c(0,20,30,40,50,60,70,80,90,100,120))+ geom_point() +
labs(title="Gain Chart")


#Lift chart
ggplot(attrition_decile,aes(x = bucket,y=Cumlift))+geom_line(color="blue")+scale_x_discrete(limits=c(1:10))+
 geom_point() + labs(title="Lift Chart")

library(InformationValue)
ks_plot(test_actual_attrition, test_cutoff_attrition)
detach("package:InformationValue", unload=TRUE) 


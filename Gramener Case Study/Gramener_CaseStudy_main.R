#------------------------------------------------------------------------------
# Authors : 
## Ishita Aggarwal
## Muralidharan Srinivasan
## Sundaresan V
## Venkatesh Duraisamy
# DESC : 
## When the company receives a loan application, the company has to make 
## a decision for loan approval based on the applicant's profile. 
## Use EDA methods to understand how consumer attributes and loan attributes 
## influence the tendency of default.
## Date : 25-Jan-2018
#------------------------------------------------------------------------------
# Set working directory
setwd("E:/DATA SCIENCE/EDA_Case_Study/loan/")
#Add library
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(readr)
library(scales)
library(treemap)
library(maps)
library(corrplot)
options(scipen=999)

# Load the data
LoanMaster <- read.csv("loan.csv",stringsAsFactors = F, header = T)
str(LoanMaster)



#------------------------------------------------------------------------------
###  Data clean up
#------------------------------------------------------------------------------
# Identify columns for clean up
summary(LoanMaster)
#Find duplicate records 
nrow(LoanMaster)
nrow(distinct(LoanMaster))
# Clean up NA values (identiffied for removal)
LoanCleaned<-LoanMaster[, apply(LoanMaster, 2, function(x) !(sum(is.na(x))==nrow(LoanMaster)))] 
deleteColumns<-c("pymnt_plan","url","initial_list_status","policy_code","application_type","delinq_amnt","tax_liens","collections_12_mths_ex_med","chargeoff_within_12_mths","acc_now_delinq")
LoanCleaned<-LoanCleaned[, !(names(LoanCleaned) %in% deleteColumns)] 
View(deleteColumns)

# Validate fixed column issue
# ->No issues
# Missing value treatment

# Treat invalid values
#-> no issues


#------------------------------------------------------------------------------
###  Data preparation
#------------------------------------------------------------------------------
#------------------------Checking for duplicate records-----------------------------------------
length(unique(LoanCleaned$id))
# Format columns
# In "term" retain only months values
LoanCleaned$term<-as.numeric(gsub("^\\s*(\\d+)\\D*","\\1",LoanCleaned$term))

# In int_rate,revol_util remove %
LoanCleaned$int_rate<-as.numeric(gsub("\\%","",LoanCleaned$int_rate))
LoanCleaned$revol_util<-as.numeric(gsub("\\%","",LoanCleaned$revol_util))

# Convert dates to POSIX
dateConversion <- function(oriVec){
  modVec<-gsub("(.*)","01-\\1",oriVec)
  modVec=as.POSIXlt(modVec, format = "%d-%b-%y")
  return(modVec)
}
# Call dateConversion function to convert to POSIX
LoanCleaned$last_credit_pull_d<-as.Date(dateConversion(LoanCleaned$last_credit_pull_d),format = "%d-%b-%y")
LoanCleaned$next_pymnt_d<-as.Date(dateConversion(LoanCleaned$next_pymnt_d),format = "%d-%b-%y")
LoanCleaned$last_pymnt_d<-as.Date(dateConversion(LoanCleaned$last_pymnt_d),format = "%d-%b-%y")
LoanCleaned$earliest_cr_line<-as.Date(dateConversion(LoanCleaned$earliest_cr_line),format = "%d-%b-%y")
LoanCleaned$issue_d<-as.Date(dateConversion(LoanCleaned$issue_d), format = "%d-%b-%y")


#---------------Removing xx from zipcode---------------------------------------------------------
remove_string <- function(vector){
  corrected_vector <- as.numeric(gsub("[xx]","",vector))
  return(corrected_vector)
}
LoanCleaned$zip_code<-remove_string(LoanCleaned$zip_code)
# Fix employment length to closest accuracy 
# Exception ( 10+ yrs considered as 10 years)
LoanCleaned$emp_length<-  gsub("\\D","",LoanCleaned$emp_length)

#-------------------Imputing Employee Length---------------------------------------------
LoanCleaned$emp_length[which(LoanCleaned$emp_length=="")]<- NA
sum(is.na(LoanCleaned$emp_length))
LoanCleaned$emp_length
LoanCleaned$emp_length<-as.numeric(LoanCleaned$emp_length)
summary(LoanCleaned$emp_length,na.rm=T)
LoanCleaned$emp_length[(is.na(LoanCleaned$emp_length)==T)]<-median(LoanCleaned$emp_length,na.rm=T)

#-------------------Impute mths_since_last_delinq-----------------------------------------
sum(is.na(LoanCleaned$mths_since_last_delinq))
summary(LoanCleaned$mths_since_last_delinq,na.rm=T)
LoanCleaned$mths_since_last_delinq[(is.na(LoanCleaned$mths_since_last_delinq)==T)]<-0

#Derived Metrics
#-------------------------Creating Decision_Type based on loan status------------------------------
for(i in 1:nrow(LoanCleaned)){
  if(!(is.na(LoanCleaned$emp_length[i]))){
    if(LoanCleaned$loan_status[i]=="Fully Paid" | LoanCleaned$loan_status[i]=="Current"){
      LoanCleaned[i,"Decision_Type"]<-"Non-Default"
    }
    if(LoanCleaned$loan_status[i]=="Charged Off"){
      LoanCleaned[i,"Decision_Type"]<-"Default"
    }
  }}


#--------------------------------------------------------------------------------------


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


#LoanCleaned$MODIFY_inc<-outlier_treatment(LoanCleaned,"annual_inc",0.05,0.97)
#summary(LoanMaster$annual_inc)
#summary(LoanCleaned$MODIFY_inc)
#sum(is.na(LoanCleaned$MODIFY_inc))
#ToDo

#Factoring the categorical attributes
LoanCleaned$grade <- as.factor(LoanCleaned$grade)
LoanCleaned$sub_grade <- as.factor(LoanCleaned$sub_grade)
LoanCleaned$home_ownership <- as.factor(LoanCleaned$home_ownership)
LoanCleaned$emp_length <- as.factor(LoanCleaned$emp_length)
LoanCleaned$verification_status <- as.factor(LoanCleaned$verification_status)
LoanCleaned$loan_status <- as.factor(LoanCleaned$loan_status)
LoanCleaned$purpose <- as.factor(LoanCleaned$purpose)
LoanCleaned$addr_state <- as.factor(LoanCleaned$addr_state)
LoanCleaned$issue_d1 <- as.Date(LoanCleaned$issue_d, format = "%d-%b-%y")


#----------------Deriving year of the issue date--------------------------------------------------------
LoanCleaned$issue_year <- format(LoanCleaned$issue_d, "%Y")
#----------------------------------------------------------------------------------------------------------


#------------Univariate analysis on categorical variables
#---- Total Loan Defaulter percentage------------
# From this below plot we are identifying the overall defaulter percentage
ggplot(LoanCleaned,aes(x = Decision_Type)) +
    geom_bar(aes(y = (..count..)), fill = "gold") +
    geom_text(aes(y = ((..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = percent) +
    labs(title = "Loan Defaulter Distribution" , y = "Percent", x = "Decsion_Type" )+
      theme(axis.text.y=element_blank(), axis.ticks=element_blank(),
      axis.title.y=element_blank(),axis.text.x = element_text(angle = 60, hjust = 1)
    )


#---Loan volume  growth based on issue year-------------
#univariate analysis on Loan growth by Volume and Issue year
ggplot(LoanCleaned,aes(x=LoanCleaned$issue_year))+geom_bar(aes(fill=issue_year))+
  geom_text(stat='count',aes(label=..count..), vjust = -1) +
  labs(title = "Loan Issued on Various years", x ="Issue Year", y = " Volume") + theme_classic()


#Total Loan issued year on year  based on loan_amnt
#Univariate analysis on total loan amount issued over the years
ggplot(LoanCleaned, aes(x=issue_year, y=loan_amnt, fill=issue_year)) +  stat_summary(fun.y="sum", geom="bar") +
  labs(title = "Total Loan Issued on Various years ", x ="Issue Year", y = " Total Loan amount") + theme_classic()
#Conclusion : it is evident the lending club started getting more customers and their business increased in 2011.


#Loan Issued based on Verification Status
# Conclusion: The number of Not verified customer base is huge in comparison with other verification status. So in order to increase their competition in 
#market , they are taking risk by issuing lending to not verified profiles
ggplot(LoanCleaned,aes(x=LoanCleaned$verification_status))+geom_bar() + geom_text(stat = 'count',aes(label=..count..), vjust = -1) +
  labs(title = "Loan Issue on Various years", x ="Issue Year", y = " Total Loan amount") + theme_classic()

#Loan issue based on home ownership
#COnclusion : Customers belongs to the category of home ownership with rent or mortage are only applying more
#in number over the years
ggplot(LoanCleaned,aes(x=LoanCleaned$issue_year,fill=(LoanCleaned$home_ownership)))+geom_bar()+ 
  geom_text(stat='count',aes(label=..count..),position='stack',  vjust = 0.8,size = 2.5)

# loan_amt for purpose
purpose_df <- select (LoanCleaned, purpose, loan_amnt)
group_by_purp <- group_by(purpose_df, purpose)
purp_summary <- summarise(group_by_purp, volume = n(), avg_amt= sum(loan_amnt)/n())


#Market Basket analysis
#From this treep map it is evident that customer are taking loans for debt_consolidation,
#credit_card, others
treemap(purp_summary, index = "purpose", vSize = "volume", vColor = "avg_amt", 
        range = c(5000, 15000), type = "manual", palette = c("yellow", "green", 
                                                             "orange", "orange2", "firebrick"), algorithm = "pivotSize", sortID = "-size", 
        title = "Purposes of Loans", title.legend = "Avg_Amnt", fontfamily.labels = "serif", 
        fontsize.labels = 16, fontsize.legend = 10, fontface.labels = 1, position.legend = "bottom", 
        force.print.labels = T, border.col = "white")


#Histogram for loan distribution
# From this histogram, we can see there are more number of loan request
# for loan aount between 9000 to 1000 dollars
ggplot(LoanCleaned,aes(loan_amnt))+geom_histogram(bins = 50,color="slateblue", fill="slateblue2")+
  labs(x= "Loand Amount" , y ="Count",title="Loan Request based on Loan_amount") + theme_classic()

#Loan amount based on grade
# Customers who belong to B grade need more loans
ggplot(LoanCleaned, aes(x=grade, y=loan_amnt, fill=grade)) +  stat_summary(fun.y="sum", geom="bar") +
  labs(y ="Total Loan Amount",title="Total loan amount based on loan grade") + theme_classic()

#Loan growth by Grade and Term
# It is evident from this graph there are more people who are under 36 months tenure compare to 60 months.
# so it is very clear that of them need a loan for a shorter duration
ggplot(LoanCleaned, aes(x=issue_year , fill=LoanCleaned$grade))+geom_bar(stat = "count", position = "stack") + 
  facet_wrap(~term) +labs(title = "Loan Volume by Year against tenure", x ="Issued Year", y = "Volume") + theme_classic()


#Loan grades on Home ownership
#B grade people who have ownership of Rent and mortgage are driving the lending club business
mort_df <- filter(LoanCleaned,LoanCleaned$home_ownership == "MORTGAGE" | LoanCleaned$home_ownership == "RENT" | LoanCleaned$home_ownership == "OWN")
ggplot(mort_df,aes(x=grade))+geom_bar(aes(fill=grade))+facet_wrap(~home_ownership) + 
  labs(title = "Loan Issue on Home ownership", x ="Grade", y = "Number of Loans") + theme_classic()

#Loan Defaulter based on Home Ownership 
# Defaulter list is more in home owenersip category of Rent and Mortgage 
ggplot(mort_df,aes(x=home_ownership))+geom_bar(aes(fill=Decision_Type), position = "dodge")+
  labs(title = "Loan Defaulter based on Home Ownership", x ="Grade", y = "Number of Loans") + theme_classic()

#Loan Defaulter based on Busniess purpose
#Defaulter count is more in debt consolidation , credit cars and small business
ggplot(LoanCleaned,aes(x=purpose))+geom_bar(aes(fill=Decision_Type), position = "dodge")+
  labs(title = "Loan Defaulter based on Buisness purpose", x ="Grade", y = "Number of Loans") + theme_classic()+
  theme(axis.text.x = element_text(angle=60, hjust=1))


#To examine distribution of interest rates for different 
# Interest Rates are relatively low for A, B and C grade customers
ggplot(LoanCleaned, aes(x=grade, y= int_rate, fill=grade)) + geom_boxplot(outlier.colour = "blue") +
labs(title = "Interest Rate  details for different Grade", x ="Grade", y = "Interest Rate") + theme_classic()




#Univariate analysis on growth of Loan amount statewise
# Volumne of LOan issued based on state wise
LoanCleaned$region <- LoanCleaned$addr_state
LoanCleaned$region <- as.factor(LoanCleaned$region)
levels(LoanCleaned$region)<- c("alaska", "alabama","arkansas", "arizona", "california","colorado","connecticut","district of columbia","delaware","florida","georgia","hawaii","iowa","idaho","illinois","indiana","kansas","kentucky","louisiana","massachusetts","maryland","maine","michigan","minnesota","missouri","mississippi","montana","north carolina","north dakota","nebraska","new hampshire","new jersey","new mexico","nevada","new york","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","virginia","vermont","washington","wisconsin","west virginia","wyoming")

all_states <- map_data("state")
state_by_loan <- select(LoanCleaned, region)
summ_by_state <- summarise(group_by(state_by_loan, region),Value = n())
summ_by_state$region <- as.character(summ_by_state$region)

Total <- merge(all_states, summ_by_state, by="region")
p <- ggplot()
p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total$Value),colour="white") +
  scale_fill_continuous(low = "skyblue", high = "darkblue", guide="colorbar")+
  theme_bw()  + labs(fill = "Gradient of loan amount",title = "Heat Map of loan amount volume in all states", x="", y="")+
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())


#  Univariate analysis on Total Loan amount issued based on state wise
# California state is the place where most loan amount is lended
state_by_loan_amt <- select(LoanCleaned, region,loan_amnt)
summ_by_loan_state <- summarise(group_by(state_by_loan_amt, region),amount = sum(loan_amnt))
summ_by_loan_state$region <- as.character(summ_by_loan_state$region)

Total_loan_amt <- merge(all_states, summ_by_loan_state, by="region")
p <- ggplot()
p + geom_polygon(data=Total, aes(x=long, y=lat, group = group, fill=Total_loan_amt$amount),colour="white") +
scale_fill_continuous(low = "skyblue", high = "darkblue", guide="colorbar")+
  theme_bw()  + labs(fill = "Gradient of loan amount" 
                     ,title = "Heat Map of total loan amount in all states", x="", y="")+
  scale_y_continuous(breaks=c()) + scale_x_continuous(breaks=c()) + theme(panel.border =  element_blank())


# Segmented univariate analysis
#--------------Code for Segmented univariate & Business Driven--------------------------------------

# Segmented univariate analysis
#----------------------------------pub rec bankruptcies based on grade------------------------------------------------------------
#ggplot(LoanCleaned, aes(x=Risk_Potential, y=factor(pub_rec_bankruptcies), fill=Risk_Potential)) +  stat_summary(fun.y="sum", geom="bar") +
# labs(y ="Total pub rec bankruptcies",title="Total pub rec bankruptcies based on loan grade")

ggplot(filter(LoanCleaned,pub_rec_bankruptcies=="1" | pub_rec_bankruptcies=="2"), aes(x=factor(Decision_Type), fill=factor(pub_rec_bankruptcies))) + 
  geom_bar(position = "dodge") +  geom_text(stat='count',aes(label=..count..), vjust = -0.5)+
   labs(y ="Total pub rec bankruptcies",title="Total pub rec bankruptcies based on Decision Type") +
      theme_classic()
#---------------------------------Revolving Balance based on Risk Potential----------------------------------------------------------
ggplot(LoanCleaned, aes(x=Decision_Type, y=revol_bal, fill=sub_grade)) +  stat_summary(fun.y="sum", geom="bar") +
  labs(y ="Total Revolving Balance",title="Total Revolving Balance based on Risk Potential") +
    theme_classic()

ggplot(LoanCleaned, aes(x=grade, y=(dti*annual_inc)/100, fill=grade)) +  stat_summary(fun.y="sum", geom="bar") +
  labs(y ="Total debt",title="Total debt based on loan grade") + theme_classic()
#-------------------------Creating Risk_Potential based on loan status------------------------------
for(i in 1:nrow(LoanCleaned)){
  if(!(is.na(LoanCleaned$grade[i]))){
    if(LoanCleaned$grade[i]=="A" | LoanCleaned$grade[i]=="B" |LoanCleaned$grade[i]=="C"){
      LoanCleaned[i,"Risk_Potential"]<-"High Risk"
    }
    if(LoanCleaned$grade[i]=="D" | LoanCleaned$grade[i]=="E"){
      LoanCleaned[i,"Risk_Potential"]<-"Potential Risk"
    }
    if(LoanCleaned$grade[i]=="F" |LoanCleaned$grade[i]=="G"){
      LoanCleaned[i,"Risk_Potential"]<-"Low Risk"
    }
  }}
#----------------------------------pub rec bankruptcies based on grade------------------------------------------------------------

ggplot(filter(LoanCleaned,pub_rec_bankruptcies=="1" | pub_rec_bankruptcies=="2"), aes(x=factor(Risk_Potential), fill=factor(pub_rec_bankruptcies))) + geom_bar() +
  geom_text(stat='count',aes(label=..count..), vjust = -0.5)+
  labs(y ="Total pub rec bankruptcies",title="Total pub rec bankruptcies based on Risk Potential")+
   theme_classic()
#---------------------------------Revolving Balance based on Risk Potential----------------------------------------------------------
ggplot(LoanCleaned, aes(x=Risk_Potential, y=revol_bal, fill=purpose)) +  stat_summary(fun.y="sum", geom="bar") +
  labs(y ="Total Revolving Balance",title="Total Revolving Balance based on Risk Potential") +
   theme_classic()

#-------------------------------- Grade based on Risk Potential ------------------------------------------------------------------------
ggplot(LoanCleaned, aes(x=grade , fill=(Risk_Potential)))+geom_bar(stat = "count", position = "stack") + 
   facet_wrap(~Decision_Type) +labs(title = "Analysis of Loan Status based on Grade", x =" Grade", y = "Volume") +
    theme_classic()

## Business driven
## Data driven


Fico_col<-c("id","revol_util","last_credit_pull_d","mths_since_last_record","mths_since_last_delinq","earliest_cr_line","dti","annual_inc","inq_last_6mths","revol_util","pub_rec","pub_rec_bankruptcies")
Fico_data<-LoanCleaned[ ,names(LoanCleaned) %in% Fico_col]
#-------Function to substitute blank values with NA value-----------------------------------------------------------------------------------------
remove_NA <- function(vector){
  vector[is.na(vector)==T]<- 0
  corrected_vector <- as.numeric(vector)
  return(corrected_vector)
}
Fico_data$mths_since_last_record<-remove_NA(Fico_data$mths_since_last_record)
Fico_data$mths_since_last_delinq<-remove_NA(Fico_data$mths_since_last_delinq)
Fico_data$pub_rec<-remove_NA(Fico_data$pub_rec)
Fico_data$pub_rec_bankruptcies<-remove_NA(Fico_data$pub_rec_bankruptcies)
Fico_data$revol_util<-remove_NA(Fico_data$revol_util)
Fico_data$revol_util<-as.integer(Fico_data$revol_util)


#------------Acoounting to 35%---------------
for(i in 1:nrow(LoanCleaned)){
  Fico_data[i,"Payment_History"]<- as.integer((as.numeric(Fico_data$pub_rec[i]))+(as.numeric(Fico_data$pub_rec_bankruptcies[i])))
  
}
for(i in 1:nrow(LoanCleaned)){
  Fico_data[i,"Payment_History_months"]<-as.integer((as.numeric(Fico_data$mths_since_last_delinq[i]))+(as.numeric(Fico_data$mths_since_last_record[i])))
  
}
#---------Accounting to 30%-----------------------
for(i in 1:nrow(LoanCleaned)){
  Fico_data[i,"Debt_History"]<- as.integer(as.numeric((Fico_data$dti[i])*((Fico_data$annual_inc[i])/1200)))
  
}
#--------------------Accounting to 15%-----------------------------------------
for(i in 1:nrow(LoanCleaned)){
  Fico_data[i,"Credit_History_Months"]<- as.integer((as.numeric(difftime(as.Date(Fico_data$last_credit_pull_d[i]), as.Date(Fico_data$earliest_cr_line[i]), 
                                                                         unit="weeks"))/52.25)*12)
}
Fico_data$Credit_History_Months<-remove_NA(Fico_data$Credit_History_Months)


#Based on Fico Model assigning Score points to Payment History
for(i in 1:nrow(LoanCleaned)){
  if(Fico_data$Payment_History[i]==0){
    Fico_data[i,"Payment_Points"]<-75
  }
  if( Fico_data$Payment_History_months[i]>=0 && Fico_data$Payment_History_months[i]<=5  && Fico_data$Payment_History[i]!=0){
    Fico_data[i,"Payment_Points"]<-10}
  if( Fico_data$Payment_History_months[i]>=6 && Fico_data$Payment_History_months[i]<=11 && Fico_data$Payment_History[i]!=0){
    Fico_data[i,"Payment_Points"]<-15}
  if( Fico_data$Payment_History_months[i]>=12 && Fico_data$Payment_History_months[i]<=23 && Fico_data$Payment_History[i]!=0){
    Fico_data[i,"Payment_Points"]<-23}
  if( Fico_data$Payment_History_months[i]>=24 && Fico_data$Payment_History[i]!=0){
    Fico_data[i,"Payment_Points"]<-55}
}

#Based on Fico Model assigning Score points to Outstanding Debit
for(i in 1:nrow(LoanCleaned)){
  if(Fico_data$revol_util[i]==0){
    Fico_data[i,"Debt_Points"]<-30
  }
  if( Fico_data$Debt_History[i]==0  && Fico_data$revol_util[i]!=0){
    Fico_data[i,"Debt_Points"]<-55}
  if( Fico_data$Debt_History[i]>=1 && Fico_data$Debt_History[i]<=99  && Fico_data$revol_util[i]!=0){
    Fico_data[i,"Debt_Points"]<-65}
  if( Fico_data$Debt_History[i]>=100 && Fico_data$Debt_History[i]<=499 && Fico_data$revol_util[i]!=0){
    Fico_data[i,"Debt_Points"]<-50}
  if( Fico_data$Debt_History[i]>=500 && Fico_data$Debt_History[i]<=749 && Fico_data$revol_util[i]!=0){
    Fico_data[i,"Debt_Points"]<-40}
  if( Fico_data$Debt_History[i]>=750 && Fico_data$Debt_History[i]<=999 && Fico_data$revol_util[i]!=0){
    Fico_data[i,"Debt_Points"]<-25}
  if( Fico_data$Debt_History[i]>=1000 && Fico_data$revol_util[i]!=0){
    Fico_data[i,"Debt_Points"]<-15}
}

#Based on Fico Model assigning Score points to Credit History
for(i in 1:nrow(LoanCleaned)){
  if(Fico_data$Credit_History_Months[i]<12){
    Fico_data[i,"Credit_Points"]<-12
  }
  if( Fico_data$Credit_History_Months[i]>=12 && Fico_data$Credit_History_Months[i]<=23){
    Fico_data[i,"Credit_Points"]<-35
  }
  if( Fico_data$Credit_History_Months[i]>=24 && Fico_data$Credit_History_Months[i]<=47){
    Fico_data[i,"Credit_Points"]<-60
  }
  if( Fico_data$Credit_History_Months[i]>=48){
    Fico_data[i,"Credit_Points"]<-75
  }
}
#Based on Fico Model assigning Score points to New Credit
for(i in 1:nrow(LoanCleaned)){
  if(Fico_data$inq_last_6mths[i]==0){
    Fico_data[i,"New_Credit_Points"]<-70
  }
  if( Fico_data$inq_last_6mths[i]==1){
    Fico_data[i,"New_Credit_Points"]<-60
  }
  if( Fico_data$inq_last_6mths[i]==2){
    Fico_data[i,"New_Credit_Points"]<-45
  }
  if( Fico_data$inq_last_6mths[i]==3){
    Fico_data[i,"New_Credit_Points"]<-25
  }
  if( Fico_data$inq_last_6mths[i]>=4){
    Fico_data[i,"New_Credit_Points"]<-20
  }
}
# % OF each score points 
for(i in 1:nrow(LoanCleaned)){
  Fico_data$Payment_Points_score[i]<- Fico_data$Payment_Points[i]*.35
  Fico_data$Debt_Points_score[i]<-Fico_data$Debt_Points[i]*.30
  Fico_data$Credit_Points_score[i]<-Fico_data$Credit_Points[i]*.15
  Fico_data$New_Credit_Points_score[i]<-Fico_data$New_Credit_Points[i]*.10
}
#Calculation of Fico Score
for(i in 1:nrow(LoanCleaned)){
  Fico_data$Fico_Score[i]<-(as.integer((Fico_data$Payment_Points_score[i]+Fico_data$Debt_Points_score[i]+Fico_data$Credit_Points_score[i]+Fico_data$New_Credit_Points_score[i])*10))
}
summary(Fico_data$Fico_Score)

#Categorizing the fico score
for(i in 1:nrow(Fico_data)){
  if((Fico_data$Fico_Score[i])>=250 && (Fico_data$Fico_Score[i])<=499){
    Fico_data[i,"Fico_category"]<-"Poor"
  }
  if((Fico_data$Fico_Score[i])>=500 && (Fico_data$Fico_Score[i])<=579){
    Fico_data[i,"Fico_category"]<-"Low"
  }
  if((Fico_data$Fico_Score[i])>=580 && (Fico_data$Fico_Score[i])<=618){
    Fico_data[i,"Fico_category"]<-"Average"
  }
  if((Fico_data$Fico_Score[i])>=620 && (Fico_data$Fico_Score[i])<=679){
    Fico_data[i,"Fico_category"]<-"Good"
  }
  if((Fico_data$Fico_Score[i])>=680 && (Fico_data$Fico_Score[i])<=800){
    Fico_data[i,"Fico_category"]<-"Excellent"
  }
}


LoanCleaned_Final <- select(Fico_data, id, Fico_Score, Fico_category)
LoanCleaned_Final <- merge(LoanCleaned,LoanCleaned_Final, by = "id")


ggplot(LoanCleaned_Final,aes(x=Fico_category))+geom_bar(aes(fill=Fico_category))+
  geom_text(stat='count',aes(label=..count..), vjust = -1) + theme_classic() +
   labs(title="Fico Category Derivation for all profiles", x=" Fico Category", y="Count")

# ------Bivariate analysis for  Continuous Variable correlation--------------------------------
#correlation Plot using Color Method
# we see that there is a positive correlation loan  amount and fund_amt
#similarly we see a positive correlation between loan_amt and installment
# And we see a positive correlation between total paymnt and reveived principal
loan_corr<-data.frame(loanamt=LoanCleaned_Final$loan_amnt,
                      intr=LoanCleaned_Final$int_rate,
                        fund= LoanCleaned_Final$funded_amnt, 
                        instlmnt = LoanCleaned_Final$installment,
                        revbal = LoanCleaned_Final$revol_bal,
                        rcvd_prncpl = LoanCleaned_Final$total_rec_prncp, 
                        total_payment = LoanCleaned_Final$total_pymnt,
                        rcvd_int = LoanCleaned_Final$total_rec_int)

M<- cor(loan_corr)
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



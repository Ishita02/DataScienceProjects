setwd("C:/Dhanilan/Learning/Upgrad/time series/case study")
#Load neccessary libraries
library(graphics)
library(forecast)
library(dplyr)
library(lubridate)
library(tseries)
require(graphics)

#read the data
data <- read.csv("Global Superstore.csv")



#TODO Insert the Code for finding the Top segments

"###############Find the Top two consistent Market Segment with profit##################"

globalstoreorders_df <- data

# Data Understanding #51290 obs of 24 variables
str(globalstoreorders_df)
summary(globalstoreorders_df)
head(globalstoreorders_df)
#------------------------------------#
# Section 3a: Data Preparation
#------------------------------------#

################################################################

# Analysis on NA values

################################################################
colSums(is.na(globalstoreorders_df))

# only Postal.Code has NA values. Since it is not a variable of interest hence ignoring the NA values


################################################################

# Section 3a - Data Preparation:
# Converting Order Date variable into Date format and order vauable
# Create 21 subsets based on 7 Market and 3 Segments
# Aggregrate subsets on Monthly Quantity, Quantity and Profit 
################################################################

globalstoreorders_df$Order.Date <-strptime(globalstoreorders_df$Order.Date,"%d-%m-%Y")
globalstoreorders_df$Month_Yr <- format(globalstoreorders_df$Order.Date, "%Y-%m")
globalstoreorders_df$Order.Date <- as.character(globalstoreorders_df$Order.Date)
# Creating Subsets for Market and Segments

#1. Subset for Market Africa:

# 1a Market Africa , Segment== Consumer
globalstoreorders_Africa_Consumers <- subset(globalstoreorders_df,(Market=="Africa"& Segment=="Consumer"))
# 1b Market Africa,Segment == Corporate
globalstoreorders_Africa_Corporate <- subset(globalstoreorders_df,(Market=="Africa"& Segment=="Corporate"))
# 1c Market Africa, Segment== Home Office
globalstoreorders_Africa_HomeOffice <- subset(globalstoreorders_df,(Market=="Africa"& Segment=="Home Office"))

#2. Subset for Market APAC:

# 2a Market APAC,Segment== Consumer
globalstoreorders_APAC_Consumers <- subset(globalstoreorders_df,(Market=="APAC"& Segment=="Consumer"))
# 2b Market APAC,Segment== Corporate
globalstoreorders_APAC_Corporate <- subset(globalstoreorders_df,(Market=="APAC"& Segment=="Corporate"))
# 2c Market APAC ,Segment ==Home Office
globalstoreorders_APAC_HomeOffice <- subset(globalstoreorders_df,(Market=="APAC"& Segment=="Home Office"))

#3. Subset for Market Canada:

# 3a Market Canada,Segment == Consumer
globalstoreorders_Canada_Consumers <- subset(globalstoreorders_df,(Market=="Canada"& Segment=="Consumer"))
# 3b Market Canada,Segment == Corporate
globalstoreorders_Canada_Corporate <- subset(globalstoreorders_df,(Market=="Canada"& Segment=="Corporate"))
# 3c Market Canada,Segment == Home Office
globalstoreorders_Canada_HomeOffice <- subset(globalstoreorders_df,(Market=="Canada"& Segment=="Home Office"))


#4. Subset for Market EMEA:

# 4a Market EMEA ,Segment == Consumer
globalstoreorders_EMEA_Consumers <- subset(globalstoreorders_df,(Market=="EMEA"& Segment=="Consumer"))
# 4b Market EMEA,Segment == Corporate
globalstoreorders_EMEA_Corporate <- subset(globalstoreorders_df,(Market=="EMEA"& Segment=="Corporate"))
# 4c Market EMEA, Segment == Home Office
globalstoreorders_EMEA_HomeOffice <- subset(globalstoreorders_df,(Market=="EMEA"& Segment=="Home Office"))

#5. Subset for Market EU:

# 5a Market EU ,Segment == Consumer
globalstoreorders_EU_Consumers <- subset(globalstoreorders_df,(Market=="EU"& Segment=="Consumer"))
# 5b Market EU,Segment ==Corporate
globalstoreorders_EU_Corporate <- subset(globalstoreorders_df,(Market=="EU"& Segment=="Corporate"))
# 5c Market EU,Segment == Home Office
globalstoreorders_EU_HomeOffice <- subset(globalstoreorders_df,(Market=="EU"& Segment=="Home Office"))

#6. Subset for Market LATAM:

# 6a Market EU,Segment == Consumer
globalstoreorders_LATAM_Consumers <- subset(globalstoreorders_df,(Market=="LATAM"& Segment=="Consumer"))
# 6b Market EU,Segment ==Corporate
globalstoreorders_LATAM_Corporate <- subset(globalstoreorders_df,(Market=="LATAM"& Segment=="Corporate"))
# 6c Market EU,Segment ==Home Office
globalstoreorders_LATAM_HomeOffice <- subset(globalstoreorders_df,(Market=="LATAM"& Segment=="Home Office"))

#7. Subset for Market US:

# 7a Market EU,Segment== Consumer
globalstoreorders_US_Consumers <- subset(globalstoreorders_df,(Market=="US"& Segment=="Consumer"))
# 7b Market EU,Segment== Corporate
globalstoreorders_US_Corporate <- subset(globalstoreorders_df,(Market=="US"& Segment=="Corporate"))
# 7c Market EU,Segment== Home Office
globalstoreorders_US_HomeOffice <- subset(globalstoreorders_df,(Market=="US"& Segment=="Home Office"))


####################################
# Aggregating on Time -
####################################
aggr_fun <- function(df){
  aggr_df <-df %>%
    group_by(Month_Yr)%>%
    summarise(monthlyQuantity=sum(Quantity),monthlySales=sum(Sales),monthlyProfit=sum(Profit))
}

# 1. Market: Africa
# 1a Segment: consumers
Africa_Consumers_aggregate <- aggr_fun(globalstoreorders_Africa_Consumers)
# 1b Segment: Corporate
Africa_Corporate_aggregate <- aggr_fun(globalstoreorders_Africa_Corporate)
# 1c Segment: Home Office
Africa_HomeOffice_aggregate <- aggr_fun(globalstoreorders_Africa_HomeOffice )

# 2. Market: APAC
# 2a Segment: consumers
APAC_Consumers_aggregate <- aggr_fun(globalstoreorders_APAC_Consumers)
# 2b Segment: Corporate
APAC_Corporate_aggregate <- aggr_fun(globalstoreorders_APAC_Corporate)
# 2c Segment: Home Office
APAC_HomeOffice_aggregate <- aggr_fun(globalstoreorders_APAC_HomeOffice)

# 3. Market: Canada
# 3a Segment: consumers
Canada_Consumers_aggregate <- aggr_fun(globalstoreorders_Canada_Consumers)
# 3b Segment: Corporate
Canada_Corporate_aggregate <- aggr_fun(globalstoreorders_Canada_Corporate)
# 3c Segment: Home Office
Canada_HomeOffice_aggregate <- aggr_fun(globalstoreorders_Canada_HomeOffice) 

# 4. Market: EMEA
# 4a Segment: consumers
EMEA_Consumers_aggregate <- aggr_fun(globalstoreorders_EMEA_Consumers)
# 4b Segment: Corporate
EMEA_Corporate_aggregate <- aggr_fun(globalstoreorders_EMEA_Corporate)
# 4c Segment: Home Office
EMEA_HomeOffice_aggregate <- aggr_fun(globalstoreorders_EMEA_HomeOffice)

# 5. Market: EU
# 5a Segment: consumers
EU_Consumers_aggregate <- aggr_fun(globalstoreorders_EU_Consumers)
# 5b Segment: Corporate
EU_Corporate_aggregate <- aggr_fun(globalstoreorders_EU_Corporate)
# 5c Segment: Home Office
EU_HomeOffice_aggregate <- aggr_fun(globalstoreorders_EU_HomeOffice)

# 6. Market: LATAM
# 6a Segment: consumers
LATAM_Consumers_aggregate <- aggr_fun(globalstoreorders_LATAM_Consumers)
# 6b Segment: Corporate
LATAM_Corporate_aggregate <- aggr_fun(globalstoreorders_LATAM_Corporate)
# 6c Segment: Home Office
LATAM_HomeOffice_aggregate <- aggr_fun(globalstoreorders_LATAM_HomeOffice)

# 7. Market:
# 7a Segment: consumers
US_Consumers_aggregate <- aggr_fun(globalstoreorders_US_Consumers)
# 7b Segment: Corporate
US_Corporate_aggregate <- aggr_fun(globalstoreorders_US_Corporate)
# 7c Segment: Home Office
US_HomeOff_aggregate <- aggr_fun(globalstoreorders_US_HomeOffice)

# Computing coefficient of variation of the Profit for 21 subsets

df.list <- list(Africa_Consumers_aggregate,Africa_Corporate_aggregate,Africa_HomeOffice_aggregate,APAC_Consumers_aggregate,APAC_Corporate_aggregate,
                APAC_HomeOffice_aggregate,Canada_Consumers_aggregate,Canada_Corporate_aggregate,Canada_HomeOffice_aggregate,
                EMEA_Consumers_aggregate,EMEA_Corporate_aggregate,EMEA_HomeOffice_aggregate,EU_Consumers_aggregate,EU_Corporate_aggregate,
                EU_HomeOffice_aggregate,LATAM_Consumers_aggregate,LATAM_Corporate_aggregate,LATAM_HomeOffice_aggregate,US_Consumers_aggregate,
                US_Corporate_aggregate,US_HomeOff_aggregate)
result <- lapply(df.list, function(x) round(sd(x$monthlyProfit,na.rm=TRUE)/mean(x$monthlyProfit,na.rm=TRUE),2) )

###################################################################
"    Name                      CV Value
Africa_Consumers_aggregate         1.32
Africa_Corporate_aggregate         1.78  
Africa_HomeOffice_aggregate        1.79
APAC_Consumers_aggregate           0.63
APAC_Corporate_aggregate           0.70
APAC_HomeOffice_aggregate          1.05
Canada_Consumers_aggregate         1.40      
Canada_Corporate_aggregate         1.55
Canada_HomeOffice_aggregate        2.24
EMEA_Consumers_aggregate           2.19
EMEA_Corporate_aggregate           4.47
EMEA_HomeOffice_aggregate          5.88
EU_Consumers_aggregate             0.62
EU_Corporate_aggregate             0.76
EU_HomeOffice_aggregate            1.12
LATAM_Consumers_aggregate          0.66
LATAM_Corporate_aggregate          0.81
LATAM_HomeOffice_aggregate         1.18
US_Consumers_aggregate             1.01
US_Corporate_aggregate             1.00
US_HomeOff_aggregate               1.10"

"###############END#####################################################################"
#### Consumer + APAC and Consumer and EU is the consistent Bucket to be considered for processing
#################################################################################

data$OrderDateConverted <-as.Date( data$Order.Date,"%d-%m-%Y")

#Get the Initial Date and then calculate no of months from start date
startDate <-  min(data$OrderDateConverted)
data$Month <- round( interval(startDate,data$OrderDateConverted)/months(1))+1

#Group by Segment, Market and Month and then calculate the Total Sales and profit
groupedData <- group_by(data ,.dots=  c("Month","Segment","Market"))
groupby <-  summarize(groupedData,TotalSales=sum(Sales),TotalQuantity=sum(Quantity))
#Create two buckets of the best segment
FirstBucket <-  filter(groupby,Segment=="Consumer" & Market=="APAC")
FirstBucket <- as.data.frame(FirstBucket)

SecondBucket <-  filter(groupby,Segment=="Consumer" & Market=="EU")
SecondBucket <- as.data.frame(SecondBucket)



#Split to Train data and validation set 
indata <-FirstBucket[1:43,]
outdata <- FirstBucket[44:49,]
SalesDataFrame1 <- select( indata,Month,TotalSales)
SalesDataFrame1 <- select( indata,Month,TotalSales)
SalesTimeSeries1 <- ts(SalesDataFrame1$TotalSales)
plot(SalesTimeSeries1)


#Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries <- stats::filter(as.data.frame( SalesTimeSeries1), 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(SalesTimeSeries1)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series
plot(smoothedseries)

#create a data frame out of smoothed series
smoothedseriesdf <- as.data.frame(cbind(SalesDataFrame1$Month, as.vector(smoothedseries)))
names( smoothedseriesdf)<-c("Month","TotalSales")

set.seed(100)
#since the smoothed series has a trend and seasonality , we try to fit a mulitplicative model
polynomial<- 2
lmfit <- lm(TotalSales ~ sin(0.5*Month) * poly(Month,polynomial) + cos(0.5*Month) * poly(Month,polynomial)
             + Month, data=smoothedseriesdf)
globalpred <- predict(lmfit)
plot(globalpred, col='red', type = "l")

#visualise the global prediction against the smoothed series
plot(smoothedseries)
par(new=TRUE)
plot(globalpred, col='red', type = "l")
# Calculating the Root of mean square
sqrt( (sum((smoothedseriesdf$TotalSales - globalpred)^2))/nrow(smoothedseriesdf))
#comparing various Root mean square decides to keep the polynomial as 2.



#remove the trend and seasonality from the smoothed series
localpred <- smoothedseries - globalpred
plot(localpred, col='red', type = "l")

#Do ACF and PCF to find auto correlation
acf(localpred)
acf(localpred, type="partial")

auto.arima(localpred)
#the ACF and PACF graph tells that it is ARMA(1,1)

# guessp, guessq - Initial guesses for p and q (from ACF/PACF plots)
# delta - window in which we want to change guessp and guessq to
# look for alternative ARMA models
# timeseries - the one we want to analyze

tryArma <- function(delta, guessp, guessq, timeseries) {
  df <- data.frame()
  # generate all possible ARMA models
  for (p in max(0,(guessp-delta)):(guessp+delta)) {
    for (q in max(0,(guessq-delta)):(guessq+delta)) {
      order <- c(p,0,q)
      # Fit a maximum likelihood ARMA(p,q) model
      armafit <- Arima(timeseries, order=order, method="ML")
      # Add the results to the dataframe
      df <- rbind(df, c(p, q, armafit$loglik, armafit$aic, armafit$aicc, armafit$bic))
    }
  }
  names(df) <- c("p","q","log.likelihood", "AIC", "AICc", "BIC")
  return(df)
}



df <- tryArma(2,1,1,localpred)

df
#decide arma 2,3
"p q log.likelihood      AIC     AICc      BIC
1  0 0      -481.2039 966.4078 966.6687 970.1915
2  0 1      -472.2095 950.4189 950.9522 956.0944
3  0 2      -463.8512 935.7024 936.6115 943.2697
4  0 3      -459.3528 928.7055 930.1009 938.1646
5  1 0      -475.3632 956.7263 957.2597 962.4018
6  1 1      -471.3756 950.7512 951.6603 958.3185
7  1 2      -463.8190 937.6380 939.0333 947.0971
8  1 3      -456.4323 924.8646 926.8646 936.2156
9  2 0      -463.0118 934.0236 934.9327 941.5909
10 2 1      -457.6837 925.3675 926.7628 934.8266
11 2 2      -457.6523 927.3046 929.3046 938.6555
12 2 3      -452.5526 919.1051 921.8368 932.3479
13 3 0      -457.1535 924.3069 925.7023 933.7660
14 3 1      -456.9779 925.9558 927.9558 937.3067
15 3 2      -456.9704 927.9409 930.6726 941.1836
16 3 3      -451.6858 919.3716 922.9716 934.5061"



#Create ARMA model of 2,3
armafit<- Arima(localpred, order=c(2,0,3), method="ML")
tsdiag(armafit)

#create residulal from ARMA fit
resi <- localpred-fitted(armafit)
plot(resi)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

"Test	P-value	  Null Hypothesis	                                            Result
ADF 	0.01	    Null hypothesis assumes that the series is not stationary	  Stationary
KPSS	0.1	      Null hypothesis assumes that the series is stationary	      Stationary"

#hence residual is white noise 

#Do Validation by predicting from month 43 to 49 and calculate MAPE
predictedForSixMonthArma <- stats::predict(armafit, n.ahead = 6)
predictedForSixMonthArmalm <-stats:: predict(lmfit,data.frame(Month =outdata$Month))
predictedForSixMonth <-as.data.frame(predictedForSixMonthArma$pred)[1,] +predictedForSixMonthArmalm


MAPE_class_dec <- accuracy(predictedForSixMonth,outdata[,4])[5]
MAPE_class_dec
#MAPE 27 %


#Classical decomposition is Completed


#Do auto ARMA 
autoarima <- auto.arima(SalesTimeSeries1)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise
resi_auto_arima <- smoothedseries - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)
#Results is White noise

#Also, let's evaluate the model using MAPE

fcast_auto_arima <-stats::predict(autoarima, n.ahead = 6)
MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata[,4])[5]
MAPE_auto_arima # 37 %

# We take classical decomposition result because MAPE was 25%
#classical Decomposition MAPE is Better and hence we go with the same.

# Predictions for Next 6 months

finalArma <- stats::predict(armafit, n.ahead = 12)
finalLmprediction <-stats:: predict(lmfit,data.frame(Month =c(50:55)))

finalpredictedForSixMonth <-as.data.frame(finalArma$pred)[7:12,] +finalLmprediction
finalpredictedForSixMonth

# 25423.97 26484.93 34279.03 47392.23 62694.03 76069.63 


# #Lastly, let's plot the predictions along with original values, to
fcast <- forecast(armafit,h=6)
plot(fcast)




#APAC Quantity


APACQ_indata <-FirstBucket[1:43,]
APACQ_outdata <- FirstBucket[44:49,]
APAC_Qtity <- select( APACQ_indata,Month,TotalSales,TotalQuantity)
APACQ_TS <- ts(APAC_Qtity$TotalQuantity)

plot(APACQ_TS)

w<-1
APACQ_SmthSeries <- stats::filter(as.data.frame(APACQ_TS),filter=rep(1/(2*w+1),(2*w+1)), method='convolution',sides=2)
APACQ_diff<-APACQ_SmthSeries[w+2] - APACQ_SmthSeries[w+1]
# 37.67
for (i in seq(w,1,-1)) {
  APACQ_SmthSeries[i] <- APACQ_SmthSeries[i+1] - APACQ_diff
}

n <- length(APACQ_SmthSeries)

APACQ_R_diff <- APACQ_SmthSeries[n-w] - APACQ_SmthSeries[n-w-1]

for (i in seq(n-w+1, n)) {
       APACQ_SmthSeries[i] <- APACQ_SmthSeries[i-1] + APACQ_diff
}

plot(APACQ_SmthSeries)

APACQ_smthedseries_df <- as.data.frame(cbind(APAC_Qtity$Month, as.vector(APACQ_SmthSeries)))
names( APACQ_smthedseries_df)<-c("Month","Quantity")

APACQ_lmfit <- lm(Quantity ~ Month,APACQ_smthedseries_df)
polynomial<- 2
APACQ_lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,polynomial) + cos(0.5*Month) * poly(Month,polynomial)
                  +             + Month, data=APACQ_smthedseries_df)

APACQ_glb_pred<- predict(APACQ_lmfit)

plot(APACQ_SmthSeries)

par(new=TRUE)
plot(APACQ_glb_pred, col='red', type = "l")

APACQ_localpred <- APACQ_SmthSeries - APACQ_glb_pred

plot(APACQ_localpred, col='red', type = "l")



acf(APACQ_localpred)

acf(APACQ_localpred, type="partial")
#Arma 2,3 Seems to be fit
AAPCQ_AAR<- auto.arima(APACQ_localpred)

AAPCQ_AAR
#Amra 2,3,1 from auto arma

tryArma <- function(delta, guessp, guessq, timeseries) {
  df <- data.frame()
  # generate all possible ARMA models
  for (p in max(0,(guessp-delta)):(guessp+delta)) {
    for (q in max(0,(guessq-delta)):(guessq+delta)) {
      order <- c(p,0,q)
      # Fit a maximum likelihood ARMA(p,q) model
      armafit <- Arima(timeseries, order=order, method="ML")
      # Add the results to the dataframe
      df <- rbind(df, c(p, q, armafit$loglik, armafit$aic, armafit$aicc, armafit$bic))
    }
  }
  names(df) <- c("p","q","log.likelihood", "AIC", "AICc", "BIC")
  return(df)
}
df <- tryArma(2,3,1,APACQ_localpred)
View(df)
# Arma 2,3

APACQ_armafit<- Arima(APACQ_localpred, order=c(2,3,1), method="ML")
tsdiag(APACQ_armafit)

APACQ_resi <- APACQ_localpred-fitted(APACQ_armafit)

plot(APACQ_resi)
adf.test(APACQ_resi,alternative = "stationary")
kpss.test(APACQ_resi)

#Do Validation by predicting from month 43 to 49 and calculate MAPE


APACQ_predictedForSixMonth <-stats:: predict(APACQ_lmfit,data.frame(Month =APACQ_outdata$Month))
APACQ_MAPE_class_dec <- accuracy(APACQ_predictedForSixMonth,APACQ_outdata$TotalQuantity)[5]
APACQ_MAPE_class_dec
#MAPE 18 %



#Auto Arima for APAC Quantity
APACQ_autoarima <- auto.arima(APACQ_TS)
tsdiag(APACQ_autoarima)


plot(APACQ_autoarima$x, col="black")
lines(fitted(APACQ_autoarima), col="red")
APACQ_resi_auto_arima <- APACQ_SmthSeries - fitted(APACQ_autoarima)
adf.test(APACQ_resi_auto_arima,alternative = "stationary")
kpss.test(APACQ_resi_auto_arima)

#Do Validation by predicting from month 43 to 49 and calculate MAPE
APACQ_predictedForSixMonthAutoArma <- stats::predict(APACQ_autoarima, n.ahead = 6)



APACQ_MAPE_Auto_Arma <- accuracy(APACQ_predictedForSixMonthAutoArma$pred,APACQ_outdata$TotalQuantity)[5]
APACQ_MAPE_Auto_Arma
#MAPE 24 %

#  Predictions for Next 6 months
APAC_Quantity_finalArma <- stats::predict(APACQ_armafit, n.ahead = 12)
APAC_Quantity_finalLmprediction <-stats:: predict(APACQ_lmfit,data.frame(Month =c(50:55)))

APAC_Quantity_finalpredictedForSixMonth <-as.data.frame(APAC_Quantity_finalArma$pred)[7:12,] +APAC_Quantity_finalLmprediction
APAC_Quantity_finalpredictedForSixMonth
#346.3371 355.1816 428.9107 552.4259 697.1509 826.2182 

#plot

APACQ_fcast<- forecast(APACQ_armafit,h=6)
plot(APACQ_fcast)


####################################################################




#------------------ Second Bucket EU Consumer Market forecast ---------------------------------

indata_EU_Consumer <-SecondBucket[1:43,]
outdata_EU_Consumer <- SecondBucket[44:49,]
SalesDataFrame_EU_consumer <- select( indata_EU_Consumer,Month,TotalSales)
SalesTimeSeries_EU_Consumer <- ts(SalesDataFrame_EU_consumer$TotalSales)
plot(SalesTimeSeries_EU_Consumer)


#Smoothing the series - Moving Average Smoothing
w <-1
smoothedseries_EU_consumer <- stats::filter(as.data.frame( SalesTimeSeries_EU_Consumer), 
                                            filter=rep(1/(2*w+1),(2*w+1)), 
                                            method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_EU_consumer[w+2] - smoothedseries_EU_consumer[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_EU_consumer[i] <- smoothedseries_EU_consumer[i+1] - diff
}

#Smoothing right end of the time series

n <- length(SalesTimeSeries_EU_Consumer)
diff <- smoothedseries_EU_consumer[n-w] - smoothedseries_EU_consumer[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_EU_consumer[i] <- smoothedseries_EU_consumer[i-1] + diff
}

#Plot the smoothed time series

plot(smoothedseries_EU_consumer)
title(main = "Smoothed Series for EU Cosnumer")

smoothedseries_EU_consumer_df <- as.data.frame(cbind(SalesDataFrame_EU_consumer$Month, as.vector(smoothedseries_EU_consumer)))
names(smoothedseries_EU_consumer_df)<-c("Month","TotalSales")

polynomial<- 2
lmfit <- lm(TotalSales ~ sin(0.5*Month) * poly(Month,polynomial) + cos(0.5*Month) * poly(Month,polynomial)
            + Month, data=smoothedseries_EU_consumer_df)


globalpred_EU_consumer <- predict(lmfit, Month=c(1:nrow(indata_EU_Consumer)))

plot(smoothedseries_EU_consumer)
par(new=TRUE)
plot(globalpred_EU_consumer, col='red', type = "l")


localpred_EU_consumer <- smoothedseries_EU_consumer - globalpred_EU_consumer
plot(localpred_EU_consumer, col='red', type = "l")




acf(localpred_EU_consumer)
acf(localpred_EU_consumer, type="partial")
autoarimaresult <- auto.arima(localpred_EU_consumer)
autoarimaresult
tryres <- tryArma(2,1,2,localpred_EU_consumer)

autoarimaresult

Eu_consumer_predictedForSixMonthArma <- stats::predict(autoarimaresult, n.ahead = 6)
Eu_consumer_predictedForSixMonthArmalm <-stats:: predict(lmfit,data.frame(Month =outdata_EU_Consumer$Month))
Eu_consumer_predictedForSixMonth <-as.data.frame(Eu_consumer_predictedForSixMonthArma$pred)[1,] +Eu_consumer_predictedForSixMonthArmalm


MAPE_class_dec <- accuracy(Eu_consumer_predictedForSixMonth,outdata_EU_Consumer$TotalSales)[5]
MAPE_class_dec
#26 %


#Auto arima 
autoarima_EU_consumer <- auto.arima(SalesTimeSeries_EU_Consumer)
autoarima_EU_consumer

#ARIMA(3,1,1)
tsdiag(autoarima_EU_consumer)
plot(autoarima_EU_consumer$x, col="black")
lines(fitted(autoarima_EU_consumer), col="red")

#Again, let's check if the residual series is white noise

EU_Sales_armafit<- Arima(localpred_EU_consumer, order=c(3,1,1), method="ML")
tsdiag(EU_Sales_armafit)

resi_auto_arima_EU_consumer <- smoothedseries_EU_consumer - fitted(autoarima_EU_consumer)

adf.test(resi_auto_arima_EU_consumer,alternative = "stationary")
kpss.test(resi_auto_arima_EU_consumer)
#Also, let's evaluate the model using MAPE
fcast_auto_arima_EU_consumer <- predict(autoarima_EU_consumer, n.ahead = 6)

MAPE_auto_arima_EU_consumer <- accuracy(fcast_auto_arima_EU_consumer$pred,outdata[,4])[5]
MAPE_auto_arima_EU_consumer
#37.30768

#  Predictions for Next 6 months
EU_SALES_finalArma <- stats::predict(autoarimaresult, n.ahead = 12)
EU_SALES_finalLmprediction <-stats:: predict(lmfit,data.frame(Month =c(50:55)))

EU_SALES_finalpredictedForSixMonth <-as.data.frame(EU_SALES_finalArma$pred)[7:12,] +EU_SALES_finalLmprediction
EU_SALES_finalpredictedForSixMonth
#24580.31 26367.66 34872.70 48117.86 62973.47 75764.59 

# #Lastly, let's plot the predictions along with original values, to
EU_sales_fcast<- forecast(EU_Sales_armafit,h=6)
plot(EU_sales_fcast)

#------------------------------------Future predictions for EU Quantity----------------------------------------

indata2<-SecondBucket[1:43,]

QuantityDataFrame2 <- select( indata2,Month,TotalQuantity)
QuantityTimeSeries2 <- ts(QuantityDataFrame2$TotalQuantity)
plot(QuantityTimeSeries2)
#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(QuantityTimeSeries2, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(QuantityTimeSeries2)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series 

timevals_in <- indata2$Month
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf_Quantity <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf_Quantity) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function
#as amplitude is increasing so it is a multiplicative model
polynomialForQuantity <- 2
lmfit_quantity <- lm(Quantity ~ sin(0.5*Month) * poly(Month,polynomialForQuantity) + cos(0.5*Month) * poly(Month,polynomialForQuantity)
                     + Month, data=smootheddf_Quantity)
global_pred_Quantity <- predict(lmfit_quantity, Month=timevals_in)
global_pred_Quantity
summary(global_pred_Quantity)
lines(timevals_in, global_pred_Quantity, col='red', lwd=2)

sqrt( (sum((smootheddf_Quantity$Quantity - global_pred_Quantity)^2))/nrow(smootheddf_Quantity))

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_quantity <- QuantityTimeSeries2-global_pred_Quantity
plot(local_pred_quantity, col='red', type = "l")
acf(local_pred_quantity)
acf(local_pred_quantity, type="partial")


#So, that was classical decomposition, now let's do an ARIMA fit

autoarima_Quantity <- auto.arima(QuantityTimeSeries2)
autoarima_Quantity
tsdiag(autoarima_Quantity)
plot(autoarima_Quantity$x, col="black")
lines(fitted(autoarima_Quantity), col="red")

tryr <- tryArma(1,0,1,local_pred_quantity)
View(tryr)



EUQ_armafit<- Arima(local_pred_quantity, order=c(1,0,1), method="ML")
tsdiag(EUQ_armafit)





EUQ_resi <- local_pred_quantity-fitted(EUQ_armafit)
plot(resi)
adf.test(EUQ_resi,alternative = "stationary")
kpss.test(EUQ_resi)
#Both kpss and adf test states that the series is stationary
#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the next 6 months

outdata2<-SecondBucket[44:49,]
timevals_out <- outdata2$Month


global_pred_out_Q <- predict(lmfit_quantity,data.frame(Month = timevals_out))


global_pred_out_Q
fcast_Quantity <- global_pred_out_Q

#Now, let's compare our prediction with the actual values, using MAPE
outdata2
MAPE_class_dec <- accuracy(fcast_Quantity,outdata2$TotalQuantity)[5]
MAPE_class_dec
#17.05325

class_dec_pred <- c(ts(global_pred_Quantity),ts(global_pred_out_Q))
plot(QuantityTimeSeries2, col = "black")
lines(class_dec_pred, col = "red")





#Again, let's check if the residual series is white noise

resi_auto_arima_Q <- QuantityTimeSeries2 - fitted(autoarima_Quantity)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima_Q)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima_Quantity, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,outdata2[,5])[5]
MAPE_auto_arima

#  Predictions for Next 6 months
EU_Quantity_finalLmprediction <-stats:: predict(lmfit_quantity,data.frame(Month =c(50:55)))
EU_Quantity_finalLmprediction
#338.1107 340.5198 418.5152 559.4912 731.6367 891.1283 



# #Lastly, let's plot the predictions along with original values, to
EUQ_fcast<- forecast(EUQ_armafit, h=6)
plot(EUQ_fcast)







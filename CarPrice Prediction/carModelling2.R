setwd("C:/Users/Admin/Desktop/DATA_ANALYTICS/Module 3/Car Assignment")
CarPrice<-read.csv("CarPrice_Assignment.csv")
library(stringr)
library(car)
library(MASS)
library(corrplot)
library(ggplot2)
library(scales)
#Deriving Company Name from Car Model
CarModel<-str_split(CarPrice$CarName," ",simplify = T)
CarPrice$CarModel<-CarModel[,1]
summary(factor(CarPrice$CarModel))
#Replacing all the spelling errors
CarPrice$CarModel <- str_replace_all(CarPrice$CarModel,"toyouta","toyota")
CarPrice$CarModel <- str_replace_all(CarPrice$CarModel,"porcshce","porsche")
CarPrice$CarModel <- str_replace_all(CarPrice$CarModel,"Nissan","nissan")
CarPrice$CarModel <- str_replace_all(CarPrice$CarModel,"maxda","mazda")
CarPrice$CarModel <- str_replace_all(CarPrice$CarModel,"vokswagen","volkswagen")
CarPrice$CarModel <- str_replace_all(CarPrice$CarModel,"vw","volkswagen")
CarPrice$CarModel<-as.factor(CarPrice$CarModel)
summary(CarPrice$CarModel)
#Removing CarName
CarPrice<-CarPrice[,-3]
#Converting few variables to numeric
CarPrice$highwaympg<-as.numeric(CarPrice$highwaympg)
CarPrice$citympg<-as.numeric(CarPrice$citympg)
CarPrice$peakrpm<-as.numeric(CarPrice$peakrpm)
CarPrice$horsepower<-as.numeric(CarPrice$horsepower)
CarPrice$enginesize<-as.numeric(CarPrice$enginesize)
CarPrice$curbweight<-as.numeric(CarPrice$curbweight)
CarPrice$car_ID<-as.numeric(CarPrice$car_ID)
#Converting Symboling to factor as it is a categorical variable
CarPrice$symboling<-as.factor(CarPrice$symboling)

summary(CarPrice$symboling)
str(CarPrice)
# convert factors with 2 levels to numerical variables
levels(CarPrice$fueltype)<-c(1,0)
CarPrice$fueltype<- as.numeric(levels(CarPrice$fueltype))[CarPrice$fueltype]

levels(CarPrice$aspiration)<-c(1,0)
CarPrice$aspiration <- as.numeric(levels(CarPrice$aspiration))[CarPrice$aspiration]

levels(CarPrice$doornumber)<-c(1,0)
CarPrice$doornumber <- as.numeric(levels(CarPrice$doornumber))[CarPrice$doornumber]

levels(CarPrice$enginelocation)<-c(1,0) 
CarPrice$enginelocation <- as.numeric(levels(CarPrice$enginelocation))[CarPrice$enginelocation]

summary(CarPrice$enginetype)
# Create the dummy variable for enginetype variable
dummy_1 <- data.frame(model.matrix( ~enginetype, data = CarPrice))
View(dummy_1)
dummy_1 <- dummy_1[,-1]
# Create the dummy variable for cylindernumber variable
dummy_2 <- data.frame(model.matrix( ~cylindernumber, data = CarPrice))
View(dummy_2)
dummy_2 <- dummy_2[,-1]
# Create the dummy variable for fuelsystem variable
dummy_3 <- data.frame(model.matrix( ~fuelsystem, data = CarPrice))
View(dummy_3)
dummy_3 <- dummy_3[,-1]
# Create the dummy variable for carbody variable
dummy_4 <- data.frame(model.matrix( ~carbody, data = CarPrice))
View(dummy_4)
dummy_4 <- dummy_4[,-1]
# Create the dummy variable for drivewheel variable
dummy_5 <- data.frame(model.matrix( ~drivewheel, data = CarPrice))
View(dummy_5)
dummy_5 <- dummy_5[,-1]

# Create the dummy variable for CarModel variable
dummy_6 <- data.frame(model.matrix( ~CarModel, data = CarPrice))
View(dummy_6)
dummy_6 <- dummy_6[,-1]

# Create the dummy variable for symboling variable
dummy_7 <- data.frame(model.matrix( ~symboling, data = CarPrice))
View(dummy_7)
dummy_7 <- dummy_7[,-1]

# Combine the dummy variables and the numeric columns of CarPrice dataset, in a new dataset called CarPrice_1
CarPrice_1 <- cbind(CarPrice[,-c(6,7,14,15,17,26)], dummy_1)
CarPrice_1 <- cbind(CarPrice_1, dummy_2)
CarPrice_1 <- cbind(CarPrice_1, dummy_3)
CarPrice_1 <- cbind(CarPrice_1, dummy_4)
CarPrice_1 <- cbind(CarPrice_1, dummy_5)
CarPrice_1 <- cbind(CarPrice_1, dummy_6)
CarPrice_1 <- cbind(CarPrice_1[,-c(2)], dummy_7)
str(CarPrice_1)

# Let us create the new metric and assign it to "carvolume"
CarPrice_1$carvolume <- CarPrice_1$carlength*CarPrice_1$carheight*CarPrice_1$carwidth

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(CarPrice_1), 0.7*nrow(CarPrice_1))
train = CarPrice_1[trainindices,]
test = CarPrice_1[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)
#######NA are there which means the correlation of those variables with other variable is one
#Verfying that by finding correlation
corrs = cor(CarPrice_1)
View(corrs)
#Removing variables where correlation is 1 And also removing Car_Id as it Unique id of each observation and could not be related to price  

model_2<-lm(formula = price ~ fueltype +aspiration+doornumber +      
             enginelocation +wheelbase+carlength+ carwidth+             
             carheight+curbweight+enginesize+boreratio+stroke+               
             compressionratio+horsepower +peakrpm + 
             citympg +highwaympg+enginetypedohcv +enginetypel +          
             enginetypeohc+enginetypeohcf+enginetypeohcv+   
             enginetyperotor+cylindernumberfive+cylindernumberfour+   
             cylindernumbersix +cylindernumberthree+fuelsystem2bbl+  
            fuelsystemmfi +fuelsystemmpfi+fuelsystemspdi+carbodyhardtop+
            carbodyhatchback+carbodysedan+carbodywagon+
             drivewheelfwd +drivewheelrwd+CarModelaudi+CarModelbmw +  
             CarModelbuick +CarModeldodge +CarModelhonda +CarModelisuzu +
            CarModeljaguar+CarModelmazda +CarModelmercury +CarModelmitsubishi+
              CarModelnissan+CarModelplymouth+CarModelrenault +CarModelsaab+
             CarModeltoyota+CarModelvolkswagen+CarModelvolvo+
             symboling.1+symboling0 +symboling1+symboling2+symboling3+carvolume, data = train)
 
summary(model_2)
step <- stepAIC(model_2, direction="both")
#Great, so many iterations have been done through the stepwise command. 
# now we need to know our model equation so lets write the Step command here. 

step

model_3<-lm(formula = price ~ aspiration + enginelocation + carlength + 
              carheight + curbweight + enginesize + stroke + peakrpm + 
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor + cylindernumberfive + cylindernumberthree + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              drivewheelrwd + CarModelbmw + CarModelbuick + CarModeldodge + 
              CarModelhonda + CarModeljaguar + CarModelmazda + CarModelmercury + 
              CarModelmitsubishi + CarModelnissan + CarModelplymouth + 
              CarModelrenault + CarModelsaab + CarModeltoyota + CarModelvolkswagen + 
              symboling.1 + symboling0 + symboling3 + carvolume, data = train)

summary(model_3)
#Calculating the VIF to check whether carvolume has to kept or not
vif(model_3)

# removing symboling.1 , symboling0 , symboling3 , CarModelmercury, CarModelhonda ,cylindernumberfive ,peakrpm  as are insignificant
#removing carvolume as having high VIF as compared to carlength,carheight
model_4<-lm(formula = price ~ aspiration + enginelocation + carlength + 
              carheight + curbweight + enginesize + stroke +
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor +  cylindernumberthree + 
              carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
              drivewheelrwd + CarModelbmw + CarModelbuick + CarModeldodge + 
              CarModeljaguar + CarModelmazda + 
              CarModelmitsubishi + CarModelnissan + CarModelplymouth + 
              CarModelrenault + CarModelsaab + CarModeltoyota + CarModelvolkswagen , data = train)

summary(model_4)
# removing carheight carbodyhardtop carbodyhatchback carbodysedan  CarModelrenault as are insignificant
model_5<-lm(formula = price ~ aspiration + enginelocation + carlength + 
              curbweight + enginesize + stroke +
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor +  cylindernumberthree + carbodywagon + 
              drivewheelrwd + CarModelbmw + CarModelbuick + CarModeldodge + 
              CarModeljaguar + CarModelmazda + 
              CarModelmitsubishi + CarModelnissan + CarModelplymouth + 
              CarModelsaab + CarModeltoyota + CarModelvolkswagen , data = train)


summary(model_5)
# removing carlength  cylindernumberthree carbodywagon as are insignificant
model_6<-lm(formula = price ~ aspiration + enginelocation + 
              curbweight + enginesize + stroke +
              enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
              enginetyperotor +
              drivewheelrwd + CarModelbmw + CarModelbuick + CarModeldodge + 
              CarModeljaguar + CarModelmazda + 
              CarModelmitsubishi + CarModelnissan + CarModelplymouth + 
              CarModelsaab + CarModeltoyota + CarModelvolkswagen , data = train)
summary(model_6)
# removing CarModelsaab ,CarModelvolkswagen ,enginetypeohc  as are insignificant
model_7<-lm(formula = price ~ aspiration + enginelocation + 
              curbweight + enginesize + stroke +
              enginetypedohcv + enginetypel  + enginetypeohcf + enginetyperotor +
              drivewheelrwd + CarModelbmw + CarModelbuick + CarModeldodge + 
              CarModeljaguar + CarModelmazda + 
              CarModelmitsubishi + CarModelnissan + CarModelplymouth + 
             CarModeltoyota  , data = train)

summary(model_7)

# removing  enginetypel,  CarModelnissan ,CarModelplymouth , CarModelmazda as are less significant
model_8<-lm(formula = price ~ aspiration + enginelocation + 
              curbweight + enginesize + stroke +
              enginetypedohcv + enginetypeohcf + enginetyperotor +
              drivewheelrwd + CarModelbmw + CarModelbuick + CarModeldodge + 
              CarModeljaguar + CarModelmitsubishi +
              CarModeltoyota  , data = train)


summary(model_8)
# removing CarModeldodge as is less significant 
model_9<-lm(formula = price ~ aspiration + enginelocation + 
              curbweight + enginesize + stroke +
              enginetypedohcv + enginetypeohcf + enginetyperotor +
              drivewheelrwd + CarModelbmw + CarModelbuick + 
              CarModeljaguar + CarModelmitsubishi +
              CarModeltoyota  , data = train)
summary(model_9)
#removing CarModelmitsubishi CarModeltoyota as are less significant
model_10<-lm(formula = price ~ aspiration + enginelocation + 
               curbweight + enginesize + stroke +
               enginetypedohcv + enginetypeohcf + enginetyperotor +
               drivewheelrwd + CarModelbmw + CarModelbuick + 
               CarModeljaguar , data = train)

summary(model_10)
# Now as all variables are significant,checking the VIF
vif(model_10)
#removing enginesize as having high VIF 
model_11<-lm(formula = price ~ aspiration + enginelocation + 
               curbweight  + stroke +enginetypedohcv +
              enginetypeohcf + enginetyperotor +
               drivewheelrwd + CarModelbmw + CarModelbuick +
                CarModeljaguar , data = train)

summary(model_11)
#removing aspiration as having p value >0.5
model_12<-lm(formula = price ~  enginelocation + 
               curbweight  + stroke +enginetypedohcv +
               enginetypeohcf + enginetyperotor +
               drivewheelrwd + CarModelbmw + CarModelbuick +
               CarModeljaguar , data = train)

summary(model_12)
# predicting the results in test dataset
Predict_1 <- predict(model_12,test[,-1])
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted sales. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2
rsquared
#Although after removing engine size many variables have become insignificant which were significant earlier.
#But this model is the best model as the rsquare value of test dataset is very close to that of training dataset
#Hence we can say that price of a car is dependent on enginelocation,curbweight,stroke,enginetype

# Predicting values and calculating error
CarPrice_1$Price_model12 <- predict(model_12, CarPrice_1)
CarPrice_1$error_model12 <-  CarPrice_1$price - CarPrice_1$Price_model12

ggplot(CarPrice_1, aes(price, error_model12)) + geom_point()+
scale_y_continuous(name = "Error", breaks = seq(-5000,5000,500), limits = c(-5000,5000)) + geom_hline(yintercept = 0)
#the errors (the differences between the actual price and the price predicted by the model) were randomly distributed. 
#Hence there are no variables that could have helped explain the model better. 


# Actual vs predicted values from model12
ggplot(CarPrice_1, aes(stroke, price)) + geom_line(aes(colour = "blue")) + 
geom_line(aes(x=stroke, y=Price_model12, colour="red"))

#as the actual and predicted price for stroke overlaps we can say that stroke is the best predictor for price
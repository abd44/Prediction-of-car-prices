library(lubridate)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(plyr)

install.packages("MASS")
library(MASS)
install.packages("car")
library(car)

#Reading Dataset and checking structure:
carPrice <- read.csv("CarPrice_Assignment.csv")
str(carPrice)

##############  Data understanding, preparation and EDA  ###############################################################################

#Checking NA's in dataset
colSums(is.na(carPrice))

#Checking Unique value
nrow(unique(carPrice))
sapply(carPrice, function(x) {length(unique(x))})
#No Duplicates Found

#Checking for Blanks:
sum(carPrice =="")

#No Blanks or Missing Value

#To check for outliers, we find out the quantile values at each 1% interval and wherever there is a high jump from one
#| quantile to another, we cap/floor those values.
#Checking Outliers in variable wheelbase
quantile(carPrice$wheelbase,seq(0,1,0.01))
#There is outlier after 99% in wheel base,hence all values above 115.544 (99%) is capped  
carPrice$wheelbase[which(carPrice$wheelbase >115.544)]<-115.544

#Checking Outliers in carlength
quantile(carPrice$carlength,seq(0,1,0.01))
#No Outliers in carlength
#There is a steep change in carlength from 99% to 100%,hence capping it to 202.480
carPrice$carlength[which(carPrice$carlength > 202.480)] <- 202.480

#Checking Outliers in carwidth:
quantile(carPrice$carwidth,seq(0,1,0.01))
#No Outliers in carwidth

#Checking Outliers in carheight:
quantile(carPrice$carheight,seq(0,1,0.01))
#No Outliers in carheight

#Checking Outliers in curbweight:
quantile(carPrice$curbweight,seq(0,1,0.01))
#There is a sharp increase in curbweight from 0% to 1%.Hence capping it to 1819.72
carPrice$curbweight[which(carPrice$curbweight < 1819.72)] <- 1819.72


#Checking outliers for enginesize
quantile(carPrice$enginesize,seq(0,1,0.01))
#After 96% there is a steep increase in enginesize,hence capping after 96% values as 209.00
carPrice$enginesize[which(carPrice$enginesize > 209.00)] <- 209.00

#Checking quantiles for boreratio:
quantile(carPrice$boreratio,seq(0,1,0.01))
#No Outliers Found.

#Checking Quantile for stroke:
quantile(carPrice$stroke,seq(0,1,0.01))
#No Outliers Found

#Checking Quantile for compressionratio:
quantile(carPrice$compressionratio,seq(0,1,0.01))
#Afer 90% of data their is steep increase in compression ratio from 10.94 to 21.00,so assigning greater than 10.94 to 10.94.
carPrice$compressionratio[which(carPrice$compressionratio > 10.9400)] <- 10.9400


#checking Outliers for horsepower:
quantile(carPrice$horsepower,seq(0,1,0.01))
#There is very sharp increase in horse power after 99% from 207 to 288.hence capping it to 207
carPrice$horsepower[which(carPrice$horsepower > 207.00)] <- 207.00

#Cheking Outliers for peakrpm:
quantile(carPrice$peakrpm,seq(0,1,0.01))
#There is very sharp increase in peakrpm after 99% from 6000 to 6600.hence capping it to 6000
carPrice$peakrpm[which(carPrice$peakrpm > 6000)] <- 6000

#Cheking Outliers for citympg:
quantile(carPrice$citympg,seq(0,1,0.01))
#There is a sharp increase in citympg after 98%,hence capping it ffurthur after 98% as 38.00 
carPrice$citympg[which(carPrice$citympg > 38.00)] <- 38.00

#Cheking Outliers for highwaympg:
quantile(carPrice$highwaympg,seq(0,1,0.01))
#There is sharp increase in highwaympg after 99% from 49.88,hence capping it to 49.88
carPrice$highwaympg[which(carPrice$highwaympg > 49.88)] <- 49.88

#Checking Outliers in price:
quantile(carPrice$price,seq(0,1,0.01))
#There is a sharp increase in price after 90% ,hence capping 90% value 22563.00 for price:
carPrice$price[which(carPrice$price>22563.00)] <- 22563.00

###################Coverting Categoricals Variable to Numeric for Linear Regression Modelling#########################
#Variable with only 2 Levels such as fueltype,aspiration,doornumberand enginelocation

#For variable"fueltype" :
# convert fueltype variable to numeric is to replace the levels-  with 1:diesel and 0:gas(Not diesel) is:
summary(carPrice$fueltype)
levels(carPrice$fueltype)<-c(1,0)

# Now store the numeric values in the same variable
carPrice$fueltype<- as.numeric(levels(carPrice$fueltype))[carPrice$fueltype]

# Check the summary of fueltype variable
table(carPrice$fueltype)
str(carPrice)

#For variable "aspiration" :
# Converting aspiration variable to numeric:0-turbo(not std) and 1 :std
summary(carPrice$aspiration)
levels(carPrice$aspiration) <-c(1,0)

# Now store the numeric values in the same variable
carPrice$aspiration <- as.numeric(levels(carPrice$aspiration))[carPrice$aspiration]
# Check the summary of aspiration variable
table(carPrice$aspiration)


# For variable "doornumber" :
# Converting doornumber variable to numeric:0-two(not four) and 1 :four
summary(carPrice$doornumber)
levels(carPrice$doornumber) <-c(1,0)

# Now store the numeric values in the same variable
carPrice$doornumber <- as.numeric(levels(carPrice$doornumber))[carPrice$doornumber]
# Check the summary of doornumber variable
table(carPrice$doornumber)

#For variable "enginelocation":
# Converting enginelocation variable to numeric:0-rear(not front) and 1 :front
summary(carPrice$enginelocation)
levels(carPrice$enginelocation) <-c(1,0)

# Now store the numeric values in the same variable
carPrice$enginelocation <- as.numeric(levels(carPrice$enginelocation))[carPrice$enginelocation]
# Check the summary of enginelocation variable
table(carPrice$enginelocation)




#Changing Coulmn Car name by only company name:
carPrice$CarName <-  sapply(str_split(carPrice$CarName, ' '), function(x) x[1])
carPrice$CarName <- as.factor(carPrice$CarName)
levels(carPrice$CarName)
table(carPrice$CarName)
#Some comapnies names have typo ,fixing it:
revalue(carPrice$CarName, c("toyouta" = "toyota")) -> carPrice$CarName
revalue(carPrice$CarName, c("maxda" = "mazda")) -> carPrice$CarName
revalue(carPrice$CarName, c("vokswagen" = "volkswagen")) -> carPrice$CarName
revalue(carPrice$CarName, c("vw" = "volkswagen")) -> carPrice$CarName
revalue(carPrice$CarName, c("nissan" = "Nissan")) -> carPrice$CarName
revalue(carPrice$CarName, c("porcshce" = "porsche")) -> carPrice$CarName




#Create Dummy Variable for more than 2 variables:
####### Create the dummy variable 1 for "carbody" variable
dummy_1 <- data.frame(model.matrix( ~carbody, data = carPrice))
View(dummy_1)
dummy_1 <- dummy_1[,-1]
#remove 'carbody' variable and Combine the dummy variables in a new dataset called carPrice_1
carPrice_1 <- cbind(carPrice[,-7], dummy_1)

#######Create the dummy variable 2 for "drivewheel" variable
dummy_2 <- data.frame(model.matrix( ~drivewheel, data = carPrice))
View(dummy_2)
dummy_2 <- dummy_2[,-1]
#Remove drivewheel variable & Combine the dummy variables in a new dataset called carPrice_1
carPrice_1 <- cbind(carPrice_1[,-7], dummy_2)

#######Create Dummy variable 3 for "enginetype"
dummy_3 <- data.frame(model.matrix( ~enginetype, data = carPrice))
View(dummy_3)
dummy_3 <- dummy_3[,-1]
#Remove 'enginetype' variable & Combine the dummy variables in a new dataset called carPrice_1
carPrice_1 <- cbind(carPrice_1[,-13], dummy_3)


#Create the dummy variable 4 for "cylindernumber" variable
dummy_4 <- data.frame(model.matrix( ~cylindernumber, data = carPrice))
View(dummy_4)
dummy_4 <- dummy_4[,-1]
#Remove 'enginetype' variable & Combine the dummy variables  in a new dataset called carPrice_1
carPrice_1 <- cbind(carPrice_1[,-13], dummy_4)


#Create the dummy variable 5 for "fuelsystem" variable
dummy_5 <- data.frame(model.matrix( ~fuelsystem , data = carPrice))
View(dummy_5)
dummy_5 <- dummy_5[,-1]
#Remove 'enginetype' variable & Combine the dummy variables in a new dataset called carPrice_1
carPrice_1 <- cbind(carPrice_1[,-14], dummy_5)

str(carPrice_1)


#converting symboling to Factor type:
carPrice_1$symboling <- as.factor(carPrice_1$symboling)

str(carPrice_1)



#Create the dummy variable 6 for "symboling" variable
dummy_6 <- data.frame(model.matrix( ~symboling , data = carPrice_1))
View(dummy_6)
dummy_6 <- dummy_6[,-1]
#Remove 'symboling' variable & Combine the dummy variables in a new dataset called carPrice_1
carPrice_1 <- cbind(carPrice_1[,-2], dummy_6)


#Create the dummy variable 7 for "CarName" variable
dummy_7 <- data.frame(model.matrix( ~CarName , data = carPrice_1))
View(dummy_7)
dummy_7 <- dummy_7[,-1]
#Remove 'CarName' variable & Combine the dummy variable in a new dataset called carPrice_1
carPrice_1 <- cbind(carPrice_1[,-2], dummy_7)


#Dropping Carid Variable as it's primary key with no relevance in linear regression model building:
carPrice_1 <- within(carPrice_1,rm(car_ID))

str(carPrice_1)



##############################  Model building  ###################################################


# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(carPrice_1), 0.7*nrow(carPrice_1))
train = carPrice_1[trainindices,]
test = carPrice_1[-trainindices,]


#Model 1:
model_1 <-lm(price~.,data=train)
summary(model_1)
#Using StepAIC Function:
step <- stepAIC(model_1, direction="both")

step

#After Step removes insignificant Variable,all the left out variable are evaluated to be of significance base on p value greater than 0.05 and VIF greater than 2:
model1 <-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carheight + curbweight + enginesize + 
              stroke + compressionratio + horsepower + citympg + highwaympg + 
              enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
              fuelsystem2bbl + fuelsystemmfi + fuelsystemmpfi + fuelsystemspdi + 
              symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
              CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
              CarNamevolkswagen, data = train)


summary(model1)
vif(model1)

#Observing that fuelsystemmfi variable has high VIF & pvalue ,hence dropping "fuelsystemmfi" for next model.


model2 <-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase + carlength + carheight + curbweight + enginesize + 
              stroke + compressionratio + horsepower + citympg + highwaympg + 
              enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
              fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
              symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
              CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
              CarNamevolkswagen, data = train)

summary(model2)
vif(model2)


# carlength has VIF & high p value ,hence dropping "carlength" for next model,as it is insignigicant.


model3 <-lm(formula = price ~ fueltype + aspiration + doornumber + enginelocation + 
              wheelbase  + carheight + curbweight + enginesize + 
              stroke + compressionratio + horsepower + citympg + highwaympg + 
              enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
              fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
              symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
              CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
              CarNamevolkswagen, data = train)


summary(model3)
vif(model3)


# fueltype has high VIF & high p value .hence dropping "fueltype" for next model,as it is insignigicant

model4 <-lm(formula = price ~ aspiration + doornumber + enginelocation + 
              wheelbase  + carheight + curbweight + enginesize + 
              stroke + compressionratio + horsepower + citympg + highwaympg + 
              enginetypel + enginetypeohc + enginetypeohcf + enginetyperotor + 
              fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
              symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
              CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
              CarNamevolkswagen, data = train)
summary(model4)
vif(model4)


# enginetyperotor has high VIF & high p value hence dropping "enginetyperotor" for next model,as it is insignigicant
model5 <-lm(formula = price ~ aspiration + doornumber + enginelocation + 
              wheelbase  + carheight + curbweight + enginesize + 
              stroke + compressionratio + horsepower + citympg + highwaympg + 
              enginetypel + enginetypeohc + enginetypeohcf  + 
              fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + 
              symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
              CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
              CarNamevolkswagen, data = train)
summary(model5)
vif(model5)



# fuelsystemspdi has high VIF & high p value .hence dropping "fuelsystemspdi" for next model,as it is insignigicant

#Model 6:

model6 <-lm(formula = price ~ aspiration + doornumber + enginelocation + 
              wheelbase  + carheight + curbweight + enginesize + 
              stroke + compressionratio + horsepower + citympg + highwaympg + 
              enginetypel + enginetypeohc + enginetypeohcf  + 
              fuelsystem2bbl + fuelsystemmpfi  + 
              symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
              CarNameisuzu + CarNamejaguar + CarNamemazda + CarNamemitsubishi + 
              CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
              CarNamevolkswagen, data = train)
summary(model6)
vif(model6)

#CarNamejaguar has high VIF & high p value ,hence dropping "CarNamejaguar" for next model,as it is insignigicant

#Model 7:

model7 <-lm(formula = price ~ aspiration + doornumber + enginelocation + 
              wheelbase  + carheight + curbweight + enginesize + 
              stroke + compressionratio + horsepower + citympg + highwaympg + 
              enginetypel + enginetypeohc + enginetypeohcf  + 
              fuelsystem2bbl + fuelsystemmpfi  + 
              symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
              CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
              CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
              CarNamevolkswagen, data = train)
summary(model7)
vif(model7)

# doornumber has high VIF & high p value ,hence dropping "doornumber" for next model,as it is insignigicant


#Model 8:
model8 <-lm(formula = price ~ aspiration  + enginelocation + 
              wheelbase  + carheight + curbweight + enginesize + 
              stroke + compressionratio + horsepower + citympg + highwaympg + 
              enginetypel + enginetypeohc + enginetypeohcf  + 
              fuelsystem2bbl + fuelsystemmpfi  + 
              symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
              CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
              CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
              CarNamevolkswagen, data = train)
summary(model8)
vif(model8)

# horsepower high p value & High VIF .hence dropping "horsepower" for next model,as it is insignigicant
#Model 9:
model9 <-lm(formula = price ~ aspiration  + enginelocation + 
              wheelbase  + carheight + curbweight + enginesize + 
              stroke + compressionratio  + citympg + highwaympg + 
              enginetypel + enginetypeohc + enginetypeohcf  + 
              fuelsystem2bbl + fuelsystemmpfi  + 
              symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
              CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
              CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
              CarNamevolkswagen, data = train)
summary(model9)
vif(model9)

# highwaympg has high p value((p>0.05).hence dropping "highwaympg" for next model,as it is insignificant

#Model 10:
model10 <-lm(formula = price ~ aspiration  + enginelocation + 
               wheelbase  + carheight + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf  + 
               fuelsystem2bbl + fuelsystemmpfi  + 
               symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota + 
               CarNamevolkswagen, data = train)
summary(model10)
vif(model10)

#CarNamevolkswagen has high p value((p>0.05).hence dropping "CarNamevolkswagen" for next model,as it is insignificant.

#Model 11:
model11 <-lm(formula = price ~ aspiration  + enginelocation + 
               wheelbase  + carheight + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf  + 
               fuelsystem2bbl + fuelsystemmpfi  + 
               symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota , data = train)
summary(model11)
vif(model11)

# wheelbase has high  p value.hence dropping "wheelbase" for next model,as it is insignificant

#Model 12:
model12 <-lm(formula = price ~ aspiration  + enginelocation + 
                 carheight + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf  + 
               fuelsystem2bbl + fuelsystemmpfi  + 
               symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth + CarNamerenault + CarNametoyota , data = train)
summary(model12)
vif(model12)

# CarNamerenault has high  p value,so low significance,hence dropping and checking R squared value furthur in ,model:

#Model 13:
model13 <-lm(formula = price ~ aspiration  + enginelocation + 
               carheight + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf  + 
               fuelsystem2bbl + fuelsystemmpfi  + 
               symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth  + CarNametoyota , data = train)
summary(model13)
vif(model13)


#fuelsystemmpfi has high p value.So dropping it for next model.
model14 <-lm(formula = price ~ aspiration  + enginelocation + 
               carheight + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf  + 
               fuelsystem2bbl   + 
               symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth  + CarNametoyota , data = train)
summary(model14)
vif(model14)


# fuelsystem2bbl has high p value .So dropping it for next model.

model15 <-lm(formula = price ~ aspiration  + enginelocation + 
               carheight + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
               symboling1 + symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth  + CarNametoyota , data = train)
summary(model15)
vif(model15)

# symboling1 has high p value and VIF  so dropping it and observing R-squared

model16 <-lm(formula = price ~ aspiration  + enginelocation + 
               carheight + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
                symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth  + CarNametoyota , data = train)
summary(model16)
vif(model16)

#carheight has high p value ,which insignificant variable indicator,hence dropping it.

model17 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
               symboling2 + CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth  + CarNametoyota , data = train)
summary(model17)
vif(model17)
#Now symboling2 has high p value ,henced dropping it for furthur model.
model18 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
                 CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu  + CarNamemazda + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth  + CarNametoyota , data = train)
summary(model18)
vif(model18)

#CarNamemazda variable is high  p value,hence dropping it for furthur model:

model19 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
               CarNameaudi + CarNamebmw + CarNamedodge + 
               CarNameisuzu   + CarNamemitsubishi + 
               CarNameNissan + CarNameplymouth  + CarNametoyota , data = train)
summary(model19)
vif(model19)

# CarNameNissan is having high P value ,seems insignificant,dropping it and checking for R square value change:
model20 <-
  model19 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + enginesize + 
                 stroke + compressionratio  + citympg  + 
                 enginetypel + enginetypeohc + enginetypeohcf   + 
                 CarNameaudi + CarNamebmw + CarNamedodge + 
                 CarNameisuzu   + CarNamemitsubishi + 
                   CarNameplymouth  + CarNametoyota , data = train)


summary(model20)
vif(model20)

#CarNamedodge is now having 1* p value ,which makes it insignificant,Hence dropping it
model21 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
               CarNameaudi + CarNamebmw  + 
               CarNameisuzu   + CarNamemitsubishi + 
               CarNameplymouth  + CarNametoyota , data = train)


summary(model21)
vif(model21)

# Now CarNameplymouth has high p value 1* ,hence seems less significant and hence dropping and observing change in R sqaured value.
model22 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
               CarNameaudi + CarNamebmw  + 
               CarNameisuzu   + CarNamemitsubishi + CarNametoyota , data = train)


summary(model22)
vif(model22)


# CarNamemitsubishi has Pvalue with 2* ,hence seems less significant and hence dropping and observing change in R sqaured value.
model23 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
               CarNameaudi + CarNamebmw  + 
               CarNameisuzu    + CarNametoyota , data = train)


summary(model23)
vif(model23)

#dropping CarNameisuzu as seems less significant and hence dropping and observing change in R sqaured value.
model24 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + enginesize + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
               CarNameaudi + CarNamebmw  + CarNametoyota , data = train)


summary(model24)
vif(model24)

# enginesize seems less significant and hence dropping and observing change in R sqaured value.
model25 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + 
               stroke + compressionratio  + citympg  + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
               CarNameaudi + CarNamebmw  + CarNametoyota , data = train)


summary(model25)
vif(model25)




#citympg  seems less significant and hence dropping and observing change in R sqaured value.
model26 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + 
               stroke + compressionratio    + 
               enginetypel + enginetypeohc + enginetypeohcf   + 
               CarNameaudi + CarNamebmw  + CarNametoyota , data = train)


summary(model26)
vif(model26)

# compressionratio has Pvalue with 2* ,hence seems less significant and hence dropping and observing change in R sqaured value.
model27 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + 
               stroke +enginetypel + enginetypeohc + enginetypeohcf   + 
               CarNameaudi + CarNamebmw  + CarNametoyota , data = train)


summary(model27)
vif(model27)

#Dropping enginetypeohc,and noting chnage in R-sqauared value:

model27.1 <-lm(formula = price ~ aspiration  + enginelocation + curbweight + 
                 stroke +enginetypel  + enginetypeohcf   + 
                 CarNameaudi + CarNamebmw  + CarNametoyota , data = train)

summary(model27.1)
vif(model27.1)
# Dropping aspiration and observing R-sqaured value change in coming model

model27.2 <-lm(formula = price ~   enginelocation + curbweight + 
                 stroke +enginetypel  + enginetypeohcf   + 
                 CarNameaudi + CarNamebmw  + CarNametoyota , data = train)


summary(model27.2)
vif(model27.2)
#Dropping CarNameaudi and  observing R-sqaured value change in coming model:
model27.3 <-lm(formula = price ~   enginelocation + curbweight + 
                 stroke +enginetypel  + enginetypeohcf + CarNamebmw  + CarNametoyota , data = train)


summary(model27.3)
vif(model27.3)
#Dropping enginetypeohcf and  observing R-sqaured value change in coming modeldrop:
model27.4 <-lm(formula = price ~   enginelocation + curbweight + 
                 stroke +enginetypel   + CarNamebmw  + CarNametoyota , data = train)

summary(model27.4)
vif(model27.4)
# stroke has 2* p value, so dropping it as insignificant value:
model27.5 <-lm(formula = price ~   enginelocation + curbweight + 
                  enginetypel   + CarNamebmw  + CarNametoyota , data = train)

summary(model27.5)
vif(model27.5)





############################################  Model testing      #########################################################################


# Now all the Variables are Highly significant with 3*.
#Now we test the model on test dataset
#Adjusted R-squared:  0.9405 and R-squared:  0.9455 which is very close and hence mno redundant variables selected in the model

# Predict the car prices in the testing dataset
Predict_1 <- predict(model27.5,test[,-1])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test$price,test$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test$price,test$test_price)^2

# check R-squared
rsquared



#The difference between R-squared in train data and test data is around 7,so turns out to be decent model for price prediction using Linear Regression.
#Train data has 0.8744 It indicates that the model explains 87.44% variability of the dependent variable with VIF is less than 2 with no multi-colinearity.
#Test data R-sqaured 0.8021651,which is pretty close to training model.

# So best fit line is described by equation:
# -2483.8492 - 8369.3127*(enginelocation) + 9.0400*(curbweight) - 3083.1527*(enginetypel) + 4940.2718*(CarNamebmw) - 1476.0003*(CarNametoyota)


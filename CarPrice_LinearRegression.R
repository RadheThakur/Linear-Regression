
#############################################################
# Linear Regression Assignment: Car Price Prediction Model
# Submitted By: Radhe Thakur
#############################################################
#Set Working Directory
# setwd("C:/Users/RadheThakur/Desktop/Linear Regression/Assignment")
#Load Libraries
library("MASS")
library("car")
library("stringr")
library("tidyr")
library(ggplot2)
library(reshape2)

#Load the data
Car.Price<-read.csv("CarPrice.csv")
#View the overall data
str(Car.Price)
summary(Car.Price)


#############################################################
##                Data Cleansing                           ##
##############################################################

# Check for NA ,Blank and Duplicate

colSums(is.na(Car.Price)) #No NA Values
colSums(Car.Price =="") #No Blank Values 
sum(duplicated(Car.Price))#No Duplicates

#Car Company 
Car.Price$CarCompany<-toupper(word(Car.Price$CarName)) # Word from stringr pulls first word from a string

table(Car.Price$CarCompany)#Check for Car COmpany data

#Correcting the spelling mistakes

Correct_company <- function(name){
  changed_name <- name
  if(name=='MAXDA'){
    changed_name = 'MAZDA'
  } else if (name == 'PORCSHCE') {
    changed_name = 'PORSCHE'
  } else if (name == 'TOYOUTA') {
    changed_name = 'TOYOTA'
  } else if (name %in% c('VOKSWAGEN', 'VW')){
    changed_name = 'VOLKSWAGEN'
  } else if (name=='ALFA-ROMERO'){
    changed_name = 'ALFA-ROMEO'
  }
  return(changed_name)
}

Car.Price$CarCompany<-sapply(Car.Price$CarCompany,Correct_company)# all spelling corrected

# RegionWise COmpany

European <- toupper(c('alfa-romeo','audi','bmw','jaguar','peugeot','porsche','renault','saab','volkswagen','volvo'))
Asian <- toupper(c('honda','isuzu','mazda','mitsubishi','nissan','subaru','toyota'))
US <- toupper(c('buick','chevrolet','dodge','mercury','plymouth'))

Car.Price$Comp_Region <- ifelse(Car.Price$CarCompany %in% European, 'European', 
                                ifelse(Car.Price$CarCompany %in% US, 'US','Asian'))
Car.Price$Comp_Region <- as.factor(Car.Price$Comp_Region)

table(Car.Price$Comp_Region)# Check for COmpany on region 

#Cars door number 4 and 2 form string to numeric
levels(Car.Price$doornumber)<-c(4,2)
Car.Price$doornumber <- as.numeric(levels(Car.Price$doornumber))[Car.Price$doornumber]# Door Number to numeric


#Convert Cylinder Numbver to Numerical Representations

levels(Car.Price$cylindernumber) <- c(8, 5, 4, 6, 3, 12, 2)
Car.Price$cylindernumber <- as.numeric(levels(Car.Price$cylindernumber))[Car.Price$cylindernumber]# Cylinder number to numeric


#Outliers and their fixing



# Numerical COlumns
# wheelbase,# carlength,# carwidth,# carheight,# curbweight,# enginesize,# boreratio,# stroke,
# compressionratio,# horsepower,# peakrpm,# citympg,# highwaympg

Numeri_Column <- c('wheelbase', 'carlength', 'carwidth','carheight','curbweight','enginesize','boreratio','stroke','compressionratio',
                   'horsepower','peakrpm','citympg','highwaympg','price')


#plot all the numeric columns against price for Outliers
Car.Price_Numeri<-Car.Price[ , names(Car.Price) %in% Numeri_Column]
Car.Price_Numeri<-melt(Car.Price_Numeri,id.vars = "price")
ggplot(Car.Price_Numeri,aes(price,value))+geom_boxplot()+facet_wrap(~variable)#Plot


#Columns with outliers
#wheelbase carlength carwidth enginesize stroke compressionratio horsepower peakrpm citympg highwaympg
# Function to fix Outliers
Modify_outliers <- function(x){
  qnt <- quantile(x, probs=c(.25, .75))
  max_and_min <- quantile(x, probs=c(.05, .95))
  Inter_Q_Range <- 1.5 * IQR(x)
  x[x < (qnt[1] - Inter_Q_Range)] <- max_and_min[1]
  x[x > (qnt[2] + Inter_Q_Range)] <- max_and_min[2]
}


#Numerical Columns for OUtliers
Outlier_Treatment <- c('wheelbase', 'carlength', 'carwidth','enginesize','stroke','compressionratio',
                       'horsepower','peakrpm','citympg','highwaympg')

#Outlier Treatment for All the analysed Column
for(z in 1:length(Outlier_Treatment)){
  
  Car.Price[,Outlier_Treatment[z]] <- sapply(Car.Price[,Outlier_Treatment[z]], Modify_outliers)
}
summary(Car.Price)
#Function to handle Two Level Treatment
Two_level_Treatment<-function(x){
  levels(x)<-c(1,0) 
  x <- as.numeric(levels(x))[x] 
  
}

#Convert Factor with 2 levels to numerical variables
# fueltype,# aspiration,# enginelocation

level_2<-c('fueltype','aspiration','enginelocation')
Car.Price[,level_2]<-sapply(Car.Price[,level_2],Two_level_Treatment)


# Convert the multiple level factors with Dummy variables
#CarName
#Drivewheel
#carBody
#EngineType
#CylinderNumber
#FuelSystem
Empty_DF <- data.frame(matrix(ncol = 0, nrow = 205)) # To Store the Dummy Variables
Multilevel_Variable<-c('drivewheel','carbody','enginetype','fuelsystem','CarCompany','Comp_Region')#COlumns for which dummy needs to be created
Dummy_Multilevel <- function(x) {
  Dummy_Variable <- data.frame(model.matrix(~x,data =Car.Price))
  Dummy_Ready<-Dummy_Variable[,-1]
  Empty_DF<-cbind(Empty_DF,Dummy_Ready)
  return(Dummy_Ready) 
}
Dummy_Combined<-as.data.frame(sapply(Car.Price[,Multilevel_Variable],Dummy_Multilevel))

Cols_to_Remove<-c(Multilevel_Variable,'car_ID','CarBrand','CarName')


Updated_CarPrice<-Car.Price[ , !names(Car.Price) %in% Cols_to_Remove] 


# Combine the dummy variables and the numeric columns

Updated_CarPrice<-cbind(Updated_CarPrice,Dummy_Combined)
names(Updated_CarPrice)<-gsub("x","",names(Updated_CarPrice))# handle extra x added due to custom funciton

# Extract Testing and Training Data

# Cor_Matrix<-cor(Updated_CarPrice)
# write.csv(Cor_Matrix, 'cors.csv')

set.seed(123)
Training_indice<- sample(1: nrow(Updated_CarPrice),(.7*nrow(Updated_CarPrice)))
Training_data<-Updated_CarPrice[Training_indice,]
Test_data<-Updated_CarPrice[-Training_indice,]


################################################# 
##           Data Modelling                    ##
#################################################


#Model with all the Variables
model1<-lm(price~.,data = Training_data)

summary(model1)# Multiple R-squared:  0.9781,	Adjusted R-squared:  0.9638 

step <- stepAIC(model1, direction="both")# StepAIC BOthWays and Select the Model

model2<-lm(price ~ fueltype + aspiration + doornumber + enginelocation + 
             carwidth + carheight + cylindernumber + enginesize + boreratio + 
             stroke + compressionratio + horsepower + peakrpm + drivewheel.fwd + 
             carbody.hardtop + carbody.hatchback + carbody.sedan + carbody.wagon + 
             enginetype.dohcv + enginetype.l + enginetype.ohc + enginetype.ohcf + 
             enginetype.ohcv + enginetype.rotor + fuelsystem.2bbl + fuelsystem.mpfi + 
             fuelsystem.spdi + CarCompany.AUDI + CarCompany.BMW + CarCompany.BUICK + 
             CarCompany.CHEVROLET + CarCompany.DODGE + CarCompany.HONDA + 
             CarCompany.ISUZU + CarCompany.JAGUAR + CarCompany.MAZDA + 
             CarCompany.MITSUBISHI + CarCompany.NISSAN + CarCompany.PLYMOUTH + 
             CarCompany.PORSCHE + CarCompany.RENAULT + CarCompany.SAAB + 
             CarCompany.TOYOTA + CarCompany.VOLKSWAGEN + CarCompany.VOLVO,data=Training_data)

summary(model2) #Multiple R-squared:  0.9768,	Adjusted R-squared:  0.9661 
VIF_Model2<-data.frame("vifvalue"= vif(model2))
VIF_Model2[order(-VIF_Model2$vifvalue), , drop=FALSE]
# horsepower  0.203793 vif 44.815740
model3<-update(model2,~.-horsepower)
summary(model3)
VIF_Model3<-data.frame("vifvalue"= vif(model2))
VIF_Model3[order(-VIF_Model3$vifvalue), , drop=FALSE]
# peakrpm     0.363815   vif 4.706958
model4<-update(model3,~.-peakrpm)
summary(model4)
vif(model4)
# enginetype.ohcf   0.213201  vif 14.209353
model5<-update(model4,~.-enginetype.ohcf)
summary(model5)
vif(model5)
# carheight  0.196403 vif 5.720828
model6<-update(model5,~.-carheight)
summary(model6)
vif(model6)
# enginetype.l  0.239330 vif 3.675862
model7<-update(model6,~.-enginetype.l)
summary(model7)
vif(model7)
# doornumber  0.188764 vif 6.897459
model8<-update(model7,~.-doornumber)
summary(model8)
# CarCompany.MITSUBISHI   0.209071
model9<-update(model8,~.-CarCompany.MITSUBISHI)
summary(model9)
# enginetype.ohcv  0.297797  
model10<-update(model9,~.-enginetype.ohcv)
summary(model10)
# CarCompany.RENAULT    0.353271 
model11<-update(model10,~.-CarCompany.RENAULT)
summary(model11)
# CarCompany.PLYMOUTH 0.270356   
model12<-update(model11,~.-CarCompany.PLYMOUTH)
summary(model12)
# CarCompany.TOYOTA 0.162588 
model13<-update(model12,~.-CarCompany.TOYOTA)
summary(model13)
# CarCompany.DODGE  0.336588    
model14<-update(model13,~.-CarCompany.DODGE)
summary(model14)
# CarCompany.NISSAN  0.469794    
model15<-update(model14,~.-CarCompany.NISSAN)
summary(model15)
# CarCompany.MAZDA  0.350283 
model16<-update(model15,~.-CarCompany.MAZDA)
summary(model16)
# enginetype.ohc  0.321212    
model17<-update(model16,~.-enginetype.ohc)
summary(model17)
# fuelsystem.spdi  0.150792 
model18<-update(model17,~.-fuelsystem.spdi)
summary(model18)
# CarCompany.CHEVROLET  0.122813   
model19<-update(model18,~.-CarCompany.CHEVROLET)
summary(model19)
vif(model19)
# CarCompany.VOLKSWAGEN  0.105301  
model20<-update(model19,~.-CarCompany.VOLKSWAGEN)
summary(model20)
vif(model20)
#  compressionratio  0.070870 vif 119.792009 
model21<-update(model20,~.-compressionratio)
summary(model21)
vif(model21)
# fueltype 0.753638 vif 2.571386
model22<-update(model21,~.-fueltype)
summary(model22)
vif(model22)
#  fuelsystem.2bbl  0.06791 vif 4.528604
model23<-update(model22,~.-fuelsystem.2bbl)
summary(model23)
vif(model23)
# CarCompany.HONDA  0.134698 
model24<-update(model23,~.-CarCompany.HONDA)
summary(model24)
vif(model24)
# CarCompany.ISUZU 0.101676
model25<-update(model24,~.-CarCompany.ISUZU)
summary(model25)
vif(model25)
# carbody.hatchback 0.005918 vif 12.056942 
model26<-update(model25,~.-carbody.hatchback)
summary(model26) #Not much drop in R Squared and Adjusted R Squared
vif(model26)
# carbody.wagon 0.916323  
model27<-update(model26,~.-carbody.wagon)
summary(model27)
vif(model27)
# carbody.sedan 0.639238
model28<-update(model27,~.-carbody.sedan)
summary(model28)
vif(model28)
# carbody.hardtop  0.473694 
model29<-update(model28,~.-carbody.hardtop)
summary(model29)
vif(model29)
# enginesize vif 41.093245
model30<-update(model29,~.-enginesize)
summary(model30)#.02 reduction in Adjusted R square keep this model
vif(model30)
# enginetype.dohcv     -511.9     2988.9  -0.171 0.864301
model31<-update(model30,~.-enginetype.dohcv)
summary(model31)#.02 reduction in Adjusted R square keep this model
vif(model31)
# CarCompany.VOLVO     1539.5      915.7   1.681 0.095185 .  
model32<-update(model31,~.-CarCompany.VOLVO)
summary(model32)
vif(model32)
# aspiration          -1185.6      511.6  -2.318 0.022064 *  
model33<-update(model32,~.-aspiration)
summary(model33)
vif(model33)
# boreratio            3430.4     1067.7   3.213  0.00166 ** 
model34<-update(model33,~.-boreratio)
summary(model34)
vif(model34)
# stroke  0.012695 *
model35<-update(model34,~.-stroke)
summary(model35)# .003 reduction in R Squared accepting this model
vif(model35)
#CarCompany.AUDI  0.24500
model36<-update(model35,~.-CarCompany.AUDI)
summary(model36)
vif(model36)

# enginetype.rotor  0.03366 *
model37<-update(model36,~.-enginetype.rotor)
summary(model37)
vif(model37)
# CarCompany.SAAB  0.011693 *
model38<-update(model37,~.-CarCompany.SAAB)
summary(model38)
vif(model38)
# fuelsystem.mpfi   0.00112 ** 
model39<-update(model38,~.-fuelsystem.mpfi)
summary(model39)
vif(model39)

# CarCompany.PORSCHE  0.008421 ** 
model40<-update(model39,~.-CarCompany.PORSCHE)#COnsider Model as all the variables are significant and less VIF
summary(model40)
vif(model40)
summary(model40)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5772.7 -1387.5  -315.8   846.2  7835.4 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -79558.4     8995.9  -8.844 4.48e-15 ***
#   enginelocation    -21643.6     1795.8 -12.053  < 2e-16 ***
#   carwidth            1665.7      144.3  11.546  < 2e-16 ***
#   cylindernumber      1136.1      259.6   4.376 2.40e-05 ***
#   drivewheel.fwd     -2648.6      503.1  -5.264 5.40e-07 ***
#   CarCompany.BMW      8270.0     1190.5   6.946 1.44e-10 ***
#   CarCompany.BUICK    9748.3     1303.1   7.481 8.47e-12 ***
#   CarCompany.JAGUAR   9987.4     2023.7   4.935 2.32e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2335 on 135 degrees of freedom
# Multiple R-squared:  0.9126,	Adjusted R-squared:  0.908 
# F-statistic: 201.3 on 7 and 135 DF,  p-value: < 2.2e-16

VIF_Model40<-data.frame("vifvalue"= vif(model40))
VIF_Model40[order(-VIF_Model40$vifvalue), , drop=FALSE]
# cylindernumber    2.349766
# carwidth          2.282816
# drivewheel.fwd    1.581966
# CarCompany.BUICK  1.502952
# CarCompany.JAGUAR 1.481569
# CarCompany.BMW    1.254585
# enginelocation    1.166597


# carwidth            1665.7      144.3  11.546  < 2e-16 ***
model41<-update(model40,~.-carwidth)
summary(model41)# huge drop in R Sq ~10 discarding this model
vif(model41)

model42<-update(model40,~.-cylindernumber)#COnsider this model
summary(model42)# drop of <2 is observed This model can be considered
vif(model42)

summary(model42)
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -7581.2 -1320.4  -176.1   964.1  8988.4 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -89821.4     9246.3  -9.714  < 2e-16 ***
#   enginelocation    -24079.8     1817.7 -13.248  < 2e-16 ***
#   carwidth            1928.5      139.7  13.809  < 2e-16 ***
#   drivewheel.fwd     -2389.8      531.9  -4.493 1.49e-05 ***
#   CarCompany.BMW     10083.6     1188.2   8.486 3.23e-14 ***
#   CarCompany.BUICK   11464.2     1323.0   8.665 1.18e-14 ***
#   CarCompany.JAGUAR  14491.7     1855.1   7.812 1.37e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2486 on 136 degrees of freedom
# Multiple R-squared:  0.9002,	Adjusted R-squared:  0.8957 
# F-statistic: 204.4 on 6 and 136 DF,  p-value: < 2.2e-16

VIF_Model42<-data.frame("vifvalue"= vif(model42))
VIF_Model42[order(-VIF_Model42$vifvalue), , drop=FALSE]
# carwidth          1.887391
# drivewheel.fwd    1.560105
# CarCompany.BUICK  1.366872
# CarCompany.BMW    1.102573
# CarCompany.JAGUAR 1.098333
# enginelocation    1.054492

RSq<- c()
ARSq<-c()
model_Names<-str_c("model",seq(1:42))
list_models<-lapply(model_Names,get)
names(list_models)<-model_Names

inc<-1
for(inc in 1:42){
  Rsq_dum<-summary(list_models[[inc]])$r.sq
  RSq<-c(RSq,Rsq_dum)
  ARSq_dum<-summary(list_models[[inc]])$adj.r.sq
  ARSq<-c(ARSq,ARSq_dum)
  # COFF<-cbind(as.data.frame(Rsq),as.data.frame(ARSq))
  
}

model_Names<-as.data.frame(model_Names)
RSq<-as.data.frame(RSq)  
ARSq<-as.data.frame(ARSq) 
Modle_wise_coef<-data.frame("Model"= model_Names,"RSquared"= RSq,"Adjusted R Squared"= ARSq)
names(Modle_wise_coef)<-c("Model","RSquared","Adjusted R Squared") 

Modle_wise_coef

################################################# 
## predicting the car price in test dataset model 42 ##
#################################################

Test_data$Predicted_Price<-predict(model42,Test_data[,-which(colnames(Test_data)=='price')])
Test_data$Carid<-seq(1:62)

#Test the r square between actual and predicted price
rsquared_42<-(cor(Test_data$price,Test_data$Predicted_Price))^2 #0.8556636

#Plot the Predicted vs Actual Price
Test_data_plot<-data.frame(CarID=seq(1:nrow(Test_data)),
                           Predicted_Price=Test_data$Predicted_Price,Price=Test_data$price,
                           Residual=(Test_data$price-Test_data$Predicted_Price))


ggplot(data = Test_data_plot,aes(x=CarID,y=Price))+               
  geom_line(aes(colour="Actual"))+
  geom_line(aes(x=CarID,y=Predicted_Price,colour="Predicted"))+
  labs(title ="Predicted vs Actual Price-model 42", x = "CarID", y = "Price")  #Line Graph



ggplot(Test_data_plot, aes(CarID, Price)) +
  geom_smooth(method = 'lm', se = FALSE, color='blue') +
  geom_segment(aes(xend=CarID, yend=Predicted_Price), alpha=0.2) +
  geom_point(aes(color=abs(Residual), size=abs(Residual))) + # alpha mapped to absolute residuals
  scale_color_continuous(low = 'green', high = 'red') + # color mapped here
  guides(color=F, size=F) + # colour legends removed 
  labs(x='car id', y='price', title='actual vs predicted price (hollow circle)') +
  geom_point(aes(y=Predicted_Price), shape=1) +
  theme_bw()

par(mfrow=c(2,2))
plot(model42)

################################################# 
## predicting the car price in test dataset using model 40  ##
#################################################
Test_data$Predicted_Price<-predict(model40,Test_data[,-which(colnames(Test_data)=='price')])
Test_data$Carid<-seq(1:62)

#Test the r square between actual and predicted price
rsquared_40<-(cor(Test_data$price,Test_data$Predicted_Price))^2 #0.8677048

#Plot the Predicted vs Actual Price
Test_data_plot<-data.frame(CarID=seq(1:nrow(Test_data)),
                           Predicted_Price=Test_data$Predicted_Price,Price=Test_data$price,
                           Residual=(Test_data$price-Test_data$Predicted_Price))


ggplot(data = Test_data_plot,aes(x=CarID,y=Price))+               
  geom_line(aes(colour="Actual"))+
  geom_line(aes(x=CarID,y=Predicted_Price,colour="Predicted"))+
  labs(title ="Predicted vs Actual Price-model 40", x = "CarID", y = "Price")  #Line Graph



ggplot(Test_data_plot, aes(CarID, Price)) +
  geom_smooth(method = 'lm', se = FALSE, color='blue') +
  geom_segment(aes(xend=CarID, yend=Predicted_Price), alpha=0.2) +
  geom_point(aes(color=abs(Residual), size=abs(Residual))) + # alpha mapped to absolute residuals
  scale_color_continuous(low = 'green', high = 'red') + # color mapped here
  guides(color=F, size=F) + # colour legends removed 
  labs(x='car id', y='price', title='actual vs predicted price (hollow circle)-model 40') +
  geom_point(aes(y=Predicted_Price), shape=1) +
  theme_bw()

par(mfrow=c(2,2))
plot(model40)

################################################# 
## Final model 40  ##
#################################################

#Training R-Squared

Modle_wise_coef[Modle_wise_coef$Model == "model40",]
# model40 0.9125591          0.9080251

# Test R Squared
# 0.8677048


#COntributing Factors

as.data.frame(rownames(VIF_Model40))#
# rownames(VIF_Model40)
# 1        enginelocation
# 2              carwidth
# 3        cylindernumber
# 4        drivewheel.fwd
# 5        CarCompany.BMW
# 6      CarCompany.BUICK
# 7     CarCompany.JAGUAR


# The training data has mainly medium ranged cars prices and description i.e 7k to 20k
# so the model bests describes data in this range as suggested by Residual vs fitted and theoritical quantiles# 
# Same is complemented by the Actual vs predicted price of test data where model best predicts the
# price falling in the the range 7 to ~20 k




setwd("D:/ml/R/timeseries/Assignment")

##########################List of library##################
library(MASS)
library(car)
library(plyr)
library(lubridate)
library(raster)
library(zoo)
library(stats)
library(dplyr)
library(forecast)
require(graphics)
library(ggplot2)

###################Loading The Data #########################
globalData <- read.csv("Global Superstore.csv",stringsAsFactors = F)

str(globalData)
summary(globalData)


#################Data Cleaning ###############################3

##Checking for duplicate Data###
sum(duplicated(globalData))

##no duplicate Data ##

##checking for na##
sum(is.na(globalData))


##41296 na values ##

sum(is.na(globalData$Postal.Code))

###so all the na values are rom the postal code columns###

##so we remove the postalcode column

ncol(globalData)

globalData$Postal.Code <- NULL

ncol(globalData)

###removed the postalcode attribute

sum(is.na(globalData))

##no more na values###

#Since we have the Order.Id & Order.Dates we don't need the Row.Id column
globalData<-globalData[,-1]
str(globalData)


##now convert the chr to date columns wherever neccesarry 

globalData$Order.Date<- as.Date(globalData$Order.Date, "%d-%m-%Y")
globalData$Ship.Date<- as.Date(globalData$Ship.Date, "%d-%m-%Y")
str(globalData) 


#########################Data Preparation ################################################33

##the data cleaning process has been done.. lets start with the data preparation process before we start
##lets check for na value once again


sum(is.na(globalData))

####everything is fine now


globalData$Market.Segment <- paste(globalData$Market, globalData$Segment, sep = " ")


#we create a new variable so that we can do the aggregate which will not affect the orginal datasets


globalDatanew <- globalData

##aggregating the monthly data

globalDatanew$Year.Month <- format(as.Date(globalDatanew$Order.Date), "%Y-%m")


#Subset the data in 21 "Market Segments"
invisible(lapply(split(globalDatanew, globalDatanew$Market.Segment), function(x) {assign(paste0("Market.Segment", x$Market.Segment[1]), x, pos = .GlobalEnv)}))

#Aggregating the data for the Atribute: Sales, Profit & Qty compared on PI & Coeff of Variation

PI_1 <- globalDatanew %>%                                                    # Use dplyr for Aggregation
  group_by(Market.Segment) %>% 
  summarise(., sum(Sales),                           # Total Sales Per Segment Per Market
            sum(Profit),                          # Total Profit Per segment Per Market
            sum(Quantity),                        # Total Quantity per segment per Market
            sd(Profit)*100/mean(Profit),              # Profit Co-efficient of Variation
            sum(Sales)/(sum(Sales)-sum(Profit)))   

#Aggregate the Data monthly on each atribute
PI_2 <- globalDatanew %>%                                                    # Use dplyr for Aggregation
  group_by(Market.Segment, Year.Month) %>% 
  summarise(., sum(Sales),                           # Total Sales Per Segment Per Market
            sum(Profit),                          # Total Profit Per segment Per Market
            sum(Quantity),                        # Total Quantity per segment per Market
            sd(Profit)*100/mean(Profit),              # Profit Co-efficient of Variation
            sum(Sales)/(sum(Sales)-sum(Profit)))   # Profitability Index, (selling cost/actual cost)


# Exploratory DATA Analysis -----

# . . . . Categorical ----

pa_segments <- globalDatanew[,c("Profit","Sales","Market","Segment","Quantity")] %>%   # Use dplyr for Aggregation
  group_by(Market,Segment) %>% 
  dplyr::summarise(., sum(Sales),                           # Total Sales Per Segment Per Market
                   sum(Profit),                          # Total Profit Per segment Per Market
  )

colnames(PI_2) <- c("Mkt_Segment", "Yr_Month", "Sales", "Profit", "Quantity", "Coeff_of_Var", "PI")
retail_cov <- aggregate.data.frame(globalDatanew$Profit,by=list(globalDatanew$Market.Segment),FUN = function(x) {sd(x)/mean(x)})
colnames(retail_cov) <- c("Market Segments","COV")

retail_order <- retail_cov[with(retail_cov,order(COV)),]

retail_order

colnames(pa_segments) = c("Market","Segment","Sales","Profit")

ggplot(pa_segments, aes( Segment, Sales, fill=Market)) + geom_bar(position = "dodge",stat = "identity")



#Since data for Canada market is not available for all the 4 years (48 months),
#so we have ignored Canada from Top 2 Mkt Segments

#Top 2 Mkt Segments based on CoV are:
# 1. APAC Consumer
# 2. APAC Corporate

#considering cannda it would have been these top segements
# 
# 7     Canada Consumer  2.093766
# 8    Canada Corporate  2.767642
# 9  Canada Home Office  3.175392
# 4       APAC Consumer  4.206702
# 5      APAC Corporate  4.231301
# 6    APAC Home Office  4.633390
# 13        EU Consumer  4.718084
# 14       EU Corporate  4.776482
# 15     EU Home Office  4.923759
# 18  LATAM Home Office  5.336331
# 16     LATAM Consumer  5.438845
# 17    LATAM Corporate  5.789517
# 3  Africa Home Office  6.264113
# 21     US Home Office  6.280008
# 1     Africa Consumer  7.351006
# 20       US Corporate  7.616929
# 2    Africa Corporate  9.334133
# 19        US Consumer  9.389450
# 10      EMEA Consumer 14.441103
# 12   EMEA Home Office 21.800527
# 11     EMEA Corporate 22.038317


#Subsetting the PI_2 to these 2 market segments
top_2 <- subset(PI_2, Mkt_Segment == "APAC Consumer" | 
                  Mkt_Segment == "APAC Corporate")
str(top_2)

#Change column names
names(top_2) <- c("Mkt_Segment", "Yr_Month", "Sales", "Profit", "Quantity", "Coeff_of_Var", "PI")


#Subset the data in Top 2 "Market Segments"
invisible(lapply(split(top_2, top_2$Mkt_Segment), function(x) {assign(paste0("Mkt_Segment", x$Mkt_Segment[1]), x, pos = .GlobalEnv)}))


#############Modeling############################

APAC_Consumer <- `Mkt_SegmentAPAC Consumer`[,c(2:5)]

# US_Consumer <-`Mkt_SegmentUS Consumer`[,c(2:5)]


APAC_Corporate <- `Mkt_SegmentAPAC Corporate`[,c(2:5)]

# **************************************************************
#                 APAC CONSUMER ----
# **************************************************************



# . . . . APAC_consumer_sales ----

#Conversion of APAC sales data to time series..
APAC_consumer_sales_ts <- ts(APAC_Consumer$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_consumer_sales_ts)


# Smootheing of the time series
Smoothed_APAC_consumer_sales <- stats::filter(APAC_consumer_sales_ts,                # Here had to use stats::filter due tp plyr issue#
                                              filter=rep(1/3,3,method='convolution',
                                                         sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_consumer_sales_df <- data.frame(cbind(APAC_Consumer$Yr_Month,
                                           Smoothed_APAC_consumer_sales))

#Renaming the coloumns
colnames(APAC_consumer_sales_df) <- c("month","sales")

APAC_consumer_sales_df$sales <- as.numeric(as.character((APAC_consumer_sales_df$sales)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_consumer_sales_df$sales[3] - APAC_consumer_sales_df$sales[2]
APAC_consumer_sales_df$sales[1] <- APAC_consumer_sales_df$sales[2]-diff_1

diff_2 <- APAC_consumer_sales_df$sales[46] - APAC_consumer_sales_df$sales[45]
APAC_consumer_sales_df$sales[47] <- APAC_consumer_sales_df$sales[46]+ diff_2

diff_3 <- APAC_consumer_sales_df$sales[47] - APAC_consumer_sales_df$sales[46]
APAC_consumer_sales_df$sales[48] <- APAC_consumer_sales_df$sales[47]+ diff_3

#plot the smoothed sales curve
lines(Smoothed_APAC_consumer_sales,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_consumer_sales.ts <- ts(APAC_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                             end=c(2014,12))

APAC_Consumer_sales_hw <- ts(APAC_consumer_sales_df$sales,frequency=12,start=c(2011,1),
                             end=c(2014,12))
#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_Consumer_sales_hw)
plot(APAC_Consumer_sales_hw)
HoltWinters(APAC_Consumer_sales_hw)$fitted
APAC_Consumer_sales_hw_hw<-HoltWinters(APAC_Consumer_sales_hw)
plot(APAC_Consumer_sales_hw)
#lines(predict(APAC_Consumer_sales_hw_hw$fitted,n.ahead=48),col=2)
predict(APAC_Consumer_sales_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
# Jan      Feb      Mar      Apr      May      Jun
# 74889.15 69135.22 65191.81 69070.63 75428.15 72743.61


#SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_consumer_sales.ts)-ntest
train.ts <- window(APAC_consumer_sales.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_consumer_sales.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))


# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_consumer_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")


# MAPE STATISTIC CALCULATION

#Calculate MAPE and other performance metrics
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_consumer_accuracy

# FOR LINEAR MODEL MAPE IS 10.69


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values

#Plot acf                                  
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=2 or 2 as there is only one significant peak in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 200 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,2
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,2))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="green")
summary(train.res.ar2)

# from the summaries of both the models using pdq of 2,0,1 was giving a better MAPE vale ( MAPE =190 vs MAPE=165)

#plot the ACF of AR(2,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.

#calculate the accuracy of combined linear and AR model
APAC_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
APAC_consumer_combined_accuracy

#Accuracy of the model decreased. Mape is 9.9


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 25.74
#  The Linear model is better than the auto arima model.


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #linear model and plot it
# autoarima_combined <- auto.arima(APAC_consumer_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales

train.lm.model <- tslm(APAC_consumer_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="red")

# . . . . APAC_consumer_Quantity ----

#Conversion of APAC qunatity data to time series..
APAC_consumer_qunatity_ts <- ts(APAC_Consumer$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_consumer_qunatity_ts)


#Smootheing of the time series

Smoothed_APAC_consumer_quantity <- stats::filter(APAC_consumer_qunatity_ts,                
                                                 filter=rep(1/3,3,method='convolution',
                                                            sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_consumer_quantity_df <- data.frame(cbind(APAC_Consumer$Yr_Month,
                                              Smoothed_APAC_consumer_quantity))

##Renaming the coloumns
colnames(APAC_consumer_quantity_df) <- c("month","quantity")

APAC_consumer_quantity_df$quantity <- as.numeric(as.character((APAC_consumer_quantity_df$quantity)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_consumer_quantity_df$quantity[3] - APAC_consumer_quantity_df$quantity[2]
APAC_consumer_quantity_df$quantity[1] <- APAC_consumer_quantity_df$quantity[2]-diff_1

diff_2 <- APAC_consumer_quantity_df$quantity[46] - APAC_consumer_quantity_df$quantity[45]
APAC_consumer_quantity_df$quantity[47] <- APAC_consumer_quantity_df$quantity[46]+ diff_2

diff_3 <- APAC_consumer_quantity_df$quantity[47] - APAC_consumer_quantity_df$quantity[46]
APAC_consumer_quantity_df$quantity[48] <- APAC_consumer_quantity_df$quantity[47]+ diff_3

#plot the smoothed quantity curve
lines(Smoothed_APAC_consumer_quantity,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_consumer_quantity.ts <- ts(APAC_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                                end=c(2014,12))
APAC_Consumer_quantity_hw <- ts(APAC_consumer_quantity_df$quantity,frequency=12,start=c(2011,1),
                                end=c(2014,12))

APAC_consumer_quantity.ts

#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_Consumer_quantity_hw)
plot(APAC_Consumer_quantity_hw)
HoltWinters(APAC_Consumer_quantity_hw)$fitted
APAC_Consumer_quantity_hw_hw<-HoltWinters(APAC_Consumer_quantity_hw)
plot(APAC_Consumer_quantity_hw)
lines(predict(APAC_Consumer_quantity_hw_hw,n.ahead=48),col=2)
predict(APAC_Consumer_quantity_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#      Jan      Feb      Mar      Apr      May      Jun
# 2015 713.4561 616.5936 582.6012 646.8259 751.6196 740.7689


# SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_consumer_quantity.ts)-ntest
train.ts <- window(APAC_consumer_quantity.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_consumer_quantity.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))



# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_consumer_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")



# MAPE STATISTIC CALCULATION
#Calculate MAPE and other performance metrics
APAC_consumer_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_consumer_accuracy
# FOR LINEAR MODEL MAPE IS 13.11


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values                                
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=3 as there are 3  significant peaks in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 203 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,3
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,3))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                # MAPE- 144.6

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="blue")
summary(train.res.ar2)               # MAPE- 190.00

# from the summaries of both the models using pdq of 2,0,3 was giving a better MAPE vale

#plot the ACF of AR(2,0,3)
acf(train.res.arima.pred1$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.
#calculate the accuracy of combined linear and AR model
APAC_consumer_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred1$mean),valid.ts)
APAC_consumer_combined_accuracy

#Accuracy of the model is 13.077, so Linear model is beter


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 25.34
#  so manual linear model is better than auto arima model


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #autoarima and plot it
# autoarima_combined <- auto.arima(APAC_consumer_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales
train.lm.model <- tslm(APAC_consumer_quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="red")

APAC_consumer_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(APAC_consumer_final_df) <- c("Sales","Qty")




###############################3
#################################3
#########################################






###############################################333
# . . . . APAC Corporate ----
################################################33
APAC_Corporate
#Conversion of APAC Corporate sales data to time series..
APAC_Corporate_sales_ts <- ts(APAC_Corporate$Sales,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Corporate_sales_ts)


# Smootheing of the time series
Smoothed_APAC_Corporate_sales <- stats::filter(APAC_Corporate_sales_ts,                
                                            filter=rep(1/3,3,method='convolution',
                                                       sides=2)) 

#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_Corporate_sales_df <- data.frame(cbind(APAC_Corporate$Yr_Month,
                                            Smoothed_APAC_Corporate_sales))
#Renaming the coloumns
colnames(APAC_Corporate_sales_df) <- c("month","sales")

APAC_Corporate_sales_df$sales <- as.numeric(as.character((APAC_Corporate_sales_df$sales)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_Corporate_sales_df$sales[3] - APAC_Corporate_sales_df$sales[2]
APAC_Corporate_sales_df$sales[1] <- APAC_Corporate_sales_df$sales[2]-diff_1

diff_2 <- APAC_Corporate_sales_df$sales[46] - APAC_Corporate_sales_df$sales[45]
APAC_Corporate_sales_df$sales[47] <- APAC_Corporate_sales_df$sales[46]+ diff_2

diff_3 <- APAC_Corporate_sales_df$sales[47] - APAC_Corporate_sales_df$sales[46]
APAC_Corporate_sales_df$sales[48] <- APAC_Corporate_sales_df$sales[47]+ diff_3

#plot the smoothed sales curve
lines(Smoothed_APAC_Corporate_sales,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_Corporate_sales.ts <- ts(APAC_Corporate_sales_df$sales,frequency=12,start=c(2011,1),
                           end=c(2014,12))

APAC_Corporate_sales_hw <- ts(APAC_Corporate_sales_df$sales,frequency=12,start=c(2011,1),
                           end=c(2014,12))
#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_Corporate_sales_hw)
plot(APAC_Corporate_sales_hw)
HoltWinters(APAC_Corporate_sales_hw)$fitted
APAC_Corporate_sales_hw_hw<-HoltWinters(APAC_Corporate_sales_hw)
plot(APAC_Corporate_sales_hw)
lines(predict(APAC_Corporate_sales_hw_hw,n.ahead=48),col=2)
predict(APAC_Corporate_sales_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
# Jan      Feb      Mar      Apr      May      Jun
# 37328.61 27477.05 23498.11 28989.13 35732.73 34674.85


#SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_Corporate_sales.ts)-ntest
train.ts <- window(APAC_Corporate_sales.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_Corporate_sales.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))


# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_Corporate_sales,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red")  
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$lower,col="blue")


# MAPE STATISTIC CALCULATION

#Calculate MAPE and other performance metrics
APAC_Corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_Corporate_accuracy

# FOR LINEAR MODEL MAPE IS 11.44


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values

#Plot acf                                  
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=1 or 0  as there is only one significant peak in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 200 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,0
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,0))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="green")
summary(train.res.ar2)

# from the summaries of both the models using pdq of 1,0,1 was giving a better MAPE vale ( MAPE =318 vs MAPE=174)

#plot the ACF of AR(1,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.

#calculate the accuracy of combined linear and AR model
APAC_Corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
APAC_Corporate_combined_accuracy

#Accuracy of the model decreased. Mape is 13.44


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 29.74
#  The Linear model is better than the auto arima model.


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #linear model and plot it
# autoarima_combined <- auto.arima(APAC_consumer_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales

train.lm.model <- tslm(APAC_Corporate_sales.ts~trend+I(sin(2*pi*trend/12))+I(cos(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast
plot(train.lm.total.forecast,col="red")



# . . . . APAC_Corporate_Quantity ----

#Conversion of APAC qunatity data to time series..
APAC_Corporate_qunatity_ts <- ts(APAC_Corporate$Quantity,frequency=12,start=c(2011,1),end=c(2014,12))
plot(APAC_Corporate_qunatity_ts)


#Smootheing of the time series

Smoothed_APAC_Corporate_quantity <- stats::filter(APAC_Corporate_qunatity_ts,                
                                                  filter=rep(1/3,3,method='convolution',
                                                             sides=2)) 
#Converting the Smoothed series into a data frame and adding the Yr_Month coloum to from original sales data.
APAC_Corporate_quantity_df <- data.frame(cbind(APAC_Corporate$Yr_Month,
                                               Smoothed_APAC_Corporate_quantity))

##Renaming the coloumns
colnames(APAC_Corporate_quantity_df) <- c("month","quantity")

APAC_Corporate_quantity_df$quantity <- as.numeric(as.character((APAC_Corporate_quantity_df$quantity)))

#As the smoothening removes the first and the last data values, they need to befilled in
diff_1 <- APAC_Corporate_quantity_df$quantity[3] - APAC_Corporate_quantity_df$quantity[2]
APAC_Corporate_quantity_df$quantity[1] <- APAC_Corporate_quantity_df$quantity[2]-diff_1

diff_2 <- APAC_Corporate_quantity_df$quantity[46] - APAC_Corporate_quantity_df$quantity[45]
APAC_Corporate_quantity_df$quantity[47] <- APAC_Corporate_quantity_df$quantity[46]+ diff_2

diff_3 <- APAC_Corporate_quantity_df$quantity[47] - APAC_Corporate_quantity_df$quantity[46]
APAC_Corporate_quantity_df$quantity[48] <- APAC_Corporate_quantity_df$quantity[47]+ diff_3

#plot the smoothed quantity curve
lines(Smoothed_APAC_Corporate_quantity,col='red',lwd=2)

#Reconversion of the filled in data frame to time series
APAC_Corporate_quantity.ts <- ts(APAC_Corporate_quantity_df$quantity,frequency=12,start=c(2011,1),
                                 end=c(2014,12))
APAC_Corporate_quantity_hw <- ts(APAC_Corporate_quantity_df$quantity,frequency=12,start=c(2011,1),
                                 end=c(2014,12))

APAC_Corporate_quantity.ts

#Predicting the time series using Holtwinters algorithm.
HoltWinters(APAC_Corporate_quantity_hw)
plot(APAC_Corporate_quantity_hw)
HoltWinters(APAC_Corporate_quantity_hw)$fitted
APAC_Corporate_quantity_hw_hw<-HoltWinters(APAC_Corporate_quantity_hw)
plot(APAC_Corporate_quantity_hw)
lines(predict(APAC_Corporate_quantity_hw_hw,n.ahead=48),col=2)
predict(APAC_Corporate_quantity_hw_hw,n.ahead=6)
# predicted sales values based on Holtwinters
#      Jan      Feb      Mar      Apr      May      Jun
# 2015 346.3824 251.6362 262.8900 330.3660 407.8282 381.4570


# SPlitting the Data into TRAIN and Validation for Regression line fitting

#create windows for train and validation
ntest <- 6
nTrain <- length(APAC_Corporate_quantity.ts)-ntest
train.ts <- window(APAC_Corporate_quantity.ts,start = c(2011,1),end=c(2011,nTrain))
valid.ts <- window(APAC_Corporate_quantity.ts,start=c(2011,nTrain+1), 
                   end=c(2011,nTrain+ntest))



# Curve fitting for Linerar Regression Model ----

#create a linear regression model and plot it
train.lm <- tslm(train.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm)
train.lm.forecast <- forecast(train.lm,h=ntest,level=0)
plot(Smoothed_APAC_Corporate_quantity,ylab="Sales",xlab="Time",bty='l',
     xaxt='n',col="red")  
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.lm.forecast$fitted,lwd=2,col="black",lty=3)
lines(train.lm.forecast$mean,col="blue")



# MAPE STATISTIC CALCULATION
#Calculate MAPE and other performance metrics
APAC_Corporate_accuracy <- accuracy(train.lm.forecast$mean,valid.ts)  
APAC_Corporate_accuracy
# FOR LINEAR MODEL MAPE IS 9.89


# . .AUTOREGRESSION Models ARIMA ----

# Plotting of ACF and PACF plots for caluclation of the p,q,d values                                
acf(train.lm.forecast$residuals,lag.max = 12)
pacf(train.lm.forecast$residuals,lag.max = 12)

# Looking at the ACF and PACF graphs, the p and q values can be determined as 
# p= 2 as there are 2 significant peaks in the PACF plot (AR=2)
# q=1 as there are 1 significant peaks in the ACF plot (MA)
# d=0 as we did not do any differencing and the model is stationary
# first i will run the model with pdq as 203 and then one more model with pdq as 201, depending on the statistics 
# the final pdq can be selected


#Model AR and plot it with pdq = 2,0,0
train.res.ar1 <- arima(train.lm.forecast$residuals,order=c(2,0,2))
train.res.arima.pred1 <- forecast(train.res.ar1,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred1$fitted,lwd=2,col="blue")

#check the summary of the AR model
summary(train.res.ar1)                # MAPE- 144.6

#Model AR and plot it with pdq = 2,0,1
train.res.ar2 <- arima(train.lm.forecast$residuals,order=c(2,0,1))
train.res.arima.pred2 <- forecast(train.res.ar2,h=ntest)
plot(train.lm$residuals,ylab="Residuals",xlab="Time",
     bty="l",xaxt="n",xlim=c(2011,2014),main="")
axis(1,at=seq(2011,2014,1),labels=format(seq(2011,2014,1)))
lines(train.res.arima.pred2$fitted,lwd=2,col="blue")
summary(train.res.ar2)               # MAPE- 190.00

# from the summaries of both the models using pdq of 2,0,1 curve fitting even if the MAPE VALUE IS Abit higher

#plot the ACF of AR(2,0,1)
acf(train.res.arima.pred2$residuals,lag.max = 12)
# Now the graph resembles noise, not singnificant peaks apart from the 0 lag.
#calculate the accuracy of combined linear and AR model
APAC_Corporate_combined_accuracy <- accuracy((train.lm.forecast$mean+train.res.arima.pred2$mean),valid.ts)
APAC_Corporate_combined_accuracy

#Accuracy of the model is 11.12, so Linear model is beter


# . . .Curve Fitting VIA Auto ARIMA  Auto ARIMA ----

#Create autoarima model and check its accuracy
autoarima_ts <- auto.arima(train.ts)
tsdiag(autoarima_ts)
autoarima_forecast <- forecast(autoarima_ts,h=ntest)
autoarima_acc <- accuracy(autoarima_forecast,valid.ts)
autoarima_acc
# Mape of validation is # 9.72
#  so manual linear model is better than auto arima model


# . . . . . . . . Forecast ----

# #Forecasting for the next six months.
# #Forecast for the next 6 months will be made on the 
# #autoarima and plot it
# autoarima_combined <- auto.arima(APAC_Corporate_sales)
# future_forecast_APAC_sales <- forecast(autoarima_combined,h=6,level=0.2,0.4,0.6,0.8)
# plot(future_forecast_EU_sales,col="blue")
# future_forecast_APAC_sales
train.lm.model <- tslm(APAC_Corporate_quantity.ts~trend+I(sin(2*pi*trend/12))+season)
summary(train.lm.model)
train.lm.total.forecast_quan <- forecast(train.lm.model,h=6,level=c(0.2,0.4,0.6,0.8))
train.lm.total.forecast_quan
plot(train.lm.total.forecast_quan,col="red")

APAC_Corporate_final_df <- data.frame(train.lm.total.forecast$mean,train.lm.total.forecast_quan$mean)
colnames(APAC_Corporate_final_df) <- c("Sales","Qty")


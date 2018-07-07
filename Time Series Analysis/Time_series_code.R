
#### Install Libraries#######
install.packages("xlsx")
install.packages("tseries")
install.packages("forecast")
install.packages("MASS")
install.packages("fUnitRoots")

################################## Loading Libraries ##########################3
library("xlsx")
library("tseries")
library("forecast")
library("MASS")
library("fUnitRoots")

###########Setting working directory#############

getwd()

path <- "D:/R Directory"

setwd(path)


####################Data Preparation#################################

air<- data.frame(AirPassengers)

air_tc_train <- ts(air$AirPassengers, start = c(2005,1), end = c(2016,12), frequency = 12)

air_tc_train<-window(air_tc_train, 2005, c(2015, 12))

class(air_tc_train)

boxplot(air_tc_train~cycle(air_tc_train))

plot(decompose(air_tc_train), xlab= "Year(t)")

##################### Using Boxcox function for data transformation##################

lambda <- BoxCox.lambda(air_tc_train)

plot.ts(BoxCox(air_tc_train, lambda = lambda))

plot.ts(diff(BoxCox(air_tc_train, lambda = lambda)))


plot(decompose(diff(BoxCox(air_tc_train, lambda = lambda))))

air_tc_train_box <- BoxCox(air_tc_train, lambda = lambda)


air_tc_train_box_d <- diff(BoxCox(air_tc_train, lambda = lambda))

plot(decompose(air_tc_train_box_d))

stl(air_tc_train_box_d, s.window="periodic")


###### Dickey Fuller test for Stationarity###########

adf.test(air_tc_train_box_d)


####ACF and PACF Plots for P, Q#######

acf(air_tc_train_box_d)

#q= 1,5

pacf(air_tc_train_box_d)

#p = 2,4


#######Model fitting and testing on various p,d,q values###########

fit <- arima(air_tc_train_box, c(2, 1, 1),seasonal = list(order = c(2, 1, 1), period = 12) )

#sigma^2 estimated as 5.055e-05:  log likelihood = 416.9,  aic = -819.8

fit1 <- arima(air_tc_train_box, c(2, 1, 5),seasonal = list(order = c(2, 1, 5), period = 12) )

#sigma^2 estimated as 7.303e-05:  log likelihood = 265.71,  aic = -505.42

fit2 <- arima(air_tc_train_box, c(4, 1, 1),seasonal = list(order = c(4, 1, 1), period = 12) )

#sigma^2 estimated as 8.28e-05:  log likelihood = 263.84,  aic = -505.68

fit3 <- arima(air_tc_train_box, c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12) )

#sigma^2 estimated as 0.0001021:  log likelihood = 260.5,  aic = -515.01

fit4 <- auto.arima(air_tc_train_box_d)

fit5<- arima(air_tc_train_box, c(1, 1, 1),seasonal = list(order = c(0, 0, 2), period = 12) )

##### Check RMSE and MAPE values for each model, Minimum need to be picked ######

ds <-summary(fit1)

remove(ds)



################# Check ACF Plot of the residuals of each model and pick model in which bars don't cut the threshold

acf(fit1$residuals)

####### Forecasting for next 4 years########

pred_fit1 <- predict(fit1, n.ahead = 1*12)

ts.plot(air_tc_train_box,pred_fit1$pred ,lty = c(1,3),col.lab="red", col= c(12,258))

ts.plot(exp(log(lambda * air_tc_train_box + 1) / lambda),exp(log(lambda * pred_fit1$pred + 1) / lambda) ,
        lty = c(1,3),col.lab="red", 
        col= c(12,258), ylab= "Values(#)",xlab= "Year(t)")

    ts.plot(air_tc_train)


##### Combining actual and forecasted values###############

comb <- ts.union(air_tc_train_box, pred_fit1$pred)
final_t <-pmin(comb[,1], comb[,2], na.rm = TRUE)

class(final_t)


##### Removing transformation from the dataset #############


  final_forecast = exp(log(lambda * final_t + 1) / lambda)

####### storing the data in excel for comparison##############

write.xlsx(final_forecast,file = "Time_Series.xlsx",sheetName = "Sheet1")

write.xlsx(AirPassengers,file = "Time_Series.xlsx",sheetName = "Sheet2")

write.csv(final_forecast,file = "C:/Users/ekratsh/Desktop/Training/Day 4/Time Series Analysis/Time_Series.csv")





library(XLConnect)

library(stargazer)

writeWorksheetToFile("model1.xlsx", 
                     out, 
                     sheet = "summary", 
                     header = TRUE,
                     clearSheets = TRUE)

write.xlsx(out,file = "Tim.xlsx",sheetName = "Sheet1")

stargazer(fit1, summary=TRUE, rownames=FALSE)


out <- capture.output(summary(fit1))

cat("My title", out, file="summary_of_time_series_model.txt", sep="n", append=TRUE)





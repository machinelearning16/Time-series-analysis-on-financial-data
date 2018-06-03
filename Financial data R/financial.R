data <- read.table(choose.files(), sep=",")
View(data)

myts1  <- ts(data$V1, start=1, end=14400, frequency=1)

plot(myts1)

install.packages("forecast")
require(forecast)

#Gaussian Processes

#Holtwinters Exponential Smoothening
#Single exponential smoothening
fit1 <- HoltWinters(myts1, beta=FALSE, gamma=FALSE)
accuracy(fit1$fitted, myts1)
forecast(fit1,3)

#Double exponential smoothening
fit2 <- HoltWinters(myts1, gamma=FALSE)
accuracy(fit2$fitted, myts1)
forecast(fit2,3)
plot(forecast(fit2, 3))

#In holtwinters method we have to do double exponential smoothening 
#to get accurate result in variations

# ARIMA Method(AR(Autoregression),I(Integerated),MA(Moving Average))(Manually)

#Find value of P by ACF(Autocorrelation)=40
acf(myts1)

#Find value of Q by PCF(Partial Autocorrelation)=4
pacf(myts1)

#Find the value of D to check data is stationary or non stationary
#If data is stationary then value will be 0 but if is non stationary then
#then it will have some value to make data stationary
adf.test(myts1)
diff(myts1, differences=24)
ndiffs(myts1)

#Fitting ARIMA Model
fit <- arima(myts1,order=c(24,1,4))
summary(fit)
fit <-auto.arima(myts1)
summary(fit)

forecast(fit, 1)

accuracy(fit)

plot(forecast(fit, 4))
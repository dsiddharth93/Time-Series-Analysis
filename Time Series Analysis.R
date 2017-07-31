USBdata<-read.csv(file.choose(),header=T)
class(USBdata)
USBTP<-ts(USBdata$TP,start=c(1995,1),freq=12)
class(USBTP)
USBV<-ts(USBdata$V,start=c(1995,1),freq=12)
class(USBV)
boxplot(USBTP~cycle(USBTP))
boxplot(USBV~cycle(USBV))
USBTP.july<-window(USBTP,start=c(2000,7),freq=T)
USBTP.july
plot(USBTP.july)
USBV.july<-window(USBV,start=c(2000,7),freq=T)
USBV.july
plot(USBV.july)
decompose(USBTP)
plot(decompose(USBTP))
decompose(USBV)
plot(decompose(USBV))
ts.plot(USBTP,USBV,col=c("green","red"))
****************************************
  #correlation
  acf(USBTP)
acf(USBV)
acf(ts.intersect(USBTP,USBV))
****************************************
  #US border cossing prediction for next 10 years
  USBTP.HW<-HoltWinters(USBTP)
plot(USBTP.HW)
USBTP.forecast<-predict(USBTP.HW,n.ahead=10*12)
ts.plot(USBTP,USBTP.forecast,lty=1:2,col=c("blue","red"))
USBV.HW<-HoltWinters(USBV)
plot(USBV.HW)
USBV.forecast<-predict(USBV.HW,n.ahead=10*12)
ts.plot(USBV,USBV.forecast,lty=1:2,col=c("green","brown"))
***********************************************************
  #US border cossing prediction for first six months of 2015
  USBdata2<-read.csv(file.choose(),header=T)
USBTP2<-ts(USBdata2$TP,start=c(1995,1),freq=12)
class(USBTP2)
USBTP2.HW<-HoltWinters(USBTP2)
plot(USBTP2.HW)
USBTP2.forecast<-predict(USBTP2.HW,n.ahead=6)
ts.plot(USBTP2,USBTP2.forecast,lty=1:2,col=c("blue","red"))
USBTP2.forecast
USBTP.test<-window(USBTP,start=c(2015,1))
accuracy(USBTP2.forecast,USBTP.test) #gives MAPE value for predicted vs true data 
USBV2<-ts(USBdata2$V,start=c(1995,1),freq=12)
class(USBV2)
USBV2.HW<-HoltWinters(USBV2)
plot(USBV2.HW)
USBV2.forecast<-predict(USBV2.HW,n.ahead=6)
ts.plot(USBV2,USBV2.forecast,lty=1:2,col=c("blue","red"))
USBV2.forecast
USBTV.test<-window(USBTV,start=c(2015,1))
accuracy(USBTV2.forecast,USBTV.test) # gives MAPE value for predicted vs true data 
************************************************************
  acf(diff(USBTP))
acf(diff(USBV))
USBTP.ar<-ar(USBTP,method="mle")
USBTP.ar
acf(USBTP.ar$res[-1],na.action=na.pass)
USBV.ar<-ar(USBV,method="mle")
USBV.ar
acf(USBV.ar$res[-1],na.action=na.pass)
*************************************************************
  #arima model for passengers
  USBTP.ma<-arima(USBTP,order=c(0,0,3))
USBTP.ma
USBTP.arma<-arima(USBTP,order=c(1,0,1))
USBTP.arma
USBTP.arma<-arima(USBTP,order=c(2,0,2))
USBTP.arma
USBTP.arma<-arima(USBTP,order=c(9,0,9))
USBTP.arma
USBTP.arima<-arima(USBTP,order=c(11,1,1))
USBTP.arima
USBTP.predict<-predict(USBTP.arima,n.ahead=5*12)
ts.plot(USBTP,USBTP.predict$pred,lty=1:2,col=c("red","green"))
#arima model for vehicles
USBV.ma<-arima(USBV,order=c(0,0,3))
USBV.ma
USBV.arma<-arima(USBV,order=c(1,0,1))
USBV.arma
USBV.arma<-arima(USBV,order=c(2,0,2))
USBV.arma
USBV.arma<-arima(USBV,order=c(9,0,9))
USBV.arma
USBV.arima<-arima(USBV,order=c(11,1,2))
USBV.arima
USBV.predict<-predict(USBV.arima,n.ahead=5*12)
ts.plot(USBV,USBV.predict$pred,lty=1:2,col=c("blue","brown"))
#arima model for Total passengers for 2015
USBTP2.ma<-arima(USBTP2,order=c(0,0,3))
USBTP2.ma
USBTP2.arma<-arima(USBTP2,order=c(1,0,1))
USBTP2.arma
USBTP2.arma<-arima(USBTP2,order=c(2,0,2))
USBTP2.arma
USBTP2.arma<-arima(USBTP2,order=c(9,0,9))
USBTP2.arma
USBTP2.arima<-arima(USBTP2,order=c(11,1,1))
USBTP2.arima
USBTP2.predict<-predict(USBTP2.arima,n.ahead=6)
USBTP2.predict
ts.plot(USBTP2,USBTP2.predict$pred,lty=1:2,col=c("red","green"))
#arima model for total vehicles for 2015
USBV2.ma<-arima(USBV2,order=c(0,0,3))
USBV2.ma
USBV2.arma<-arima(USBV2,order=c(1,0,1))
USBV2.arma
USBV2.arma<-arima(USBV2,order=c(2,0,2))
USBV2.arma
USBV2.arma<-arima(USBV2,order=c(6,0,6))
USBV2.arma
USBV2.arma<-arima(USBV2,order=c(9,0,9))
USBV2.arma
USBV2.arima<-arima(USBV2,order=c(11,1,2))
USBV2.arima
USBV2.predict<-predict(USBV2.arima,n.ahead=6)
USBV2.predict
ts.plot(USBV2,USBV2.predict$pred,lty=1:2,col=c("blue","brown"))
******************************************************************
  #sarima for total passengers and total vehicles
  USBdata<-read.csv(file.choose(),header=T)
USBTP<-ts(USBdata$TP,start=c(1995,1),freq=12)
USBTV<-ts(USBdata$V,start=c(1995,1),freq=12)
USBTP.actual<-window(USBTP,start=c(1995,1),end=c(2014,12))
USBTP.test<-window(USBTP,start=c(2015,1))
USBTV.actual<-window(USBTV,start=c(1995,1),end=c(2014,12))
USBTV.test<-window(USBTV,start=c(2015,1))
library(forecast)
Tsarima=auto.arima(USBTP.actual,trace=T,test="kpss",ic="aic")
summary(Tsarima)
confint(Tsarima)
Tsarima.forecast=forecast.Arima(Tsarima,h=6)
Tsarima.forecast
plot(Tsarima.forecast,xlab="years",ylab="no.of passengers")
accuracy(Tsarima.forecast,USBTP.test) #gives MAPE value for predicted vs true data 
library(TSPred)
plotarimapred(USBTP.test,Tsarima,xlim=c(2014,2016),range.percent=0.01)
Tsarima.forecast=forecast.Arima(Tsarima,h=60)
plot(Tsarima.forecast,xlab="years",ylab="no.of passengers")
Vsarima=auto.arima(USBTV.actual,trace=T,test="kpss",ic="aic")
summary(Vsarima)
confint(Vsarima)
Vsarima.forecast=forecast.Arima(Vsarima,h=6)
Vsarima.forecast
plot(Vsarima.forecast,xlab="years",ylab="no.of passengers")
accuracy(Vsarima.forecast,USBTV.test) # gives MAPE value for predicted vs true data 
library(TSPred)
plotarimapred(USBTV.test,Vsarima,xlim=c(2014,2016),range.percent=0.01)
Vsarima.forecast=forecast.Arima(Vsarima,h=60)
plot(Vsarima.forecast,xlab="years",ylab="no.of passengers")
********************************************************************
  Comparing MAPE of HOLTWINTERS vs SARIMA
accuracy(Tsarima.forecast,USBTP2.forecast) #Forecast of SARIMA vs Forecast of Holtwinters for first 6 months of jan 2015

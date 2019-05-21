#Daily data

fileTemp="D:/CitiData.csv"
CCP2 <- read.csv(fileTemp, header=TRUE)
#CCP2 <- read.csv(fileTemp, header=TRUE,colClasses=c("character", "numeric", "numeric","numeric", "numeric", "integer"))
CCP2$Date_TL2 <- as.POSIXct("2016-01-01 23:59:00") + seq(0,911*86400,by=86400)

# Load libraries

install.packages('nnet')
require(nnet)
library(nnet)

install.packages('caret')
require(caret)
library(caret)

install.packages('neuralnet')
require(neuralnet)
library(neuralnet)

install.packages('TSA')
require(TSA)
library('TSA')

install.packages('astsa')
require(astsa)
library(astsa)

install.packages("kernlab")
require(kernlab)
library(kernlab)

install.packages('forecast')
require(forecast)
library(forecast)

install.packages("xts")
install.packages("psych")

require(xts)
require(psych)



# Exploratory analysis

head(CCP2$Date_TL2)
tail(CCP2$Date_TL2)
require(psych)
class(CCP2)
names(CCP2)
str(CCP2)          
describe(CCP2[,2:3])
#detach(CCP2)
attach(CCP2)
head(CCP2)
tail(CCP2)

# Fill in missing Values 

CCP2$Trips[CCP2$Trips==0] <- 10

#CCP2$Precip[CCP2$Precip > 0] <- 1

TP<-(window(CCP2$Trips,1,length(CCP2[,1])))

describe(TP)

# TP<-(na.locf(TP) + rev(na.locf(rev(TP))))/2           # > Skip

# Use xts object  

library('xts')
CCP.XTS<-xts(CCP2[,"Trips"], order.by=CCP2$Date_TL2)
plot(CCP.XTS,ylab="Daily Pickups",main='CitiBike Daily Pickups',cmajor.ticks='months',minor.ticks=FALSE)

plot(CCP.XTS['/2016-09-15'],ylab="Traffic Flow",main="CitiBike Daily Pickups till September 15th",
     major.ticks='months',minor.ticks=FALSE)

# Select appropriate hours by subsetting xts object

CCP.XTS.2<-CCP.XTS["T08:00/T18:00"]

# Summer months only
CCP.XTS.summer<-CCP.XTS.2[.indexmon(CCP.XTS.2) %in% c(4,5,6,7,8)] # note zero-based indexing


##############################
# Start TS processing ########
# Consider Sept - Mid Nov 2016 as in-sample? Y

#TP<-(window(CCP2$Total.Carriageway.Flow,1,length(CCP2[,1])))

#describe(TP)


# Spectral Analysis
library('TSA')
S1<-spec(TP,spans=c(7,7),taper=.1,sub='',xlab='Frequency',ylab='Values',
         main="Spectral Analysis")
abline(v=1/(7))
#abline(v=1/24,type='dashed')
max(S1$spec)


# In-Sample
TP.in<-(window(TP, 1,731))
describe(TP.in)
TP.in.ts <- ts(TP.in, frequency=7, start=c(1),names="TOT.in")
plot(TP.in.ts, xlab = "Days", ylab = "Daily Pickups")
plot.ts(TP.in.ts[1:1000])

#Out-of-Sample
length(TP)
TP.out<-(window(TP, 732,length(TP)))
describe(TP.out)
TP.out.ts <- ts(TP.out, frequency=7, start=c(1),names="TOT.out")
plot(TP.out.ts)

# ACF / PACF
acf(TP.in, main='ACF CitiBike Data',lag.max=1000)
pacf(TP.in, main='PACF CitiBike Data',lag.max=1000)

acf(diff(TP.in,7), main='Differenced ACF CitiBike Data',lag.max=100)
pacf(diff(TP.in,7), main='Differenced PACF CitiBike Data',lag.max=1000)


acf(diff(diff(TP.in,7)), main='Differenced ACF CitiBike Data',lag.max=20)
pacf(diff(diff(TP.in,7)), main='Differenced PACF CitiBike Data',lag.max=20)


# SARIMA
library(astsa)
T1<- sarima(TP.in,1,0,0,1,1,1,7)
#SAX1<- sarima(SR3.in, 1,0,2,0,1,0,365,details = FALSE, xreg=NULL,
#       tol = sqrt(.Machine$double.eps),no.constant = TRUE)

#T2<- sarima(TP.in,1,0,1,0,1,1,7)

T1

#T2

# Auto -> vital
auto.arima(TP.in.ts)  # has to TS object for seasonal to be option
auto.arima(TP.in)


T1 <- auto.arima(TP.in.ts)

T1

T2 <- auto.arima(TP.in)

T2

# Prediction
library(astsa)
#TP.in.modified<-(window(TP, 1,11640))
#plot.ts(TP.in.modified[8761:11640])
#TP.out.modified<-(window(TP, 11641,length(TP)))

POSH.p<-sarima.for(TP.in,length(TP.out),p=1,d=1,q=1,P=0,D=0,Q=2,S=7, 
                   tol = sqrt(.Machine$double.eps),no.constant = FALSE)

#Orig#POSH.p<-sarima.for(TP.in,length(TP.out),p=2,d=0,q=0,P=1,D=1,Q=1,S=24, 
#			 tol = sqrt(.Machine$double.eps),no.constant = FALSE)

#POSH1.p<-sarima.for(TP.in,length(TP.out),p=0,d=1,q=0,P=1,D=1,Q=1,S=24, 
#			 tol = sqrt(.Machine$double.eps),no.constant = FALSE)

#POSH2.p<-sarima.for(TP.in.modified,length(TP.out.modified),p=2,d=0,q=0,P=1,D=1,Q=1,S=24, 
#			 tol = sqrt(.Machine$double.eps),no.constant = FALSE)


describe(POSH.p$pred)

POSH.p$pred
head(POSH.p$pred)
cbind(POSH.p$pred,TP.out)

#POSH1.p$pred
#head(POSH1.p$pred)
#cbind(POSH1.p$pred,TP.out)

#POSH2.p$pred
#head(POSH2.p$pred)
#cbind(POSH2.p$pred,TP.out.modified)

# Out-sample RMSE
SSH=sum((TP.out-POSH.p$pred)^2)
MSE_H=SSH/length(TP.out)
RMSE_H=sqrt(MSE_H)
RMSE_H

#Out-sample NRMSE
describe(TP.out)
mean(TP.out)
range(TP.out)
NRMSE=RMSE_H/mean(TP.out)
NRMSE=RMSE_H/25.30281    
NRMSE

#Out-sample MAE
MAE = sum(abs(TP.out-POSH.p$pred))/length(TP.out)
MAPE = 100*sum(abs(TP.out-POSH.p$pred))/((length(TP.out))*(sum(TP.out)))
MAE
MAPE

install.packages("hydroGOF")
require (hydroGOF)
rmse(POSH.p$pred,TP.out)
nrmse(POSH.p$pred,TP.out)
mae(POSH.p$pred,TP.out)
#mape(POSH.p$pred,TP.out)


# Performance Metric - PICP

status_11<-c()
H1.lower<-c()
H1.upper<-c()
L3<-length(POSH.p$pred)
for (i in 1:L3) {
  
  H1.lower[i]<-POSH.p$pred[i] - 1.96*POSH.p$se[i]
  H1.upper[i]<-POSH.p$pred[i] + 1.96*POSH.p$se[i]
  
  if(TP.out[i] < H1.upper[i] || TP.out[i] > H1.lower[i] ) # 18/3/2017		 
    #		if(POSH.p$pred[i] < H1.upper[i] || POSH.p$pred[i] > H1.lower[i] ) # Wrong		
    status_1<-c(print(1))
  else      
    status_1<-c(print(0))
  status_11<-rbind(status_11,status_1)
}

t1<-table(status_11)
#100*t1[[2]]/(t1[[2]]+t1[[1]])
100*t1[[1]]/L3



# Overlap plots


plot.ts(TP.out,main='SARIMA Forecast ',ylab='Daily Pickups',lwd=2, col='blue',xlab='Time')
lines(732:(731+length(TP.out)),POSH.p$pred,col='red',lty='dashed')
legend('bottomright', legend=c("Actual", "Forecast"),lty=1:2, lwd=2, col=c("blue", "red"), horiz=FALSE)
grid()

#plot.ts(TP.out[1:240],main='Hourly Forecast for the first 10 Days of January 2015 (Out-of_Sample)',ylab='Average Temperature (deg. Celsius)',lwd=2, col='blue',xlab='Time (Hours)')
#lines(1:(0+240),POSH1.p$pred[1:240],col='red',lty='dashed')
#legend('bottomright', legend=c("Measured (Jan 2015 Actual)", "Forecasted"),lty=1:2, lwd=2, col=c("blue", "red"), horiz=FALSE)
#grid()


#plot.ts(TP.out.modified[1:744],main='Hourly Forecast for May 2015 (Out-of_Sample)',ylab='Average Temperature (deg. Celsius)',lwd=2, col='blue',xlab='Time (Hours)')
#lines(1:(0+744),POSH2.p$pred[1:744],col='red',lty='dashed')
#legend('bottomright', legend=c("Measured (May 2015 Actual)", "Forecasted"),lty=1:2, lwd=2, col=c("blue", "red"), horiz=FALSE)
#grid()



## Lagging # TP.in b'se model errors only available for in-sample # 1st not using model errors

L<-length(TP)    #L<-length(TP.in)

SR3.lag0  <-(TP[10:(L-0)])    # TP>TP.in SR3.lag0  <-(TP.in[10:(L-0)])
SR3.lag1  <-(TP[9:(L-1)])
SR3.lag2<-(TP[8:(L-2)])
SR3.lag7<-(TP[3:(L-7)])
SR3.lag8<-(TP[2:(L-8)])
SR3.lag9<-(TP[1:(L-9)])

Rain.lag0 <- CCP2$Precip[10:(L-0)]
Temp.lag0 <- CCP2$Tavg[10:(L-0)]
Hols.lag0 <- CCP2$Hol[10:(L-0)]



df<- data.frame(SR3.lag1,SR3.lag2,SR3.lag7,
                SR3.lag8,SR3.lag9,Rain.lag0,Temp.lag0,Hols.lag0,SR3.lag0)


colnames(df)<-c("Lag1","Lag2","Lag7",
                "Lag8","Lag9","RainLag0", "TempLag0","HolsLag0","Lag0")

describe(df)
str(df)

tail(df)
head(df)


# Part II

L2<-length(df$Lag0)
trainingdata<-df[1:721,]
testdata<-df[722:L2,]

head(testdata)
tail(testdata)
tail(trainingdata)

#Load Packages
require(quantmod) #for Lag()
require(nnet)
library(nnet)
require(caret)

#install.packages('neuralnet')
#require(neuralnet)
library(neuralnet)

#Fit model

set.seed(3)

## SVM

#install.packages("kernlab")
#require(kernlab)
library(kernlab)

modelx3 <- train(Lag0~ Lag1 + Lag2 + Lag7 + Lag8 + Lag9 + RainLag0 + TempLag0 + HolsLag0
                 ,data=trainingdata,method='svmRadial', linout=T, trace = F)

ls(modelx3)

ps3 <- predict(modelx3$finalModel, newdata=testdata[,c(1:8)]) # Fast, other one was slow
ps3.lower<-ps3 - 1.96*sd(ps3[1:length(ps3)])
ps3.upper<-ps3 + 1.96*sd(ps3[1:length(ps3)])


length(ps3)
cbind(ps3,testdata[,"Lag0"])

my.limits<-c(min(ps3.lower),max(ps3.upper))

#main='15-min Forecast for the last 2 weeks of Nov 2016 (Out-of_Sample)',ylab='Traffic Flow'

plot.ts(testdata$Lag0,main='SARIMAX+NN Forecast (Out-of-Sample)',ylab='Daily Pickups'
        ,lwd=2, col='blue', xlab='Time (Days)')
lines(ps3,col='red',lty='dashed')
legend('bottomright', legend=c("Actual", "Predicted"),lty=1:2, lwd=2, col=c("blue", "red"), horiz=FALSE)
grid()

lines(ps3.upper,col='red',lty='dashed')
lines(ps3.lower,col='red',lty='dashed')


#length(ps3)

cbind(testdata$Temperature.lag0[1:length(ps3)],ps3[1:length(ps3)])

plot(modelx3)

modelx3

modelx3$results


#require (hydroGOF)
rmse(ps3[,1], testdata[,"Lag0"])
nrmse(ps3[,1], testdata[,"Lag0"])
mae(ps3[,1], testdata[,"Lag0"])
MAPE_x = 100*sum(abs(testdata$Lag0-ps3))/((length(testdata$Lag0))*(sum(testdata$Lag0)))
MAPE_x


# R^2

rss <- sum((ps3[,1] - testdata[,"Lag0"]) ^ 2)  ## residual sum of squares
tss <- sum((testdata[,"Lag0"] - mean(testdata[,"Lag0"])) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss


# Part III : Multivariate linear regression

head(df)

model.lin <- lm(Lag0 ~ RainLag0 + TempLag0 + HolsLag0, data = df)

summary(model.lin)




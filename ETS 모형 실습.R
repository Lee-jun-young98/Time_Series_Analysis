library(forecast)
library(fpp2)

## 2005부터 시작하게 만들기
austourists_data <- window(austourists, start=2005)

## ets모형에 대입
austourists_ets <- ets(austourists_data)

## ETS(M,A,M)승법에 추세를 더해주고 계절성분도 승법이다.
summary(austourists_ets)

autoplot(austourists_ets)

## ets에서 나온 객체를 forecast에 대입해 예측한다.
austourists_ets_fore <- forecast(austourists_ets, h=8)
autoplot(austourists_ets_fore, xlab="Time", ylab="visitor")


## 미래데이터 예측하기
## 데이터 나누기
train <- window(austourists, start=c(2000,1), end=c(2013,4))
train
testdata <- window(austourists, start=c(2014,1), end=c(2015,4))
testdata

## ets모형
fit_ets <- ets(train, model="MAM")
asdf <- ets(train, model="MAN")
dmlvd <- ets(train, model="AAM") # X
ASVMLK <- ets(train, model="AAA")
asdm <- ets(train, model="MAA")
ASDKM <- ets(train, model="MNN")
evmd <- ets(train, model="AMA")
asdmv <- ets(train, model="MMM") 
dmvd <- ets(train, model="AMM") # X
dvm <- ets(train, model="AMN")
ekeml <- ets(train, model="AMN") #x
vmm <- ets(train, model="MMA") #x

fit_hw <- hw(train, seasonal = "multiplicative")

## 예측하기
for_ets <- forecast(fit_ets, h=8)
for_hw <- forecast(fit_hw, h=8)

## 예측평가
accuracy(for_ets, testdata)
accuracy(for_hw, testdata)


library(fpp2)
library(forecast)

## test train 나누기
data(oil)
oil_train <- window(oil, start=1970, end=2000)
oil_test <- window(oil, start=2000)

plot(oil_train)
plot(oil_test)
## 오일데이터 단순지수평활법이용
oil_train_ses <- ses(oil_train, h=13)
summary(oil_train_ses)


##그래프 그리기
plot(oil_train_ses, ylim=c(0,1000))
lines(oil_test, col='red', type="o")
lines(oil_train_ses$fitted, col="blue", type="l")


## 예측하기
oil_for <- forecast(oil_train_ses)
accuracy(oil_for, oil_test)


## 오일 데이터에 ets
oil_train_ets <- ets(oil_train, model="MNN")
summary(oil_train_ets)
plot(forecast(oil_train_ets, h=13))

oil_train_ets2 <- ets(oil_train, model="ANN")
summary(oil_train_ets2)
plot(forecast(oil_train_ets))

oil_ets_for <- forecast(oil_train_ets2)
accuracy(oil_ets_for, oil_test)



plot(oil_data, xlim=c(2000,2030), ylim=c(200,800))

## for문으로 테스트하기 
lines(simulate(oil_ets, 12), col="red")
#############################################홀트선형지수평활법######################

ausair_train <- window(ausair, start=1975, end=2005)
ausair_test <- window(ausair, start=2006, end=2016)
## ausair 데이터 단순선형지수 평활법

plot(ausair_data)

## holt 감쇠
ausair_holt_d <- holt(ausair_train, damped=TRUE, h=15)
summary(ausair_holt_d)

## holt법 
ausair_holt <- holt(ausair_data, h=15)
summary(ausair_holt)

plot(ausair_holt, ylim=c(0,100))

## holt 예측하기
ausair_for <- forecast(ausair_holt, h=15)
accuracy(ausair_for, ausair_test)


## ausair 데이터에 ets
## 가법모형
ausair_ets <- ets(ausair_train, model="AAN")
summary(ausair_ets)
plot(forecast(ausair_ets, h=15), ylim=c(0,100))
## 승법 모형
ausair_ets <- ets(ausair_train, model="MAN")
summary(ausair_ets)
plot(forecast(ausair_ets), ylim=c(0,100))
lines(ausair_ets$fitted, col="red", type="o")
lines(ausair_test, type="l")
## 예측하기 
ausair_for_2 <- forecast(ausair_ets,h=10)
accuracy(ausair_for_2, ausair_test)

################################################홀트 윈터 선형지수평활법#########################
austourists_train <- window(austourists, start=2000, end=2014)
austourists_test <- window(austourists, start=2015)


austourists_hw <- hw(austourists_train)

## 예측
aus_for <- forecast(austourists_hw, h=1)
accuracy(aus_for, austourists_test)

## 그림
plot(aus_for)
lines(aus_for$fitted, col="blue", type="o")


## 데이터에 hw 방법에 넣기
austourists_hw <- hw(austourists_train, h=5)
summary(austourists_hw)
plot(austourists_hw)


## MAA모형에 train데이터 넣기
austourists_ets <- ets(austourists_train, model="MAA")
summary(austourists_ets)
plot(forecast(austourists_ets))

## MAM모형에 train 데이터 넣기 
austourists_ets2 <- ets(austourists_train, model = "MAM")
summary(austourists_ets2)
plot(forecast(austourists_ets2))

## 예측하기 
austourists_ets2_for <- forecast(austourists_ets2)
accuracy(austourists_ets2_for, austourists_test)



### 차트그리기
lines(for_ets$fitted,col="red",type="o")
lines(my_oil_test,type="l")

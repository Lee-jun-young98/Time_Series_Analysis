library(fpp2)
library(forecast)

data(oil)
oil_data <- window(oil, start=c(1996))
oil_data
plot(oil_data)
## 오일데이터 단순지수평활법이용
oil_ses <- ses(oil_data)
summary(oil_ses)
plot(oil_ses)
## 오일 데이터에 ets
oil_ets <- ets(oil_data, model="MNN")
summary(ets(oil_data,model="ANN"))
summary(oil_ets)
plot(forecast(oil_ets))

plot(oil_data, xlim=c(2000,2030), ylim=c(200,800))

## for문으로 테스트하기 
lines(simulate(oil_ets, 12), col="red")
#############################################홀트선형지수평활법######################

ausair_data <- window(ausair, start=1990)
## ausair 데이터 단순선형지수 평활법

plot(ausair_data)
ausair_holt <- holt(ausair_data, damped=TRUE, h=15)
ausair_holt <- holt(ausair_data, h=15)
summary(ausair_holt)
plot(ausair_holt, ylim=c(0,100))

## ausair 데이터에 ets
## 가법모형
ausair_ets <- ets(ausair_data, model="AAN")
plot(forecast(ausair_ets))
## 승법 모형
ausair_ets <- ets(ausair_data, model="MAN")
summary(ausair_ets)
plot(forecast(ausair_ets), ylim=c(0,100))

################################################홀트 윈터 선형지수평활법#########################
austourists_data <- window(austourists, start=2005)
plot(austourists_data)
austourists_hw <- hw(austourists_data, h=15)

summary(austourists_hw)


austourists_ets <- ets(austourists_data, model="MAA")
summary(austourists_ets)
plot(forecast(austourists_ets))

austourists_ets <- ets(austourists_data, model = "MAM")
summary(austourists_ets)
plot(forecast(austourists_ets))


library(fpp2)
library(urca)
data <- sheep

plot(data)
ggtsdisplay(data)
# 검정통계량으로 정상데이터인지 아닌지 판별
summary(ur.kpss(data))


data <- diff(data)
# 데이터 예비분석, acf와 pacf를 통해 모형 차수 예측
ggtsdisplay(data)

auto.arima(data)
summary(ur.kpss(data))
# 대안 모형 설정
model1 <- Arima(data, order = c(1, 0, 1))
model2 <- Arima(data, order = c(3, 0, 1))
model3 <- Arima(data, order = c(3, 0, 3))
model4 <- auto.arima(data)


summary(model1)
summary(model2)
summary(model3)
summary(model4)

# 잔차검증
checkresiduals(model1)
checkresiduals(model2)
checkresiduals(model3)
checkresiduals(model4)


#예측하기
model_for <- forecast(model3, h=10)

autoplot(model_for) +
  autolayer(data) +
  autolayer(fitted(model_for), series = "fitted")

# 예측값
model_for

C <- -5.4862*(1+0.0608-0.0442+0.7259)
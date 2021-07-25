library(fpp2)
library(urca)
data <- ibmclose

train <- window(data, start = c(1), end = c(349))
test <- window(data, start = c(350))
# 검정통계량으로 정상데이터인지 아닌지 판별
summary(ur.kpss(train))

# 데이터 예비분석, acf와 pacf를 통해 모형 차수 예측
train <- diff(train)

summary(ur.kpss(train))
ggtsdisplay(train)

train <- diff(train)
summary(ur.kpss(train))
ggtsdisplay(train)

auto.arima(train)

# 대안 모형 설정
model1 <- Arima(train, order = c(0, 0, 1))
model2 <- Arima(train, order = c(1, 0, 1))
model3 <- Arima(train, order = c(2, 0, 1))
model4 <- auto.arima(train)


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
model_for <- forecast(model2, h=20)

autoplot(model_for) +
  autolayer(ibmclose) +
  autolayer(train) +
  autolayer(fitted(model_for), series = "fitted") +
  autolayer(test, series = "test")

# 예측값
model_for

df <- data.frame()

for (i in 1:20) {
  ETS <- forecast(ets(train), h=i)  
  ARIMA <- forecast(auto.arima(train), h=i)
  NNAR <- forecast(nnetar(train), h=i)
  Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + NNAR[["mean"]])/3

  acc <- c(ETS = accuracy(ETS, test)["Test set","RMSE"],
      ARIMA = accuracy(ARIMA, test)["Test set","RMSE"],
      NNAR = accuracy(NNAR, test)["Test set","RMSE"],
      Combination = accuracy(Combination, test)["Test set","RMSE"])

  df <- rbind(df, acc, colnames = c("ETS", "ARIMA", "NNAR", "Combination"))
}
colnames(df) <- c("ETS", "ARIMA", "NNAR", "Combination")
min(df$Combination)

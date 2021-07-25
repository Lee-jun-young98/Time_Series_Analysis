library(fpp2)
library(forecast)
train <- window(auscafe, end = c(2012,9))
h <- length(auscafe) - length(train)

ETS <- forecast(ets(train), h=h)  
ARIMA <- forecast(auto.arima(train, lambda = 0), h=h)
STL <- stlf(train, lambda = 0, h=h)
NNAR <- forecast(nnetar(train), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]] + NNAR[["mean"]])/4

c(ETS = accuracy(ETS, auscafe)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, auscafe)["Test set","RMSE"],
  `STL-ETS` = accuracy(STL, auscafe)["Test set","RMSE"],
  NNAR = accuracy(NNAR, auscafe)["Test set","RMSE"],
  Combination = accuracy(Combination, auscafe)["Test set","RMSE"])


far2 <- function(x, h) {forecast(Arima(x, order=c(2,0,0)), h=h) }
e <- tsCV(lynx, far2, h=1, window = 30)

fit1 <- Arima(lynx[1:30], order = c(2,0,0))
fcast1 <- forecast(fit1, h=1)
lynx[31] - fcast1$mean[1]; e[30]

fit2 <- Arima(lynx[2:31], order = c(2,0,0))
fcast2 <- forecast(fit2, h = 1)
lynx[32] - fcast2$mean[1]; e[31]


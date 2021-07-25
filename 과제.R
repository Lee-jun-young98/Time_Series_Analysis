library(fpp2)
library(urca)
library(ggplot2)

# AR 자기회귀모델 조건
# 매개변수 = 0 백색잡음
# 매개변수 1 c = 0 확률보행 모델
# 매개변수 1 c != 0 표류가 있는 확률 보행 모델
# 매개변수 < 0 평균값을 중심으로 진동하는 경향
# AR(1) -1 < 매개변수 < 1
# AR(2) -1< 매개변수2 < 1, 매개변수1 + 매개변수2 < 1, 매개변수2 - 매개변수1 < 1
# MA(1) -1 < 매개변수 < 1
# MA(2) -1 < 매개변수2 < 1, 매개변수2 + 매개변수1 > -1, 매개변수1 - 매개변수2 < 1

setwd("C://R_data")
data <- read.csv("data_ARIMA_DIY.csv")
set.seed(1234)

data_1 <- ts(data[,1], start=c(2001,1), end = c(2006,12), frequency = 12)
data_2 <- ts(data[,2], start=c(2001,1), end = c(2006,12), frequency = 12)
data_3 <- ts(data[,3], start=c(2001,1), end = c(2006,12), frequency = 12)
data_4 <- ts(data[,4], start=c(2001,1), end = c(2006,12), frequency = 12)


# data1 예비분석
plot(data_1)
summary(ur.kpss(data_1)) # 임계값 0.04가 유의수준 1%일때의 값 0.739보다 작으므로 귀무가설을 채택 -> 약정상성

# data1 판별
ggtsdisplay(data_1) # ARIMA(1,0,0) 형태를 보이고 -1 < 매개변수 < 0이다. 매개변수 -0.7
arima(data_1, order = c(1,0,0))
arima(data_1, order = c(1,0,1)) 
arima(data_1, order = c(1,0,2))

# data1 계수값 추정 
data1_arima1 <- arima(data_1, order = c(1,0,0)) # C = 99.8060 * (1 -(-0.6608)) 백색잡음 표준편차 31.76
data1_arima2 <- arima(data_1, order = c(1,0,1))
data1_arima3 <- arima(data_1, order = c(1,0,2))

C <- 99.8025 * (1 + 0.7248)
# data1 진단summary(data1_arima)
summary(data1_arima1)
summary(data1_arima2) 
summary(data1_arima3) # rmse가 가장 작으므로 채택한다.
checkresiduals(data1_arima3) # ACF의 관측값들이 모두 신뢰구간 안에 있으므로 각 관측값들의 자기상관성이 없다고 볼수 있다.
Box.test(data1_arima3$residuals, lag = 10, type = "Ljung-Box") # p-value가 유의수준인 0.05보다 크므로 자기상관성이 없다고 볼 수 있다.

autoplot(data_1) +
  autolayer(fitted(data1_arima3), series = "fitted")

# data1 예측 
# 식 표현 yt = 179.5779 - 0.7983yt-1 - 0.1495et-2 - 0.39et-1 + et
C <- 99.8598 * (1 + 0.7983)
data_1_F <- forecast(data1_arima3, h = 5)
autoplot(data_1_F) + 
  autolayer(data_1) +
  autolayer(fitted(data_1_F), series = "fitted")
data_1_F


####################################data1###############################################################
# data2 예비분석
plot(data_2)
summary(ur.kpss(data_2)) # 임계값 0.04가 유의수준 1%일때의 값 0.739보다 작으므로 귀무가설을 채택 -> 약정상성

# data2 판별
ggtsdisplay(data_2) 
auto.arima(data_2, seasonal = FALSE)
arima(data_2, order = c(0,0,1))
arima(data_2, order = c(1,0,1)) 
arima(data_2, order = c(2,0,1))

# data2 계수값 추정 
data2_arima1 <- arima(data_2, order = c(0,0,1)) 
data2_arima2 <- arima(data_2, order = c(1,0,1))
data2_arima3 <- arima(data_2, order = c(2,0,1))


# data2 진단summary(data2_arima)
summary(data2_arima1)
summary(data2_arima2) 
summary(data2_arima3) # rmse가 가장 작으므로 채택한다.
checkresiduals(data2_arima3) # ACF의 관측값들이 모두 신뢰구간 안에 있으므로 각 관측값들의 자기상관성이 없다고 볼수 있다.
Box.test(data2_arima3$residuals, lag = 10, type = "Ljung-Box") # p-value가 유의수준인 0.05보다 크므로 자기상관성이 없다고 볼 수 있다.

autoplot(data_2) +
  autolayer(fitted(data2_arima3), series = "fitted")

# data2 예측 
# 식 표현 yt = 87.514 + 0.1198yt-1 + 0.0143yt-2 + 0.7659et-1 + et
C <- 101.0671 * (1-0.1198-0.0143)
C
data_2_F <- forecast(data2_arima3, h = 5)
autoplot(data_2_F) + 
  autolayer(data_2) +
  autolayer(fitted(data_2_F), series = "fitted")
data_2_F


########################################################data3##############################################
# data3 예비분석
plot(data_3)
summary(ur.kpss(data_3)) # 임계값 0.04가 유의수준 1%일때의 값 0.739보다 작으므로 귀무가설을 채택 -> 약정상성

# data3 판별
ggtsdisplay(data_3) # ARIMA(1,0,0) 형태를 보이고 -1 < 매개변수 < 0이다. 매개변수 -0.7
auto.arima(data_3)
arima(data_3, order = c(1,0,1))
arima(data_3, order = c(2,0,1)) 
arima(data_3, order = c(1,0,2))
arima(data_3, order = c(2,0,2))
# data3 계수값 추정 
data3_arima1 <- arima(data_3, order = c(1,0,1)) # C = 99.8060 * (1 -(-0.6608)) 백색잡음 표준편차 31.76
data3_arima2 <- arima(data_3, order = c(2,0,1))
data3_arima3 <- arima(data_3, order = c(1,0,2))
data3_arima4 <- arima(data_3, order = c(2,0,2))

C <- -0.0085 * (1 + 0.7634)
# data3 진단summary(data3_arima)
summary(data3_arima1)
summary(data3_arima2) 
summary(data3_arima3) 
summary(data3_arima4)
checkresiduals(data3_arima4) # ACF의 관측값들이 모두 신뢰구간 안에 있으므로 각 관측값들의 자기상관성이 없다고 볼수 있다.
Box.test(data3_arima3$residuals, lag = 10, type = "Ljung-Box") # p-value가 유의수준인 0.05보다 크므로 자기상관성이 없다고 볼 수 있다.

autoplot(data_3) +
  autolayer(fitted(data3_arima4), series = "fitted")

# data3 예측 
# 식 표현 yt = -0.0092 - 0.4256yt-1 + 0.2143yt-2 + 0.3722et-2 - 1.2240et-1 + et
C <- -0.0076 * (1 + 0.4256 - 0.2143)
data_3_F <- forecast(data3_arima4, h = 5)
autoplot(data_3_F) + 
  autolayer(data_3) +
  autolayer(fitted(data_3_F), series = "fitted")
data_3_F


#########################################data4#################################################################
# data4 예비분석
plot(data_4)
summary(ur.kpss((data_4))) # 임계값 0.04가 유의수준 1%일때의 값 0.739보다 작으므로 귀무가설을 채택 -> 약정상성

data_4 <- diff(data_4)
plot(data_4)
summary(ur.kpss((data_4))) 
# data4 판별
ggtsdisplay(data_4) # ARIMA(1,0,0) 형태를 보이고 -1 < 매개변수 < 0이다. 매개변수 -0.7
auto.arima(data_4)
Carima(data_4, order = c(1,0,1))
arima(data_4, order = c(1,0,1)) 
arima(data_4, order = c(1,0,2))

# data4 계수값 추정 
data4_arima1 <- arima(data_4, order = c(1,0,1)) # C = 99.8060 * (1 -(-0.6608)) 백색잡음 표준편차 31.76
data4_arima2 <- arima(data_4, order = c(2,0,1))
data4_arima3 <- arima(data_4, order = c(1,0,2))
data4_arima4 <- arima(data_4, order = c(2,0,2))

C <- 0.1215 * (1 + 0.6993)
# data4 진단summary(data4_arima)
summary(data4_arima1)
summary(data4_arima2) 
summary(data4_arima3) # rmse가 가장 작으므로 채택한다.
summary(data4_arima4)
checkresiduals(data4_arima3) # ACF의 관측값들이 모두 신뢰구간 안에 있으므로 각 관측값들의 자기상관성이 없다고 볼수 있다.
Box.test(data4_arima3$residuals, lag = 10, type = "Ljung-Box") # p-value가 유의수준인 0.05보다 크므로 자기상관성이 없다고 볼 수 있다.

autoplot(data_4) +
  autolayer(fitted(data4_arima3), series = "fitted")

# data4 예측 
# 식 표현 yt = 179.5779 - 0.7983yt-1 - 0.1495et-2 - 0.39et-1 + et

C <- 0.1219 * (1 + 0.6510 - 0.0478)
data_4_F <- forecast(data4_arima3, h = 5)
autoplot(data_4_F) + 
  autolayer(data_4) +
  autolayer(fitted(data_4_F), series = "fitted")
data_4_F


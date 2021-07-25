library(fpp2)

autoplot(euretail) + ylab("Retail index") + xlab("Year")

# 계절 차분
ggtsdisplay(diff(euretail, lag = 4))

# 계절 차분후 한번더 실행
ggtsdisplay(diff(diff(euretail, lag = 4)))

# 대안 모형들 바탕으로 모형 적합
sArima_1_fit_1 <- Arima(euretail, order = c(0,1,1), seasonal = c(0,1,1))
sArima_1_fit_1
ggtsdisplay(residuals(sArima_1_fit_1))
checkresiduals(sArima_1_fit_1)

sArima_1_fit_2 <- Arima(euretail, order = c(0,1,2), seasonal = c(0,1,1))
sArima_1_fit_2
ggtsdisplay(residuals(sArima_1_fit_2))
checkresiduals(sArima_1_fit_2)

sArima_1_fit_3 <- Arima(euretail, order = c(0,1,3), seasonal = c(0,1,1))
sArima_1_fit_3
ggtsdisplay(residuals(sArima_1_fit_3))
checkresiduals(sArima_1_fit_3)

# 적합 및 예측
sArima_1_fit_3_F <- forecast(sArima_1_fit_3, h=13)
sArima_1_fit_3_F

autoplot(sArima_1_fit_3_F) + ylab("Retail index") + xlab("Year") +
  autolayer(fitted(sArima_1_fit_3_F))


lh02 <- log(h02)
dlh02 <- diff(lh02, lag = 12)
ggtsdisplay(dlh02)

m_1 <- Arima(h02, order = c(3,0,1), seasonal = c(0,1,2), lambda = 0)
m_1

m_1_F <- forecast(m_1, h=24)
autoplot(m_1_F) + autolayer(fitted(m_1_F))
checkresiduals(m_1, lag = 36)

checkresiduals(auto.arima(h02), lag = 36)

auto <- auto.arima(h02)
auto_F <- forecast(auto, h=24)
autoplot(auto_F) + autolayer(fitted(auto_F))


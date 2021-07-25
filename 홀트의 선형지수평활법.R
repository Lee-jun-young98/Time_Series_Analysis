library(fpp2)
ausair
plot(ausair)

air.ts <- window(ausair, start=1990)
## 홀트의 선형지수 평활법 적용
Air_holt <- holt(air.ts, h=15)
plot(Air_holt)


names(Air_holt)
Air_holt$model
Air_holt$fitted

Air_holt$model$states
summary(Air_holt)

Air_holt_de <- holt(air.ts, damped = TRUE, h=15)
Air_holt_de <- holt(air.ts, h=15)
plot(Air_holt_de)
lines(fitted(Air_holt), type="l", col="blue")



setwd("C://R_data")
library(fpp2)

guarantee <- read.csv("guarantee.csv")
guarantee.ts <- ts(guarantee$gurantee, start=c(2010,1), frequency = 12)
guarantee.ts

plot(guarantee.ts)

##가법
fit1_guarantee.ts <- hw(guarantee.ts, seasonal = "additive", h=12)

##승법
fit2_guarantee.ts <- hw(guarantee.ts, seasonal = "multiplicative", h=12)

##가법 계절지수평활법
plot(fit1_guarantee.ts, type="l", ylab = "Sypply of Housing finance credit guarantee", xlab="Year", fcol="white", lwd=1)
lines(fitted(fit1_guarantee.ts), col="blue", type="o", pch=1, lwd=1)
lines(fit1_guarantee.ts$mean, col="blue", type="o", pch=1,lwd=1)

## 승법 계절지수평활법 mean은 미래의 예측값
plot(fit2_guarantee.ts, type="l", ylab = "Sypply of Housing finance credit guarantee", xlab="Year", fcol="white", lwd=1)
lines(fitted(fit2_guarantee.ts), col="blue", type="o", pch=1, lwd=1)
lines(fit2_guarantee.ts$mean, col="blue", type="o", pch=1,lwd=1)


data("austourists")
#가법
class(austourists)
austourists.ts <- window(austourists, start=2005)
fit_austourists <- hw(austourists.ts, seasonal = "additive", h=8)
plot(fit_austourists, type="l")
lines(fitted(fit_austourists), col="blue",type="o",pch=1,lwd=1)
lines(fit_austourists$mean, col="blue", type="o", pch=1, lwd=1)

summary(fit_austourists)

#승법
fit_austourists2 <- hw(austourists.ts, seasonal = "multiplicative", h=8)
plot(fit_austourists2, type="l")
lines(fitted(fit_austourists2), col="blue",type="o",pch=1,lwd=1)
lines(fit_austourists2$mean, col="blue", type="o", pch=1, lwd=1)

summary(fit_austourists2)

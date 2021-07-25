setwd("C://R_data")
mid <- read.csv("mid_term.csv")
colnames(mid) <- 'data_mid'

mid.ts <- ts(mid, start=1906, frequency = 1)
mid.ts

plot(mid.ts)

mid_ets <- ets(mid.ts, model="ANN")
mid_ses <- ses(mid.ts, initial = "optimal")
mid_holt_damped <- holt(mid.ts, damped=TRUE)
mid_holt <- holt(mid.ts, damped=FALSE)

mid_ets_for <- forecast(mid_ets)

mid_ses_for <- forecast(mid_ses)
mid_ses2_for <- forecast(mid_ses2)
summary(mid_ets)
summary(mid_ses)
summary(mid_holt_damped)
summary(mid_holt)

plot(mid.ts)
lines(mid_holt$fitted, col="red", type="o")

hist(mid_holt$residuals, main="holt모형의 잔차")


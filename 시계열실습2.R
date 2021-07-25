library(fpp2)
library(forecast)

plot(oil)
oil_train <- window(oil,start=1970,end=2000)
oil_test <- window(oil, start=2001)

oil_ses <- ses(oil_train, h=13)
oil_ses_for <- forecast(oil_ses)
summary(oil_ses_for)

plot(oil_ses_for)
lines(oil_ses$fitted, col="red", type="o")
lines(oil_test, col="black", type="l")


## ets
oil_ets <- ets(oil_train, model="ANN")
summary(oil_ets)

oil_etsM <- ets(oil_train, model="MNN")
summary(oil_etsM)

oil_ets_for <- forecast(oil_ets, h=13)
plot(oil_ets_for)
lines(oil_ets$fitted, col="red", type="o")
lines(oil_test, col="black", type="l")

accuracy(oil_ets_for, oil_test)

#### ausair hw####
plot(ausair)
ausair_train <- window(ausair, start=1975, end=2005)
ausair_test <- window(ausair, start=2006, end=2016)

ausair_hd <- holt(ausair_train, damped=T, h=11)
ausair_for <- forecast(ausair_hd)
summary(ausair_for)

ausair_h <- holt(ausair_train, damped=F, h=11)
summary(ausair_h)
plot(ausair_h)
lines(ausair_h$fitted, col="red", type="o")
lines(ausair_test, col="black", type="l")

aus_ets_AA <- ets(ausair_train, model="AAN")
aus_for_AA <- forecast(aus_ets_AA, h=11)
summary(aus_for_AA)




aus_ets_MA <- ets(ausair_train, model="MAN")
aus_for_MA <- forecast(aus_ets_MA, h=11)
summary(aus_for_MA)

aus_ets_AM <- ets(ausair_train, model="AMN")
aus_for_AM <- forecast(aus_ets_AM, h=11)
summary(aus_for_AM)


aus_ets_MM <- ets(ausair_train, model="MMN")
aus_for_MM <- forecast(aus_ets_MM, h=11)
summary(aus_for_MM)

plot(aus_for_MM)
lines(aus_ets_MM$fitted, col="red", type="o")
lines(ausair_test, col="black", type="l")

accuracy(aus_for_MM, ausair_test)

#### hw###
plot(austourists)
austourists_train <- window(austourists, start=2000, end=2014)
austourists_test <- window(austourists, start=2015)

austourists_hw <- hw(austourists_train, h=1)
summary(austourists_hw)

plot(austourists_hw)
lines(austourists_hw$fitted, col="red", type="o")
lines(austourists_test, col="black", type="l")

aust_ets <- ets(austourists_train)
summary(aust_ets)

aust_ets_MAA <- ets(austourists_train, model="MAA")
summary(aust_ets_MAA)
aust_ets_for <- forecast(aust_ets_MAA, h=1)
plot(aust_ets_for)
lines(aust_ets_for$fitted, col="red", type="o")
lines(austourists_test, col="black", type="l")

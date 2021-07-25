library(quantmod)
library(TSA)
library(forecast)

data("beersales")
data("wineind")


## beersales 월별
beersales.ts <- beersales
plot(beersales.ts, xlab="Time(month)", ylab="beersales", main="beersales_month")


## beersales 분기별
beersales_mean <- aggregate(beersales.ts, nfrequency = 4, FUN=mean)
beersales_mean
plot(beersales_mean, xlab="Time(Quarterly)", ylab="beersales", main="beersales_quarterly")



## beersales 연도별
beersales_yearly <- aggregate(beersales.ts, nfrequency = 1, FUN=sum)
beersales_yearly
plot(beersales_yearly, xlab="Time(yearly)", ylab="beersales", main="beersales_yearly")




## wineind 월별
wineind.ts <- wineind
plot(wineind.ts, xlab="Time(month)", ylab="wineind", main="wineind_month")


## wineind 분기별
wineind_mean <- aggregate(wineind.ts, nfrequency = 4, FUN=mean)
wineind_mean
plot(wineind_mean, xlab="Time(Quarterly)", ylab="wineind", main="wineind_quarterly")



## wineind 연도별
wineind_yearly <- aggregate(wineind.ts, nfrequency = 1, FUN=sum)
wineind_yearly
plot(wineind_yearly, xlab="Time(yearly)", ylab="wineind", main="wineind_yearly")

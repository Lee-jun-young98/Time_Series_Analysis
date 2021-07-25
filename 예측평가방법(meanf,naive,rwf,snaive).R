library(fpp2)
data("ausbeer")

## 예측 모형 예측 
beer2 <- window(ausbeer,start=1992, end=c(2007,4))
autoplot(beer2) +
  autolayer(meanf(beer2, h=11), series = "Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11), series = "Naive", PI=FALSE) +
  autolayer(snaive(beer2, h=11), series = "Seaonal naive", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour = guide_legend(title="Forecast"))

autoplot(goog200) +
  autolayer(meanf(goog200, h=40), series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40), series="Naive", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40), series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))


train <- window(ausbeer, start=c(1992,1), end=c(2007,4))
test <- window(ausbeer, start=2008)

## rwf에 drift false면 naive이다. drift false가 디폴트 값
beerfit1 <- meanf(train, h=10)
beerfit2 <- naive(train, h=10)
beerfit3 <- snaive(train, h=10)


autoplot(train) +
  autolayer(meanf(beer2, h=10), series = "Mean", PI=FALSE) +
  autolayer(naive(beer2, h=10), series = "Naive", PI=FALSE) +
  autolayer(snaive(beer2, h=10), series = "Seaonal naive", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour = guide_legend(title="Forecast"))

accuracy(beerfit1, test)
accuracy(beerfit2, test)
accuracy(beerfit3, test)



train <- goog200
test <- window(goog, start=201, end=240)

autoplot(goog200) +
  autolayer(meanf(goog200, h=40), series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40, drift=FALSE), series="Naive", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40), series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

googfit1 <- meanf(train, h=40)
googfit2 <- rwf(train, h=40, drift=FALSE)
googfit3 <- rwf(train, h=40, drift=TRUE)


accuracy(googfit1, test)
accuracy(googfit2, test)
accuracy(googfit3, test)
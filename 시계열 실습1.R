setwd("C://TSA_data_week_2")
?AirPassengers
is.ts(AirPassengers)
class(AirPassengers)
frequency(AirPassengers)

## ts공부하기 frequency가 월별 데이터인데 년으로 묶여버림
airline.ts <- ts(AirPassengers)
airline.ts

## 1949년으로 시작 frequency를 월 별로

airline1.ts <- ts(AirPassengers, start=c(1949,1), frequency = 12)
airline1.ts

## 1949년 1월 의미
start(airline1.ts)
## 1960년 12월 의미
end(airline1.ts)
## 주기가 12
frequency(airline1.ts)

## 분기별 원/달러 환율 자료
exchange.df <- read.csv("BOK_exchange_rate_krw_usd.csv")
str(exchange.df)

exchange.df
is.ts(exchange.df)
class(exchange.df)


exchange1.ts <- ts(exchange.df$exchange_rate_krw_usd, start=c(1980,1), frequency = 4)
exchange1.ts

oil_user_enter.df <- read.csv("Oil_User_Enter.csv")
head(oil_user_enter.df)
tail(oil_user_enter.df, n=3)
is.ts(oil_user_enter.df)
class(oil_user_enter.df)

oil_user_enter.ts <- ts(oil_user_enter.df$oil, start=c(1994,1), frequency = 12)
oil_user_enter.ts

is.ts(oil_user_enter.ts)
class(oil_user_enter.ts)

oil_enter_in_R <- c(8047,7173,8156,6527,6364,6169,6273,6256,6498,7329,8057,9494)
oil_enter_in_R
is.ts(oil_enter_in_R)
class(oil_enter_in_R)

## ts로변환
oil_enter_in_R.df <- data.frame(oil_enter_in_R)
oil_enter_in_R.ts <- ts(oil_enter_in_R.df, start=c(1994,1), frequency = 12)
is.ts(oil_enter_in_R.ts)
class(oil_enter_in_R.ts)


## csv로 저장하기
write.csv(oil_enter_in_R.ts, "Oil_User_Enter_in_R.csv")

a <- read.csv("Oil_User_Enter_in_R.csv")

# write.csv를 할때 날짜가 저장이 안됨
is.ts(a)
class(a)

# 따라서 ts 저장시 날짜 입력하기
date <- seq(as.Date("1994/1/1"), by="month", length.out=12)
date
oil_user_enter_in_R <- data.frame(date,oil_enter_in_R)
oil_user_enter_in_R

write.csv(oil_user_enter_in_R, "Oil_User_Enter_in_R.csv", row.names = F)
b <- read.csv("Oil_user_enter_in_R.csv")
b



## 경제 시계열 자료, 주기는 월별
economic.df <- read.csv("BOK_macro_economic_rate.csv")
economic.df
str(economic.df)

economic.ts <- ts(economic.df[-c(1)], start=c(2010,1), frequency=12)
economic.ts
economic.ts[,1]
economic.ts[,2]

employment.ts <- ts(economic.df$employment_rate, start = c(2010,1), frequency = 
                      12)
bonds.ts <- ts(economic.df$bonds_3_year, start = c(2010,1), frequency = 12)

## 일별데이터 가져요기 quantmod 패키지
facebook.df <- read.csv("Stock_facebook.csv")
facebook.df

twitter.df <- read.csv("Stock_twitter.csv")
twitter.df

facebook.ts <- ts(facebook.df$Adj.Close, start=c(2015,8), frequency = 12)
facebook.ts

twitter.ts <- ts(twitter.df$Adj.Close, start=c(2015,8), frequency = 12)
twitter.ts

library(quantmod)

getSymbols("FB", src="yahoo", from=as.Date("2015-08-01"), to=as.Date("2018-08-31"))
getSymbols("TWTR", src="yahoo", from=as.Date("2015-08-01"), to=as.Date("2018-08-31"))

FB
TWTR
head(FB)
head(TWTR)
class(FB)
class(TWTR)


unemploy.df <- read.csv("BOK_unemployment_rate.csv")
oil.df <- read.csv("BOK_energy_oil.csv")
exchange.df <- read.csv("BOK_exchange_rate_krw_usd.csv")

## ts 객체변환
unemploy.ts <- ts(unemploy.df$unemployment_rate, start=2000, frequency = 1)
oil.ts <- ts(oil.df$oil, start=c(1994,1), frequency = 12)
exchange.ts <- ts(exchange.df$exchange_rate_krw_usd, start=c(1980,1), frequency = 4)

##plot
par(mfrow=c(2,2))
plot(FB$FB.Adjusted, xlab="Time(Daily)", ylab="Adjusted Price", main = "Facebook")
plot(oil.ts, xlab = "Time(Monthly)", ylab="Pertrolem consumption", main="Korean energy Petroleum consumption")
plot(exchange.ts, xlab="Time(Quarterly)", ylab="Exchange rate", main="Exchange Rate KRW per USD")
plot(unemploy.ts, xlab="Time(Yearly)", ylab="Adjusted Price", main = "Korean 
unemployment rate")

plot(economic.ts, main="Two time series in one file")
par(mfrow = c(1,2))
plot(employment.ts, col="blue", lwd=2, ylab="Rate %", main="Monthly employment 
rate")
plot(bonds.ts, col="red", lwd=2, ylab="bonds 3 years", main="Treasury bond 3 
years")


## 두시계열 붙이기
two.ts <- cbind(facebook.ts, twitter.ts)
two.ts
two.ts[1:5,]

plot(two.ts, col="blue", lwd=2, ylab="", main="Adjusted close")
plot(two.ts, plot.type = "single", main="Monthly closing prices on Facebook and 
Twitter using plot()", ylab="Adjusted close price", col=c("blue","red"), lty=1:2)
legend("right", legend=c("Facebook","Twitter"), col=c("blue","red"), lty=1:2) 




## 월별 자료를 분기별 자료로 변환(합산)
AirPassengers
air_quarterly <- aggregate(AirPassengers, nfrequency = 4, FUN = sum)
air_quarterly
par(mfrow = c(1,2))
plot(AirPassengers, xlab="Time(Monthly)", ylab="Number of passengers", 
       main="Airline passengers")
plot(air_quarterly, xlab="Time(Quarterly)", ylab="Number of passengers", 
       main="Sum aggregation")


## 월별자료를 분기별 자료로 변환(평균)
Air_quarterly_mean <- aggregate(AirPassengers, nfrequency = 4, FUN = mean)
Air_quarterly_mean

par(mfrow=c(1,2))
plot(AirPassengers, xlab="Time(Monthly)", ylab="Number of passengers", main="
       Airline passengers")
plot(Air_quarterly_mean, xlab="Time(Quarterly)", ylab="Number of passengers", main="Mean aggregation")


## 월별자료를 연도별 자료로 변환
air_yearly <- aggregate(AirPassengers, nfrequency = 1, FUN = sum)
air_yearly
par(mfrow=c(1,2))
plot(AirPassengers, xlab="Time(Monthly)", ylab="Number of passengers", 
       main="Airline passengers")
plot(air_yearly, xlab="Time(Quarterly)", ylab="Number of passengers", 
       main="Yearly sum aggregation")

library(TSA)
library(forecast)

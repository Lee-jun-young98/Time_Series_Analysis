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



##################################################단순지수평활법###############################################
simple_ex.ts <- ts(c(35,25,30,35,32))
simple_ex.ts

library(forecast)

## 데이터가 많아지면 초기값 initial이 중요하지 않다.
simple_ex_fit <- ses(simple_ex.ts, alpha=0.2, inital="simple")

## 모델 적합이 됐는지 확인
simple_ex_fit$fitted

## training셋에서 주로 rmse를 본다.
summary(simple_ex_fit)



## 지수평활법(단순지수 평활법)
setwd("C://R_data")
ses_pra <- read.csv("ses_pra.csv")
ses_pra.ts <- ts(ses_pra$sales, start=1987)
ses_pra.ts

## h는 예측할꺼 수, alpha값을 대입 안했을때 smoothing parameter가 알아서 해줌
ses_pra_fit <- ses(ses_pra.ts, initial = "optimal", h=4)
summary(ses_pra_fit)
ses_pra_fit$fitted

plot(ses_pra_fit)

## 점으로 된 곳이 예측 값, 회색이 신뢰구간
lines(fitted(ses_pra_fit), type="o", col="red")


library(fpp2)
oil.ts <- window(oil,start=1996)


fit_oil <- ses(oil.ts, initial = "optimal", alpha=0.1, h=5)
summary(fit_oil)
plot(fit_oil)
lines(fitted(fit_oil), type="o", col="red")
names(fit_oil)


fit_oil_2 <- ses(oil.ts, initial = "optimal", alpha=0.5, h=5)
summary(fit_oil_2)
plot(fit_oil_2)
lines(fitted(fit_oil_2), type="o", col="red")

fit_oil_3 <- ses(oil.ts, initial = "optimal", alpha=0.9, h=5)
summary(fit_oil_3)
plot(fit_oil_3)
lines(fitted(fit_oil_3), type="o", col="red")

fit_oil_4 <- ses(oil.ts, initial = "optimal", h=5)
summary(fit_oil_4)
plot(fit_oil_4)
lines(fitted(fit_oil_4), type="o", col="red")
class(fit_oil)


fit_oil$model ## 모형의 통계량
fit_oil$mean ## 적합된 시계열ㅇ 모형의 forecast
fit_oil$level ## 예측구간과 관련된 신뢰구간의 값
fit_oil$x ## 적합되기전 시계열 데이터
fit_oil$upper ## 상위 80~95에서의 신뢰구간
fit_oil$lower ## 하위 80~95에서의 신뢰구간
fit_oil$fitted ## 시계열 데이터에 fit한 값
fit_oil$method ## 적합에 사용된 방법
fit_oil$series ## 적합하기 전 사용된 데이터의 이름
fit_oil$residuals ## 적합값에서의 잔차들 



##################################################실습###################################################
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


##########################################홀트의 선형지수 평활법######################
library(fpp2)
ausair
plot(ausair)

air.ts <- window(ausair, start=1990)
## 홀트의 선형지수 평활법 적용
Air_holt <- holt(air.ts, h=15)
plot(Air_holt)


names(Air_holt) #적합된 모형의 변수확인
Air_holt$model # 모형의 통계량(추정량 확인)
Air_holt$fitted # 적합된 값 확인

Air_holt$model$states #자료의 평활과 추세의 평활을 볼 수 있다.
summary(Air_holt)

Air_holt_de <- holt(air.ts, damped = TRUE, h=15)
Air_holt_de <- holt(air.ts, h=15)
plot(Air_holt_de)
lines(fitted(Air_holt), type="l", col="blue")


######################################홀트의 계절지수 평활법####################################
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


#############################계절지수평활법2#############################
library(fpp2)
aust <- window(austourists, start=2005)

## damped = TRUE도 들어갈 수 있다.
fit_aust_add <- hw(aust,seasonal = "additive")
fit_aust_multi <- hw(aust, seasonal = "multiplicative")

plot(fit_aust_add)
lines(fitted(fit_aust_add), col="red")

plot(fit_aust_multi)
lines(fitted(fit_aust_multi), col="red")

## 상태방정식 확인
fit_aust_add$model$states
fit_aust_multi$model$states

## 시각화 
states <- cbind(fit_aust_add$model$states[,1:3], fit_aust_multi$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states)


################################ets모형##################################
library(forecast)
library(fpp2)

## 2005부터 시작하게 만들기
austourists_data <- window(austourists, start=2005)

## ets모형에 대입
austourists_ets <- ets(austourists_data)

## ETS(M,A,M)승법에 추세를 더해주고 계절성분도 승법이다.
summary(austourists_ets)

autoplot(austourists_ets)

## ets에서 나온 객체를 forecast에 대입해 예측한다.
austourists_ets_fore <- forecast(austourists_ets, h=8)
autoplot(austourists_ets_fore, xlab="Time", ylab="visitor")


## 미래데이터 예측하기
## 데이터 나누기
train <- window(austourists, start=c(2000,1), end=c(2013,4))
train
testdata <- window(austourists, start=c(2014,1), end=c(2015,4))
testdata

## ets모형에 승법에러 가법 추세 승법계절 적용
fit_ets <- ets(train, model="MAM")


## 안되는 예시 
asdf <- ets(train, model="MAN")
dmlvd <- ets(train, model="AAM") # X
ASVMLK <- ets(train, model="AAA")
asdm <- ets(train, model="MAA")
ASDKM <- ets(train, model="MNN")
evmd <- ets(train, model="AMA")
asdmv <- ets(train, model="MMM") 
dmvd <- ets(train, model="AMM") # X
dvm <- ets(train, model="AMN")
ekeml <- ets(train, model="AMN") #x
vmm <- ets(train, model="MMA") #x


## 승법홀트계절지수평활화 적용 
fit_hw <- hw(train, seasonal = "multiplicative")

## 예측하기
for_ets <- forecast(fit_ets, h=8)
for_hw <- forecast(fit_hw, h=8)

## 예측평가
accuracy(for_ets, testdata)
accuracy(for_hw, testdata)


##########################ets모형과 평활법들 비교############################
library(fpp2)
library(forecast)

## plot으로 데이터의 추세 확인
plot(oil)

## train, test셋으로 나누기 
oil_train <- window(oil,start=1970,end=2000)
oil_test <- window(oil, start=2001)

## 단순지수평활화 적용 
oil_ses <- ses(oil_train, h=13)
oil_ses_for <- forecast(oil_ses)
summary(oil_ses_for)

## 적합값과 test셋으로 plot만들기
plot(oil_ses_for)
lines(oil_ses$fitted, col="red", type="o")
lines(oil_test, col="black", type="l")


## ets 승법모형과 가법모형의 rmse 비교
oil_ets <- ets(oil_train, model="ANN")
summary(oil_ets)

oil_etsM <- ets(oil_train, model="MNN")
summary(oil_etsM)

## forecast함수를 이용해 ets모형을 예측함
oil_ets_for <- forecast(oil_ets, h=13)
plot(oil_ets_for)
lines(oil_ets$fitted, col="red", type="o")
lines(oil_test, col="black", type="l")

## 예측하기 
accuracy(oil_ses_for, oil_test)
accuracy(oil_ets_for, oil_test)


#### ausair 홀트의 선형지수평활법####

# 데이터의 추세 확인하기
plot(ausair)

# 데이터 train, test 나누기
ausair_train <- window(ausair, start=1975, end=2005)
ausair_test <- window(ausair, start=2006, end=2016)

## 감쇠효과 T/F로 RMSE확인하기
ausair_hd <- holt(ausair_train, damped=T, h=11)
ausair_for <- forecast(ausair_hd)
summary(ausair_for)

ausair_h <- holt(ausair_train, damped=F, h=11)
summary(ausair_h)
plot(ausair_h)
lines(ausair_h$fitted, col="red", type="o")
lines(ausair_test, col="black", type="l")

## ets 에러는 가법 추세는 가법
aus_ets_AA <- ets(ausair_train, model="AAN")
aus_for_AA <- forecast(aus_ets_AA, h=11)
summary(aus_for_AA)

## ets 에러는 승법 추세는 가법
aus_ets_MA <- ets(ausair_train, model="MAN")
aus_for_MA <- forecast(aus_ets_MA, h=11)
summary(aus_for_MA)

## ets 에러는 승법 추세는 승법
aus_ets_MM <- ets(ausair_train, model="MMN")
aus_for_MM <- forecast(aus_ets_MM, h=11)
summary(aus_for_MM)


plot(aus_for_MM)
lines(aus_ets_MM$fitted, col="red", type="o")
lines(ausair_test, col="black", type="l")

## test데이터에 accuracy확인하기 
accuracy(ausair_for, ausair_test)
accuracy(aus_for_MM, ausair_test)




####홀트의 계절지수 평활법 hw###
## 데이터의 추세확인 
plot(austourists)
austourists_train <- window(austourists, start=2000, end=2014)
austourists_test <- window(austourists, start=2015)

austourists_hw <- hw(austourists_train, h=10)
austourists_for <- forecast(austourists_hw)
summary(austourists_for)

plot(austourists_for)
lines(austourists_for$fitted, col="red", type="o")
lines(austourists_test, col="black", type="l")

## 오토로 넣고돌림
aust_ets <- ets(austourists_train)
summary(aust_ets)

## 에러를 승법 추세를 가법 계절을 가법으로 넣고 돌림
aust_ets_MAA <- ets(austourists_train, model="MAA")
summary(aust_ets_MAA)

## 에러를 승법 추세를 가법 계절을 승법으로 넣고 돌림
aust_ets_MAM <- ets(austourists_train, model="MAM")


aust_ets_for <- forecast(aust_ets_MAM, h=10)

plot(aust_ets_for)
lines(aust_ets_for$fitted, col="red", type="o")
lines(austourists_test, col="black", type="l")

## 테스트 데이터에 대한 accuracy 확인
accuracy(austourists_for, austourists_test)
accuracy(aust_ets_for, austourists_test)

##############################################시계열분해 이동평균####################################################
library(forecast)
library(TTR)
library(fpp2)

## 추세성분을 추정하기 위해 이동평균 방법을 사용
ma.ts <- ts(c(20,10,30,35,10,60,50,40,90,65,55,120), frequency=1)

ma.ts

## 대칭이동평균 order가 홀수 
ma(ma.ts,order=3)

## order가 짝수 중심화 이동평균 centre=FALSE로 두면 한번만 실행한다.
ma(ma.ts, order=4, centre=FALSE)

## order가 짝수 중심화 이동평균을 두번 해서 시점을 맞춰 준다(TRUE가 기본 값).
ma(ma.ts, order=4, centre=TRUE)


SMA(ma.ts, n=3)


## 시계열 분해를 위한 이동평균
data("elecsales")
plot(elecsales)

elecsales

## 대칭이동평균
ma(elecsales, order=5)

## m(order)크기를 크게할 수록 추세성분이 뚜렷하게 보인다
## 단점 정보 손실도 커진다
par(mfrow=c(2,2))
elecsales.ma <- ma(elecsales, order=3)
plot(elecsales)
lines(elecsales.ma, col="red")

elecsales.ma <- ma(elecsales, order=5)
plot(elecsales)
lines(elecsales.ma, col="red")

elecsales.ma <- ma(elecsales, order=7)
plot(elecsales)
lines(elecsales.ma, col="red")

elecsales.ma <- ma(elecsales, order=9)
plot(elecsales)
lines(elecsales.ma, col="red")


## 시계열 분해를 위한 이동평균
par(mfrow=c(1,1))
beer2 <- window(ausbeer, start=1992)
plot(beer2)

## 이동평균 한번
ma4 <- ma(beer2, order=4, centre=FALSE)

## 이동평균 두번 
ma2X4 <- ma(beer2, order=4, centre=TRUE)
plot(beer2)
lines(ma4, col="red")
lines(ma2X4, col="blue")

elecequip.ma <- ma(elecequip, 12)
plot(elecequip)
lines(elecequip.ma, col="red", lwd=4)


## Classical decomposition(decompose 함수)
elecequip.de <- decompose(elecequip, type = "additive")

## 속성보기
names(elecequip.de)

elecequip.de$x # 데이터 값
elecequip.de$seasonal # 계절 성분
elecequip.de$trend # 추세성분
elecequip.de$random # 불규칙 성분
elecequip.de$figure #계절 주기 성분 
elecequip.de$type # 사용된 모형(가법, 승법)


## 추세조정된 계열
detrendedseries <- elecequip.de$x - elecequip.de$trend
plot(detrendedseries)

## 계절성분 추정법 (월별로 평균을 낸다음 월별의 평균을 냄)
detrendedseries


## 분해방법 승법
elecequip.multi <- decompose(elecequip, type="multiplicative")

elecequip.multi$trend

## 추세조정된 계열
detrendedseries <- elecequip.multi$x / elecequip.multi$trend
plot(detrendedseries)

## 계절성분 추정
elecequip.multi$seasonal


## 계절성분
sum(elecequip.multi$figure)

## 불규칙한 성분 추정
elecequip.multi$random

## 동일한 결과
elecequip.multi$x / (elecequip.multi$trend * elecequip.multi$seasonal)

## autoplot으로 차트 확인
autoplot(elecequip.multi)


#############################예측평가방법#############################################
# log()
log(data)
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
beerfit2 <- naive(train, h=10) # rwf(train, h=10)
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



###################################백색잡음, 확률보행과정########################################
library(forecast)
library(ggplot2)
## 백색잡음 구현해보기
# 백색 잡음을 예측하는데 가장 좋은 방법은 meanf
set.seed(12)
## 평균이 0이고 표편이 1인데이터 생성
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")

## 시차별로 신뢰구간(파란색) 안에 들어있기 때문에 0이라 볼 수 있다 -> 시차 관에 관계가 없다고 본다.
ggAcf(y) # 자기 상관 함수 
ggPacf(y) # 편자기 상관 함수
ggtsdisplay(y)


z <- ts(rnorm(500))
hist(z) + ggtitle("White noise")


## 확률 보행과정 구하기(random walk process)
x <- rnorm(300)
w <- x

for(t in 2:300){ ## 절편 있이
  x[t] = x[t-1]+w[t] + 0.2
}

for(t in 2:300){
  x[t] = x[t-1] + w[t]
}
x.ts <- ts(x)

## 절편 0.2 +를 붙히면 쭉 위로 -를 붙히면 쭉 아래로
ggtsdisplay(x.ts)
# 시차가 멀어질수록 상관성이 떨어지고 있다는 것을 알 수 있다.
ggAcf(x.ts)
# 하나만 툭 튀어나와 있으면 랜덤워크 과정이다.
ggPacf(x.ts)



## 확률보행과정(차분)
a <- ts(c(4,6,7,10))
diff(a)

## 위에 꺼에 적용 diff를 통해 평균이 일정한 데이터로 변환
x.diff <- diff(x.ts)

ggtsdisplay(x.diff)
ggAcf(x.diff)
ggPacf(x.diff)


## ARIMA 모델 기초
library(fpp2)
data(AirPassengers)



#########################################정상시계열판단##############################################
library(fpp2)

goo <- goog200
autoplot(goo) + ggtitle("White noise")
ggtsdisplay(goo)

goo_diff <- diff(goo)
autoplot(goo_diff) + ggtitle("White noise")
ggtsdisplay(goo_diff)



set.seed(123)
x <- rnorm(400)
w <- x

for(t in 2:300){ ## 절편 있이
  x[t] = x[t-1]+w[t] + 0.3
}

x.ts <- ts(x)
ggtsdisplay(x.ts)


###########################urca########################
library(fpp2)
library(urca)

## value ~ 검정통계량, Critical values 유의수준
summary(ur.kpss(goog200))
summary(ur.kpss(goo_diff))
summary(ur.kpss(x.ts))

Box.test(goo, lag=10, type="Ljung-Box")
Box.test(goo_diff, lag=10, type="Ljung-Box")
Box.test(x.ts, lag=10, type="Ljung-Box")


####################################AR,MA모형##################################################
library(fpp2)
set.seed(12)

y <- ts(numeric(300))
e <- rnorm(300)

# 0.9
for (t in 2:300){
  y[t] <- 0.9*y[t-1] + e[t]
}

ggtsdisplay(y)

# 모수추정
Arima(y, order=c(1,0,0))


# 0.6
set.seed(12)
y <- ts(numeric(300))
e <- rnorm(300)

for (t in 2:300){
  y[t] <- 0.6*y[t-1] + e[t]
}

ggtsdisplay(y)
Arima(y, order=c(1,0,0))


# -0.8
set.seed(12)
y <- ts(numeric(300))
e <- rnorm(300)

for (t in 2:300){
  y[t] <- -0.8*y[t-1] + e[t]
}

ggtsdisplay(y)
Arima(y, order=c(1,0,0))


# -0.5
set.seed(12)
y <- ts(numeric(300))
e <- rnorm(300)


for (t in 2:300){
  y[t] <- -0.5*y[t-1] + e[t]
}

ggtsdisplay(y)
Arima(y, order=c(1,0,0))

## 상수항이 있는 AR(1) 형
set.seed(12)
y <- ts(numeric(300))
e <- rnorm(300)

for (t in 2:300){
  y[t] <- -0.8*y[t-1] + e[t] + 18
}

ggtsdisplay(y)
Arima(y, order=c(1,0,0))

## PACF를 보고 모형식별이 중요
# ar2일때
set.seed(12)
y <- ts(numeric(300))
e <- rnorm(300)

for (t in 3:300){
  y[t] <- 1.3*y[t-1] - 0.7*y[t-2] + e[t]
}

ggtsdisplay(y)
Arima(y, order=c(2,0,0))


############## 가상의 AR(1)시계열로 분석해보기(Box-Jenkins 방법론)##################################
library(fpp2)
library(urca)
set.seed(12)
y <- ts(numeric(300))
e <- rnorm(300)

for( t in 2:300 ){
  
  y[t] <- 0.9 * y[t-1] + e[t]
  
}
# 시계열 데이터의 그래프와 ACF, PACF를 통해 모형 식별
ggtsdisplay(y)


# 데이터 나누기
train <- window(y, start=1, end=250)
test <- window(y, start=251, end=300)

# 원데이터의 정상성 확인 
summary(ur.kpss(train))


## 모델
ar_1_m <- meanf(train,h=50)
ar_1_n <- naive(train,h=50)
ar_1 <- Arima(train,order=c(1,0,0), include.mean = FALSE)

## 적합성 진단 잔차검증
checkresiduals(ar_1)

# Ljung-BoX 잔차 검증
Box.test(ar_1$residuals, lag = 10, type = "Ljung-Box") 

## 예측하기
ar_1_m_f <- forecast(ar_1_m, h=50)
ar_1_n_f <- forecast(ar_1_n, h=50)
ar_1_f <- forecast(ar_1, h=50)

## accuracy
accuracy(ar_1_m_f,test)
accuracy(ar_1_n_f,test)
accuracy(ar_1_f,test)


autoplot(ar_1_m_f)+
  autolayer(fitted(ar_1_m_f),series='fitted')+
  autolayer(test,series='test')+
  xlab('TIME')+ylab('AR(1) simulation')


autoplot(ar_1_n_f)+
  autolayer(fitted(ar_1_n_f),series='fitted')+
  autolayer(test,series='test')+
  xlab('TIME')+ylab('AR(1) simulation')


autoplot(ar_1_f)+
  autolayer(fitted(ar_1_f),series='fitted')+
  autolayer(test,series='test')+
  xlab('TIME')+ylab('AR(1) simulation')



#############################MA 모형 시뮬레이션##################
set.seed(12)
library(fpp2)
AR_1 <- arima.sim(list(order=c(1,0,0), ar=0.7), n=200)
AR_2 <- arima.sim(list(order=c(2,0,0), ar=c(0.3,0.2)), n=300)

# MA(1) 모형
MA_1 <- arima.sim(list(order=c(0,0,1), ma=0.9), n=300)
MA_2 <- arima.sim(list(order=c(0,0,1), ma=0.6), n=300)                  
MA_3 <- arima.sim(list(order=c(0,0,1), ma=-0.8), n=300)
MA_4 <- arima.sim(list(order=c(0,0,1), ma=-0.5), n=300)

Arima(MA_1, order=c(0,0,1))
Arima(MA_2, order=c(0,0,1))
Arima(MA_3, order=c(0,0,1))
Arima(MA_4, order=c(0,0,1))

ggtsdisplay(MA_1)
ggtsdisplay(MA_2)
ggtsdisplay(MA_3)
ggtsdisplay(MA_4)


#MA(2) 모형
MA_2 <- arima.sim(list(order=c(0,0,2), ma=c(0.8,0.3)), n=300)
ggtsdisplay(MA_2)
Arima(MA_2, order=c(0,0,2))


## train, test나누기
train <- window(MA_1, start=1, end=250)
test <- window(MA_1, start=251, end=300)

arima(train, order=c(0,0,1))
auto.arima(train)

# 적합성 진단
MA_1_fit <- Arima(train, order=c(0,0,1))
checkresiduals(MA_1_fit)

MA_1_F <- forecast(MA_1_fit, h=50)

# 예측성과 비교하기
accuracy(MA_1_F, test)
accuracy(naive(train, h=50),test)
accuracy(meanf(train, h=50), test)

# 차트로 표현
library(ggplot2)
autoplot(MA_1_F) + 
  autolayer(test, series+'test')
autolayer(fitted(MA_1_F), series="fitted") +
  xlab("Time") + ylab("MA(1) simulation" )



#############################################ARIMA모형############################################
library(fpp2)
library(urca)
set.seed(1234)
arma_1_1 <- arima.sim(model = list(order=c(1,0,1), ar = 0.7, ma = -0.2), n=300)
ggtsdisplay(arma_1_1)

# AICC 값을 기준으로 모형이 설정될 수 있다.
Arima(arma_1_1, order=c(1,0,1))
auto.arima(arma_1_1)

# ar 계수 양수 mr 계수 양수
arma_1_1 <- arima.sim(model = list(order=c(1,0,1), ar = 0.7, ma = 0.2), n=300)
ggtsdisplay(arma_1_1)

Arima(arma_1_1, order=c(1,0,1))
auto.arima(arma_1_1)

# ar 계수 음수 mr 계수 양수
arma_1_1 <- arima.sim(model = list(order=c(1,0,1), ar = -0.7, ma = -0.2), n=300)
ggtsdisplay(arma_1_1)

Arima(arma_1_1, order=c(1,0,1))
auto.arima(arma_1_1)


# ar계수 음수 mr 계수 양수
arma_1_1 <- arima.sim(model = list(order=c(1,0,1), ar = -0.7, ma = 0.2), n=300)
ggtsdisplay(arma_1_1)

Arima(arma_1_1, order=c(1,0,1))
auto.arima(arma_1_1)


## ARMA(2,1)모형
arma_2_1 <- arima.sim(model = list(order=c(2,0,1), ar = c(0.7,-0.6), ma = 0.4), n=300)
ggtsdisplay(arma_2_1)

Arima(arma_2_1, order=c(2,0,1))
auto.arima(arma_2_1)


## ARMA(1,2)모형
arma_1_2 <- arima.sim(model = list(order=c(1,0,2), ar = 0.8, ma = c(0.3, -0.5)), n=300)
ggtsdisplay(arma_1_2)

Arima(arma_1_2, order=c(1,0,2))
auto.arima(arma_1_2)


## ARMA(2,2) 모형
arma_2_2 <- arima.sim(model = list(order=c(2,0,2), ar = c(0.7,-0.6), ma = c(0.3, -0.5)), n=300)
ggtsdisplay(arma_2_2)

Arima(arma_2_2, order=c(2,0,2))
auto.arima(arma_2_2)




# ARIMA(1,1,1)
set.seed(1234)
arima_1_1_1 <- arima.sim(model = list(order=c(1,1,1), ar=0.7, ma=-0.2), n=300)
ggtsdisplay(arima_1_1_1)
summary(ur.kpss(arima_1_1_1)) ## 임계값 4.00이 유의수준 1%의 값보다 크므로 귀무가설을 기각 -> 비정상 시계열이다.
# 차분
ggtsdisplay(diff(arima_1_1_1))
summary(ur.kpss(diff(arima_1_1_1)))


# 잔차 확인
arima_fit <- arima(arima_1_1_1, order=c(1,1,1))
checkresiduals(arima_fit) # 유의확률 0.56이 유의수준 0.5보다 크므로 귀무가설을 채택


###################################모형진단하는법###############################################3
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
data1_arima1 <- Arima(data_1, order = c(1,0,0)) # C = 99.8060 * (1 -(-0.6608)) 백색잡음 표준편차 31.76
data1_arima2 <- Arima(data_1, order = c(1,0,1))
data1_arima3 <- Arima(data_1, order = c(1,0,2))


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
data2_arima1 <- Arima(data_2, order = c(0,0,1)) 
data2_arima2 <- Arima(data_2, order = c(1,0,1))
data2_arima3 <- Arima(data_2, order = c(2,0,1))


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
data3_arima1 <- Arima(data_3, order = c(1,0,1)) # C = 99.8060 * (1 -(-0.6608)) 백색잡음 표준편차 31.76
data3_arima2 <- Arima(data_3, order = c(2,0,1))
data3_arima3 <- Arima(data_3, order = c(1,0,2))
data3_arima4 <- Arima(data_3, order = c(2,0,2))

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
data4_arima1 <- Arima(data_4, order = c(1,0,1)) # C = 99.8060 * (1 -(-0.6608)) 백색잡음 표준편차 31.76
data4_arima2 <- Arima(data_4, order = c(2,0,1))
data4_arima3 <- Arima(data_4, order = c(1,0,2))
data4_arima4 <- Arima(data_4, order = c(2,0,2))

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

#################################앙상블 결합 모형########################
library(fpp2)
library(forecast)
train <- window(auscafe, end = c(2012,9))
h <- length(auscafe) - length(train)

ETS <- forecast(ets(train), h=h)  
ARIMA <- forecast(auto.arima(train, lambda = 0), h=h)
STL <- stlf(train, lambda = 0, h=h)
NNAR <- forecast(nnetar(train), h=h)
Combination <- (ETS[["mean"]] + ARIMA[["mean"]] + STL[["mean"]] + NNAR[["mean"]])/4

c(ETS = accuracy(ETS, auscafe)["Test set","RMSE"],
  ARIMA = accuracy(ARIMA, auscafe)["Test set","RMSE"],
  `STL-ETS` = accuracy(STL, auscafe)["Test set","RMSE"],
  NNAR = accuracy(NNAR, auscafe)["Test set","RMSE"],
  Combination = accuracy(Combination, auscafe)["Test set","RMSE"])


far2 <- function(x, h) {forecast(Arima(x, order=c(2,0,0)), h=h) }
e <- tsCV(lynx, far2, h=1, window = 30)

fit1 <- Arima(lynx[1:30], order = c(2,0,0))
fcast1 <- forecast(fit1, h=1)
lynx[31] - fcast1$mean[1]; e[30]

fit2 <- Arima(lynx[2:31], order = c(2,0,0))
fcast2 <- forecast(fit2, h = 1)
lynx[32] - fcast2$mean[1]; e[31]



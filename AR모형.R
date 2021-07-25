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

train <- window(y, start=1, end=250)
test <- window(y, start=251, end=300)

## 모델
ar_1_m <- meanf(train,h=50)
ar_1_n <- naive(train,h=50)
ar_1 <- Arima(train,order=c(1,0,0), include.mean = FALSE)


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


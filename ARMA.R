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




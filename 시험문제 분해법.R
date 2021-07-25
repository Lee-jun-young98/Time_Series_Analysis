library(forecast)
library(TTR)
library(fpp2)
elecsales
## elecsales 원래데이터
## 시계열 분해를 위한 이동평균
data("cangas")
plot(cangas)

cangas

## 대칭이동평균
ma(cangas, order=5)

## m(order)크기를 크게할 수록 추세성분이 뚜렷하게 보인다
## 단점 정보 손실도 커진다
par(mfrow=c(2,2))
cangas.ma <- ma(cangas, order=3)
plot(cangas)
lines(cangas.ma, col="red")

cangas.ma <- ma(cangas, order=5)
plot(cangas)
lines(cangas.ma, col="red")

cangas.ma <- ma(cangas, order=7)
plot(cangas)
lines(cangas.ma, col="red")

cangas.ma <- ma(cangas, order=9)
plot(cangas)
lines(cangas.ma, col="red")


## 시계열 분해를 위한 이동평균


par(mfrow=c(1,1))
beer2 <- window(ausbeer, start=1992)
plot(beer2)

## 이동평균 한번
ma4 <- ma(beer2, order=4, centre=FALSE)

## 이동평균 두번 
ma2cangas4 <- ma(beer2, order=4, centre=TRUE)
plot(beer2)
lines(ma4, col="red")
lines(ma2cangas4, col="blue")


## elecequip 데이터 


## cangas
cangas.ma <- ma(cangas, 12)
plot(cangas)
lines(cangas.ma, col="red", lwd=4)


## Classical decomposition(decompose 함수)
plot(cangas)
cangas.de <- decompose(cangas, type = "additive")

## 속성보기
names(cangas.de)

cangas.de$cangas # 데이터 값
cangas.de$seasonal # 계절 성분
cangas.de$trend # 추세성분
cangas.de$random # 불규칙 성분
cangas.de$figure #계절 주기 성분 
cangas.de$tcangaspe # 사용된 모형(가법, 승법)


## 추세조정된 계열
detrendedseries <- cangas.de$x - cangas.de$trend
plot(detrendedseries)

## 계절성분 추정법 (월별로 평균을 낸다음 월별의 평균을 냄)
cangas.de$seasonal

cangas.de$figure

## 불규칙성분
cangas.de$random
cangas.de$cangas - cangas.de$trend - cangas.de$seasonal

sum(cangas.de$figure)


## 분해방법 승법
cangas.multi <- decompose(cangas, type="multiplicative")
plot(cangas.multi)
cangas.multi$trend


## 추세조정된 계열
detrendedseries <- cangas.multi$x / cangas.multi$trend
plot(detrendedseries)

## 계절성분 추정
cangas.multi$seasonal
plot(cangas.multi$seasonal)

## 계절성분 총합값
sum(cangas.multi$figure)

## 불규칙한 성분 추정
cangas.multi$random

## 동일한 결과
cangas.multi$x / (cangas.multi$trend * cangas.multi$seasonal)

## autoplot으로 차트 확인
autoplot(cangas.multi)

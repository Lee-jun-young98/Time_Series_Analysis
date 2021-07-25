library("forecast")
library("TTR")
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
ma4 <- ma(beer2, order=4, centre=FALSE)
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
detrendedserie

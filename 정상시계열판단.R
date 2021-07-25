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

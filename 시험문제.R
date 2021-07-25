library(fpp2)

## 데이터 usmelec

# 확인
plot(cangas)

cangas_ses <- ses(cangas, h=62)
cangas_holt <- holt(cangas, h=62)
cangas_holtw <- hw(cangas, h=62, seasonal="multiplicative")


plot(cangas_ses)
lines(cangas_ses$fitted, col="red")
plot(cangas_holt)
lines(cangas_holt$fitted, col="red")
plot(cangas_holtw)
lines(cangas_holtw$fitted, col="red")


## simple optimal rmse 비교
cangas_ses <- ses(cangas, h=62, inital="simple")
summary(cangas_ses)
cangas_ses2 <- ses(cangas, h=62, initial = "optimal")
summary(cangas_ses2)


data(livestock)
plot(livestock)

# 최적의 alpha값 찾기
alpha <- seq(0.01,1,0.01)
rmse_df <- data.frame()

for(i in alpha){
  livestock_ses <- ses(livestock,alpha=i,initial = "simple")
  alpha_df <- c(alpha=i,rmse=accuracy(livestock_ses)[2])
  rmse_df <- rbind(rmse_df,alpha_df)
}
colnames(rmse_df) <- c("alpha","rmse")
plot(rmse_df)



## 베타 
h_beta <- seq(0.01,1,0.01)
rmse_df <- data.frame()

for(i in h_beta){
  usm_holt <- holt(cangas,beta=i, Tamped = TRUE)
  beta_df <- c(beta=i,rmse=accuracy(usm_holt)[2])
  rmse_df <- rbind(rmse_df,beta_df)
}
colnames(rmse_df) <- c("beta","rmse")
plot(rmse_df)


# train test 나누기 
train <- window(cangas, start=c(1960,1), end=c(1999,12))
test <- window(cangas, start =c(2000,1))

## 단순지수 예측성과 평가하기
ses_train <- ses(train, alpha=0.99, initial = "simple", h=62)
accuracy(ses_train, test)
ses_train2 <- ses(train, alpha=0.99, initial = "optimal", h=62)
accuracy(ses_train2, test)

## 홀트 모델 평가
holt_train <- holt(train, damped=TRUE, h=62)
accuracy(holt_train, test)
holt_train2 <- holt(train, damped=FALSE, h=62)
accuracy(holt_train2, test)

## 홀트계절 가법 승법
hw_train <- hw(train, seasonal = "additive", h=62)
accuracy(hw_train, test)
hw_train2 <- hw(train, seasonal = "multiplicative", h=62)
accuracy(hw_train2, test)



ets1 <- ets(train)
ets1_for <- forecast(ets1, h=62)
ets2 <- ets(train, model="MMN")
ets2_for <- forecast(ets2, h=62)
ets3 <- ets(train, model="MMM")
ets3_for <- forecast(ets3, h=62)

accuracy(ets1_for, test)
accuracy(ets2_for, test)
accuracy(ets3_for, test)

# 제일 좋은거 넣기
cangas_for <- forecast(hw_train2)
accuracy(usm_for, test)
accuracy(ets3_for, test)


plot(cangas)
lines(hw_train$fitted, col="red", type="o")
lines(test, col="blue", type="l")

plot(ets1_for)
lines(ets1_for$fitted, col="red", type="o")
lines(test, col="blue", type="l")

par(mfrow=c(2,1))

summary(cangas_for$residuals)
summary(ets3_for$residuals)
plot(train-ests3_for$fitted, type="p")
abline(h=0)
hist(cangas_for$residuals, main="홀트윈터의 잔차")
hist(ets1_for$residuals, main="ets모형 MAM의 잔차")


error <- test - cangas_for$mean
error2 <- test - ets3_for$mean

par(mfrow=c(2,1))
summary(error)
summary(error2)
hist(error, main="홀트윈터의 예측오차")
hist(error2, main="ets MAM모형의 예측오차")


plot(test-cangas_for$mean, type="p")
abline(h=0)

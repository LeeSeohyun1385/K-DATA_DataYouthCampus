soju <- read.table("C://Users//이서현//Desktop//2021-1학기//soju.txt",header=FALSE)
soju.ts <- ts(soju[,2], frequency=4, start=c(1965,01),end=c(2021,02))
plot(soju.ts)
acf(soju.ts)
pacf(soju.ts)
#분산안정화 -> 평균안정화


#분산안정화 
#1)로그변환
plot(log(soju.ts))
#안정화가 되지 않음
#2)box-cox변환 (멱변환)
library(MASS)
x<-time(soju.ts)
bc.soju <- boxcox(soju.ts~x)
lam <- bc.soju$x[which.max(bc.soju$y)]
lam #box-cox변환을 통한 모수 람다 

par(mfrow=c(1,2))
plot(log(soju.ts))
plot((soju.ts)^lam)
cov(matrix(soju.ts))
cov(matrix(log(soju.ts)))  #분산 안정화가 더 잘됨 
cov(matrix((soju.ts)^lam))


-------
#평균안정화 
#1)추세제거
lm(log(soju.ts)~x)  #추정한 식 
DT1 <- lm(log(soju.ts)~x)$residual  #잔차계산 
plot(DT1)
plot.ts(DT1)
lines(lowess(DT1),col="blue")
#평균안정화가 잘 안됨

#1-1) 이차항 추세제거
DT2<- lm(log(soju.ts)~x+I(x^2))$residual
plot.ts(DT2)
lines(lowess(DT2),col="blue") 

DT3<- lm(log(soju.ts)~x+I(x^2)+I(x^3))$residual
plot.ts(DT3)
lines(lowess(DT3),col="blue")

DT4<- lm(log(soju.ts)~x+I(x^2)+I(x^3)+I(x^4))$residual
plot.ts(DT4)
lines(lowess(DT4),col="blue")
#어느정도 평균이 안정화 되었지만 아직 일정한 값은 아님

#2)계절조정
DT.ts <- ts(DT4,start=c(1965,1),frequency=4)
DT.ts

dec <- decompose(DT.ts,type="multiplicative")
plot(decompose(DT.ts,type="multiplicative"))
SA <- dec$x-dec$seasonal 
plot(SA)




#3)차분 - 확률적 추세가 존재하는 경우 추세제거해도 정상성 만족하지 않음 
plot(log(soju.ts))
#1차 차분
plot(diff(log(soju.ts),lag=1,differences = 1))
#2차 차분
plot(diff(log(soju.ts),lag=1,differences = 2))
#2차 차분 했을 때 더 정상화 된 것을 확인 

#box-cox변환한 데이터 차분 
z <- ((soju.ts)^lam)/lam
z1 <- diff(z,differences = 2)
plot(z1)


#비교
par(mfrow=c(2,2))
plot(diff(log(soju.ts),lag=1,differences = 1))
plot(diff(log(soju.ts),lag=1,differences = 2))
plot(diff(log(soju.ts),lag=12,differences = 1))
plot(diff(log(soju.ts),lag=1,differences = 1),lag=12,differences=1)
#2차 차분이 제일 정상화 

#이데이터는 선형추세방법 보다는 차분이 시계열을 정상화 하기에는 더 적합한 방법이라고 생각 

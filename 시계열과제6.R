soju <- read.table("C://Users//이서현//Desktop//2021-1학기//soju.txt",header=FALSE)
soju.ts <- ts(soju[,2], frequency=4, start=c(1965,01),end=c(2021,02))
plot(soju.ts)

#모형식별 1단계
#적당한 변수변환
#분산안정화 ->과제5의 결과 box-cox변환 보다 log변환이 분산을 더 안정화 시켜줌
SJ <- log(soju.ts)
plot(SJ)
--------------------------
#모형식별 2단계 : 차분차수 결정 및 차분 시행
plot(SJ)
acf(SJ)  #sacf 가 완만하게 줄어듦 -> 차분 필요 
pacf(SJ)
diff1 <- diff(SJ)

plot(diff1)
acf(diff1)  #sacf 급격하게 줄어듦  
pacf(diff1)

diff2 <- diff(diff1)
plot(diff2)
acf(diff2)
pacf(diff2)

#차분차수=2

#모형식별 3단계 :ARMA모형의 차수 결정 
------------------------------------
plot(SJ)
library(tseries)
#차분이 필요한지 확인 
adf.test(SJ)
#H0:차분이 필요한비정상시계열 H1:차분X정상시계열
#p-value가 큼 귀무가설 지지 -차분필요
kpss.test(SJ)
#p-value가 0.05 보다 작음 : 대립가설 지지 - 차분이 필요함 

SJ1<-diff(SJ)
plot(SJ1)
acf(SJ1)
pacf(SJ1)

adf.test(SJ1)
#유의확률이 유의수준보다 작음 : 귀무가설 기각 - 차분X 정상시계열
## 차분차수 1로 결정 


#최적의 모형 찾기 
library(forecast)
ar1<-Arima(SJ1,order=c(1,1,0))
ar1

ma1 <- Arima(SJ1,order=c(0,1,1))
ma1

arma11 <- Arima(SJ1,order=c(1,1,1))
arma11

---
WN <- Arima(SJ1, order = c(0,0,0))

ar1 <- Arima(SJ1,order=c(1,1,0))
ar2 <- Arima(SJ1,order=c(2,1,0))
ar3 <- Arima(SJ1,order=c(3,1,0))
ar4 <- Arima(SJ1,order=c(4,1,0))
ar5 <- Arima(SJ1,order=c(5,1,0))
ar6 <- Arima(SJ1,order=c(6,1,0))
ar7 <- Arima(SJ1,order=c(7,1,0))
ar8 <- Arima(SJ1,order=c(8,1,0))
ar9 <- Arima(SJ1,order=c(9,1,0))
ar10 <- Arima(SJ1,order=c(10,1,0))

ma1 <- Arima(SJ1,order=c(0,1,1))
ma2 <- Arima(SJ1,order=c(0,1,2))
ma3 <- Arima(SJ1,order=c(0,1,3))
ma4 <- Arima(SJ1,order=c(0,1,4))
ma5 <- Arima(SJ1,order=c(0,1,5))
ma6 <- Arima(SJ1,order=c(0,1,6))
ma7 <- Arima(SJ1,order=c(0,1,7))
ma8 <- Arima(SJ1,order=c(0,1,8))
ma9 <- Arima(SJ1,order=c(0,1,9))
ma10 <- Arima(SJ1,order=c(0,1,10))

arma11 <- Arima(SJ1,order=c(1,1,1))
arma21 <- Arima(SJ1,order=c(2,1,1))
arma12 <- Arima(SJ1,order=c(1,1,2))
arma22 <- Arima(SJ1,order=c(2,1,2))
arma31 <- Arima(SJ1,order=c(3,1,1))
arma13 <- Arima(SJ1,order=c(1,1,3))
arma33 <- Arima(SJ1,order=c(3,1,3))
arma23 <- Arima(SJ1,order=c(2,1,3))
arma32 <- Arima(SJ1,order=c(3,1,2))
arma44 <- Arima(SJ1,order=c(4,1,4))

aic<- AIC(WN,ar1,ar2,ar3,ar4,ar5,ar6,ar7,ar8,ar9,ar10,ma1,ma2,ma3,ma4,ma5,ma6,ma7,ma8,ma9,ma10,arma11,arma21,arma12,arma22,arma31,arma13,arma33,arma23,arma32,arma44)
#arma11 이 aic최소 

bic<- BIC(WN,ar1,ar2,ar3,ar4,ar5,ar6,ar7,ar8,ar9,ar10,ma1,ma2,ma3,ma4,ma5,ma6,ma7,ma8,ma9,ma10,arma11,arma21,arma12,arma22,arma31,arma13,arma33,arma23,arma32,arma44)
#arma11이 bic최소 

min(aic)
min(bic)


#aic와 bic가 최소가 되는 arma11모형 선택 
arma1 <- Arima(SJ1,order=c(1,1,1))


#모형진단

#과적합
#선택한 모형MA(2)
arma11 <- Arima(SJ1,order=c(1,1,1))  

abs(-0.0433/0.0673) #<2 -> ar2모수가 유의하지 않음 

#잔차분석
tsdiag(arma11)

residuals(arma11)#잔차 다 계산해줌 (1000개)
res <- residuals(arma11)
par(mfrow=c(1,1))
plot(res)
#평균이 0인 수평선타입으로 보이는가 ? (O) ~ WN
acf(res) #다 0이어야함 (O)
pacf(res)

Box.test(arma11$residuals,type="Ljung-Box") #높은 유의확률을 보임 
-----------------------------------------------
#미래시점 값 12개 예측 
fit<-Arima(SJ1,order=c(1,1,1))
fit
predict(fit,n.ahead=12)

forecast(fit,h=12)
plot(forecast(fit,h=12))
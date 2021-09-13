plot(AirPassengers)
#종속변수 : airpassengers , time:독립
#평균 일정, 분산일정, 반복되는패턴 없애기 를 해야함

#분산안정화변환
#로그변환
plot(log(AirPassengers))

#box-cox변환
library(MASS)
?boxcox#lamda값 지정 (필요한 경우), default는 -2부터2까지 0.1간격으로 lamda값 지정해줌
#plotit=FALSE라고 해주면 값 출력

AirPassengers #time존재하지 않음 
time(AirPassengers) #time이라는 변수 지정 
boxcox(AirPassengers~time(AirPassengers))
#lamda가 0일때 최대가됨:lamda에 대한 mle /95%신뢰구간
#윗 값들을 프린트
bc <-boxcox(AirPassengers~time(AirPassengers))
bc

#lamda 추정 (최대가능도 추정방법)

#x:람다값, y:로그라이클리후드 (loglikelihood)
which.max(bc$y) #loglikelihood의 최대값이 위치하는 위치 :52번
lam <- bc$x[which.max(bc$y)] #최대가되는 람다값 
(AirPassengers)^lam  #Xt^(lamda) - box-cox변환 

par(mfrow=c(1,2))
plot(log(AirPassengers)) #log변환
plot((AirPassengers)^lam) #box-cox변환 
#비슷한경우 log사용하는 것이 더 좋다. 


#lamda 추정(회귀모형)

par(mfrow=c(1,1))
plot(log(AirPassengers))

lm(log(AirPassengers)~time(AirPassengers))
#추정한 식fitted? 
--------------------------------

#평균의 정상화#

#추세제거 - 1)잔차계산 
#1차추세추정 
DT1<- lm(log(AirPassengers)~time(AirPassengers))$residual  #잔차
plot(DT1)
plot.ts(DT1)#분산이 안정화됨

lines(lowess(DT1),col="blue") #곡선으로 보임 : 추세를 완전히 제거하지 않음


#추세제거:이차항추세추정 
x<-time(AirPassengers)
DT2<- lm(log(AirPassengers)~x+I(x^2))$residual
plot.ts(DT2)
lines(lowess(DT2),col="blue")  #직선으로 평평하게 바뀜: 추세 제거해줌 
#선형추세, 이차항도 부족하면 데이터에 따라서 더 조정해야함

---
#계절조정#
#계절성분제거 (반복되는것)
start(AirPassengers)
DT.ts <-ts(DT2,start=start(AirPassengers),frequency = 12) #DT2(추세제거된 데이터)를 시계열 데이터로 변환 
DT.ts
dec <- decompose(DT.ts,type="additive") #분해법사용 
plot(decompose(DT.ts,type="additive"))
#계절조정
SA <- dec$x-dec$seasonal #observed-seasonal 
plot(SA)
lines(lowess(SA),col="blue") #내가 추가한거 -> 평균이 특정범위안에 있음:정상시계열
---

#추세제거 - 2) 회귀분석기법 
xm <- 1:length(AirPassengers)
xm%%12
#1월은1 ,3월은3,,,,, 12월은 0라고 표시
x1 <- xm%%12==1  #1월만 1(TRUE)이 들어있고 나머지는 다 0(FALSE)인 가변수
x2 <- xm%%12==2
x3 <- xm%%12==3
x4 <- xm%%12==4
x5 <- xm%%12==5
x6 <- xm%%12==6
x7 <- xm%%12==7
x8 <- xm%%12==8
x9 <- xm%%12==9
x10 <- xm%%12==10
x11 <- xm%%12==11
#12월은 따로 안해줌: 다0이면 12월이기 때문(범주의개수-1개를 입력)

air.s<- lm(log(AirPassengers)~xm+I(xm^2)+x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)$residuals
plot.ts(air.s)

--------------------
#차분#
#1차차분
diff(1:10,lag=1,differences = 1) #한시차전 빼주고 차분을 한번한 것
diff(1:10)
#계절성분, 추세성분 모두 제거한 것

diff(1:10,lag=2,differences = 1)
diff(1:10,lag=1,differences = 2) #차분을 한번더 해줌 : 다0 (differences:n차 차분)

plot(log(AirPassengers))
plot(diff(log(AirPassengers),lag=1,differences = 1)) #1차차분 

#계절성분 없애야함
par(mfrow=c(2,2))
plot(log(AirPassengers))
plot(diff(log(AirPassengers),lag=1,differences = 1)) #추세차분 
plot(diff(log(AirPassengers),lag=12,differences = 1)) #계절차분
plot(diff(log(AirPassengers),lag=1,differences = 1),lag=12,differences=1) #1차차분, 계절차분 동시에
#반복하는 패턴 : 계절차분 , 증가하는 추세 : 추세차분->정상화
#이 경우에는 증가하는 추세보임 -> 추세차분이 적절 


#이 데이터는 차분보다는 선형추세 방법(회귀분석)이 적합하다고 생각 

#단위근 검정
install.packages("tseries")
install.packages("urca")

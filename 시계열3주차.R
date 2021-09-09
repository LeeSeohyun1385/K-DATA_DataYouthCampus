plot(co2)

#단순이동평균값#
library(TTR)
library(forecast)

#오른쪽으로 다 밀린 상태
sma.12 <- SMA(co2,n=12)  #n:이동평균기간(m):m번째부터 시작 
sma.12
#12번째 부터 시작하여 평균 계산하여 값 표시

lines(sma.12,col="red")
#전체적으로 증가하는 추세 



#중심화이동평균#
#오른쪽으로 밀려있던 것을 왼쪽으로 밀어준 상태 
ma.cen <- ma(co2,order=12,centre=TRUE)
#order: 몇개
lines(ma.cen, col="blue")


---
###ma.cen 이 이렇게 이루어짐 (이렇게 계산됨)  #k짝수일때 
ma.P <- ma(co2,order=12,centre=FALSE) #중심화 안하는 것 
#앞으로 당겨진 상태

end<-length(ma.P)
ma.F <- c(NA, ma.P[-end]) #ma.P를 하나씩 뒤로 민 상태

(ma.P+ma.F)/2
###
---
  
  
#위 결과 다 출력해보기
ma.mat <- cbind(sma.12, ma.P, ma.F, ma.cen)
head(ma.mat, 20)
tail(ma.mat, 20)

#sma 마지막까지 표시
#ma 가운데


lines(ma.cen, col="blue")
#파: 살짝 왼쪽으로 있고
#빨: 살짝 오른쪽으로 치우쳐져 있다.



#이동평균 n값을 얼마로 할건가?
m.vec <- 5:20  #후보 설정
sma.mse.vec <- c()  #저장장소 설정

for(m in m.vec){
  sma.m <- c(NA, SMA(co2, n=m))  #한 시점 늘려줌(하나를 미뤄놈): NA
  end <- length(sma.m)
  sma.mse <- mean((co2-sma.m[-end])^2, na.rm=TRUE) #원스텝 와이드 프로캐스트 에러^2의 평균#길이가 같게하여(-end)
  sma.mse.vec <- c(sma.mse.vec, sma.mse)
}

which.min(sma.mse.vec) #최소가되는 값 찾기 (10번째 값이다.)->mse값 
m.vec[which.min(sma.mse.vec)]
#최소가되는 m 값 <- 우리가 궁금한 것 (m=14일 때 최소이다.)

-----------
#이동평균값 m 설정 (내방식): 찾아줄 때 말고 두개중 선택할 때 
sma.m<-c(NA,SMA(co2,n=14))
end<-length(sma.m)
sma.mse <- mean((co2-sma.m[-end])^2,na.rm=TRUE)
sma.mse  #최소가되는 m값 
#나는 여기에 n(m)값만 바꿔서 sma.mse가 최소가되는 n찾아줄것임 
---------------------------
  

#lowess: 국소회귀 
plot(co2)

#독립변수 설정 안해도 알아서 times 해서 계산해줌 하려면 trd <- 1:length(co2)
lowess(co2, f=2/3)
#x:times, y:co2,  f:윈도우사이즈(보통 2/3):2k+1,  iter :반복 
#$y: 적합값

lines(lowess(co2,f=2/3), col="red")
lines(lowess(co2,f=0.1), col="blue") #윈도우 사이즈 작으면 평활이 덜 되고, 크면 평활이 많이 돼서 평평해짐
#lowess는 미래시점 예측 못해줌

-------------------------------
#Holtwinters:지수평활
#단순지수평활
plot(co2)
w <- 0.5  #평활상수 
es.5 <- HoltWinters(co2, alpha=w, beta=FALSE, gamma=FALSE) 
#단순지수 평활:beta,gamma:FALSE로 지정 / 평활상수(w):alpha  / 초기치설정ES0:l.start
#optim.start : sse가 최소가 되는 것으로 자동으로 계산해주는 것 
es.5
#coeffients : 맨 마지막 지수평활값 (미래의 값 예측할 때 사용-Fn(l)식에 적용):1998 1월 값 

es.5$fitted  #지수평활값 다 출력 (ESt = X^)/ xhat=level+trend*1 (선형지수평활에서) 
#한시점 미뤄서 표기: 2월부터 표기 (head(es.5$x)는 1월부터지만)

#w값 정하기 
#잔차제곱합
es.5$SSE

#w바꿔주며 반복(0.1,0.8)- 0.8이 적합 (평활상수 w선택)
-------
#시계열그림 
lines(ts(c(NA,es.1$fitted),start=c(1959,1),frequency=12),col="red")
lines(ts(c(NA,es.5$fitted),start=c(1959,1),frequency=12),col="blue")
lines(ts(c(NA,es.8$fitted),start=c(1959,1),frequency=12),col="yellow")
--------
#w를 자동으로 설정
es.auto <- HoltWinters(co2, beta=FALSE, gamma=FALSE)

forecast(es.auto)
#예측값 (Forecast), 예측구간 (80%,95%)

plot(forecast(es.auto))
#파란선: 예측값 ,회색:95%예측구간 ,진한회색:80%예측구간 

es.auto$SSE #잔차제곱합 ->최소되는 값 ( 이 방법도 있음 )



#[ARIMA 1번째 ppt]

plot(lh)
#약간 오른쪽으로 증가하는 추세를 띄고 있지만 눈에 띄게는 전반적으로 일정

#arima모형 차수 설정
x1 <- arima.sim(n=1000, model=list(order=c(0,0,0))) #white noise
plot(x1)#평균0을 중심으로 수평선타입 : 정상시계열
acf(x1) #1~30시점 acf가 다 0에 가까움(lag>=1일 때)
pacf(x1) #표본자기상관함수, 평균0인정상시계열 lag>=1다,white noise

---
  
#차분이 필요한지 알아보기
library(tseries) 
tseries::adf.test(x1)
#lag:한번차분했을 때 9개의 과거값 넣어줌
#H0:차분이 필요한비정상시계열 H1:차분X정상시계열 -> 귀무가설기각(차분X)

tseries::kpss.test(x1)
#level stationarity - H0:수평선타입의 정상시계열 H1:확률적보행을 띈 비정상시계열 (차분필요)
#귀무가설 기각할 수 없다. :수평선모양의 정상시계열일 것이다. (WN)

var(x1) #sigma^2추정량 
----


x2 <- arima.sim(n=1000,model=list(order=c(1,1,0),ar=c(0.5)))#arima(1,1,0):비정상시계열/차분차수1이상일때 order지정

plot(x2) #확률적 추세 존재(비정상)-차분이 필요하지 않을까?
acf(x2)#천천히 감소-비정상시계열
pacf(x2)#절단된형태 (한번차분이 필요한 비정상시계열)

library(tseries)
kpss.test(x2,null="Level") #null="trend"도 가능(level :수평선/trend : 증가하는 직선) 
#pvalue 작음 : 대립가설 지지 - 차분이 필요함 

kpss.test(diff(x2)) #차분해서 다시 test
#H0기각X - 차분필요X (수평선 티타입처럼 보인다.)
plot(diff(x2))

acf(diff(x2))#한번 차분한 것으로 acf 그리기
#급격히 감소 
pacf(diff(x2))

par(mfrow=c(1,2))
acf(diff(x2))
pacf(diff(x2))
#ARIMA(1,1,0)모형 (AR(1)모형이라서 1,0, 가운데1: 한번차분)
------
  
x3 <- arima.sim(n=1000,model=list(ar=c(0.5,0.3))) #AR(2)
par(mfrow=c(2,2))
plot(x3)#수평선 정상시계열
acf(x3) #지수적으로 빠르게 감소
pacf(x3) #AR(2)

plot(diff(x3))#차분  -> 차분필요하지 않은 것을 알 수 있음 
-----
x4 <- arima.sim(n=1000,model=list(ma=c(0.5,0.3))) #MA(2)
plot(x4)
acf(x4) #lag=3부터 다 0인 절단된 형태
pacf(x4)#지수적으로 빠르게 감소
#MA(2)
#AR,MA :두개다 지수적으로 감소한다고 해석하면 
#AR(4)도 가능 

----------------------------------------
#모형의 모수 추정# 
#ar(2)모형#

#Yule-walker 
?ar
ar(x3, aic=FALSE , order.max=2)  #AR(2)
#두개의 추정값 ->식추정 
ar(x3, aic=TRUE , order.max=2)
ar(x3, aic=TRUE , order.max=2)$aic #최소가되는 aic값을 각 모형에서 빼서 출력 
#0:ar(0),1:ar(1),2:ar(2)  #ar(2)일 때 aic가 최소가 됨<-0  

ar(x3, aic=TRUE , order.max=10)$aic
#10일 때 aic가 최소인 것-ar(2), (ar(2)모형도 최소가되는 모형과 값이 차이가 많이 나지 않기 때문에 ar(2)모형 선택하는 것도 가능)
----
  
?arima #ARIMA(p,d,q)
#arma -> 평균이 M 뮤 : include.mean

#CLS방법 
arima(x3,order=c(2,0,0),include.mean=TRUE, method="CSS")
#평균 뮤 M^ = intercept(평균추정량)
#추정식 : Xt-0.07=0.49(Xt-1-0.07)+0.32(Xt-2-0.07)+at
#s.e. 표준오차->추정치와 비교 (가설:beta=/0이아니다.(유의)) #사진
#ㅣtㅣ=ㅣbeta^/s.e.(beta^)ㅣ>2 -> beta는 0이 아니다 


#ML방법
arima(x3,order=c(2,0,0),include.mean=TRUE, method="ML")

#방법이 디폴트로 주어지는 것
arima(x3,order=c(2,0,0),include.mean=TRUE)


arima(x3,order=c(2,0,0),include.mean=FALSE) #M=0일 때 
#Xt= (사진)

arima(x2,order=c(1,1,0),include.mean=TRUE) #뮤추정 안하고 있음
#(Xt-X)M (사진)
#이미 차분을 한번한 데이터 x2 + 한번더 차분 =차분차수2->M추정X 
---------------------------------------
#[ARIMA 2번째 ppt]

forecast::Arima
forecast::Arima(x2,order=c(1,1,0),include.mean=TRUE,include.drift = TRUE) #drift(c)존재 
forecast::Arima(x2,order=c(1,1,0),include.mean=TRUE,include.drift = FALSE)
forecast::Arima(x2,order=c(1,1,0),include.mean=FALSE,include.drift = TRUE)

#잘 모르겠으면 알아서 해줌(constant)
forecast::Arima(x2,order=c(1,1,0),include.constant =  TRUE)

----------
#모형비교
#ar(4)모형 
forecast::Arima(x4,order=c(4,0,0),include.constant =  TRUE)
#aic=2899

#ma(2)
forecast::Arima(x4,order=c(0,0,2),include.constant =  TRUE)
#aic=2896.57

#arma(1,1)
forecast::Arima(x4,order=c(1,0,1),include.constant =  TRUE)
#aic=2918.37

#aic가 가장 작은거 ? ma(2) 모형이 가장 적절 

----------
#과다적합#
  
#선택한 모형MA(2)
ma2 <- forecast::Arima(x4,order=c(0,0,2),include.constant =  TRUE)  
ma3 <- forecast::Arima(x4,order=c(0,0,3),include.constant =  TRUE)  #m2보다 하나 과다적합한 것 (모수하나 추가)
arima2 <- forecast::Arima(x4,order=c(1,0,2),include.constant =  TRUE)
ma3 #3번째모수가 유의X (과적합할 필요X)
arima2 #ar1모수가 유의 X 
##결과 MA(2) 모형으로 충분 
#abs(coef/s.e.)<2 -> 계수가 0처럼 보임->추가된 모수가 유의하지 않다. 
---------------------------
#모형진단# - 잔차분석
  
residuals(ma2)#잔차 다 계산해줌 (1000개)
res <- residuals(ma2)
par(mfrow=c(1,1))
plot(res)
#평균이 0인 수평선타입으로 보이는가 ? (O) ~ WN
acf(res) #다 0이어야함 (O)
pacf(res) #다 0이어야함 (O)-전반적으로 0처럼 보인다. 
#잔차~white noise 라고 하기에 문제 없다 


---
             #lag=10(H0: 로우0~10까지 동시에 0인지, H1: 0이아닌값이 있는지)
Box.test(res,lag=10,type="Ljung-Box",fitdf=2)
#귀무가설 채택-> 다0이라고 볼 수 있음 ~WN라고 판단 

#한번에 하는 함수
tsdiag(ma2)
#시계열그림 
#res 그림
#box.test 결과 그림 (파란점선(=0.05)위쪽에 존재)-H0기각X 


----
forecast::checkresiduals(ma2) #룽박스테스트 결과 
#잔차로 그린 시계열그림
#lag=1~30일 때 acf 
#잔차로 그린 히스토그램 (종모양:잔차가 정규분포 따름)

---------------------
#실제 데이터 예제로 

plot(lh)
tseries::kpss.test(lh)
#수평선티타입인지 아닌지
#p-value가 큼 h0기각X (수평선이라고 생각해도 문제 없다)
#정상시계열로 판단됨
adf.test(lh)

par(mfrow=c(1,1))
acf(lh) #빠르게 감소(절단형태)
pacf(lh) #절단 #AR(1)

#ar(1),ma(1),arma(1,1) 다 가능

#세개 다 적합해보겠음
ar1 <- forecast::Arima(lh,order=c(1,0,0))
ma1 <- forecast::Arima(lh,order=c(0,0,1))
arma11 <- forecast::Arima(lh,order=c(1,0,1))

ar1
ma1
arma11
#aic기준 ar모형이 가장 작음 (bic,aicc도 마찬가지)
#->ar(1)가장 적절 


#모형진단 -잔차분석
tsdiag(ar1) #AR(1)
#잔차들이 WN라고 하기 문제 없다. 
tsdiag(ma1)
#ljung boxtest결과 - p-value가 0에 가까움 : acf가 동시에 0라고 하기 문제 있음
tsdiag(arma11)

#과다적합
#ar(1)->(ar(1)보다 조금더 복잡한 모형 ::arma(1,1),ar(2))
forecast::Arima(lh,order=c(2,0,0))
#ar2값 유의하지 않다. (ar(1)까지가 적절)
#aic기준 (ar(2)가 더 적절)
#bic기준 (ar(1)이 더 적합)

abs(-0.2128/0.1398) #<2

#결과 보고 최종선택 모형식 구하기 : 사진 



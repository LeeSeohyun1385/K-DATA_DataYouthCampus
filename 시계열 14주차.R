#[온라인강의]
AirPassengers #월별데이터
plot(AirPassengers)

#분산안정화
plot(log(AirPassengers))

#차분
plot(diff(log(AirPassengers), lag=1, differences = 1))
#추세 제거됨
#한가지 특징 보임:계절성분 아직 남아있음(반복되는 패턴)

#계절차분(선형추세,계절성분 동시에 제거 가능?)
plot(diff(log(AirPassengers), lag=12, differences = 1))
#(1-B^12)Xt=Xt-Xt-12
#눈에 띄는 추세, 반복패턴 줄어듬-> 확률적추세 존재 의심 -> 한번더 (그냥)차분해봄 

plot(diff(diff(log(AirPassengers), lag=12, differences = 1), lag=1, differences = 1))

#단위근 검정
tseries::kpss.test(diff(log(AirPassengers), lag=12, differences = 1))
#H0:level stationary vs H1: level stationary(X)-차분필요
#유의수준의 값에 따라 결과가 달라져서 adf테스트 해봄
tseries::adf.test(diff(log(AirPassengers), lag=12, differences = 1))
#H0:차분필요 비정상시계열 vs H1:차분X 정상시계열 
#---->차분하기로 결정


x<- diff(diff(log(AirPassengers), lag=12, differences = 1), lag=1, differences = 1)
#한번 더 차분할 때 계절차분 아닌 보통의 차분 
x #정상시계열

#ARMA or SARMA모형 차수결정
plot(x)
par(mfrow=c(2,1))
acf(x)   #acf(x,xlim=c(0,3))
pacf(x) #pacf(x,xlim=c(0,3))
#acf 중간 파트는 0으로 보임, 0이아닌 파트 존재 - seasonal모형
#pacf 앞부분에 0이 아닌 값 존재- 승법모형 사용
#pacf 뒷부분이 0으로 보일 경우(절단)-AR, acf 뒷부분이 0으로 보일경우(절단)- MA라고 판단 가능

#seasonal승법모형 적합
#nonseasonal part, seasonal part
arima(x, order=c(1,0,0),seasonal=list(order=c(1,0,0)))#AR(1),seasonal AR
arima(x, order=c(0,0,1),seasonal=list(order=c(0,0,1)))#MA(1),seasonal MA
arima(x, order=c(1,0,1),seasonal=list(order=c(1,0,1)))#ARMA(1.1),seasonal ARMA
#추정한 모형식(사진)

#모형진단
tsdiag(arima(x, order=c(1,0,0),seasonal=list(order=c(1,0,0))))
#lag1~10동시에 다 0처럼 보임 - WN라고 하는 것에 큰 문제가 없다. 
tsdiag(arima(x, order=c(0,0,1),seasonal=list(order=c(0,0,1))))
tsdiag(arima(x, order=c(1,0,0),seasonal=list(order=c(0,0,1))))
#non-ar,seasona-ma이런식으로 되도 된다.
tsdiag(arima(x, order=c(0,0,1),seasonal=list(order=c(1,0,0))))

#교수님 : AR모형 선호 
#aic정보함수 사용하면 -> 최소가되는 모형 선택 (여기서는 중간:해보기)


#차분안한 데이터 넣어보기

arima(log(AirPassengers),order=c(0,1,1),seasonal=list(order=c(0,1,1)))
#차분,계절차분 한번씩 해준 것 
#차분한 데이터를 x에 넣어줬을 때와 거의 비슷한 값 나옴 

arima(x,order=c(0,0,1),seasonal=list(order=c(0,0,1)),include.mean = FALSE)
arima(x,order=c(0,0,1),seasonal=list(order=c(0,0,1)))
#결과일치 (차분한거)

#교수님->차분 안한 데이터로 하는 것 선호
#예측값 고려할때 더 좋음 -예측값 차분안한 데이터 사용하기 때문에 차분한 데이터로 하면 번거로움.

#다음시간
auto.arima(log(AirPassengers))


-----------------------------
#[실시간]
#자동으로 적합해주는 함수(승법까지)
  
?forecast::auto.arima
#non차분차수:d, seasonal 차분차수:D - adf,kpss test통해 차수-> 모르면 자동 
#non-소문자 , seasonal-대문자

plot(log(AirPassengers))#월별데이터
forecast::auto.arima(log(AirPassengers),stepwise = TRUE,trace=TRUE)
#inf:aicc 
#차분차수의 합이 2 이상이면 intercept,dirft 다 0
#non먼저,seasonal 순으로 출력

forecast::auto.arima(log(AirPassengers),stepwise = FALSE,trace=TRUE) #stepwise 
#모든모델 고려 


fit<-forecast::auto.arima(log(AirPassengers),stepwise = TRUE,trace=TRUE)
fit
fit$coef
fit$sigma2#ma1의 계수의 추정량의 분산(루트해준거?)
fit$var.coef#계수에 대한 추정량의 공분산 
fit$aic
fit$bic
fit$aicc
fit$arma #차수 출력
#앞에두개 non  part- ar,ma 차수 /그 옆 두개 seasonal-ar,ma차수
#가운데-주기 #non시즈널 차분차수#시즈널 차분차수

fit$x #원데이터
fit$fitted #best model적합값
fit$residual

fit$series #데이터이름?
fit$nobs #분석한 데이터 개수


tsdiag(fit)
#수평선처럼보임
#acf-0일 때만 1, 나머지 다 0
#p-value 다 높음
#WN라고 하기 문제 없다. 

forecast::forecast(fit,h=30)
plot(forecast::forecast(fit,h=30))

-----
##시험- 데이터 불러올 때
#header=TRUE  :x가 text로 들어가면X
#=FALSE : x가 text로 들어감 
#둘다 출력해봐서 괜찮은 거 해줘야할듯


#단위근 검정 : 차분이 필요한지?
#H0채택 : 차분 , H1채택 : 차분X (선형추세)
library(tseries)
library(urca)

?adf.test #df test 확장
#alternative(대립가설 설정)-따로지정 안하면 stationary 를 디폴트로 지정
#trunc:버림한 값 #k:lag order  ##x만 입력해주면된다. 

x <- rnorm(1000) #표준정규분포 따르는 난수발생
plot.ts(x) #평균이 0인 정상시계열

adf.test(x)
#dickey-fuller : 검정통계량 t
#lag order(차수) : (n-1)^1/3
#alternative hypothesis :H1(대립가설)  #차분이 필요하지 않은 정상시계열
#p-value는 0.01보다 더 작은 값 (밑 경고메세지)


#상수없는 모형
#c
#c+bt -> 이것만에 대한것 :  adf test 

--------------

#변환된 모형 (모형선택)#
?ur.df
#type= 어떤모형을 설정할 것? none(c,b가 0인모형), drift(c만포함된 모형), trend(c+bt)-> 설정안하면 trend
#lags=(차수)과거의 값을 얼마나? 설정안하면: (n-1)^1/3 
#k 선택할 때 fixed:lag만큼 모두다 넣어서,AIC:10개의 경우에서 AIC가 최소가 되는 경우 선택  

plot.ts(x) #그림 그려보고 세 모형중 어떤것인지 판단 

m <- trunc((length(x)-1)^(1/3))
ur.df(x, type="trend",lags=m , selectlags="Fixed")
#adf test와 같은결과
#제일 첫번째 나오는 검정통계량만 고려하면 된다. 

df.res <- ur.df(x, type="trend",lags=m , selectlags="Fixed")
df.res@lags
df.res@cval #유의수준1%,5%,10%임계값(Calpha)-(tau주의깊게)
df.res@teststat # 검정통계량(tau에 관심) : 검정통계량<임계값 :차분필요 x
df.res@testreg
#1+tt :모형 c+bt (trend가 고려되어있는 모형에서 회귀분석)

#AIC선택했을 때
df.res1 <- ur.df(x,type="trend", lags=m , selectlags= "AIC")
df.res1@lags
df.res1@testreg
df.res1@teststat #검정통계량
df.res1@cval #임계값  ->> 검정통계량과 임계값 비교하여 검정 

#모형변경
df.res2 <- ur.df(x, type="drift", lags=m , selectlags= "AIC")
df.res2@testreg
df.res2@teststat
df.res2@cval 
#모형이 달라지니 다른분포, 다른값들 가진다. 
#차분이 필요 없는 정상시계열

df.res3 <- ur.df(x, type="none", lags=m , selectlags= "AIC")
df.res3@testreg
df.res3@teststat
df.res3@cval
#대립가설 지지 ; 검정통계량 < 임계값


y <- 10+x
df.res4 <- ur.df(y, type="none", lags=m , selectlags= "AIC")
df.res4@teststat #10더하기 전과 달라짐 
df.res4@cval #10더하기 전과 같음
#검정통계량 > 임계값 : H0기각x ->> 차분필요 


----
#시계열 그림 보고 파악 
par(mfrow=c(1,2))
plot.ts(x) #평균이 0인 경우에만 정상시계열(평균이 0가 아닐 때는 차분필요)#none
plot.ts(y) #평균이 0이아닌 m일 때 정상시계열#drift

df.res5 <- ur.df(y, type="drift", lags=m , selectlags= "AIC")
df.res5@teststat
df.res5@cval 


z<-x+0.1*(1:length(x))
plot.ts(x) #차분필요 
plot.ts(z) #비정상 시계열 (추세추정하여 제거하는 방법: 차분필요 X)

df.res6 <- ur.df(z,type="drift",lags=m,selectlags = "AIC")
df.res6@teststat
df.res6@cval 
#귀무가설 기각X -> 차분필요

#이 방법 선택 
df.res6 <- ur.df(z,type="trend",lags=m,selectlags = "AIC")
df.res6@teststat
df.res6@cval
#귀무가설 기각 대립가설 채택 -> 차분필요X 

##어디까지 차분의 필요성에 따라서 모형 설정해주면 된다.

#pp.test는 비슷해서 돌려보기


?kpss.test
kpss.test(x, null="Level")
#KPSS Level =검정통계량
#가운데 결과값은 해석안해도됨 

kpss.test(y, null="Level")
#수평선모형이다.
kpss.test(z, null="Level")
#귀무가설 기각 ->수평선모형 아니다.
kpss.test(z, null="Trend")
#trend stationary하다. #여기 해석 다시 듣기 

###
pp.test(z)
#z(aplha)= 검정통계량 
#유의수준 0.01,0.05에서 귀무가설 기각 _ 차분이 필요하지 않은 정상시계열 

###
?ur.ers
#adf-gls 검정
ers.res <- ur.ers(z,type="DF-GLS",model="constant")
ers.res@teststat
ers.res@cval
#귀무가설 기각 X :차분필요 

ers.res1 <- ur.ers(z,type="DF-GLS",model="trend")
ers.res1@teststat
ers.res1@cval
#귀무가설 기각 : 차분 X
#같은 데이터도 모델을 어떤거로 하냐에 따라서 검정통계가 다름 



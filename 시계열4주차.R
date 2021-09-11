plot(co2)
#이동평균
m<-12
lines(ma(co2, order=m),col="blue") #중심화 이동평균

#별로인 예제
m <- 5
lines(ma(co2, order=m),col="red")
#원래 시계열과 큰 차이가 없음(이동평균기간 잘 선택해줘야함)- 주기의 배수 선택

m <-24   #주기의 배수
lines(ma(co2, order=m),col="green")

#데이터의 성질이 제일 작은 것 선택
head(ma(co2,order=24),30)
head(ma(co2,order=12),30) #데이터의 손실 개수가 12개 (이게 더 좋음)-손실이 더 적게 발생 
#이동평균의 길이 : 주기로하는 것이 제일 좋음 
--------------------------------------
#추세추정(추세모형)- 회귀분석기법
trd <- 1:length(co2)
lm(co2~trd)

abline(lm(co2~trd),col="red") #x개수 다 그래프에 안나타남
plot(trd,co2,type="l")
abline(lm(co2~trd),col="red")
----------------------------------
#실시간 
library(TTR)
library(forecast)
plot(co2)

#분해법
#추세성분->추세조정->계절~ (이과정 찾기)
#x:time series(2이상의 주기를 가져야 함수사용가능)
#additive:가법모형(+) , multiplicative:승법모형 (곱하기로 연결)
dec.co2 <- decompose(co2, type="additive")
#추세(값)가 커질 수록 진폭(분산)이 커지면 가법모형으로 설명 불가능 (승법으로 설명)
#일정하면: 가법모형 ,승법모형에서 log취해주면 일정해져 가법모형사용가능 
dec.co2
#$x:co2데이터 출력(original data), $seasonal:1년을 주기로한 계절성분(월별로 동일한 값)
#$figure:계절성분에 해당하는 핵심 12개 값, trend:중심화이동평균 구하는 것 

plot(dec.co2)  #observed: original series #불규칙성분(random) : 오리지날-추세+계절(반복되는 패턴)

-----------------------------
#1)추세성분추정  2)추세조정 
#중심화 이동평균(decompose함수로도 가능)
co2.tr <- ma(co2,order=12) #추세성분
dtr <- co2-co2.tr #추세조정(제거)
dtr
#왜 설명 했느냐 : 회귀분석기법으로 추세성분 구해줄 때 이 방법 사용
 
#추세모형 (회귀분석기법)
tt  <- 1:length(co2)
co2.tr <- lm(co2~tt)$fitted
dtr <- co2-co2.tr
dtr
head(co2.tr)
head(dtr) #추세 제거된(추세 제거하려면 decompose아닌 이 과정 거쳐야함)


#3)계절성분 추정 
seasonal.co2 <- rep(NA,12) #12개 라고 백터 지정 

seasonal.co2[1] <- mean(dtr[tt%%12==1]) #tt를 12로 나눴을 때 나머지가 1 ->1월  
seasonal.co2[1]                         #dtr :추세제거된시계열
#1월의 평균
seasonal.co2

#반복
seasonal.co2 <- rep(NA,12)
for (i in 1:12){
  if(i<12) seasonal.co2[i] <- mean(dtr[tt%%12==i])
  if(i==12) seasonal.co2[i] <- mean(dtr[tt%%12==0])
}
seasonal.co2  #1월~12월의 평균 : Sj- 

#다시 평균 구해서 편차를 구해줌 #S--:한번더 평균 
seasonal.co2 - mean(seasonal.co2)   #St^ :계절성분값  
#이제 계절성분의 값들을 보면 됨 

--------
#다시 decompose

dec.co2$x
dec.co2$trend
dec.co2$seasonal  #계절성분만 구해줌(출력)

sa.co2 <- dec.co2$x - dec.co2$seasonal #계절조정해준 co2

#그림
par(mfrow=c(1,2))
plot(co2)
plot(sa.co2)

-----
##예측-분해법##
#계절조정된 시계열로 추세성분 추정 예측 (회귀분석기법)
#계절성분에 대한 예측값도 구하기 (방법?)

sa.co2 <- dec.co2$x - dec.co2$seasonal #계절조정된 시계열 
tt  <- 1:length(co2)


reg <- lm(sa.co2~tt) #계절조정된 데이터 회귀분석 
reg

tt #마지막 나온 숫자가 468 = 자료 개수 
tail(co2)
predict(reg, newdata=data.frame(tt=c(469,470)))  #Tt^
#추세성분에 대한 예측값 
#설명 사진 찍어놈

dec.co2$figure  #계절성분 핵심 12개 값  #st^
  
#xt^ = Tt^ + st^  (1998.1날짜에 대한 예측값)
------

#HoltWinters #단순지수평활 #선형지수평활, #계절지수평활
#노란색에 필기
  
HoltWinters(co2,beta=FALSE,gamma=FALSE) #단순지수평활
HoltWinters(co2,gamma=FALSE) #선형지수평활
HoltWinters(co2,seasonal="additive")# 계절지수평활(가법모형)
#지정안한 alpha,beta,gamma자동으로 계산

###그림
par(mfrow=c(1,1))
plot(co2)
#이 다음 HoltWinters위 세개 돌려줌 
###

#예측
HoltWinters(co2,beta=FALSE,gamma=FALSE) 
#confficients:미래시점에는 363.3399를 예측값으로 사용해준다는 뜻 

hw<- HoltWinters(co2,beta=FALSE,gamma=FALSE)
forecast(hw)  #예측값 ,예측구간


HoltWinters(co2,gamma=FALSE)
#a:마지막시점 값
#b:기울기
#364.34+1.85*1 ~ 강의다시듣기 
hw<- HoltWinters(co2,gamma=FALSE)
forecast(hw)

HoltWinters(co2,seasonal="additive")  #Fn(l)모형의 계수들??? 
#s1~s12:계절성분 추정값 , a=y절편(Ln) , a+b*1+s: Ln+Tn*l+Sn+l-L
hw <- HoltWinters(co2,seasonal="additive")
forecast(hw)

#그림
par(mfrow=c(1,1))
plot(co2)
hw<- HoltWinters(co2,beta=FALSE,gamma=FALSE)
plot(forecast(hw))
hw<- HoltWinters(co2,gamma=FALSE)
plot(forecast(hw))
hw <- HoltWinters(co2,seasonal="additive")
plot(forecast(hw))



---------------------------
#시계열6주차
  #정상성과 인과성 만족하는가
  #해구하기
polyroot(c(1,-1,0.25))
abs(polyroot(c(1,-1,0.25))) #해의 크기 

#해의 크기=2 >1 :인과성O,정상성O
#해의 크기<1 : 단위원 안에 있음 => 정상성애매, 인과성만족하지 못함  
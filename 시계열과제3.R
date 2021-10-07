soju <- read.table("C://Users//이서현//Desktop//2021-1학기//soju.txt",header=FALSE)
soju.ts <- ts(soju[,2], frequency=4, start=c(1965,01),end=c(2021,02))
plot(soju.ts)

#분해법
##분해과정
#추세성분추정->추세조정->계절성분추정->불규칙성분 추정
#분석순서설명: 주어진 시계열을 여러개의 성분으로 분해하고 성분별로 미래의 값을 추정하고 시계열의 값을 예측한다. 


#시계열자료 분해 

#additive:가법모형(+) , multiplicative:승법모형 (곱하기로 연결)
dec.so <- decompose(soju.ts, type="additive")
#가법모형을 이용하여 더하기로 연결한 분해법을 이용하여 3개의 성분으로 분해시킨 그림
#원데이터,추세성분,계절성분,불규칙성분

dec.so
#$x:시계열 자료의 원데이터 출력(original data), $seasonal:4분기를 주기로한 계절성분(분기별로 동일한 값)
#$figure:계절성분에 해당하는 핵심 4개 값, trend:중심화이동평균 구하는 것 
dec.so$x
dec.so$seasonal
dec.so$figure
dec.so$random

plot(dec.so)
#그림의 위에서부터 보면 원 데이터, 추세성분, 계절성분, 불규칙성분이 순서대로 표현된다. 
#불규칙성분(random) : 오리지날-추세+계절(반복되는 패턴)

--------------

#추세성분추정 : 회귀분석기법을 통해 추세추정 

#계절성분추정 : 추세조정된 시계열을 이용
#불규칙성분추정 
#-->여기까지 이용하여 예측 
-------------------------
  
#예측: 시계열분해 -> 계절조정 -> 추세성분예측 -> 계절성분예측 -> 시계열예측 

#시계열분해
dec.so$x
dec.so$trend
dec.so$seasonal
#계절조정
sa.so <- dec.so$x - dec.so$seasonal #계절조정된 시계열 

#추세성분예측
#계절조정된 시계열로 추세성분 추정 예측 (회귀분석기법)
reg <- lm(sa.so~tt)
reg

tt #마지막 나온 숫자가 226 = 자료 개수 
tail(soju.ts)
predict(reg, newdata=data.frame(tt=c(227:246)))  #마지막시점에서 20개 예측 
#trend에 대한 예측값 
#설명 사진 찍어놈

dec.so$figure  #계절성분
# 예측값 - 계절성분 = 원자료 (Tt^+St^=xt^)

#계절성분예측(?)
reg2 <- lm(dec.so$seasonal~tt)
predict(reg2, newdata=data.frame(tt=c(227:246)))

-----------------------------------

#단순지수평활 
plot(soju.ts)

simple <- HoltWinters(soju.ts,beta=FALSE,gamma=FALSE)
lines(simple$fitted[,-1],col="red") #level, xhat
#원데이터와 비슷한 형태로 평활이 아직 잘 이루어졌다고 말할 수 없다. 
forecast(simple)
predict.sim <- predict(simple,20,prediction.interval = TRUE)
plot(simple,predict.sim)
#일정한 값으로 예측된다고 설명할 수 있다. 


#다른 예측결과를 볼 수 있는지 확인하기위해 선형지수평활을 해본다.
library(forecast)
linear <- HoltWinters(soju.ts,gamma=FALSE)
forecast(linear)
#그림그려주기위해 예측값의 출력형태 변형시킴 
predict.line <- predict(linear,20,prediction.interval = TRUE) #20개의 미래시점 예측값 
predict.line
plot(linear,predict.line)
#예측값: 증가하는 추세 : 계절지수평활 

#계절지수평활 예측
season <- HoltWinters(soju.ts,seasonal="additive")
forecast(season)
predict.season <- predict(season,20,prediction.interval = TRUE) 
predict.season 
plot(season,predict.season)


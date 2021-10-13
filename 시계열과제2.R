
soju <- read.table("C://Users//이서현//Desktop//2021-1학기//soju.txt",header=FALSE)
head(soju)
tail(soju)
soju.ts <- ts(soju[,2], frequency=4, start=c(1965,01),end=c(2021,02))
head(soju.ts)
plot(soju.ts)
#단순이동평균(기간:4)
sma.4 <- SMA(soju.ts,n=4)
sma.4
#4번째 부터 시작함

#짝수 차수에 대한 중심화 이동평균
#center=TRUE,  대칭적이려면 중심화 이동평균 
#n:과거자료의 적정개수, 일반적으로 시계열 자료에 뚜렷한 추세가 나타나 있거나 불규칙변동이 심하지 않은 경우에는 작은 n의 개수를 사용
#이동평균의 기간이 짝수인 경우 인접한 두 이동평균의 평균을 계산하여 이동평균을 중심화 하는 중심이동평균을 구한다. 

lines(sma.4,col="red")
#전체적으로 증가하는 추세

-----

#중심화이동평균 (왼쪽으로 살짝 미뤄짐 확인)
ma.cen <- ma(soju.ts,order=4,centre=TRUE)
ma.cen
#앞에 2개 뒤에 2개 띄어짐

lines(ma.cen,col="blue")
#파: 살짝 왼쪽으로 있고
#빨: 살짝 오른쪽으로 치우쳐져 있다.


#보통 단순이동 평균법에서는 m을 한 기간 후의 예측오차의 제곱을 평균을 최소로 하는 값으로 설정
#중심화 이동평균을 사용하여 추세분석을 할 때 m을 계절성분의 주기로 설정
#n값 설정
m.vec <- 5:20  #후보 설정
sma.mse.vec <- c()  #저장장소 설정

for(m in m.vec){
  sma.m <- c(NA, SMA(soju.ts, n=m))  #한 시점 늘려줌(하나를 미뤄놈): NA
  end <- length(sma.m)
  sma.mse <- mean((soju.ts-sma.m[-end])^2, na.rm=TRUE) #원스텝 와이드 프로캐스트 에러^2의 평균#길이가 같게하여(-end)
  sma.mse.vec <- c(sma.mse.vec, sma.mse)
}

which.min(sma.mse.vec) #최소가되는 값 찾기
m.vec[which.min(sma.mse.vec)]

###안해도됨

#lowess이용하여 분석 (국소회귀)

plot(soju.ts)
trd<- 1:length(soju.ts)  #독립변수 설정
lowess(trd,soju.ts, f=2/3) #y종속변수

lines(lowess(soju.ts,f=2/3), col="red")
lines(lowess(soju.ts,f=0.1), col="blue")
#f(윈도우사이즈)크게하면 평활이 잘 되어서 평평해짐을 확인(작으면 덜 됨)

------
#단순이동평균을 이용한 예측
  
#단순이동평균 내용 추가
  
n <- length(soju.ts)
int <- 1:4 #예측하고자하는 길이
xhat1 <- sma.4[n]+0*int
xhat1
#주기가 4분기 이므로 4개의 값이 주어지고 마지막값으로 부터 20시점뒤도 44.52475로 동일하게 예측가능하다.
#평행선
int <- 1:20
xhat2 <- sma.4[n]+0*int
xhat2

#단순지수평활을 이용한 예측
#평활상수 w의 값은 한기간후 예측오차의 제곱의 평균을 최소로 하는 w를 선택함

es.1 <- HoltWinters(soju.ts, alpha=0.1, beta=FALSE, gamma=FALSE) 
lines(es.1$fitted[,-1],col="red")

forecast(es.1)   #예측값 
plot(forecast(es.1))

#20시점 후 
es.1.pred <- predict(es.1,20,prediction.interval = TRUE)
es.1.pred #예측값

plot(es.1,es.1.pred)
#한 값으로 일정 

es.2 <- HoltWinters(soju.ts, alpha=0.8, beta=FALSE, gamma=FALSE) 
lines(es.2$fitted[,-1],col="red")


#20시점 후 
es.2.pred <- predict(es.2,20)
es.2.pred #예측값

plot(es.2,es.2.pred)
--- #이 패턴 alpha만 변경하며 돌려주면 0.8이 가장 적절 
  
-----------
#선형예측
#시계열 데이터가 증가하는 추세패턴을 따르므로 이중이동평균법을 통한 예측

sma.4 <- SMA(soju.ts, n=4)
sma.4.2 <- SMA(sma.4,n=4)
sma.4.2
lines(sma.4,col="red")
lines(sma.4.2,col="green")
#오른쪽으로 좀더 미뤄짐

n <- length(soju.ts)
int <- 1:4 #예측하고자하는 길이
xhat2 <- sma.4.2[n]+0*int
xhat2


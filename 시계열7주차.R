?arima.sim  #전체 데이터 생성 
#model:어떤모형(AR모형, MA모형)/n 입력

#AR(1) : Xt=0.5Xt-1+at
arima.sim(model=list(ar=c(0.5)),n=100)
#AR(1)이라 계수 하나 입력 , AR(2)부터는 두개 이상 (c()로 입력)

x<-arima.sim(model=list(ar=c(0.5), sd=2),n=100)
#sb: 시그마값(표준편차값)추가해서 돌려줌 
x

#MA(1)
#Xt=at+0.3at-1
y <- arima.sim(model=list(ma=c(0.3),sd=1),n=100)
y

#ARMA(1,1)
#Xt=0.5Xt-1+at+0.3at-1
#ar 계수:0.5 ,ma계수:0.3, 시그마값
y <- arima.sim(model=list(ar=c(0.5),ma=c(0.3),sd=1),n=100)
y

arima.sim(model=list(ar=1),n=100)
#이 함수 돌아가지 않음 ar계수가 1일 때 비정상시계열 : 안돌아감

#ar계수가 1인 경우 : 모형 바꿔줌 
#Xt=Xt-1+at 
arima.sim(model=list(order=c(0,1,0)),n=10)
#order:차수  :결과값 0시작 : 0빼고 데이터로 생각해도 된다. 
 
#ARMA(1,1) : order=c(1,0,1)
#order은 다음주부터 

#Xt=0.5Xt-1+at-0.5at-1
arima.sim(model=list(ar=c(0.5),ma=c(-0.5)),n=10)

----------------------------------
#arima
#중간고사 이후 

  
#plot
plot(x) #시계열 그림 (x:시간 y:시계열 데이터)

#데이터 100개 ,평균0 ,표준편차1
y<-rnorm(100, 0, 1)
plot(y)
plot(y,type="l") #type을 선형그래프로 표현
plot.ts(y) #시계열그림 
#평균과 진폭이 일정한가? yes-> 정상시계열

--------------------
#x:시계열데이터입력
#표본평균
mean(x)

#표본자기공분산함수  
acf(x,type="covariance") 

#표본자기상관함수:acf
acf(x, type="correlation")
#lag: 시차 (로우p0^,p1^....),0부터
#급격히 0으로 감소하는 형태 

#pacf:부분자기상관함수
acf(x,type="partial")

#시차1부터 시작 
#절단된 형태 : pie11만 영이아니고 pie22부터 다 0 
#즉 AR(1)모형에 적합하지 않을까 생각할 수 있음   



#표본부분자기상관함수:SACF 
pacf(x)     #partial 대신 이 함수 사용가능


#false:그림 그리지 않고 숫자 출력 
acf(x,type="correlation",plot=FALSE)
acf(x,type="covariance",plot=FALSE)
pacf(x,plot=FALSE) 



#AR(1)모형의 ARMAacf <- ARMA모형에서의 acf 
ARMAacf(ar=c(0.5),ma=c(0),lag.max=5,pacf=FALSE) #acf 계산 
ARMAacf(ar=c(0.5),ma=c(0),lag.max=5,pacf=TRUE) #pacf계산
#절단된 형태: 두개빼고 다0
#lag.max :시차 개수를 몇개로 해줄 것인지 

ARMAacf(ar=c(0),ma=c(0.3),lag.max=5,pacf=FALSE) #절단된형태
ARMAacf(ar=c(0),ma=c(0.3),lag.max=5,pacf=TRUE) #지속적으로 0으로 빠르게 감소
#ar특징, ma특징 알 수 있음 : 지속감소, 절단

#그림
a<-ARMAacf(ar=c(0.5),ma=c(0),lag.max=5,pacf=FALSE)
plot(a,type="h")
plot(a,type="h",ylim=c(-1,1))#축 범위 조정 
abline(h=0,col="blue",lty=2) #0일때 수평선 그려줌
#0부터시작 (그래프에선 1인데 그냥 자릿수 표현한 것)
#acf 0부터시작 pacf 1부터시작

-----------------------------------
  
#AR모형을 MA모형으로 바꿔줌:ARMAtoMA
ARMAtoMA(ar=c(0.5),ma=c(0),lag.max=10)



acf2AR #acf 를 이용하여 ar 계수를 찾는것 

Acf <- ARMAacf(c(0.6,0.3,-0.2))  #ar(3)모형 
#지속적으로 0으로 수렴하는 형태 
Acf   #자기상관함수 -> 3차까지 들어있음 :AR(3)모형 까지 찾아주는 것 
acf2AR(Acf)
#위의 자기상관함수가 되는 AR계수를 찾아줌
#ar(1)  : AR(1)모형 중에서 Acf와 계수가 똑같이 되는 것을 찾음
#ar(2) :AR(2)에서  

#이론적인경우에만 적용 -> 실제에선 사용 안함 
#pacf 표본부분자기상관함수 
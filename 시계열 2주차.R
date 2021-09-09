gdp <- read.table("C:/Users/이서현/Desktop/2021-1학기/gdp.txt",sep="",header=FALSE) #sep:공백구분하여 불러오기  
head(gdp)


gdp.ts <- ts(data=gdp[,3], frequency=4, start=c(1960,2)) #ts:시계열 데이터로 변환
#시작시점 숫자형태로 입력
#frequency: 1년을 기준으로 데이터를 몇번 관찰 가능? 분기별 4, 월별 12
gdp.ts
#Qtr1-1사분기


is.ts(data) #타임시리즈 데이터 인가?
is.data.frame(gdp) # 데이터 프레임 인가?
dim(gdp.ts) #크기는 무엇인가?



par(mfrow=c(1,1)) #할 때만 하는 것
plot(gdp.ts) #이 함수만 그려주면 됨

-------------------------------
# trend analysis #
#시계열 그림
JJ <- JohnsonJohnson
JJ
plot(JJ) #전체적으로 증가하는 추세, 곡선의 형태로 증가, 평균함수 - 이차곡선 비슷 
#뒤로갈수록 진폭이 늘어남 : 시간의 흐름에 따라 변화폭이 크다 = 분산함수가 시간에 따라 점점 증가
#반복되는 패턴 : 계절성분 (1년이 주기)


#time value로 바꿔주기 
tt<- 1:length(JJ) #1부터 time에 해당하는 변수를 만들어줌 
tt #독립변수 
plot(tt,JJ) #JJ:종속변수

#회귀분석 
#1차식
reg1<-lm(JJ~tt)
summary(reg1)
abline(reg1,col="red") #앞 그림에 회귀분석 결과 덧붙여서 그려줌

#2차식 : 다항회귀분석 비슷 (직선으로는 설명 부족) 
reg2<-lm(JJ~tt+I(tt^2)) #tt제곱한 값 독립변수로 찾아 넣어줌 
summary(reg2)
lines(reg2$fitted.values, col="blue")
          #적합값
#3차항도 해줄 수 있음 (이 데이터의 경우 2차식으로 충분)

-------------------------------------------------
#로그변환 : 분산이 뒤로갈수록 커져서 변환해서 분석해봄 
logJJ <- log(JJ)
plot(logJJ)  #분산일정해짐 
plot(tt,logJJ)

reg3 <- lm(logJJ~tt)
summary(reg3)

abline(reg3, col="green")
lines(exp(reg3$fitted.values), col="green") #이렇게 변형하여 그려줌 
 #그림들 중에 무엇이 가장 적합한지 비교 ? 초록 , 파랑 , 빨강X
 
--------------------------------
  
#가변수 (질적변수)
Q1 <- (tt%%4)==1 #1사분기만 1 나머지 0로 지정  (자료에서 4로 나눠 나머지가 1: 1사분기)
Q2 <- (tt%%4)==2  #2사분기
Q3 <- (tt%%4)==3  #3사분기 
#4사분기 지정하지 않음 (3개로 지정) : 3개 모두 아닌 것이 4사분기      
#질적변수 : 하나적은 개수로 만들어줌 


reg4 <- lm(JJ~tt+I(tt^2)+Q1+Q2+Q3)
summary(reg4)
lines(reg4$fitted.values,col="purple")


reg5 <- lm(logJJ~tt+Q1+Q2+Q3)
summary(reg5)
lines(exp(reg5$fitted.values),col="pink")
#추세 추정 가능 
lines(reg5$fitted.values,col="pink")  #내생각에는 로그에 exp안하는 거 같음 





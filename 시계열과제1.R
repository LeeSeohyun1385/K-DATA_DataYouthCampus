soju <- read.table("C://Users//이서현//Desktop//2021-1학기//soju.txt",header=FALSE)
soju

head(soju)

#분기별 소주 소비자 물가를 시계열 데이터로 변환  
soju.ts <- ts(data=soju[,2], frequency=4, start=c(1975,01))
soju.ts



plot(soju.ts)
#시계열 그림

#설명변수
time <- 1:length(soju.ts)
time

#종속변수 
so <- soju.ts
so

#종속변수와 설명변수 그림 그려보기
plot(time,so)

#회귀분석
#1차식
reg1 <- lm(so~time)
summary(reg1)
#회귀모형
o.787710 + 0.212184time

abline(reg1,col="red")
#설명가능 


#2차식
reg2 <- lm(so~I(time^2))
summary(reg2)
lines(reg2$fitted.values,col="blue")
#설명 어려움(적합하지 않음)

#로그변환
logS <- log(so)
plot(logS)
plot(time,logS)
reg3 <- lm(logS~time)
summary(reg3)
lines(reg3$fitted.values, col="green")
#적합X

#가변수
Q1 <- (time%%4)==1
Q2 <- (time%%4)==2
Q3 <- (time%%4)==3
reg4 <- lm(so~time+I(time^2)+Q1+Q2+Q3)
lines(reg4$fitted.values,col="purple")
#설명가능 적합  

reg5 <- lm(logJJ~tt+Q1+Q2+Q3)
summary(reg5)
lines(exp(reg5$fitted.values),col="pink")
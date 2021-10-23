
>M0<-lm(mpg~1,data=mtcars)
>M0

> summary(M0)


 reg1 <- lm(mpg~disp,data=mtcars)
 reg2 <- lm(mpg~hp,data=mtcars)
 reg3 <- lm(mpg~wt,data=mtcars)
 reg4 <- lm(mpg~qsec,data=mtcars)
 summary(reg1) #결정계수- R-squared
 summary(reg2)
summary(reg3)
summary(reg4)

names(summary(reg1)) #r.squared
reg1 <- summary(lm(mpg~disp,data=mtcars))$r.squared
reg2 <- summary(lm(mpg~hp,data=mtcars))$r.squared
reg3 <- summary(lm(mpg~wt,data=mtcars))$r.squared
reg4 <- summary(lm(mpg~qsec,data=mtcars))$r.squared
c(reg1,reg2,reg3,reg4) #결정계수만 모아보기 
 
M1<-lm(mpg~wt,data=mtcars)
reg11 <- summary(lm(mpg~wt+disp,data=mtcars))$r.squared
reg12 <- summary(lm(mpg~wt+hp,data=mtcars))$r.squared
reg13 <- summary(lm(mpg~wt+qsec,data=mtcars))$r.squared
c(reg11,reg12,reg13)
 
M2<-lm(mpg~wt+hp,data=mtcars)
reg21 <- summary(lm(mpg~wt+hp+disp,data=mtcars))$r.squared
reg22 <- summary(lm(mpg~wt+hp+qsec,data=mtcars))$r.squared
c(reg21,reg22)

M3<-lm(mpg~wt+hp+qsec,data=mtcars)
M4<-lm(mpg~wt+hp+qsec+disp,data=mtcars)
#변수를 하나씩 추가하는 것이 전진선택법에서 결정계수를 사용한 방법(모형선택)

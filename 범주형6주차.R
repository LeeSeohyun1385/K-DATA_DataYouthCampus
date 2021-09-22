
# 예제: 암참게와 부수체에 관한 연구

Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", 
					  header = TRUE)

str(Crabs) #7개변수 

plot(y ~ width, xlab = "Width", ylab = "Number of satellites", data = Crabs) 
#width에 따라서 y가 새틀라이트를 가지고 있는지


plot(jitter(y, 0.08) ~ width, xlab = "Width", ylab = "Number of satellites", data = Crabs) 
#0.08씩 빼져서 표현되는 것 


#로지스틱회귀모형
fit <- glm(y ~ width, family = binomial, data = Crabs)
                       #이항분포를 따름
summary(fit)

coef(fit)
coef(fit)[1]  #alpha
coef(fit)[2]  #beta

curve(predict(fit, data.frame(width = x), type = "resp"), add = TRUE)
#평균 26즈음 :0.5보다 큼 ->부수체가질 확률 높다?
#mean(Crabs$width)#너비의 표본평균

summary(fit)
#alpha : intercept , beta: width
#Estimate:추정량 , Std.Error:표준오차 , z value:왈드 검정통계량


--------#ppt내용 

  
  
  
# profile likelihood confidence interval(가능도함수를 이용한 신뢰구간)
confint(fit) 
exp(0.308)   #지수함수를 취해줌 
exp(0.709)
#오즈비 (약 2배만큼)

#B에 대한 왈드 신뢰구간 (confit함수 쓴 것과 동일)
coef(fit)[2]-qnorm(0.975)*0.1017
#B^ + qnorm:분위수 (0.025) *SE (fit결과값에 std.Error (표준오차))
coef(fit)[2]+qnorm(0.975)*0.1017 #오즈비를 통해서 (강의 다시듣기)
#신뢰구간 하한, 상한 값 : 여기에 지수 취해서 가능도 신뢰구간

-----------------

# likelihood ratio test :가능도비검정
install.packages("car")
library(car)

Anova(fit) #LR Chisq :가능도비검정통계량 
--
-coef(fit)[1]/coef(fit)[2] #중간효율수준

-------------------------
  #여기서 부터 잘 모르겠음(맨밑으로 넘어가기)  
  
  
# confidence interval for estimated probability :추정된 확률에 대한 신뢰구간 
#신뢰구간 
plot(jitter(y, 0.1) ~ width, xlim = c(18, 34), pch = 16, 
     ylab = "Prob(satelite)", data = Crabs)

#여기서부터
plot.data <- data.frame(width = 33.5)
plot.data

lp <- predict(fit, newdata = plot.data, se.fit = TRUE)
lp #로짓값

--
  ##추정된 확률(=24.8,26.7)에 대한 신뢰구간
test.data <- data.frame(width=c(24.8,26.7))
test.lp <- predict(fit, newdata=test.data, se.fit = TRUE)
test.lp
rst <- exp(test.lp$fit)/(1 + exp(test.lp$fit)) #성공확률 신뢰구간(로지스틱 회귀모형의 추정값)
#pie^(x)

coef(fit)[2] * rst 
--

#역변환
pred.prob <- exp(lp$fit) / (1 + exp(lp$fit))
pred.prob #예측값
               #1.96 
LB <- lp$fit - qnorm(0.975) * lp$se.fit #se.fit: standard error값을 사용 

UB <- lp$fit + qnorm(0.975) * lp$se.fit

LB.p <- exp(LB) / (1 + exp(LB)) #역변환 

UB.p <- exp(UB) / (1 + exp(UB))

lines(18:34, pred.prob)

lines(18:34, LB.p, col = "red")

lines(18:34, UB.p, col = "blue") #신뢰구간 파악 가능 
#신뢰구간의 거리가 좁다는 것은 더 정확하다???



----------------------
#pie^(33.5)-넓이가 정해졌을 때 예측확률값 구하기 
plot.data <- data.frame(width = 33.5)
plot.data

lp <- predict(fit, newdata = plot.data, se.fit = TRUE)
lp #로짓값

pred.prob <- exp(lp$fit) / (1 + exp(lp$fit))
pred.prob #예측값
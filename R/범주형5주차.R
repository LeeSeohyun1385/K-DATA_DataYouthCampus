
# 예제: 암참게와 부수체에 관한 연구 (p. 98)

Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", 
					  header = TRUE)
Crabs
#sat:부수체 수 , width:암참게등딱지너비
str(Crabs)

#반응변수~설명변수 
plot(sat ~ width, xlab = "Width", ylab = "Number of satellites", data = Crabs) 
#부수체수, 넓이 그림그려본 것 
---------------------
  
  
######모형적합 (포아송분포)
                          #family=분포(link=연결함수)
fit <- glm(sat ~ width, family = poisson(link = log), data = Crabs)

summary(fit)

#너비가 26.3일 때의 적합값(M^)
fit$fitted[26.3]
--------
  
install.packages("gam")

library(gam)

gam.fit <- gam(sat ~ s(width), family = poisson, data = Crabs)  #s:smoothing

curve(predict(gam.fit, data.frame(width = x), type = "resp"), add = TRUE)


# 연습문제 3.13(b) #암참게 평균무게 2.44일 때 적합값 
exp(-0.42841 + 0.58930*(2.44))
#(c): x(무게)가 한단위 증가할 때 발생하는 승법효과 
exp(-0.42841 + 0.58930*(2.44+1))
# 한단위 증가할 때 exp(0.589)배 증가
exp(0.58930) #exp(B)  #승법효과 

-------------------
  # likelihood ratio test :가능도비검정
  install.packages("car")
library(car)

Anova(fit) #LR Chisq :가능도비검정통계량 

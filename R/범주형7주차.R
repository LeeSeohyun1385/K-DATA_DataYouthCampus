# 3장 연습문제 3.3 : 선형확률모형, 상대위험도
#표2.7
no <- c(17066, 14464, 788, 126, 37)
yes <- c(48, 38, 5, 1, 1)
score <- c(0, 0.5, 1.5, 4.0, 7.0)

#ML적합결과
                #yes/합계     #설명변수:score
fit <- glm(yes / (yes + no) ~ score, 
           family = quasi(link = identity, variance = "mu(1-mu)"),
           weights = (yes + no))

summary(fit)
#예측식, 절편, 기울기

predict(fit) #예측함수
#상대위험도=0.01016/0.002547  =pie1^/pie2^

#c. 점수를 0,1,2,3,4로 주어 적합
score <- c(0, 1.0, 2.0, 3.0, 4.0)

fit <- glm(yes / (yes + no) ~ score, 
           family = quasi(link = identity, variance = "mu(1-mu)"),
           weights = (yes + no))

summary(fit)

predict(fit)
#상대위험도 = 0.0046/0.0026
#score를 어떤 점수를 주는가에 따라 달라짐 <- 상대위험도 
---------------------------------------------------------


# 예제: 마리화나 사용에 대한 조사 

Marijuana <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Marijuana.dat", 
					header = TRUE)

str(Marijuana)

Marijuana

#X: gender(성별), Z: race(인종)
      #항상 절편은 포함되니까 절편은 뺌 
fit <- glm(yes / (yes + no) ~ gender + race, weights = yes + no,
           family = binomial, data = Marijuana)
          #family=binomial : 로짓 


summary(fit)
#B1^= 0.20261
#조건부오즈비(여자일경우 마리화나 사용)
#모형적합 
exp(0.20261)

#왈드통계량(B1^/SE)^2
#(z value)^2 #제곱해줘야함 
#그냥 anova에서 확인 



#0일 때 1일 때 상대위험도
#여성인경우 x=0 = -0.83 + 0.20261*0 + 0.4437z
#p값이 0.05보다 작음 -> 3개의 모수는 모두 유의함 
#null절편만 있는 경우 #deviance 자료가 잘 적합되어있는지 이 두 값들을 비교하며 확인 
# null deviance 와 residual deviance 사이의 차이가 얼마나 크냐에 따라서 null model로 부터 얼마나 떨어졌느
--------
  
#x=0,1, z=0,1부여 확인?
factor(Marijuana$race) #other0, white를 1
factor(Marijuana$gender) #female을 0 male을 1로 설정해주었다. 
--------
  

library(car)

Anova(glm(yes / (yes + no) ~ gender + race, weights = yes + no,
					family = binomial, data = Marijuana) )

Anova(fit)    #위와같은거 
#카이스퀘어값 => 왈드통계량, p-value -> 이 값들로 유의성검정 

# * -> p-value < 유의수준 -> 귀무가설 기각 : 그룹간에 차이 있음
----------------------------------------------------------------
# 색깔과 너비를 예측변수로 사용하는 참게 예제

Crabs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", 
					  header = TRUE)

str(Crabs)

Crabs$color
#아직 범주는 아님
factor(Crabs$color)
#범주로 바꿔주는것 (color는 범주형 변수:1,2,3,4)

#연속형변수와범주형변수 #factor(color):c2,c3,c4(변수)를 알아서 생성
fit <- glm(y ~ width + factor(color), family = binomial, data = Crabs)
summary(fit)
#factor(color)2=c2


coef(fit) #적합된 계수만 얻음
param <- coef(fit)

#새로운 변수 생성(설명변수의 값)    너비가 x = 26.3,  1:alpha
predictor <- c(1, 26.3, 0, 0, 1) #어두운 색일 경우에 너비가 26.3일 때 로짓값  
param*predictor #이걸 이용하여 #적용한 로짓예측식
elogit <- sum(param * predictor) #예측된 로짓값
elogit
pred <- exp(elogit) / (1 + exp(elogit))  #예측확률값 
pred

#위들을 함수로 만든 것 
pred_prob <- function(param, predictor) {

	elogit <- sum(param * predictor)

	pred <- exp(elogit) / (1 + exp(elogit)) 

	return(pred)
}

# 어두운 색
predictor <- c(1, 26.3, 0, 0, 1)

pred_prob(param, predictor) #예측확률값 

# 중간색
predictor <- c(1, 26.3, 1, 0, 0)

pred_prob(param, predictor)
#어두운색에 비해중간색이 되니 부수체를 가질 확률이 커짐 
0.73/0.4 # 부수체 가질 확률이 1.8배커짐
#중간색/어두운색= 중간색:1.8*어두운색  
-------
  
  
#너비의 변화에 따라 부수체가질 확률(그림4.4)
min(Crabs$width) #최소값
max(Crabs$width) #최대값

X <- seq(min(Crabs$width), max(Crabs$width), by = 0.1)
#변수생성

# 중간색인 경우
#x가갖는 값 만큼 c2를 생성(예측값을 갖는 변수생성)
c2 <- c()
i <- 1
for(x in X) {
	c2[i] <- pred_prob(param, c(1, x, 1, 0, 0))
	i <- i + 1
}

#width에 따른 y 그림그리기 
plot(y ~ width, xlab = "Width", ylab = "Number of satellites", data = Crabs) 
lines(X, c2)
#c2라는 변수를 작성 (선으로)
# 중간색인 경우에는(0.5):너비가 넓지 않아도 성공확률이 0.5보다 커짐


#약간 어두운색인 경우:c3 (같은너비에서)
c3 <- c()
i <- 1
for(x in X) {
	c3[i] <- pred_prob(param, c(1, x, 0, 1, 0))
	i <- i + 1
}

lines(X, c3)
#중간색일 때 약간 어두울 때 보다 부수체 가질확률이 전체적으로 높다는 결과 얻음

#어두운색인 경우
c4 <- c()
i <- 1
for(x in X) {
	c4[i] <- pred_prob(param, c(1, x, 0, 0, 1))
	i <- i + 1
}

lines(X, c4)
#등딱지넓이가 넓지 않아도 부수체가질 확률이 높음 


#중간효율값 계산 해보기 
#지금은 그림을 통해

-----------------------------
# width=26.3가 주어졌을 때 어두운색일 때 비해 밝은 색일 때 오즈비가  몇배 커질 수 있는지 ?(이런문제)
predictor <- c(1,26.3,0,0,1)
pred_prob(param,predictor) #어두운색
#예측확률값

predictor <- c(1,26.3,0,0,0)
pred_prob(param,predictor) #밝은색

0.72/0/39 #어두운색일 때 비해 밝은 색일 때 오즈비 (밝은/어두운)


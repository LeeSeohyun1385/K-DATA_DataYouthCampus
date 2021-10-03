# 예제 : 정치성향에 관한 자료 p.220

Polviews <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Polviews.dat", 
					  header = TRUE)

str(Polviews)
#y1~y5 :매우진보적~매우보수보적  


Polviews 

# female : 0, male : 1
# dem민주당 : 0, repub공화당 : 1
# '매주 진보적' ~ '매우 보수적' 5개 순서형 범주

library(VGAM)

#반응변수:y1~5-> 누적로짓 이용:cumulative 
fit <- vglm(cbind(y1, y2, y3, y4, y5) ~ party + gender, 
            family = cumulative(parallel = TRUE), data = Polviews)

summary(fit)
#관심있는 것 ; beta1, beta2 : partyrepub=-3.63366(공화당일 때)/gendermale=0.04731(남자일때)

#진보적
# repub이  Y > j 대신에 Y <= j일(보수적이아닌 진보적) 오즈 추정값은,dem에 비해exp(-3.634) = 0.026배이다. 
# repub : Y <= j 대신에 Y > j  ==> exp(3.634) = 37.9
#공화당: 진보적인성향을 가질 가능성이 크다. 


predict(fit) #비례오즈모형 적합값 (내가추가)
coef(fit,matrix=TRUE)#내가추가
#repub가 매우진보적방향일 오즈의 추정값은 dem에 비해 exp(-3.633)배 


#누적확률 
#attach동안은 변수들과 적합값 출력가능 
attach(Polviews)
data.frame(gender, party, fitted(fit))
detach()
#attach를 끝내기 


#정치성향에 가입된 정당은 영향을 미쳐도 성별은 영향미치지 않는다???


# 모형의 추론
# H_0 : beta_1 = 0 (정당 효과검정 -성별만 포함된 모형)
fit2 <- vglm(cbind(y1, y2, y3, y4, y5) ~ gender, 
            family = cumulative(parallel = TRUE), data = Polviews)

lrtest(fit, fit2)
#H0기각 : 정당이 영향을 강하게 미침

# H_0 : beta_2 = 0 (성별 효과검정)
#정당만포함된 모형적합 
fit3 <- vglm(cbind(y1, y2, y3, y4, y5) ~ party, 
            family = cumulative(parallel = TRUE), data = Polviews)

lrtest(fit, fit3)
#0.7522>0.05 
#성별은 정치성향에 영향을 미치지 않는다. 

#~p.222







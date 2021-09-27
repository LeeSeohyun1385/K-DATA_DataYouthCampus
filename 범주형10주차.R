# 예제 : 악어의 먹이 p.209

Gators <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Alligators.dat", 
					  header = TRUE)

str(Gators) #x:악어의 길이 y:악어가 선호하는 먹이 종류 

Gators$y #문자형 변수


factor(Gators$y)


#로짓모형 적합# - 다범주 로짓 모형(명목형) - 기준범주로짓 
install.packages('VGAM')

library(VGAM)
#반응변수 : 요인(factor변수)으로 전환하여(범주형변수로)
fit <- vglm(factor(y) ~ x, family = multinomial, data = Gators)
#기타가 기준범주 

summary(fit)
#coefficients 
#첫번째 범주의 절편(alpha1)
#두번째 범주의 절편(alpha2)
#첫번째 범주의 기울기(beta1)
#두번째 범주의 기울기(beta2)

coef(fit, matrix = TRUE) # 어류/기타log(pie1^/pie3^), 연체류/기타log(pie2^/pie3^) #로그오즈  

# 연체류가 아니고 어류가 될 로그 오즈의 추정값=log(pie1^/pie2^)
#(1.618 - 5.697) + [-0.110 - (-2.465)] => -4.08, 2.355


#연체류를 기준범주로 주면 log(pie1/pie2)의 베타계수를 바로 알 수 있음 
fit2 <- vglm(factor(y) ~ x, family = multinomial(refLevel = "I"), data = Gators)
                                                #연채류가 기준범주 
coef(fit2,matrix=TRUE) #첫번째범주와 두번째범주의 기준범주 

#2.355>0: 길이가 긴 악어일수로 연체류 보다는 어류를 선호하는 경향을 갖는 것으로 해석할 수 있음. 
#2.465446>0 :길이가 길수록 연체류보다는 기타를 더 선호 

# 즉, 길이가 한 단위(미터) 증가하면 먹이가 연체류가 아니고 어류일 오즈의 추정값은
# 길이가 x인 악어의 오즈추정값의 exp(2.355) = 10.5배 

confint(fit2, method = 'profile') #95%신뢰구간 #어류를 더 선호 


# 가능도비 검정
#독립성검정 

# H_0 : beta_1 = beta_2 = 0 #귀무가설
#길이의 변화에 먹이를 선호하는 종류에 영향을 받지 않음
#귀무가설을 기각해야 길이가 영향을 준다는 결과를 얻을 수 있음 
#두 가능도비 값의 차이가 커지면 의미가 없고, 작으면 모형이 자료에 잘 적합하다. 
fit0 <- vglm(factor(y) ~ 1, family = multinomial, data = Gators)

coef(fit0) #절편만 있는 모형 

deviance(fit0)
deviance(fit)
deviance(fit0) - deviance(fit)  #가능도비검정통계량 

#기준범주로짓모형의 적합결과#
lrtest(fit, fit0)
#자유도2개 ,p값 작음 -> 귀무가설 기각 : 악어의 길이가 오즈에 영향을 줌 :모형이 잘 적합 


----------------------------
# 반응확률들의 추정

prob <- fitted(fit)
prob #각 범주의 추정된 확률 
Gators$x #길이
#악어가 길이가 길수록 어류를 더 선호 (어류의 확률이 뒤로갈수록 연체류보다 커짐짐)

#시각적으로 표현
plot(Gators$x, prob[,1], ylim = c(0, 1), type = 'l',col="red")#어류선호확률 
lines(Gators$x, prob[,2],col="blue")#연채류를 선호하는 길이: 길이가 길 수록 연채류 선호하지 않음 
lines(Gators$x, prob[,3],col="green") #3번째 범주(기타) 에 대한 반응확률

----------------------------------
#예제 : 사후세계에 관한 연구   #모형선택 #변수선택 
Afterlife <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Afterlife.dat",header = TRUE)
str(Afterlife)

Afterlife #yes:사후세계 있다. undecided:모르겠다. no:없다. 

#각 범주들 cbind()로 묶어주기->반응변수 
model_gr <- vglm(cbind(yes, undecided, no)~gender+race,family=multinomial,data=Afterlife)

summary(model_gr)
coef(model_gr,matrix=TRUE)


#여성이 0 남성이 1
#black이 0 white가 1  알파벳, 한글 순서

#-0.4186 : 인종이 주어져있다고 하면 male의 로그오즈 
#오즈로 해석하려면 지수 취해줌
exp(-0.4186)
#남성에 대하여 아니오 보다 예라고 대답할 오즈가 여성의 오즈보다 0.65배 높다 
#남성에 대하여 예라고 답하기보다 아니오라고 답할 경향이 높다 <- beta<0 
exp(0.3417744)
#백인에 대하여 아니오보다 예라고 답할 오즈가 흑인의 오즈보다 1.4배 높다 


#성별의 효과 검정 
#인종만 적합시킨 모형
model_r <-  vglm(cbind(yes, undecided, no)~race,family=multinomial,data=Afterlife)
#H_0 : gender에 대한 효과는 없다. (genderX)

#이탈도
deviance(model_r)
deciance(model_gr)# 이탈도가 작아짐
deviance(model_r)-deviance(model_gr)

lrtest(model_gr,model_r)
#젠더라는 효과가 사후세계 판단에 영항을 미침

#인종의 효과 검정 
#H0: race에 대한 효과는 없다. (성별만 적합시킨 모형)
model_g <- vglm(cbind(yes, undecided, no)~gender,family=multinomial,data=Afterlife)
deviance(model_g)
lrtest(model_gr, model_g) #영향 X #인종효과는 유의하지 않다 

summary(model_g) 

coef(model_g,matrix=TRUE)
#-0.4008 ; 분모가 크다(여성에 대한 인종효과)
exp(-0.4008) #남선/여성 의 오즈가 1보다 작음 
#남성에 비해 여성이 사후세계에 대한 믿음이 강하다. 


#예측확률
fitted(model_gr)

#6.2.5절까지의 내용 
#기준범주가 아닌 다른범주들의 오즈 추정 
#명목형
#다음시간 순서형 
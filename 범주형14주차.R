# 연습문제 2.28: 조건부 독립성 / 주변 독립성  p.85

# X : treatment
# Z : medical center
# Y : response

Z <- c(1, 1, 1, 1, 2, 2, 2, 2)
X <- c('A', 'A', 'B', 'B', 'A', 'A', 'B', 'B')
Y <- c('S', 'F', 'S', 'F', 'S', 'F', 'S', 'F') #성공실패 
count <- c(18, 12, 12, 8, 2, 8, 8, 32)

data <- cbind(Z, X, Y, count)
#명목형은 다 factor로 바꿔줘야함 
dataset <- data.frame(Z = factor(Z), X = factor(X), Y = factor(Y), count = count)
str(dataset)

#완전독립성모형 
model0 <- glm(count ~ Z + X + Y, family = poisson, data = dataset)
summary(model0)
#교호작용모형 적합
model2 <- glm(count ~ Z + X + Y + X*Y + X*Z + Y*Z, family = poisson, data = dataset)
summary(model2)
#이탈도와 자유도 차이 별로 안남 #과적합

#조건부독립성모형 
#(XZ,YZ)
model_XY_Z <- glm(count ~ Z + X + Y + X*Z + Y*Z, family = poisson, data = dataset)
summary(model_XY_Z)

#교차적비(오즈비)#
#m12*m21/m11*m22
GLS <- xtabs(count~X+Y,data=dataset)
GLS
# XY
(20*40) / (20*20)  
#1에서 멀리 떨어짐 
#두변수가 독립이 아님 :z가 없어서

GLS <- xtabs(count~X+Y+Z,data=dataset)
GLS
# XY : Z = 1
(18*8) / (12*12)
#독립:교차적비 1이기 때문

# XY : Z = 2
(2*32) / (8*8)
#독립:1

GLS <- xtabs(count~X+Z,data=dataset)
GLS
# XZ
(30*40) / (20*10)  #1 2 순서이므로 원래 오즈비 계산방법 
log((30*40) / (20*10))#두변수의 연관항=교차적비에 로그 취해줌 


# Y가 반응변수인 로지스틱 회귀모형#

Z <- c(1, 1, 2, 2)#의료센터-명목형 
X <- c('A', 'B', 'A', 'B')#치료방법 -명목형
S <- c(18, 12, 2, 8) #성공
F <- c(12, 8, 8, 32)#실패 

dataset <- data.frame(Z=factor(Z), X=factor(X), S=S, F=F)

#로지스틱 회귀모형
model <- glm(S/(S+F) ~ X + Z, family=binomial, 
               weights=S+F, data=dataset)

summary(model)#XB: 의료센터(z)에 대해 x의 효과 없음(오즈) Z2:y에 대한 z의 효과 
summary(model_XY_Z)
#이 해석 다시공부 (녹화 보기)


# 연습문제 4.14 : 감염치료제 효과 p.155
#위약:가짜약(비타민등)
#신약 : 새로 개발 

Infection <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Infection.dat", 
					         header = TRUE)

Infection
#center:센터
#treat:처리(1-신약,0-대조군)
#y:성공(반응변수)
#n:실패 (반응변수)

model <- glm(y/(y+n) ~ factor(center) + factor(treat), family=binomial, 
               weights=y+n, data=Infection)
summary(model)
#(treat)1:신약의 효과 (0이 대조군)
exp(0.3939)

exp(-1.8230)
#의료센터 6일 때 다른센터에 비해 신약에대한 성공효과(y)가 별로 없음 
                                


exp(0.9797)
#의료센터2일때 다른 센터에 비해 치료효과(y)가 2.66




# 연습문제 5. 10: 흡연과 폐암(중국 도시별) p.202

Lungs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Lungs.dat", 
					      header = TRUE)

Lungs

#흡연이 폐암에 연관이 있는지
#폐암이걸릴 확률이 도시와 연관이 있는지

#반응변수 폐암 #설명변수:흡연 
model1 <- glm(Yes/(Yes+No) ~ factor(Smoking), family=binomial, 
               weights=Yes+No, data=Lungs)
summary(model1)
#이탈도 커서(자유도에비해) p값이 0에 가까움
#흡연하고 서로 연관이 없음

#도시에 대해 
model0 <- glm(Yes/(Yes+No) ~ factor(City) + factor(Smoking), family=binomial, 
               weights=Yes+No, data=Lungs)
summary(model0)

#
exp(0.777062) #폐암에 대한 담배의 효과 
#담배를 피는 사람이  안피는 사람에 비해 2.17배 효과
#담배를 피는 사람의 오즈가 안피는 사람의 오즈에 비해 2.17배 
exp(-0.745683)
#이 도시는 다른 도시에 비해 폐암에 걸릴 확률이 낮음 (coef작을수록 exp도 낮음 )
exp(-0.054906)
exp(0.055618)
#상하이는 



# 연습문제 7.7: 예산 지출액 p. 288
#로그선형모형#

Spending <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Spending.dat", 
					      header = TRUE)

Spending
#3*3*3*3 4차원 분할표 

#명목형 변수 취급 - factor로 지정 
e <- factor(Spending$e)  #환경
h <- factor(Spending$h)  #보건
c <- factor(Spending$c)  #도시 
l <- factor(Spending$l)  #법집행 

#3차연관항 -과포함된 모형
model_3 <- glm(Spending$count ~ e + h + c + l
					+ e*h + e*c + e*l + h*c + h*l + c*l
					+ e*h*c + e*h*l + e*c*l + h*c*l,
				   family = poisson)
summary(model_3)
#이탈도 자유도 보다 많이 작음 -> 자료가 잘 적합 (너무 잘 적합-과적합)
#동질연관모형 : 모형을 좀 제거해야함


#2차연관항 
model_2 <- glm(Spending$count ~ e + h + c + l
					+ e*h + e*c + e*l + h*c + h*l + c*l,
				   family = poisson)
summary(model_2)
#이탈도 값이 자유도에 비해 작음 - 적절한 연관항을 제외해도 모형적합에 문제 없음 

#각각 한개의 연관항 제거


#eh연관항 제외
#연관항이 영향을 주고있다면 연관항이 포함된 모형의 이탈도가 제외된 이탈도와 차이 
#영향을 안주고 있다면 연관항이 포함된이탈도와 제외된 이탈도가 차이가 안남 
model_eh <- glm(Spending$count ~ e + h + c + l
					+ e*c + e*l + h*c + h*l + c*l,
				   family = poisson)
summary(model_eh)
#이탈도값이 제법 커지고 모형이 잘 적합됨(model2-31.669와 비교)



#ec제외
model_ec <- glm(Spending$count ~ e + h + c + l
					+ e*h + e*l + h*c + h*l + c*l,
				   family = poisson)
summary(model_ec)
#이탈도 커짐 

model_el <- glm(Spending$count ~ e + h + c + l
					+ e*h + e*c + h*c + h*l + c*l,
				   family = poisson)
summary(model_el)
#크게 이탈도 값이 변하지 않음-제거해도 문제 없음  

model_hc <- glm(Spending$count ~ e + h + c + l
					+ e*h + e*c + e*l + h*l + c*l,
				   family = poisson)
summary(model_hc)
#크게 이탈도 값이 변하지 않음-제거해도 문제 없음 

model_hl <- glm(Spending$count ~ e + h + c + l
					+ e*h + e*c + e*l + h*c + c*l,
				   family = poisson)
summary(model_hl)
#과적합 해결 자유도와 가까운 값 

model_cl <- glm(Spending$count ~ e + h + c + l
					+ e*h + e*c + e*l + h*c + h*l,
				   family = poisson)
summary(model_cl)


model_el <- glm(Spending$count ~ e + h + c + l
					+ e*h + e*c + h*c + h*l + c*l,
				   family = poisson)
summary(model_el)
#모형의 적합도가 크게 바뀌지 않음 -제거해도됨




# best model (ec, hl, cl)
model_ec_hc_l <- glm(Spending$count ~ e + h + c + l
					+ e*c + h*l + c*l,
				   family = poisson)
summary(model_ec_hc_l)
#모형이 자료에 잘 적합되었다. 
#ec:환경하고 대도시보존은 서로 연관 잇음
#hl,cl :보건하고 대도시는 법집행이 주어져 있을 때 서로 독립


#추가
#ec를 제거하면 어떻게 될까 
#(e,hl,cl)
model_hc_l <- glm(Spending$count ~ e + h + c + l
                     + h*l + c*l,
                     family = poisson)
summary(model_hc_l)
#p-값 
1-pchisq(80.965,64)
#(이탈도,자유도)
#자료에 잘 적합 (이유: >0.05??)

#환경에서는 보곤, 대도시보존,법집행에서는 관련이 없다.(e)
#보건하고 대도시 보존은 법집행에있어서 조건부 독립(hl,cl) 
#해석 다시듣기 

#자료 제일 적합한 모형 찾기 -> (e,hl,cl)이런식으로 모형 찾아서 위처럼 서로 어떤 관련 있는지해석 



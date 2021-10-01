#이 내용 많이 헷갈림 . 

#로그선형모형#

# 예제 : 행복과 천국에 대한 믿음 p.251

HappyHeaven <- read.table("http://www.stat.ufl.edu/~aa/cat/data/HappyHeaven.dat", 
					  header = TRUE)

str(HappyHeaven)

HappyHeaven
#행->happy
#열->heaven 

# happy not <- lambda1^X = 0
# heaven no <- lambda1^Y = 0

#독립성모형 
fit <- glm(count ~ happy + heaven, 
            family = poisson, data = HappyHeaven)

summary(fit)
#행복도에 대한 천국믿음의 "로그오즈비": 1.749 / 천국믿음에 대한 "오즈"는 행복도에 대하여 exp(1.749) = 5.75
#lamda1^Y-lamda2^Y= hevenyes - heavenno = 1.749 - 0 = 1.749 
#lamda만 가지고 적합한 이탈도 (residual deviance): 이 값이 작음 : 칸의 칸도수가 차이가 많이 안남 :독립성모형이 자료에 잘 적합: 독립성 
#->확률적으로 독립 
#자유도 2와 이탈도 0.89111의 차이 많이나지 않음 

# 포화모형

fit <- glm(count ~ happy + heaven + happy:heaven, 
            family = poisson, data = HappyHeaven)

summary(fit)
#포화모형보다는 독립성이 더 적합하다 (이유 ??자유도와 이탈도 모두 0에 가까움)

#사후세계믿음 여부와 행복도 간의 오즈비 추정값
# 믿음 -> 보통/아니오 => exp(-0.094) (보통행복도 와 아니오행복도 간)#exp(-0.09358(lamda보통)-0(lamda아니오))
# 믿음 -> 행복/아니오 => exp(0.074) (행복행복도 와 아니오행복도 간)
# 믿음 -> 보통에 비하여 행복(행복/보통:행복과보통 간) => exp(0.074 - (-0.094)) #1에 가까움 : 둘의 관계가 확률적으로 독립(독립성모형이 잘 적합)->추정된 오즈비가 1값과 통계적으로 유의하게 다르지 않다. 
#~p.253(다른교재에서)
########################################################

# 예제 : 음주,흡연,마리화나 경험 p.255
#표7.3 
Drugs <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Substance.dat", 
					  header = TRUE)

Drugs
#2*2*2분할표
             
A <- Drugs$alcohol
C <- Drugs$cigarettes
M <- Drugs$marijuana

# 완전 독립성 모형
fit0 <- glm(count ~ A + C + M, family = poisson, data = Drugs)
summary(fit0)
#yes인경우의 모수 추정 (4개의 모수추정):estimate(lamda모수추정값)

#### 모형의 적합성 검정
# H_0 : 완전 독립성 모형
# H_1 : 포화모형
# 이탈도:1286.0->매우큼 ( df = 4 ):4보다 많이 떨어져있음 - p값0에가까움):완전독립성모형 적절하지 않음 
fitted(fit0)
---
# 독립성 모형 : (AC, M)
fit_ac <- glm(count ~ A + C + M + A:C, family = poisson, data = Drugs)
summary(fit_ac)
#5개모수 추정
#이탈도 작아짐,자유도3 :여전히 모형이 적절하지 않음 (자유도가3인데 이탈도값이 큼)

#2.87373:lamda22
exp(2.87373) #오즈비 추정값 : (AC,M)모형에서 AC오즈비 값
#연관이 큼 <-이유: 1과 가깝지 않음

fitted(fit_ac)

# 독립성 모형 : (AM, C)
fit_am <- glm(count ~ A + C + M + A:M, family = poisson, data = Drugs)
summary(fit_am)
#이탈도 큼
exp(4.12509)#AM의 오즈가 큼



# 독립성 모형 : (A, CM)
fit_cm <- glm(count ~ A + C + M + C:M, family = poisson, data = Drugs)
summary(fit_cm)

exp(3.22431) #오즈가 비교적 작아짐 
#이탈도 여전히 큼

-------

# 조건부 독립성 모형 : (AM, CM)
fit_am_cm <- glm(count ~ A + C + M + A:M + C:M, family = poisson, data = Drugs)
summary(fit_am_cm)
#이탈도:187.75 :이탈도가 굉장히 작아짐:독립성모형보다는 모형을 판단하는데 적절한 모형
exp(4.12509)#AM의 조건부 오즈비 
exp(3.22431)#CM의 조건부 오즈비 
fitted(fit_am_cm) #로그선형모형 적합결과
#원자료표 형태 그대로의 적합결과 

# 조건부 독립성 모형 : (AC, CM)
fit_ac_cm <- glm(count ~ A + C + M + A:C + C:M, family = poisson, data = Drugs)
summary(fit_ac_cm)
#이탈도가 훨씬 줄어듬 (위 모형보다는 적합)
#그럼에도불구하고 자유도가 2면 p값이 0에 가까움(자유도와 비교했을 때 이탈도가 큼)
#아직도 여전히 자료설명에 충분하지 않음 

exp(2.87373)



# 동질 연관성 모형 : (AC, AM, CM)
fit_ac_am_cm <- glm(count ~ A + C + M + A:C + A:M + C:M, family = poisson, data = Drugs)
summary(fit_ac_am_cm)
#이탈도가 0.3일때 자유도가 1 
#-> p값이 큼 -> 0.5보다도크다고할 수 있음
#제일 잘 적합된 모형 
exp(2.05453)#AC의 조건부 오즈비 추정값 
exp(2.98601) #odds ratio : 가장 큰 값:추정값중 큰값 
exp(2.84789)


#오즈비에 대한 해석 강의 다시 들어보기 (꼭)
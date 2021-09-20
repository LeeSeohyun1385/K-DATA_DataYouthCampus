# 예제: 코골이와 심장병에 관한 연구

Heart <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Heart.dat", 
	header = TRUE)

Heart

str(Heart)
#24/1379 / 35/638 : 상대적위험도

install.packages("dplyr")

library(dplyr)
                 #Heart$범주(코고는정도) / x= 0,2,4,5 :범주에 대한 점수 할당
Heart$x <- recode(Heart$snoring, never = 0, occasional = 2, 
	nearly_every_night = 4, every_night = 5)
#x라는 변수로 변환
Heart$x
Heart
#n
Heart$n <- Heart$yes + Heart$no

Heart
#24/1379
#여기까지가 데이터를 셋해준 것 

#선형확률모형적합 (내가추가함)
fit1<- glm(yes / n ~ x, family = quasi(link = identity, variance = "mu(1-mu)"),
            weights = n, data = Heart)

summary(fit1)


#로지스틱회귀모형
#모형적합 (절편기울기 추정-> 적합값 얻음)
fit <- glm(yes / n ~ x, family=binomial(link=logit), weights = n, data = Heart) 
#n전체 대상자수,  yes/n:심장병 앓고는 사람들,x:추정기댓값 


summary(fit)
#적합결과
#절편, 기울기의 추정값
#residual:잔차


#선형확률모형 적합시키려면 (강의에서)
fit2 <- glm(yes / n ~ x, family = quasi(link = identity, variance = "mu(1-mu)"),
	weights = n, data = Heart)

summary(fit2)











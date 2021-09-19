# 예제: 정당지지도에 대한 성별차이

Political <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Political.dat", header = TRUE)

Political

str(Political)

#반응변수 생성(범주형 변수, 요인변수)
Party <- factor(Political$party, levels = c("Dem", "Rep", "Ind"))

#이차원 분할표를 얻기위한 모형식  (~그룹변수+반응변수)

GenderGap <- xtabs(~gender + Party, data = Political)
 
GenderGap
#분할표 

#피어슨 카이제곱 검정
chisq.test(GenderGap)
#X^2통계량 , 자유도, 유의확률

chi<-chisq.test(GenderGap)
chi$observed  #분할표
chi$expected #기댓값 (Mij)
chi$residuals #잔차
chi$statistic #카이제곱통계량
chi$parameter #자유도
chi$p.value #유의확률

#가능도비 통계량  
2*sum(chi$observed*log(chi$observed/chi$expected))
-----

#칸 표준화 잔차
stdres <- chisq.test(GenderGap)$stdres

stdres
#표준화 잔차의 부호로 결과 분석 

---------------------
# 예제: 음주와 영아의 기형성에 관한 연구
#선형추세 독립성검정 계산 (p.59/p.58 표 2.6)
                              #첫열입력->그다음열입력         #열 2개 (5개,5개)
Malform <- matrix(c(17066, 14464, 788, 126, 37, 48, 38, 5, 1, 1), ncol = 2)

Malform

install.packages("vcdExtra")

library(vcdExtra)
                          #첫번째 범주 #두번째 범주 ...(범주의 중간값) : 행점수 
CMHtest(Malform, rscores = c(0, 0.5, 1.5, 4.0, 7.0))
#첫번째줄만 해석 (선형추세검정)
#카이제곱, 자유도, 유의확률 


6.5699 / (sum(Malform) - 1) #카이제곱 통계량/자유도 

#R값 : 표본상관계수 
#6.5699 = CMHtest 에서의 카이제곱통계량 
sqrt(6.5699 / (sum(Malform) - 1))   #R

#M^2
sum(Malform)*(0.0142)^2  #CMHTEST에서의 통계량과 같음 
#p값 구하기
#결과 : 알콜소비량 높아질수록 기형아 확률 높아짐 






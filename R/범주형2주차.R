# 예: 아스피린과 심근경색 비율차 

#비율차                                   
# Wald 통계량                           95% 신뢰구간
#            x           n             신뢰수준:1-conf.level
prop.test(c(189, 104), c(11034, 11037), conf.level=0.95, correct=FALSE)
#결과 : 신뢰구간, pie1^, pie2^

install.packages('PropCIs')
library(PropCIs)

# Score
#비율의 차이에 대한 신뢰구간 
#diffscoreci(x1,n1,x2,n2, conf.level)
diffscoreci(189, 11034, 104, 11037, conf.level=0.95)

#로그오즈비 신뢰구간(log seta^)
riskscoreci(189, 11034, 104, 11037, conf.level=0.95)
#신뢰구간에 1을 포함하지 않음 : 차이가 있다.(유의)<-귀무가설기각


# odds ratio :오즈비 

install.packages('epitools')
library(epitools)

#오즈비
# Wald    #x11,x12,x21,x22
oddsratio(c(189, 10845, 104, 10933), conf.level=0.95)
#odds ratio estimate :오즈비 , 신뢰구간 : 로그오즈비의 신뢰구간 

library(PropCIs)
# Score   x11 n1 x21 n2
orscoreci(189, 11034, 104, 11037, conf.level=0.95) #로그오즈비 신뢰구간 


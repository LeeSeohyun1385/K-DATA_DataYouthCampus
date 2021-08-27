library(aplpack)
library(lattice)

intake.data<-read.table("C://Users//이서현//Desktop//intake.txt")
intake.data

intake<-intake.data[,-1] #1열을 빼기 
intake

rownames(intake)<-intake.data[,1] #행에 이름 붙이기 
intake

stars(intake) #별그림 그리기 


plot(intake) #산점도

install.packages("aplpack") 
library(aplpack)
faces(intake)   #얼굴그림 그리기 


parallelplot(~intake, data=intake, horizontal=FALSE) #프로파일 도표 

colMeans(intake) #표본평균벡터
cov(intake) #표본공분산

cor(intake) #표본상관계수

dim(intake) #행 열 개수 


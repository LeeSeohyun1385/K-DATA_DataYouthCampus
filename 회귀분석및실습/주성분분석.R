price.data <- read.table("C:/Users/이서현/Desktop/price.txt")
price<-price.data[,-1]

rownames(price) <- price.data[,1] #행에 이름 주기
plot(price)
stars(price) #도형의 크기가 크면 5가지 변수값이 크고 물가가 높음 

#<주성분>
s<-cov(price) #표본공분산 행렬
eigen(s)  #고유값(value),vectors:고유벡터(세로줄) 
e.val<-eigen(s)$values #고유값만 저장 
V<-(-1)*eigen(s)$vectors #양수로 만들어줌 

#pc1 = 0.028 * bread + 0.20 * burger (첫번째 고유벡터에 곱해줌,5개 다 해서 다 더함) 
#PC2~PC5 #n번째 주성분

#X~*V :주성분점수계산 
price.c<-scale(price,center=TRUE,scale=FALSE)
PC.score<-price.c%*%V


summary(PC.score) #표본평균=0
cov(PC.score)
round(cov(PC.score),10) #소수점이 너무 복잡해 반올림 해주기 
e.val #이거와 값이 똑같음 (고유값들을 대각원소로 가짐)

sum(e.val)
diag(s) #대각원소값만 출력 
sum(diag(s)) #위 sum과 값이 같음 
e.val/sum(diag(s)) #k번째 주성분의 설명력(58%정도만 퍼져있는 정도를 설명)
cumsum(e.val/sum(diag(s))) #순서대로 주성분 늘려가며 선택 (누적합)
plot(PC.score[,1:2]) #pc가 플러스로 값이 크게나오면 가격이 높음/ pc1과pc2

plot(e.val,type="b") #삼비탈그림


------------------------#이 이전 단계는 실제로는 계산 잘 안함
  
  
#주성분 분석 수행
prcomp(price, center=TRUE, scale=FALSE) #공분산행렬(스케일폴스),상관계수행렬(스케일 트루)
#V매트릭스와 같은 값 

price.pca<-prcomp(price, center=TRUE, scale=FALSE)
names(price.pca)
price.pca$sdev #(1~5)번째 주성분의 표준편차 값
price.pca$sdev^2 #표본공분산행렬의 고유값과 동일한 값 

#결과
summary(price.pca) #주성분의 설명력
#Standard deviation 표준편차 , 제곱하면 분산 
#Proportion of Variance 분산비율 (e.val/sum(diag(s))와 같은 값)
#Cumulative Proportion (cumsum(e.val/sum(diag(s)))와 같은값)

predict(price.pca) #주성분 점수
biplot(price.pca) 
screeplot(price.pca,type="1")
-----------------------------
  
prcomp(price,center=TRUE,scale=FALSE)
prcomp(~bread+burger+milk+orange+tomato,data=price,center=TRUE,scale=FALSE)
prcomp(~bread+burger+milk+orange+tomato,data=price,center=TRUE,scale=TRUE)

price.pca.s<-prcomp(~bread+burger+milk+orange+tomato,data=price,center=TRUE,scale=TRUE)
summary(price.pca.s)
predict(price.pca.s)
biplot(price.pca.s)

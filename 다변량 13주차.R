> data(iris)
> head(iris)
> plot(iris[,-5],col=c("blue","red","green")[iris[,5]]) #반응변수(Species)를 뺀 반응변수에 대한 산점도
> plot(iris[,2:3],col=c("blue","red","green")[iris[,5]]) #두번째와 세번째 판별변수의 산점도 
-------------------
  > x<-c(60,22,50,15) #점 설정 #새로운 관측값 설정 
  > iris.set<-subset(iris,iris[,5]=="setosa")
  > iris.ver<-subset(iris,iris[,5]=="versicolor")
  > iris.vir<-subset(iris,iris[,5]=="virginica") #3가지 그룹으로 분류
  
  > mean.set<-colMeans(iris.set[,-5])   #그룹별 표본 평균 
  > cov.set<-cov(iris.set[,-5])     #그룹별 표본 공분산
  > mean.ver<-colMeans(iris.ver[,-5])
  > cov.ver<-cov(iris.ver[,-5])
  > mean.vir<-colMeans(iris.vir[,-5])
  > cov.vir<-cov(iris.vir[,-5])
  
  #마할라노비스 거리 
  > t(x-mean.set)%*%solve(cov.set)%*%(x-mean.set)
  > t(x-mean.ver)%*%solve(cov.ver)%*%(x-mean.ver)
  > t(x-mean.vir)%*%solve(cov.vir)%*%(x-mean.vir)
  
  > mahalanobis(x, center=mean.set, cov=cov.set)
  > mahalanobis(x, center=mean.ver, cov=cov.ver)
  > mahalanobis(x, center=mean.vir, cov=cov.vir)
  #위와 같은 값 (마할라노비스)^2의 값
  
  ------------------------------------
    #LDA
    #합동표본공분산(각각의 공분산행렬의 평균)                                                          #n-g
    > S.p<-(cov.set*(nrow(iris.set)-1)+cov.ver*(nrow(iris.ver)-1)+cov.vir*(nrow(iris.vir)-1))/(nrow(iris)-3)
    #마할라노비스(최소가 되는 그룹)
    > mahalanobis(x,center=mean.set, cov=S.p)
    [1] 220.5132
    > mahalanobis(x,center=mean.ver, cov=S.p)
    [1] 201.1433
    > mahalanobis(x,center=mean.vir, cov=S.p)
    [1] 192.9264
    
    > mean<-mean.set
    #Lg(x):선형판별함수 
    > L <- t(mean)%*%solve(S.p)%*%x - 0.5*t(mean)%*%solve(S.p)%*%mean     
    > (mahalanobis(x,center=mean, cov=S.p)-t(x)%*%solve(S.p)%*%x)/(-2)  #위와 같은 값 
    
    > mean<-mean.ver
    > L <- t(mean)%*%solve(S.p)%*%x - 0.5*t(mean)%*%solve(S.p)%*%mean
    > (mahalanobis(x,center=mean, cov=S.p)-t(x)%*%solve(S.p)%*%x)/(-2)
    
    
    > mean<-mean.vir
    > L <- t(mean)%*%solve(S.p)%*%x - 0.5*t(mean)%*%solve(S.p)%*%mean
    > (mahalanobis(x,center=mean, cov=S.p)-t(x)%*%solve(S.p)%*%x)/(-2)
    #L을 보고 최대인 그룹 구할 수 있음 (vir이 최대)
    #L최대 /  아까 구한 마할라노비스가 최소 
    
    ------------------------
      #14주차 R 실습
      > library(MASS)
    
    #판별분석 
    #첫번째
    > DA1<-lda(Species~., data=iris, prior=c(1/3,1/3,1/3))
    #~.나머지 독립변수 모두 회귀분석이라는 뜻
    #각 꽃의 종류에 1/3사전확률을 주는 것(1/2,1/4,1/4로 다르게 지정 가능)
    > DA1
    Call:
      lda(Species ~ ., data = iris, prior = c(1/3, 1/3, 1/3))
    
    Prior probabilities of groups: #사전확률
      setosa versicolor  virginica 
    0.3333333  0.3333333  0.3333333 
    
    Group means: #그룹별 판별변수의 표본평균 (중심위치가 각각 다르다)
      Sepal.Length Sepal.Width Petal.Length Petal.Width
    setosa            5.006       3.428        1.462       0.246
    versicolor        5.936       2.770        4.260       1.326
    virginica         6.588       2.974        5.552       2.026
    
    Coefficients of linear discriminants: #수업에 설명 X(생략)
      LD1         LD2
    Sepal.Length  0.8293776  0.02410215
    Sepal.Width   1.5344731  2.16452123
    Petal.Length -2.2012117 -0.93192121
    Petal.Width  -2.8104603  2.83918785
    
    Proportion of trace:
      LD1    LD2 
    0.9912 0.0088 
    
    #예측값(분류결과 계산)
    >PA1 <- predict(DA1) #$posterior:사후확률(최대가 되는 그룹에 분류(몇번 데이터 에서))/yi^이라고 보면됨
    > predict(DA1)$posterior 
    #분류결과표 
    > table(iris$Species, PA1$class)
    
    setosa versicolor virginica  #뒤에있는 변수(PA1$class)
    setosa         50          0         0  #원래 세토사로 분류되어있는 값이 50개 나머지는 없다 
    versicolor      0         48         2  #실제로는 버쉬컬러에 속하는 것인데 세토사로는 0개 버시컬러로는 48개, 버지니카로 2개로 분류 
    virginica       0          1        49
    #앞에있는 변수(iris$Species) 강의 노트와 반대
    #(실제 오분류 3개) : AER= 3/150(원래 개수)
    
    #오분류 AER만 계산
    library(biotools)
    > aer(iris$Species,PA1$class)
    
    #2번째 
    #training sample 
    > train<- sample(1:150,75) #랜덤하게 75개 뽑은 결과의 번호 
    > train
    
    > DA2<-lda(Species~., data=iris, subset=train, prior=c(1/3,1/3,1/3))
    > DA2
    
    > PA2 <- predict(DA2)
    predict(DA2)$posterior   #사후확률 
    
    > PA2
    
    #새로운 데이터셋
    > PA2<-predict(DA2, newdata=iris[-train,])#벨리데이션 셋만 가지고 온 것(-train)#75개
    predict(DA2, newdata=iris[-train,])$posterior
    > table(iris$Species[-train],PA2$class)
    
    setosa versicolor virginica
    setosa         21          0         0
    versicolor      0         23         1
    virginica       0          1        29
    
    > aer(iris$Species[-train],PA2$class)
    [1] 0.02666667
    
    #3번째
    #jackknife기법 
    > DA3<- lda(Species~.,data=iris, prior=c(1/3,1/3,1/3),CV=TRUE)
    
    #결과 비교 
    > head(DA3$posterior)
    setosa   versicolor    virginica
    1      1 5.087494e-22 4.385241e-42
    2      1 9.588256e-18 8.888069e-37
    3      1 1.983745e-19 8.606982e-39
    4      1 1.505573e-16 5.101765e-35
    5      1 2.075670e-22 1.739832e-42
    6      1 5.332271e-21 8.674906e-40
    > head(PA1$posterior)
    setosa   versicolor    virginica
    1      1 3.896358e-22 2.611168e-42
    2      1 7.217970e-18 5.042143e-37
    3      1 1.463849e-19 4.675932e-39
    4      1 1.268536e-16 3.566610e-35
    5      1 1.637387e-22 1.082605e-42
    6      1 3.883282e-21 4.566540e-40
    > table(iris$Species, DA3$class)
    
    setosa versicolor virginica
    setosa         50          0         0
    versicolor      0         48         2
    virginica       0          1        49
    #오분류 : 3/150 (작으면 분류가 잘 된 것)
    
    #lda대신 qda 이용해도 됨 
    
    
    > lda(Species~., data=iris)
    > lda(iris[,-5],group=iris[,5])
    #두개다 사용 가능 
    
    #그림그려보기 
    > library(klaR)
    > partimat(Species~.,data=iris,method="lda")
> import<-read.table("C:/Users/Lee seohyun/Desktop/회귀/import.txt",header=TRUE)
> y.s <- scale(import[,2]) #자료 y를 표준화 변환
> Z<- scale(import[,-(1:2)]) #설계행렬 표준화 변환 

#능선회귀 추정값 (능선모수)
> gamma<-1
> b.ridge <- solve(t(Z)%*%Z+diag(rep(gamma,3)))%*%t(Z)%*%y.s
#능선추정량의 표본평균, 표준편차
> mu<- colMeans(import)
> sd<- sqrt(diag(cov(import)))

#원래 회귀계수 추정값 
> beta.est <- b.ridge*sd[2]/sd[3:5] #b1.b2...
> beta.est
[,1]
doprod 0.06610373
stock  0.55675052
consum 0.10597675
> beta0 <- mu[2]-t(beta.est)%*%mu[3:5] #b0
> beta0
[,1]
[1,] -7.618359
#원래 회귀계수, 관계식 계산가능 
#y^= -7.618 + 0.06doprod + 0.55stock + 0.1consum 


----------------------
> b.ridge <- c() #추정값이 저장될 장소를 지정 
> ridge.par <- seq(0,10,by=0.01) #능선 모수값으로 사용할 값 지정
> gam<-1.0
> for(gam in ridge.par){
   est<-solve(t(Z)%*%Z+gam*diag(rep(1,3)))%*%t(Z)%*%y.s #능선모수
   b.ridge <- rbind(b.ridge,t(est))
   }
> b.ridge
#가로줄 감마, 감마에 대한 회귀계수의 추정값 
doprod     stock    consum
[1,] -0.339342628 0.2130484 1.3026815
[2,] -0.117445843 0.2150324 1.0802416
[3,]  0.009742647 0.2160829 0.9525392
[4,]  0.092149232 0.2166923 0.8696328
[5,]  0.149852919 0.2170584 0.8114372
[6,]  0.192489702 0.2172765 0.7683136
#그림
> plot(ridge.par,b.ridge[,1],ylab="ridge estimate",ylim=c(-0.5,1.5),type="1")
> lines(ridge.par,b.ridge[,3],col="red")
> lines(ridge.par,b.ridge[,2],col="blue")
> abline(h=0,lty=2)

###교차검증 이용한 선택 
> library(cvTools)
> n <- nrow(import)
> n.fold <-5 #fold의 개수 

> cross <- cvFolds(n,K=n.fold, R=1, type = "random") #fold나누기

> ridge.par <- seq(0, 1, by=0.1)
> SS.result <- c()
>SS<-0 #시작값

test.set #cross에서 Fold가 1번인 것만 가지고 오는 것
test.X #번호가 속한 것 
train.X#나머지 번호들 
b#ridge parameter의 추정값 
pred.err #예측값 
SS#제곱의 평균 
SS.result #
>for(gam in ridge.par){
   test.set <- cross$subsets[which(cross$which==i),1]
   test.X <- Z[test.set,];test.y <- y.s[test.set]
   train.X <- Z[-test.set];train.y <- y.s[-test.set]
   b<- solve(t(train.X))
--------
#glmnet이용한 추정값 계산, 교차검증
library(glmnet)
X.mat <- as.matrix(import[,-(1:2)])
y.vec <- as.vector(import[,2])
glm.result <- glmnet(X.mat, y.vec, family="gaussian",alpha=0,lamda=ridge.par, standarize=TRUE, intercept=TRUE)
#Df:독립변수 중 0이아닌 것의 개수, Lamda: 계산한 람다 값

glm.result$beta #추정값 (1.0, 0.9, 0.8 순의 감마에 대한 매칭)
glm.result$a0 #상수항 
#원 변수에 대한 식 (줄 끼리 식 계산 가능)
#y^ = -5.8 + 0.06D + 0.5S + 0.09C

> cv <- cv.glmnet(X.mat, y.vec, lamda=ridge.par,alpha=0, nfolds=3) #각각의 fold마다 속해있는 데이터의 개수=3
> cv$cvm #예측오차의 평균 (이 값으로 판단)
> plot(cv$lamda, cv$cvm)
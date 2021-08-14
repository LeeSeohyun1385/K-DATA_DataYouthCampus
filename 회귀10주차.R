##교차검증 

#Leave-one-out cross-validation 
#M1<-lm(mpg~hp+wt, data=mtcars)
#M2<-lm(mpg~hp+wt+disp+qsec, data=mtcars)

> test.set <- mtcars[1,] #평가자료 
> training.set <- mtcars[-1,] #훈련자료

> reg <- lm(mpg~hp+wt, data=training.set) #회귀분석 
> reg

Call:
  lm(formula = mpg ~ hp + wt, data = training.set)

Coefficients:
(Intercept)           hp           wt  
   37.48509     -0.03207     -3.91826  

#예측값
> pred.value <- reg$coefficient[1]+reg$coefficients[2]*test.set$hp+reg$coefficients[3]*test.set$wt
> pred.value <- c(1,test.set$hp,test.set$wt)%*%reg$coefficients
> pred.value <- predict(reg, newdata=test.set)
   #다 같은 값 (어느것을 써도O)
   
#예측오차 
> pred.error <- test.set$mpg-predict(reg, newdata=test.set) #e1
   
> n<-dim(mtcars)[1]
> pred.error.vec<-c()
> for(i in 1:n){
+     test.set <- mtcars[i,]
+     training.set <- mtcars[-i,]
+     reg<- lm(mpg~hp+wt, data=training.set)
+     pred.error <- test.set$mpg-predict(reg, newdata=test.set)
+     pred.error.vec <- c(pred.error.vec, pred.error)}
   
#하나하나 돌려줄 경우 
> i<-1
#>위함수 돌려주기
> pred.error.vec #Mazda RX4 :예측오차 
> i<-2
   
#예측평가기준    
> press1<-sum(pred.error.vec^2)
> press
   
#M2   
> for(i in 1:n){
+     test.set <- mtcars[i,]
+     training.set <- mtcars[-i,]
+     reg<- lm(mpg~hp+wt+disp+qsec, data=training.set)
+     pred.error <- test.set$mpg-predict(reg, newdata=test.set)
+     pred.error.vec <- c(pred.error.vec, pred.error)}
> press2<-sum(pred.error.vec^2) 
#press비교: press가 작은 모델이 더 좋은 모형
---------------------------------------------
#한번에 
>n<-dim(mtcars)[1]
>pred.error.mat <- c()
>for(i in 1:n){
     test.set <- mtcars[i,]
     training.set <- mtcars[-i,]
     reg1<- lm(mpg~hp+wt, data=training.set)
     reg2<- lm(mpg~hp+wt+disp+qsec, data=training.set)
     pred.error1 <- test.set$mpg-predict(reg1, newdata=test.set)
     pred.error2 <- test.set$mpg-predict(reg2, newdata=test.set)
     pred.error.mat <- rbind(pred.error.mat, c(pred.error1,pred.error2))
  }
>pred.error.mat
>colSums(pred.error.mat^2) #예측평가
----------------------------
     
#Leave-p-out cross-validation
>n<-dim(mtcars)[1] 
> p<-2 
>pred.error.mat <- c()
>for(i in 1:n){
    index<-sample(1:n, p)
    test.set <- mtcars[i,]
    training.set <- mtcars[-i,]
    reg1<- lm(mpg~hp+wt, data=training.set)
    reg2<- lm(mpg~hp+wt+disp+qsec, data=training.set)
    pred.error1 <- test.set$mpg-predict(reg1, newdata=test.set)
    pred.error2 <- test.set$mpg-predict(reg2, newdata=test.set)
    pred.error.mat <- rbind(pred.error.mat, c(pred.error1,pred.error2))
}
>pred.error.mat
>colSums(pred.error.mat^2)
---------------------   
#한번에 
>n<-dim(mtcars)[1] 
>p<-2 
>r<-choose(n,p)
>index<-combn(n,p)
>pred.error.mat <- c()
>for(i in 1:r){
     test.set <- mtcars[index[,i],]
     training.set <- mtcars[-index[,i],]
     reg1<- lm(mpg~hp+wt, data=training.set)
     reg2<- lm(mpg~hp+wt+disp+qsec, data=training.set)
     pred.error1 <- test.set$mpg-predict(reg1, newdata=test.set)
     pred.error2 <- test.set$mpg-predict(reg2, newdata=test.set)
     pred.error.mat <- rbind(pred.error.mat, c(pred.error1,pred.error2))
   }  
> i<-1 
>pred.error.mat #M1 M2
   
#제곱합평균 
> acc1<-mean(rowSums(pred.error.mat[,1:2]^2))
> acc2<-mean(rowSums(pred.error.mat[,3:4]^2))
 ----------------------------
     
#k-fold cross-validation
> library(cvTools)   
   
> n<-dim(mtcars)[1]
> cvFolds(n,K=5,R=1,type="random")  #fold 번호 , index: 1번에 32번이 배정되었다 
> cvFolds(n,K=5,R=1,type="consecutive") #R바꿀 수 있음 
> cvFolds(n,K=5,R=1,type="interleaved")   
 
> cross<-cvFolds(n,K=5,R=1,type="random")  
> acc<- c()
> count <-1
> for(i in 1:5){
   test.set <- cross$subsets[which(cross$which==i),1]
   test.data<-mtcars[test.set,]
   train.data<-mtcars[-test.set,]
   reg<-lm(mpg~hp+wt, data=train.data)
   pred.error <- test.data$mpg-predict(reg,newdata = test.data)
   acc<-c(acc,mean(pred.error^2))
   }
> i<-1
   
>acc
>sum(acc) #이 값을 비교
#M2모형에서    
> for(i in 1:5){
     test.set <- cross$subsets[which(cross$which==i),1]
     test.data<-mtcars[test.set,]
     train.data<-mtcars[-test.set,]
     reg<-lm(mpg~hp+wt+disp+qsec, data=train.data)
     pred.error <- test.data$mpg-predict(reg,newdata = test.data)
     acc<-c(acc,mean(pred.error^2))
} 
   
> acc1<-sum(acc) 
> acc2<-sum(acc) #둘을 비교 
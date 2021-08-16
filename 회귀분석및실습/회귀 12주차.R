#능선 회귀분석 개념 

> r<-0.9
> R_xx <- matrix(c(1,r,r,1),nrow=2,ncol=2)
> beta <- c(1,1)
> sigma2 <- 1

> result<- c()
> ridge.par<-1 #능선모수 

>for(ridge.par in seq(0,1,by=0.01)){
  bias <- -ridge.par*solve(R_xx+ridge.par*diag(rep(1,2)))%*%beta #편의 
  var <- sigma2*solve(R_xx+ridge.par*diag(rep(1,2)))%*%R_xx%*%solve(R_xx+ridge.par*diag(rep(1,2))) #분산 
  result<- rbind(result,c(bias,diag(var)))
}
> result
#
> cbind(seq(0,1,by=0.01),result)
#그림 
> par(mfrow=c(1,3))
> a<-seq(0,1,by=0.01)
> plot(a, result[,1],type="1")
> plot(a, result[,3],type="1")

> mse<- rowSums(result[,1:2]^2)+rowSums(result[,3:4])
> plot(a, mse, type="1") # 그래프 순서대로 바이어스 존재, 분산이 줄어듬 , 결과는 능선회귀로 하면 분산을 줄일 수 있다. 
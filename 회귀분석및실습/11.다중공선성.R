> import<-read.table("C:/Users/Lee seohyun/Desktop/import.txt",header=TRUE)
> import<-import[,-1]
> plot(import) #다중공선성 확인 
> cor(import) #상관계수 (선형관계 확인 - 0.9972로 1에 가까우므로 선형관계가 강하다.)

> MF<- lm(import~.,data=import)
Call:
  lm(formula = import ~ ., data = import)

Coefficients: #추정값
  (Intercept)       doprod        stock       consum  
-10.1280      -0.0514       0.5869       0.2868  


> summary(MF)

Call:
  lm(formula = import ~ ., data = import)

Residuals:
  Min       1Q   Median       3Q      Max 
-0.52367 -0.38953  0.05424  0.22644  0.78313 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
   (Intercept) -10.12799    1.21216  -8.355  6.9e-05 ***
  doprod       -0.05140    0.07028  -0.731 0.488344   #-0.05140는 다중공선성 때문에 생기는 문제  
  stock         0.58695    0.09462   6.203 0.000444 ***
  consum        0.28685    0.10221   2.807 0.026277 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4889 on 7 degrees of freedom
Multiple R-squared:  0.9919,	Adjusted R-squared:  0.9884 
F-statistic: 285.6 on 3 and 7 DF,  p-value: 1.112e-07

> ols_coll_diag(MF) #다중공선성 판단 (VIF) / doprod, consum 다중공선성 문제 
> Ddrop<- lm(import~stock+consum, data=import) # doprod제거(p-value유의하지 않기 때문에 )
#제거 후 ols (제거: 독립변수들의 선형변환)

----------------
#주성분 분석

#1단계 : 주성분 분석

> pca <- prcomp(import[,-1],center=TRUE, scale=TRUE) #센터링:표준화변환  
> pca
Standard deviations (1, .., p=3):
  [1] 1.41391476 0.99907666 0.05187378 #주성분의 표준편차 

Rotation (n x k) = (3 x 3): #주성분을 만들 때 사용되는 행렬 
              PC1         PC2          PC3 
doprod 0.70633041 -0.03568867 -0.706982083
stock  0.04350059  0.99902908 -0.006970795
consum 0.70654444 -0.02583046  0.707197102  
        #v1        v2         v3

> summary(pca)
Importance of components:
                       PC1    PC2     PC3
Standard deviation     1.4139 0.9991 0.05187 #주성분의 표준편차 
Proportion of Variance 0.6664 0.3327 0.00090 #주성분의 설명력 
Cumulative Proportion  0.6664 0.9991 1.00000 #누적설명력 (90%넘는 것이 두개가 있으면 그 중 작은 것 가능한 적게 (작은것)선택
                                             #-주성분의 개수 2개(90%안넘는 것과 넘는 것 중 적은것)로 선택)

#주성분 점수 계산

> predict(pca)

> m<-2
> v<-pca$rotation[,1:m] #표준화 변환
> z<-scale(import[,-1])
> z%*%v  #앞의 predict와 같은 결과 

#2단계 : 회귀분석

> pc.score<-predict(pca)
> pc.import <- as.data.frame(cbind(scale(import$import),pc.score))
> pc.import

> pcr<-lm(V1~PC1+PC2+0,data=pc.import) #0:원점을 지나는 함수 
> pcr

Call:
  lm(formula = V1 ~ PC1 + PC2 + 0, data = pc.import)

Coefficients:
  PC1     PC2  
0.6900  0.1913  

#3단계:원 독립변수에 대한 식으로 변환 
#추정식 1차변환
> alpha<-pcr$coefficients
> V<-pca$rotation[,1:m]
> sbeta<-V%*%alpha
> sbeta
#추정식 2차변환 
> mean.import<-colMeans(import)
> sd.import<-sqrt(diag(cov(import)))

> beta<-sbeta*sd.import[1]/sd.import[-1]
> beta0<-mean.import[1]-sum(beta*mean.import[-1])

> rbind(beta0,beta)
import
beta0  -9.13010782
doprod  0.07277981
stock   0.60922012
consum  0.10625939

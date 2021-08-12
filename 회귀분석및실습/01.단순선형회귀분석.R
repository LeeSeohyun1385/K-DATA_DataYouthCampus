# 단순선형 회귀 분석

> x <- c(40,50,30,60,70,60,30,60,20,80)
> y <- c(87,108,69,135,148,132,73,128,50,170)

> out <- lm(y~x)  #최소제곱법에 의한 회귀직선 추정 

Call:
  lm(formula = y ~ x)

Coefficients:     #b0=10, b1=2 (최소제곱추정값)
  (Intercept)            x  
        10            2  
        

> plot(x,y)
> abline(out,col="blue") #최소제곱회귀직선

> out$coefficients
(Intercept)           x 
   10           2 
> haty <- out$coefficients[1] + out$coefficients[2]*x #y=10+2x
> haty
[1]  90 110  70 130 150 130  70 130  50 170 #계산된 값

> out$fitted.values #적합값
1   2   3   4   5   6   7   8   9  10 
90 110  70 130 150 130  70 130  50 170 

> out$residuals #잔차
1             2             3             4             5 
-3.000000e+00 -2.000000e+00 -1.000000e+00  5.000000e+00 -2.000000e+00 
6             7             8             9            10 
2.000000e+00  3.000000e+00 -2.000000e+00 -3.146788e-15  2.182282e-15 
----------------------------------------------------------------
#단순선형 회귀계수 가설검정 (t-검정)
> out <- lm(y~x)              #y=B0 + B1x + E
> summary(out) #분석결과 

Call:
  lm(formula = y ~ x)

Residuals:
  Min     1Q Median     3Q    Max 
-3.0   -2.0   -0.5    1.5    5.0 

Coefficients:                           #p-value 유의확률 1.02e-10 < a=0.05(95%신뢰구간) : 귀무가설 기각 
            Estimate Std. Error t value Pr(>|t|)  #t검정 b1-0/s(b1)=2-0/0.047=42  , t8(0.025)=2.306 < 42.583: 귀무가설 기각  
(Intercept) 10.00000    2.50294   3.995  0.00398 ** #b0
  x          2.00000    0.04697  42.583 1.02e-10 *** #b1
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
                       #루트MSE         #자유도 
Residual standard error: 2.739 on 8 degrees of freedom
Multiple R-squared:  0.9956,	Adjusted R-squared:  0.9951 #결정계수 R^2=0.9956 (값이 클수록 추정된 회귀식 설명력 높아짐)
F-statistic:  1813 on 1 and 8 DF,  p-value: 1.02e-10

> confint(out) #신뢰구간 
              2.5 %    97.5 %
  (Intercept) 4.228211 15.771789 #b0신뢰구간   #b1=0 vs b1/=0 에서 0이 포함되어 있지 않으므로 기각
x           1.891694   2.108306 #b1

#F-검정
> anova(out)
Analysis of Variance Table    

Response: y  # F(0.025)<1813 귀무가설 기각   
          Df Sum Sq Mean Sq F value   Pr(>F)    
x          1  13600 13600.0  1813.3 1.02e-10 *** #1.02e-10 < 0.05 귀무가설 기각 
Residuals  8     60     7.5                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
----------------------------------------
#예측 (Q: x=55를 지급하였을때 예상되는~추정) x가 자료 외의 값 가질때 
  > predict(out,newdata=data.frame(x=55),interval = "confidence") #신뢰구간
fit      lwr      upr
1 120 117.9308 122.0692
> predict(out,newdata=data.frame(x=55),interval = "prediction") #예측값
fit      lwr      upr
1 120 113.3544 126.6456
> predict(out,newdata=data.frame(x=55),interval = "none")
1 
120   
--------------------


#중회귀모형 (y=B0 + B1x1 + B2x2 +E)
> x <- c(40,50,30,60,70,60,30,60,20,80)
> x1 <- c(40,50,30,60,70,60,30,60,20,80)
> x2 <- c(15,20,14,22,30,24,16,20,14,32)
> y <- c(87,108,69,135,148,132,73,128,50,170)
> fit <- lm(y~x1+x2)
> fit

Call:
  lm(formula = y ~ x1 + x2)

Coefficients:
  (Intercept)           x1           x2  
      9.2304       1.9548       0.1463  

      
> summary(fit)

Call:
  lm(formula = y ~ x1 + x2)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.6178 -1.7846 -0.6493  1.4023  5.2617 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
  (Intercept)   9.2304     3.4508   2.675   0.0318 *  
  x1            1.9548     0.1388  14.080 2.16e-06 ***
  x2            0.1463     0.4197   0.349   0.7376    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.903 on 7 degrees of freedom
Multiple R-squared:  0.9957,	Adjusted R-squared:  0.9944 
F-statistic: 807.2 on 2 and 7 DF,  p-value: 5.288e-09



> confint(fit)  #신뢰구간 b1,b2각각에 대한 개별 검정  H0:B1=0 vs H1:B1=/0 
                 2.5 %    97.5 %                                     
  (Intercept)  1.0705181 17.390377  #b1,b2=0인지 아닌지 검정 
  x1           1.6265121  2.283104 #0이 포함되어 있지 않음 : 유의수준5%에서 기각
  x2          -0.8460383  1.138709 #0이 포함되어있음: 기각X 


> fitted(fit)
1         2         3         4         5         6         7         8 
89.61781 109.89757  69.92339 129.73832 150.45708 130.03099  70.21606 129.44565 
9        10 
50.37531 170.29784 
> residuals(fit)
1          2          3          4          5          6          7 
-2.6178062 -1.8975652 -0.9233891  5.2616822 -2.4570831  1.9690113  2.7839400 
8          9         10 
-1.4456468 -0.3753074 -0.2978357 
> anova(fit)
Analysis of Variance Table

Response: y
          Df Sum Sq Mean Sq   F value    Pr(>F)    
x1         1  13600 13600.0 1614.2255 1.542e-09 ***
x2         1      1     1.0    0.1216    0.7376    
Residuals  7     59     8.4                        
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#x1=60,x2=22에서 E(y0)의 신뢰구간
> predict(fit,data.frame(x1=60,x2=22),interval = "confidence")
fit      lwr     upr
1 129.7383 126.6977 132.779

---------------------------------------------------
# 다항회귀모형 y=B0 + B1x + B2x^2 + E

> y<-c(180,200,350,400,450,480,460,500,360,340)
> x<-c(10,10,20,20,30,30,40,40,50,50)
> plot(x,y)
> fit1<-lm(y~x)
> fit1

Call:
  lm(formula = y ~ x)

Coefficients:
  (Intercept)            x  
244.50         4.25  


#1차 y=B0+B1x+E , H0:B1=0 vs H1:B1=/0
> summary(fit1)

Call:
  lm(formula = y ~ x)

Residuals:
  Min      1Q  Median      3Q     Max 
-117.00  -94.50   33.00   76.12  108.00 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept)   244.50      71.63   3.413  0.00918 **
  x               4.25       2.16   1.968  0.08463 . #<0.05 - 기각
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 96.9 on 8 degrees of freedom
Multiple R-squared:  0.3262,	Adjusted R-squared:  0.2419 
F-statistic: 3.872 on 1 and 8 DF,  p-value: 0.08463

> abline(fit1,col="blue")


#2차  y=B0 + B1x + B2x^2 +E , H0:B2=0 vs H1:B2=/0
> fit2<-lm(y~x+I(x^2))  #2차다항회귀분석
> fit2

Call:
  lm(formula = y ~ x + I(x^2))

Coefficients:      #b0          b1
  (Intercept)            x       I(x^2)  
    -108.0000      34.4643      -0.5036  

> summary(fit2)

Call:
  lm(formula = y ~ x + I(x^2))

Residuals:
  Min      1Q  Median      3Q     Max 
-29.857 -13.786  -0.571  12.107  35.143 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
   (Intercept) -108.00000   34.54249  -3.127   0.0167 *  
  x             34.46429    2.63237  13.093 3.54e-06 ***
  I(x^2)        -0.50357    0.04304 -11.699 7.54e-06 *** #  <0.05 - 기각
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 22.78 on 7 degrees of freedom
Multiple R-squared:  0.9672,	Adjusted R-squared:  0.9578 
F-statistic: 103.3 on 2 and 7 DF,  p-value: 6.382e-06


> fit3<-lm(y~x+I(x^2)+I(x^3)) #3차다항회귀분석 
> fit3

Call:
  lm(formula = y ~ x + I(x^2) + I(x^3))

Coefficients:
  (Intercept)            x       I(x^2)       I(x^3)  
-38.000000    24.630952    -0.128571    -0.004167  

> fit2$fitted
1        2        3        4        5        6        7        8        9 
186.2857 186.2857 379.8571 379.8571 472.7143 472.7143 464.8571 464.8571 356.2857 
10 
356.2857 
> fit2$resid
1          2          3          4          5          6          7 
-6.285714  13.714286 -29.857143  20.142857 -22.714286   7.285714  -4.857143 
8          9         10 
35.142857   3.714286 -16.285714 

>cor(x,x^2)  #상관계수 
[1] 0.9811049

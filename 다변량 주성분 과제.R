#2-3(1)

> s<-matrix(c(10000,60,60,1),ncol=2) #공분산 행렬 
> s
[,1] [,2]
[1,] 10000   60
[2,]    60    1


r <- cov2cor(s) #공분산행렬을 이용한 상관계수 행렬

#(2)
> eigen(s)   #고유값과 고유벡터 구하기 
eigen() decomposition
$values
[1] 10000.360023     0.639977

$vectors
             [,1]         [,2]
[1,] -0.999981998  0.006000276
[2,] -0.006000276 -0.999981998

> V<-(-1)*eigen(s)$vectors #고유벡터를 양수로 만들어주기

> prcomp(s, center=TRUE, scale=FALSE)
> s.pca<- prcomp(s, center=TRUE, scale=FALSE)
> summary(s.pca)
Importance of components:
  PC1       PC2
Standard deviation     7029 3.943e-15
Proportion of Variance    1 0.000e+00
Cumulative Proportion     1 1.000e+00

#(3)
> eigen(r)
eigen() decomposition
$values
[1] 1.6 0.4

$vectors
[,1]       [,2]
[1,] 0.7071068 -0.7071068
[2,] 0.7071068  0.7071068

> prcomp(r, center=TRUE, scale=TRUE)
Standard deviations (1, .., p=2):
  [1] 1.414214e+00 4.796525e-16

Rotation (n x k) = (2 x 2):
  PC1        PC2
[1,] -0.7071068 -0.7071068
[2,]  0.7071068 -0.7071068
> r.pca <- prcomp(r, center=TRUE, scale=TRUE)
> summary(r.pca)
Importance of components:
  PC1       PC2
Standard deviation     1.414 4.797e-16
Proportion of Variance 1.000 0.000e+00
Cumulative Proportion  1.000 1.000e+00

#2.4
> s<-matrix(c(5,0,0,0,1,0,0,0,2),ncol=3)
> s
[,1] [,2] [,3]
[1,]    5    0    0
[2,]    0    1    0
[3,]    0    0    2


> eigen(s)
eigen() decomposition
$values
[1] 5 2 1

$vectors
[,1] [,2] [,3]
[1,]    1    0    0
[2,]    0    0    1
[3,]    0    1    0

> prcomp(s, center=TRUE, scale=FALSE)
Standard deviations (1, .., p=3):
  [1] 2.962249e+00 1.106835e+00 2.096692e-16

Rotation (n x k) = (3 x 3):
  PC1        PC2       PC3
[1,] -0.97290405  0.1498331 0.1760902
[2,]  0.08759358 -0.4659759 0.8804509
[3,]  0.21397446  0.8720186 0.4402255

> s.pcr <- prcomp(s, center=TRUE, scale=FALSE)
> summary(s.pcr)
Importance of components:
  PC1    PC2       PC3
Standard deviation     2.9622 1.1068 2.097e-16
Proportion of Variance 0.8775 0.1225 0.000e+00
Cumulative Proportion  0.8775 1.0000 1.000e+00

#2-5
> data <- read.table("C:/Users/이서현/Desktop/data.txt",header=TRUE)
> data
pop school employ service house
1  5700   12.8   2500     270 25000
2  1000   10.9    600      10 10000
3  3400    8.8   1000      10  9000
4  3800   13.6   1700     140 25000
5  4000   12.8   1600     140 25000

#(1)
> s<-cov(data)
> s
pop      school       employ     service
pop     1.183356e+07   59.924242 4152121.2121 173507.5758
school  5.992424e+01    3.191742     342.1212    141.9621
employ  4.152121e+06  342.121212 1540606.0606  73424.2424
service 1.735076e+05  141.962121   73424.2424  13208.3333
house   4.909091e+05 9818.181818  963636.3636 569090.9091
house
pop       490909.091
school      9818.182
employ    963636.364
service   569090.909
house   40545454.545

> prcomp(s, center=TRUE, scale=FALSE)
Standard deviations (1, .., p=5):
  [1] 1.798476e+07 5.070358e+06 2.777658e+04 1.022879e+03 2.244191e-13

Rotation (n x k) = (5 x 5):
  PC1          PC2          PC3          PC4
pop     -0.0939226686 9.390735e-01  0.330472151  0.010354390
school   0.0002407736 1.778398e-05 -0.001993497 -0.000500138
employ  -0.0138515632 3.305743e-01 -0.943620026  0.010318508
service  0.0124523670 1.440522e-02 -0.006068085 -0.999800151
house    0.9954052175 9.302732e-02  0.018127548  0.013628056
PC5
pop     -0.0006698889
school  -0.9999978588
employ   0.0018784909
service  0.0005153903
house    0.0001983690

> s.pcr <- prcomp(s, center=TRUE, scale=FALSE)
> summary(s.pcr)
Importance of components:
  PC1       PC2   PC3  PC4       PC5
Standard deviation     1.798e+07 5.070e+06 27777 1023 2.244e-13
Proportion of Variance 9.264e-01 7.363e-02     0    0 0.000e+00
Cumulative Proportion  9.264e-01 1.000e+00     1    1 1.000e+00

#(2)
> prcomp(s, center=TRUE, scale=TRUE) #scale = TRUE 해줌으로써 표준화 주성분 분석 실시 
Standard deviations (1, .., p=5):
  [1] 1.746121e+00 1.396709e+00 1.597573e-02 3.093809e-03 1.034261e-16

Rotation (n x k) = (5 x 5):
  PC1        PC2        PC3         PC4        PC5
pop     -0.2489400 0.64476886  0.4606021  0.30655155  0.4649450
school   0.5678705 0.09256412 -0.5353056  0.16824572  0.5950599
employ  -0.1451412 0.69256569 -0.5479918 -0.08783247 -0.4373526
service  0.5219752 0.29455270  0.3617470 -0.71389176 -0.0166788
house    0.5674750 0.09641815  0.2648352  0.60030287 -0.4880312
> n.pcr<-prcomp(s, center=TRUE, scale=TRUE) 
> summary(n.pcr)
Importance of components:
  PC1    PC2     PC3      PC4       PC5
Standard deviation     1.7461 1.3967 0.01598 0.003094 1.034e-16
Proportion of Variance 0.6098 0.3902 0.00005 0.000000 0.000e+00
Cumulative Proportion  0.6098 1.0000 1.00000 1.000000 1.000e+00
 #(3)
#표준화 하기전에는 소수점이 복잡한 PC1등이 나오지만 표준화를 한 후에는 간단한 숫자가 나온다. 
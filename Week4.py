##############################################################
""" Week 4 """
##############################################################

import numpy as np

# Algorithm 3.1: p.36

#구간분할법으로 최솟값 찾기
#3개의 구간으로 구간 나눠 구간을 줄여가며 최적점 찾기
#초기 구간 탐색

#구간의 상한 하한 결정
a,c = 4,3
(a,c) if a < c else (c,a)

a,c=3,4
(a,c) if a<c else (c,a)

---------------------------------------
#s or K 가 초모수 
x=0
f=(x**5)-(8*x**3)+(10*x)+6
def f(x):
    return x**5 - 8*(x**3) + 10*x + 6

df=(5*x**4)-(24*x**2)+10
def df(x):
    return (5*x**4)-(24*x**2)+10


              #f:함수 x:임의의 점 ,s:왼쪽으로 변화되는 값 k:크게 이동하는 값 결정
def bracket_minimum(f, x, s = 1E-2, k = 2.0): #구간결정 알고리즘,2만큼 이동
    a, ya = x, f(x)
    b, yb = a + s, f(a + s)#a를 s만큼 오른쪽으로 이동한 값 설정
    
    print('init: (a:%.4f, b:%.4f) (ya:%.4f, yb:%.4f)' % (a, b, ya, yb))
     
    if yb > ya:
        a, b = b, a
        ya, yb = yb, ya
        s = -s #줄여나가는 값  #s의 부호를 바꿔줌 

    while True: #무한루프(return이 일어나면 종료)
        c, yc = b + s, f(b + s)
        print('step: (a:%.4f, b:%.4f, c:%.4f) (ya:%.4f, yb:%.4f, yc:%.4f)' % (a, b, c, ya, yb, yc))
        
        if yc > yb: #만약 c>b이면
            return (a, c) if a < c else (c, a) #a,c라는 구간을 갖고 그렇지 않으면 c,a구간을 갖는다 
        else: #위: 구간의 상한 하한 
            a, ya, b, yb = b, yb, c, yc #a하고 b의 위치를 b하고c위치로 이동 
            s *= k #k만큼 이동을 시켜줌
        

bracket_minimum(f, -1) #초기구간 결정 
bracket_minimum(f, 1)

-------------------------------------
#삼분할 탐색법 
def trifold_search(f,x,epsilon=1E-6):
    a,b=bracket_minimum(f,x)
    print('init: (a:%.4f, b:%.4f)'%(a,b))
    distance=abs(a-b) #구간폭 설정
    while distance > epsilon:
        x1=a+(1.0/3.0)*distance
        x2=a+(2.0/3.0)*distance
        y1,y2=f(x1),f(x2)
        
        if y1>y2:
            a,b=x1,b #a를 x1으로 바꾸어줌 구간폭이 1/3줄어들었다는 뜻
        else:
            a,b=a,x2
        distance=abs(a - b)
        
        print('%d:(a:%.4f, b:%.4f)' % (i, a, b))
        i += 1
        
    x = 0.5*abs(a-b)
    y = f(x)
    return x, y

trifold_search(f,-1) #초기 x값을 -1로 준 것 
#반복수, 최적점, 목적함수의 값 
trifold_search(f,1) 



    
-----------------------------
#피보나치 탐색법 (Fn)
φ=1
def fibonacci_search(f, a, b,n, ϵ=0.01):
    s = (1-5**1/2)/(1+5**1/2)
    ρ = 1 / (φ*(1-s*(n+1))/(1-s*n))
    d = ρ*b + (1-ρ)*a
    yd = f(d)
    for i in range(1) :
        n-1
    
        if i == n-1:
            c = ϵ*a + (1-ϵ)*d
        else:
            c = ρ*a + (1-ρ)*b
        
        yc = f(c)
        if yc < yd:
            b, d, yd = d, c, yc
        else:
            a, b = b, c
        
        ρ = 1 / (φ*(1-s*(n-i+1))/(1-s*(n-i)))
    
    return  (a, b) if a < b else (b, a)

fibonacci_search(f, 0, 1,2)


-------
            
from IPython.display import Image
Image("./golden_section.png", width = 640)#, height = 300)

-------------------------------
#황금분할 탐색법 : 삼분할법 : 최소값  lim Fn/Fn-1 = uㅣ 

# Algorithm 3.3: p.41           허용오차(허용범위)
def golden_section_search(f, x, epsilon = 1E-6):

    a, b = bracket_minimum(f, x)
    print('init:(a:%.4f, b:%.4f)' % (a, b))#구간 하한,상한 결정

    distance = abs(a - b) #구간a,b거리 계산
    
    psi = 0.5 * (1. + np.sqrt(5))
    rho = psi ** (- 1) #황금비율 역수 계산 
  
    d = rho * b + (1. - rho) * a
    yd = f(d)
 
    i = 1 #몇번반복 
    while distance > epsilon:  #내가 설정한 구간을 벗어나면 반복을 멈추게 

        c = rho * a + (1. - rho) * b
        yc = f(c)
  
        if yc < yd: #비교 수행 
            b, d, yd = d, c, yc
        else:
            a, b = b, c

        pa, pb = (a, b) if a < b else (b, a) 
        print('%d:(a:%.4f, b:%.4f)' % (i, pa, pb))#계산결과 출력

        distance = abs(a - b) #한번 더 계산해줘야 실행이 멈춤 (아니면 무한루프)
 
        i += 1 #진행되는 변수 설정
        
    a, b = (a, b) if a < b else (b, a)
   
    x = 0.5 * (a + b) #최솟값의 근사값 
    y = f(x) #목적함수 계산 
    
    return x, y

golden_section_search(f, -1)
golden_section_search(f, 0)
--------------------------------

"""
def f(x):
    return 0.5 * x ** 2 - x

def df(x):
    return x - 1
"""

f = lambda x: 0.5 * x ** 2 - x
df = lambda x: x-1

---------------------------------------


import matplotlib.pyplot as plt
#from matplotlib import pyplot as plt
------------

x = np.arange(-3., 3., 1E-2)

plt.title('f(x) = (x**5) - (8*x**3) + (10*x) + 6') 
plt.plot(x, f(x))
plt.xlabel('x')
plt.ylabel('f(x)')
plt.show()
-------------- 

plt.title("f`(x) = x - 1") #도함수 그리기 
plt.plot(x, df(x))
plt.axhline(y = 0, color = 'orange')
plt.axvline(x = 1, color = 'orange')
plt.show()
-----------------




# algorithm 3.7: p.50
def bracket_sign_change(df, a, b, k = 2.): #df는 도함수 ,a,b,k는 초모수 바꿔주며 최솟값 설정 
 
    if a > b: #a가b보다 작아야 하기 때문에 실행해주는 것 
        a, b = b, a
 
    center, half_width = 0.5 * (b + a), 0.5 * (b - a)
      
    while df(a) * df(b) > 0: #0보다 클때 까지 
        
        half_width *= k
        
        a = center - half_width #half width만큼 구간을 벌려주면 근이 존재(최소값 존재)
        b = center + half_width

    return (a, b)

bracket_sign_change(df, 0, 0.1) #a,b는 비교적 가까운 값을 주어줌 
#도함수가 0이되는 근이 결과의 구간에 존재하니까 목적함수 f의 최솟값이 포함되는 구간
#도함수가 0이되게하는 근이 존재하니까 목적함수 f의 최솟값이 된다. 
-----------
#이분법 
#함수 f: [a,b]에서 연속/ yㅌ [f(a),f(b)]에 대하여 f(x)=y 인 xㅌ [a,b]
# algorithm 3.6: p.50
def bisection(df, x, epsilon = 1E-6): #목적함수 아닌 도함수
    
    a, b = bracket_sign_change(df, x - epsilon, x + epsilon)#x라는 값을 주면 구간을 좁혀줌
    print('init:(a:%.4f, b:%.4f)' % (a, b))

    ya, yb = df(a), df(b) #도함수 값 계산 

    if ya == 0: #예외처리 (도함수 값이 0:계산할 필요 없음)
        b = a
    if yb == 0:
        a = b

    i = 1
    while b - a > epsilon: #a<b , 구간ab의 길이가 ep보다 크면 구간을 줄이고 아니면 구간 주어주고 멈추겠다
        
        x = 0.5 * (a + b)
        y = df(x)
   
        if y == 0: #비교 
            a, b = x, x
        elif y * ya > 0:
            a = x
        else:
            b = x

        print('step %d - a:%.4f, b:%.4f, y:%.4f, ya:%.4f' % (i, a, b, y, ya))
        
        i += 1
  
        x = 0.5 * (a + b)
        y = df(x)
  
    return x, y


bisection(df, 0)












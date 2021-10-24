# -*- coding: utf-8 -*-
"""
Created on Fri Dec  4 23:59:10 2020

@author: 이서현
"""

x=-1
dx=0.001

f=x**2
df = 2*x

f_dx=f + df * dx

print((f,f_dx))

for dx in [1E-3, 1E-4, 1E-5, 1E-8]:
    f_dx = f + df *dx
    print((f,f_dx))
    
    
def f(x):
    return x ** 2

def df(x):
    return 2*x

def f_dx(x,dx):
    return f(x) + df(x) * dx

for dx in [1E-3, 1E-4, 1E-5, 1E-6]:
    print('%.4f, %.4f (%.4f)' % (f(-1),f_dx(-1,dx),abs(f(-1)-f_dx(-1,dx))))
    

###수치미분
#전방차분
def forward_diff(f,x,h=1E-6):
    df_=(f(x+h)-f(x))/h
    return df_

forward_diff(f, -1)

#후방차분

def backward_diff(f, x, h= 1E-6):
    df_=(f(x)-f(x-h))/h
    return df_

backward_diff(f,-1)

#중심차분
def central_diff(f, x, h = 1E-6):
    df_=(f(x+0.5*h)-f(x-0.5*h))/h
    return df_

central_diff(f,-1)

for h in [1E-4,1E-6,1E-8]:
    cf=central_diff(f,-1,h) #수치미분된 도함수 값
    print('df:%.8f, cf:%.8f(%.8f)' %(df(-1), cf, abs(df(-1)-cf)))
    
#x변화에 따른 도함수 변화 
    
for x in [-1E0,-5E-1,-2.5E-1, -1E-2, -1E-6]:
    cf=central_diff(f,x) #수치미분된 도함수 값
    print('df:%.8f, cf:%.8f(%.8f)' %(df(x), cf, abs(df(x)-cf)))
    
------------------------------------------

#구간분할법으로 최솟값 찾기
#3개의 구간으로 구간 나눠 구간을 줄여가며 최적점 찾기
#초기 구간 탐색

#구간의 상한 하한 결정
a,c = 4,3
(a,c) if a < c else (c,a)

a,c=3,4
(a,c) if a<c else (c,a)


    
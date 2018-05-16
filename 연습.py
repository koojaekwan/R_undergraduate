# -*- coding: utf-8 -*-
"""
Created on Sat Mar 31 23:56:58 2018

@author: koojaekwan
"""

def largestodd(L):
    L=[i for i in L if i%2==1]
    if L==[]:
        return 'no odd'
    else:
        return max(L)
L=[2,4,6] 



x=6
L=[y for y in range(1,x) if x%y==0]
if sum(L)==x:
    print sum(L)
    
    
    
x=27
z=0
for i in range(1,x):
    if x%i==0:
        z+=i
if z==x:
    print x
else:
    print "fuck you"



x=496
i=1
z=0
while i<x:
    if x%i==0:
        z+=i
    i+=1
if z==x:
    print x
else:
    print "fuck you"
    
    
    
    
    
'''2. 다음 방정식의 모든 정수해를 찾아라. 해는 음수일 수도 있음
x**5−x**4−33x**3+61x**2+32x−60=0 '''

for x in range(-100000,100000):
    if x**5-x**4-33*x**3+61*x**2+32*x-60==0:
        print(x)
    x-=1
    
K=[x for x in range(-1000000,10000000) if x**5-x**4-33*x**3+61*x**2+32*x-60==0]

'''3. 킬로미터를 미터로, 미터를 킬로미터로 변환하는 프로그램을 작성하라.
예를 들어 1500m을 입력하면, 1.5km을 출력하고, 2.3km을 입력하면 2300m를 출력하게 하라.'''

x=input('')
x=str(x)
y=len(x)
z=x[y-2]
print(x,y,z)
if z=='k':
    print(x[0]+x[2]+'00m')
else:
    print(x[0]+'.'+x[1]+'km')


x=input('')
x=float(x)
y=input('')
y=str(y)
print(x,y)
if y =='m':
    a=x*0.001
    a=str(a)
    print(a+'km')
else:
    b=x*1000
    b=str(b)
    print(b+'m')
    
    
    
        
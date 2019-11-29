MLE of normal distribution using newton raphson method in r
================
Jae Kwan Koo

-   [Newton Raphson](#newton-raphson)
    -   [loglikelihood function](#loglikelihood-function)

Newton Raphson
--------------

``` r
newton<-function(f,df,tol=1e-8,a0=1,x,N=10){
  i<-0
  a1<-a0

  while(i<=N){
    a1<-(a0-(f(a0,x,length(x))/df(a0,length(x))))
    i<-i+1
  
    
    if(abs((a1-a0)/a0)<tol) break;
    a0<-a1
  }
  
  mle<-c(a1, sum((x-a1)^2)/n)
  return(mle)
}
```

### loglikelihood function

``` r
f<-function(a,x,n) {
  b<-sum((x-a)^2)/n
  sum((x - a))/(b)
}

df<-function(a,n) {
  b<-sum((x-a)^2)/n
  -n/b
}
```

아래는 각 loglikelihood function을 a(=mu)에 대해 한번, 두번 미분한 함수이다.
*μ*를 a라고 두었고, *σ*<sup>2</sup>를 b라고 두었다.

``` r
set.seed(2019)
x<-rnorm(100, mean=10, sd=5)

n<-length(x)
a0<-100

a<-a0
bhat<-sum((x-a0)^2)/n


newton(f,df,x=x)
```

    ## [1]  9.63333 20.29119

b(=sigma^2)는 a(=mu)에 대한 식으로 표현가능하다. 따라서 MLE를 구할 때, a에 대한 MLE를 구하면 b에 대한 MLE는 a의 MLE를 대입하여 구할 수 있다. 즉, 1-dimension으로 생각할 수 있다.

``` r
set.seed(2019)
mle<-matrix(0,nrow=100,ncol=2)

for(i in 1:100){
  mle[i,]<-newton(f,df,x=rnorm(100, mean=10, sd=5))
}

apply(mle,2,mean)
```

    ## [1]  9.838331 24.761583

``` r
mse_mu<-sum((mle[,1]-10)^2)/length(x)
mse_sigma2<-sum((mle[,2]-25)^2)/length(x)

c(mse_mu, mse_sigma2)
```

    ## [1]  0.2910025 15.4193797

반복횟수로 나눠준 rmse이다. mu의 rmse가 더 작은 것을 볼 수 있다.

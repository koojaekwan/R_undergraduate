hit or miss, 표본평균법, 주표본기법 실습
================
jae kwan koo

-   [※ 모의실험을 위한 seed값은 20190926으로 사용할 것.](#모의실험을-위한-seed값은-20190926으로-사용할-것.)
-   [1. 확률변수 의 분포함수는 다음과 같다. 분포함수의 역함수를 이용하여 난수를 생성하는 알고리즘을 구성하여 모의실험을 하고 코드와 결과를 작성하시오.](#확률변수-의-분포함수는-다음과-같다.-분포함수의-역함수를-이용하여-난수를-생성하는-알고리즘을-구성하여-모의실험을-하고-코드와-결과를-작성하시오.)
    -   [선형합동법을 이용한 표준균일분포 난수 생성](#선형합동법을-이용한-표준균일분포-난수-생성)
    -   [역변환법을 이용한 분포함수를 따르는 X의 확률변수 값.](#역변환법을-이용한-분포함수를-따르는-x의-확률변수-값.)
-   [2. 다음의 정적분 값을 Hit or Miss 방법, 표본평균법, 주표본기법을 사용하여 추정하고자 한다. 정적분 값을 계산하는 알고리즘을 구성하여 모의실험을 하고 코드와 결과를 작성하시오. 또한 두 방법의 결과를 비교 설명하시오.](#다음의-정적분-값을-hit-or-miss-방법-표본평균법-주표본기법을-사용하여-추정하고자-한다.-정적분-값을-계산하는-알고리즘을-구성하여-모의실험을-하고-코드와-결과를-작성하시오.-또한-두-방법의-결과를-비교-설명하시오.)
    -   [Hit or Miss](#hit-or-miss)
    -   [표본평균법](#표본평균법)
    -   [주표본기법](#주표본기법)
    -   [Hit or Miss](#hit-or-miss-1)
    -   [표본평균법](#표본평균법-1)
    -   [주표본기법](#주표본기법-1)

#### ※ 모의실험을 위한 seed값은 20190926으로 사용할 것.

### 1. 확률변수 의 분포함수는 다음과 같다. 분포함수의 역함수를 이용하여 난수를 생성하는 알고리즘을 구성하여 모의실험을 하고 코드와 결과를 작성하시오.

*F*(*x*)=(*x* − *a*)/(*b* − *a*),*a* ≤ *x* ≤ *b*

먼저 seed값을 고정시켜 추후 같은 작업 시에도 같은 결과를 얻을 수 있게 하였다.

``` r
set.seed(20190926)
```

#### 선형합동법을 이용한 표준균일분포 난수 생성

``` r
a<-2; c<-5; x0<-10;m<-100000
sd_uniform<-function(n){
  x<-x0
  for (i in 1:n){
    x[i+1]<-(a*x[i]+c)%%m
    u<-x/m}
  return(u)
}

hist(sd_uniform(100000),freq=F)  #u~U(0,1)인 난수
```

<img src="선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

``` r
knitr::include_graphics('https://raw.githubusercontent.com/koojaekwan/jaekwan-s-R/master/Computerized_Statistics/Chapter3/선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-2-1.png')
```

<img src="https://raw.githubusercontent.com/koojaekwan/jaekwan-s-R/master/Computerized_Statistics/Chapter3/선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

runif()함수를 사용하지 않고, 선형합동법을 이용해 표준균일분포를 따르는 난수를 발생시킬 수 있다.
적당히 큰 m을 설정할 수 있으면 다양한 주기의 난수를 발생시킬 수 있다.
histogram을 보면 0~1에서 같은 확률을 가지는 것을 확인할 수 있다.

#### 역변환법을 이용한 분포함수를 따르는 X의 확률변수 값.

``` r
inverse_method<-function(a,b,n){
  u<-sd_uniform(n)
  x<-(b-a)*u+a
  return(x)
}

hist(inverse_method(0,10,100000),freq=F)  #x~U(a,b)이므로 다음과 같은 hist생성 가능
```

<img src="선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r
knitr::include_graphics('https://raw.githubusercontent.com/koojaekwan/jaekwan-s-R/master/Computerized_Statistics/Chapter3/선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-3-1.png')
```

<img src="https://raw.githubusercontent.com/koojaekwan/jaekwan-s-R/master/Computerized_Statistics/Chapter3/선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

표준균일분포를 따르는 난수를 통해 역변환법을 실행하였다.
X~U(a,b)이므로 a,b의 값을 설정한다면 a와 b사이에서 확률변수 값들은 일정한 확률을 가질 것이다.

### 2. 다음의 정적분 값을 Hit or Miss 방법, 표본평균법, 주표본기법을 사용하여 추정하고자 한다. 정적분 값을 계산하는 알고리즘을 구성하여 모의실험을 하고 코드와 결과를 작성하시오. 또한 두 방법의 결과를 비교 설명하시오.

1.  ∫<sub>0</sub><sup>2</sup>*x*<sup>2</sup>*d**x*

#### Hit or Miss

``` r
hit_miss_1 <- function(n) {
  k <- 0
  for (i in 1:n) {
    x<-runif(1,min=0,max=2)
    y<-runif(1,min=0,max=4)
    if (y<=x^2) {
      k<-k+1
    }
  }
  return(k/n)
}
8*hit_miss_1(10000)
```

    ## [1] 2.7032

x는 0에서 2까지, y는 0에서 4까지 사각형 내의 범위에서 균일분포의 난수를 발생시켰고, 전체 점의 갯수 대비 이 함수 아래 면적에 찍히는 점의 갯수의 비를 구하였다. 이 것은 확률을 의미하는데, 적분 값을 구해야 하므로 사각형의 면적을 확률 값에 곱해 정적분의 근사치를 구할 수 있다.

``` r
est<-rep(0,100)
for(i in 1:100){
  est[i]<-8*hit_miss_1(10000)
}

mean(est); sd(est); summary(est)
```

    ## [1] 2.666328

    ## [1] 0.04016678

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.584   2.638   2.669   2.666   2.692   2.754

위의 정적분 구하는 과정을 100번 반복하여 평균, 표준편차를 알아보자.
반복적으로 모의실험을 했을 때, 이 알고리즘의 성능을 다른 알고리즘과 비교할 수 있다.
잘 추정한 두 알고리즘에 대해서는 분산이 작은 것이 성능이 좋다.

##### Visualization

``` r
n<-20000

x1<-runif(n, min =0 , max =2 )
y1<-runif(n, min =0 , max =4 )
f1<-x1^2

plot(x1[(y1 > f1)],y1[(y1 > f1)],col='yellow',pch=20)
points(x1[(y1 <= f1)],y1[(y1 <= f1)],col='blue',pch=20)

f<-function(x) x^2
curve(f,0,2,n=100,col='red',lwd=5,add=TRUE)
```

<img src="선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
below<-length(y1[(y1<=f1)])
below/n                    #  = below/(below+above)
```

    ## [1] 0.3304

``` r
above<-length(y1[(y1>f1)])

8*below/n
```

    ## [1] 2.6432

``` r
knitr::include_graphics('https://raw.githubusercontent.com/koojaekwan/jaekwan-s-R/master/Computerized_Statistics/Chapter3/선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-6-1.png')
```

<img src="https://raw.githubusercontent.com/koojaekwan/jaekwan-s-R/master/Computerized_Statistics/Chapter3/선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

시각적으로도 확인할 수 있다.

#### 표본평균법

``` r
g<-function(x) x^2
f<-function(x) 1/2

n<-10000
out1<-numeric(100)

for(i in 1:100){
  u<-runif(n,0,1)
  x<-0+u*(2-0)
  out1[i]<-mean(g(x)/f(x))     
}
mean(out1); sd(out1)
```

    ## [1] 2.665576

    ## [1] 0.02427419

대체로 잘 맞아 떨어지는 모습을 보이고 있다. 분산은 Hit or Miss 방법과 비교했을 시 더 작다.

#### 주표본기법

``` r
g<-function(x) x^2
f<-function(x) (3/8)*x^2

n<-10000
out2<-numeric(100)

for(i in 1:100){
  x<-runif(n,0,2)
  out2[i]<-mean(g(x)/f(x))     
}                

mean(out2); sd(out2)
```

    ## [1] 2.666667

    ## [1] 0

테일러 급수 전개를 한 후, 0을 대입하였다. 또한, 이 함수는 2번째 terms까지만 사용한 것을 f(x)로 이용하였다.

``` r
apply(cbind(est,out1,out2), 2, summary)
```

    ##              est     out1     out2
    ## Min.    2.584000 2.623070 2.666667
    ## 1st Qu. 2.637600 2.649598 2.666667
    ## Median  2.669200 2.664659 2.666667
    ## Mean    2.666328 2.665576 2.666667
    ## 3rd Qu. 2.691800 2.683757 2.666667
    ## Max.    2.754400 2.732303 2.666667

``` r
apply(cbind(est,out1,out2), 2, sd)
```

    ##        est       out1       out2 
    ## 0.04016678 0.02427419 0.00000000

주표본기법은 여기서 추정량이 상수에 해당되어 모순에 해당한다. 따라서 나머지 방법 중 표본평균법이 Hit or Miss보다 분산이 작으므로 표본평균법이 더 나아보인다.

1.  ∫<sub>0</sub><sup>1</sup>*e*<sup>*x*</sup>*d**x*

#### Hit or Miss

``` r
hit_miss_2 <- function(n) {
  k <- 0
  for (i in 1:n) {
    x<-runif(1,min=0,max=1)
    y<-runif(1,min=0,max=exp(1))
    if (y<=exp(x)) {
      k<-k+1
    }
  }
  return(k/n)
}
exp(1)*hit_miss_2(10000)
```

    ## [1] 1.723119

위의 알고리즘을 100번 반복하여 평균과 표준편차를 계산할 수 있다.

``` r
est2<-rep(0,100)
for(i in 1:100){
  est2[i]<-exp(1)*hit_miss_2(10000)
}

mean(est2); sd(est2); summary(est2)
```

    ## [1] 1.718403

    ## [1] 0.01329894

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.680   1.709   1.719   1.718   1.729   1.750

##### Visualization

``` r
n<-20000

x1<-runif(n, min =0 , max =1 )
y1<-runif(n, min =0 , max =exp(1) )
f1<-exp(x1)

plot(x1[(y1 > f1)],y1[(y1 > f1)],col='yellow',pch=20)
points(x1[(y1 <= f1)],y1[(y1 <= f1)],col='blue',pch=20)

f<-function(x) exp(x)
curve(f,0,1,n=100,col='red',lwd=5,add=TRUE)
```

<img src="선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

``` r
below<-length(y1[(y1<=f1)])
above<-length(y1[(y1>f1)])

below/n         #probability
```

    ## [1] 0.6345

``` r
exp(1)*below/n
```

    ## [1] 1.72475

``` r
knitr::include_graphics('https://raw.githubusercontent.com/koojaekwan/jaekwan-s-R/master/Computerized_Statistics/Chapter3/선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-12-1.png')
```

<img src="https://raw.githubusercontent.com/koojaekwan/jaekwan-s-R/master/Computerized_Statistics/Chapter3/선형합동법,-Hit-or-Miss,-표본평균법,-주표본기법_files/figure-markdown_github/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

마찬가지로, 시각적으로도 쉽게 확인할 수 있다.

#### 표본평균법

``` r
g<-function(x) exp(x)
f<-function(x) 1

n<-10000
out1<-numeric(100)

for(i in 1:100){
  u<-runif(n,0,1)
  x<-0+u*(1-0)
  out1[i]<-mean(g(x)/f(x))
}           

mean(out1); sd(out1); summary(out1)
```

    ## [1] 1.719198

    ## [1] 0.004708451

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.707   1.716   1.719   1.719   1.722   1.731

#### 주표본기법

``` r
g<-function(x) exp(x)
f<-function(x) (3/5)*(1+x+(1/2)*x^2)

n<-10000
out2<-numeric(100)

for(i in 1:100){
  x<-runif(n,0,1)
  out2[i]<-mean(g(x)/f(x))     
}          

mean(out2); sd(out2); summary(out2)
```

    ## [1] 1.707602

    ## [1] 0.0004658782

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.707   1.707   1.708   1.708   1.708   1.709

테일러 급수 전개를 3번째 terms까지 한 후 f(x)로 두었다.

``` r
apply(cbind(est2,out1,out2), 2, summary)
```

    ##             est2     out1     out2
    ## Min.    1.680442 1.706803 1.706555
    ## 1st Qu. 1.708984 1.715749 1.707277
    ## Median  1.719177 1.719499 1.707519
    ## Mean    1.718403 1.719198 1.707602
    ## 3rd Qu. 1.729235 1.722267 1.707953
    ## Max.    1.750030 1.731183 1.708731

``` r
apply(cbind(est2,out1,out2), 2, sd)
```

    ##         est2         out1         out2 
    ## 0.0132989369 0.0047084513 0.0004658782

주표본기법이 표준편차가 다른 것들에 비해 더 작지만, 1.718282...이라는 값을 잘 추정하지는 못했다.
아마도 테일러 급수 전개를 3번째 항까지만 하여 잘 추정되지 않은 것 같다.
표본평균법이 값을 다른 것들보다 더 잘 추정하였고, 분산이 더 작으므로 합당한 방법으로 보인다.

chapter 5. bootstrap
================
Jae Kwan Koo

-   [bootstrap](#bootstrap)
-   [bootstrap algorithm](#bootstrap-algorithm)
    -   [Example)](#example)
        -   [1.](#section)
        -   [2.](#section-1)
        -   [3.](#section-2)

bootstrap
---------

들고 있는 표본만의 분포 , 표본분포를 가지고 이야기 해보자. 직접적인 경험을 통해 얻은 분포이므로 경험분포라고 한다. 여기서는 경험누적분포함수를 이야기해 보자. 이 것을 줄여서 그냥 `분포함수`라고 한다.
말 그대로 내가 들고 있는 표본만으로 말하게 된다. 모집단에 대해서는 이야기 하지 않는다. (empirical)

누적분포함수는 도메인 전체에 대해 확률이 1이된다. 발생확률이 같으려면 1/n이라는 확률을 가지고 있어야 한다. 즉, x1,...xn까지 1/n의 확률을 가짐. indicator 함수로 나타내어 갯수를 더해가게 된다.

경험분포함수에 n을 곱하면 n이 약분되어 summation 지시함수가 된다.
1개가 될수도 있고, 심지어는 0개가 될 수도 있다. 0부터 n까지 나타나게 된다.

nF(x)는 이항분포를 따르는데 추정을 할 수 있다. (n은 상수값)
이 추정량에 대한 기댓값이 모수의 특정한 값으로 나타나게 되면 `불편추정량`이라고 한다. 추정량은 표본만 주어져 있다면 계산가능하다. 계산되는 값이 결국 우리가 찾고자 하는 theta값이 될 것이다. ($E\[\\hat{\\theta}\] = \\theta$ )

우리는 언제든지 thata의 참값을 얻을 수 있을 것이다. (편향이 없게)
이 경험분포함수가 곧 알고싶어하는 모집단의 분포함수가 될 것이다.

bootstrap algorithm
-------------------

여러번의 표본추출 반복을 통해 일반적인 결과 도출. 표본추출은 random(무작위)가 들어간다.
재표본추출 딱히 정의할 수 없다 - 이 것은 하나의 통계량이라고 말하기는 어렵다.
수치적인 알고리즘 방법이라고 정의할 수 있다.

자료를 먼저 오름차순 정렬한다.
순서통계량 각각 이 자체를 하나의 모집단으로 본다. 표본 재추출이라는 말은 이 모집단으로부터 표본을 재추출하는 것을 말한다.

순서통계량을 가지고 경험분포함수를 만들 수 있다. 0과 1사이에 나타나는 경험분포함수의 값과 0과 1사이의 난수를 가지고 표본을 재추출한다.

n개에서 n개를 뽑았는데 표본을 뽑았을 때 복원추출이다.
표본평균을 구할 때 첫번째 표본을 모집단으로 생각해 중복이 허용되는 복원추출로 표본을 뽑아낸다. 이 작업을 B번만큼 반복한다. 즉, B개의 데이터 셋이 생긴다.
비슷한 데이터 셋 또는 계산된 통계량의 값이 비슷할수도 다를수도 있다.

우리는 이 것들을 일반화를 시켜서 이야기 해야한다. `점추정`과 `구간추정`으로 이야기 해줄 수 있다. 보통은 모수에 대한 추정을 할 때, 치중되는 방향은 점추정이다. 하지만 여기서는 구간추정을 중요시한다.
(모수적 방법에서는 이미 점추정만으로 충분하기 때문에 이전에는 다루지 않았다.(모집단을 가정하였기 때문에 비편향추정량))

어떤경우는 잘 추정되고 어떤경우는 안되고 이런경우가 있기때문에 함께 봐야한다.
믿을만한지 신뢰구간을 꼭 체크해 볼 필요성이 있다.

-   기존 : 잘 알려져 있는 분포를 가정하고 시작한다.

-   붓스트랩 : 경험분포함수는 내가 들고 있는 표본을 가지고 시작한다.

반복하여 B개의 표본평균을 구한다. 이 값들을 크기 순으로 정렬한다. (오름차순)
양쪽에서 몇 개의 값들을 버리고 나머지를 가지고 (절사평균) 구간을 구해보자.
나머지 2.5퍼를 버리고 95%자료(안정적인 값들)만 가지고 하나의 신뢰구간처럼 생각해보자. 물론 신뢰구간은 아니다.
정확하게 말하자면 `percentile interval`이라고 부른다.
95%보다는 90%가 더 안정적일 수 있다. 조금 위험한 경우는 99%인경우인데, 99%보다는 95%쓰는 이유가 이상치가 포함될 수 있을경우 때문이다.

신뢰구간이라는 말을 빼고 `구간추정`이라고 말하기도 한다.

### Example)

#### 1.

``` r
dice <- c(1,2,3,2,6,6,5,1,1,1,4,2,4,1,4,5,6,6,3,2,5,6,4,1,2,3,2,2,5,3)
n <- length(dice)
B <- 200

sd(dice)
```

    ## [1] 1.799106

``` r
boot.sample <- matrix(0, B, n)
dice.sd <- numeric(B)

set.seed(2019)
for(i in 1:B){
    boot.sample[i,] <- sample(dice, replace=T, n)
    dice.sd[i] <- sd(boot.sample[i,])
}

summary(dice.sd)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.348   1.673   1.794   1.776   1.876   2.074

``` r
quantile(dice.sd, probs=c(.025, .975))
```

    ##     2.5%    97.5% 
    ## 1.478891 2.016347

1부터 6까지 나오는 주사위의 눈 30개
B : 200번 반복하겠다. 200번의 결과를 가지고 이야기하겠다.

$\\hat{\\sigma^{\*}}=1.776$ , sd=1.799106

약간의 차이가 있다. 어느게 맞는냐는 나중의 선택의 문제이다.
실험을 많이 할 수록 더 신뢰도가 올라간다. 신뢰구간 도출 시 너무 많은 것을 버리면 정확도가 떨어질 것.

``` r
set.seed(2019)
dice.mean <- numeric(B)

for(i in 1:B) {
  dice.mean[i] <- mean(boot.sample[i,])
}

summary(dice.mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   2.467   3.092   3.267   3.280   3.467   3.967

``` r
quantile(dice.mean, probs=c(.025, .975))
```

    ##     2.5%    97.5% 
    ## 2.666667 3.866667

평균에 대한 붓스트랩 quantile interval이다.

#### 2.

``` r
dice <- c(1,2,3,2,6,6,5,1,1,1,4,2,4,1,4,5,6,6,3,2,5,6,4,1,2,3,2,2,5,3)
var(dice)
```

    ## [1] 3.236782

모집단의 분산이다.

``` r
set.seed(2019)

n <- length(dice)
B <- 200
boot.sample <- matrix(0, B, n)
dice.var <- numeric(B)

for(i in 1:B){
  boot.sample[i,] <- sample(dice, replace=T, n)
  dice.var[i] <- var(boot.sample[i,])}


summary(dice.var)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.817   2.798   3.220   3.175   3.520   4.300

``` r
quantile(dice.var, probs=c(.025, .975))
```

    ##     2.5%    97.5% 
    ## 2.187126 4.065661

dice를 굴린 횟수만큼 재표본추출하는 작업을 200번 반복할 것이다.

#### 3.

``` r
set.seed(123)

# install.packages('Lock5Data')   for using CommuteAtlanta data.
library(Lock5Data)
data(CommuteAtlanta)

A<-CommuteAtlanta$Time    #time columns
summary(A)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1.00   15.00   25.00   29.11   40.00  181.00

``` r
n<-length(A)  #n is total data number : 500
B<-200     #bootstrap sample rep # : 200
mean(A)    #mean of the original data
```

    ## [1] 29.11

``` r
boot.sample<-matrix(0,B,n)    #make the bootstrap sample
Time.mean <- numeric(B)

for(i in 1:B){ 
  boot.sample[i,]<-sample(A, replace=T,n)
  Time.mean[i] <- mean(boot.sample[i,])}

summary(Time.mean)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   26.67   28.51   29.12   29.13   29.73   32.48

``` r
quantile(Time.mean,probs=c(.025, .975))
```

    ##     2.5%    97.5% 
    ## 27.35305 30.91520

500개의 표본으로 이루어진 데이터가 총 200개 있는 것.
붓스트랩 알고리즘이니까 점 추정만으로 이야기할 수 없다고 했다.
그래서 점추정뿐만아니라, 구간추정도 해보자. 200개 각각보다 일반적인 상황을 이야기해야 하므로 `평균`을 보자.
원자료는 편차가 매우 컷다. 어떻게 표본을 뽑든간 27과 31사이 나타나니까 문제가 없는 것 같다.

모집단에 대한 분포를 모르니까 이런 알고리즘을 사용할 수 있는 것.
모집단이 가정되었다면 그냥 알고리즘보다 그냥 전체 표본평균의 값을 더 믿을 것이다.

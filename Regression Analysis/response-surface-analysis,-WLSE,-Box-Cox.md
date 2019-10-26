Regression Analysis ( Ⅱ ) Project 1 using R.
================
Jae Kwan Koo

-   [1.Make your own dataset based on data in Example 6.5 (p. 251). Let X1&lt;-X1+e, X2&lt;-X2+e, Y&lt;-Y+e, where e~N(0,1^2). Do the response surface analysis with contour plot.](#make-your-own-dataset-based-on-data-in-example-6.5-p.-251.-let-x1-x1e-x2-x2e-y-ye-where-en012.-do-the-response-surface-analysis-with-contour-plot.)
    -   [set the data](#set-the-data)
        -   [add the error terms.](#add-the-error-terms.)
    -   [Fitting a response-surface model](#fitting-a-response-surface-model)
        -   [First-order rsm](#first-order-rsm)
        -   [Second-order rsm](#second-order-rsm)
-   [2.Make your own dataset based on data in Example 6.6(p.254). Let X1&lt;-X1+e, Y&lt;-Y+e, where e~N(0,0.1^2). Compute the WLSE using the same method as the one in text.](#make-your-own-dataset-based-on-data-in-example-6.6p.254.-let-x1-x1e-y-ye-where-en00.12.-compute-the-wlse-using-the-same-method-as-the-one-in-text.)
    -   [set the data](#set-the-data-1)
    -   [add the error terms.](#add-the-error-terms.-1)
    -   [Get the weights](#get-the-weights)
    -   [linear model with weight](#linear-model-with-weight)
-   [3.Make your own dataset based on data in Example 6.7(p.259). Let Y&lt;-Y+e, where e~N(0,1).](#make-your-own-dataset-based-on-data-in-example-6.7p.259.-let-y-ye-where-en01.)
    -   [(1)Fit the data to the multiple linear regression model.](#fit-the-data-to-the-multiple-linear-regression-model.)
        -   [set the data](#set-the-data-2)
        -   [add the error terms](#add-the-error-terms)
    -   [(2)Fit the data to the Box-Cox transformation model.](#fit-the-data-to-the-box-cox-transformation-model.)
        -   [log transformation model](#log-transformation-model)
    -   [(3)Compare two models in (1) and (2) by using the Q-Q plot of residuals in each model.](#compare-two-models-in-1-and-2-by-using-the-q-q-plot-of-residuals-in-each-model.)
-   [Reference - click the hyperlink!](#reference---click-the-hyperlink)
    -   [Response Surface Analysis](#response-surface-analysis)
    -   [WLSE](#wlse)
    -   [BOX-COX](#box-cox)
    -   [참고 문헌 : 회귀분석 제 2판 (김충락, 강근석)](#참고-문헌-회귀분석-제-2판-김충락-강근석)

1.Make your own dataset based on data in Example 6.5 (p. 251). Let X1&lt;-X1+e, X2&lt;-X2+e, Y&lt;-Y+e, where e~N(0,1^2). Do the response surface analysis with contour plot.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### set the data

``` r
x1<-c(4,20,12,12,12,12,12,6.3,6.3,17.7,17.7)
x2<-c(250,250,250,250,220,280,250,229,271,229,271)
y<-c(83.8,81.7,82.4,82.9,84.7,67.9,81.2,81.3,83.1,85.3,72.7)
```

#### add the error terms.

``` r
set.seed(2019)
X1<-x1+rnorm(n=1,mean=0,sd=1)
X2<-x2+rnorm(n=1,mean=0,sd=1)
Y<-y+rnorm(n=1,mean=0,sd=1)

data<-data.frame(X1,X2,Y)
data
```

    ##           X1       X2        Y
    ## 1   4.738523 249.4852 82.15982
    ## 2  20.738523 249.4852 80.05982
    ## 3  12.738523 249.4852 80.75982
    ## 4  12.738523 249.4852 81.25982
    ## 5  12.738523 219.4852 83.05982
    ## 6  12.738523 279.4852 66.25982
    ## 7  12.738523 249.4852 79.55982
    ## 8   7.038523 228.4852 79.65982
    ## 9   7.038523 270.4852 81.45982
    ## 10 18.438523 228.4852 83.65982
    ## 11 18.438523 270.4852 71.05982

### Fitting a response-surface model

#### First-order rsm

``` r
# install.packages("rsm")  for response-surface analysis
library(rsm)
rsm1<-rsm(Y~ FO(X1,X2),data=data)

summary(rsm1)
```

    ## 
    ## Call:
    ## rsm(formula = Y ~ FO(X1, X2), data = data)
    ## 
    ##               Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept) 132.784318  17.111097  7.7601 5.433e-05 ***
    ## X1           -0.206544   0.249835 -0.8267    0.4324    
    ## X2           -0.205051   0.067214 -3.0507    0.0158 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Multiple R-squared:  0.5553, Adjusted R-squared:  0.4441 
    ## F-statistic: 4.995 on 2 and 8 DF,  p-value: 0.0391
    ## 
    ## Analysis of Variance Table
    ## 
    ## Response: Y
    ##             Df  Sum Sq Mean Sq F value  Pr(>F)
    ## FO(X1, X2)   2 160.856  80.428  4.9951 0.03910
    ## Residuals    8 128.810  16.101                
    ## Lack of fit  6 127.283  21.214 27.7911 0.03514
    ## Pure error   2   1.527   0.763                
    ## 
    ## Direction of steepest ascent (at radius 1):
    ##         X1         X2 
    ## -0.7096673 -0.7045370 
    ## 
    ## Corresponding increment in original units:
    ##         X1         X2 
    ## -0.7096673 -0.7045370

FO는 "first-order"을 의미한다. 먼저 첫번째 반응표면모형을 적합하였다.
아래의 분산분석표에서 lack of fit과 pure error의 분해를 확인할 수 있다. 이 예제에서 이 모형의 lack of fit의 p-value는 0.03514&lt;alpha=0.05이다. 이 사실은 우리가 higher-order model을 사용해야 함을 제안한다.
first-order rsm의 contour plot을 먼저 확인해본 후, 다음으로 "second-order"모형을 적합시켜보자.

``` r
contour(rsm1,~X1+X2, image = TRUE)
```

<img src="response-surface-analysis,-WLSE,-Box-Cox_files/figure-markdown_github/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

#### Second-order rsm

``` r
rsm2<-rsm(Y~ SO(X1,X2),data=data)

summary(rsm2)
```

    ## 
    ## Call:
    ## rsm(formula = Y ~ SO(X1, X2), data = data)
    ## 
    ##                Estimate  Std. Error t value Pr(>|t|)  
    ## (Intercept) -3.2996e+02  1.3609e+02 -2.4246  0.05978 .
    ## X1           6.8603e+00  2.4730e+00  2.7740  0.03918 *
    ## X2           3.1560e+00  1.0642e+00  2.9656  0.03131 *
    ## X1:X2       -3.0075e-02  9.4237e-03 -3.1914  0.02423 *
    ## X1^2         1.7134e-02  2.9597e-02  0.5789  0.58776  
    ## X2^2        -5.9682e-03  2.1178e-03 -2.8181  0.03719 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Multiple R-squared:  0.9121, Adjusted R-squared:  0.8243 
    ## F-statistic: 10.38 on 5 and 5 DF,  p-value: 0.01128
    ## 
    ## Analysis of Variance Table
    ## 
    ## Response: Y
    ##             Df  Sum Sq Mean Sq F value   Pr(>F)
    ## FO(X1, X2)   2 160.856  80.428 15.8022 0.006896
    ## TWI(X1, X2)  1  51.840  51.840 10.1853 0.024226
    ## PQ(X1, X2)   2  51.522  25.761  5.0614 0.062856
    ## Residuals    5  25.448   5.090                 
    ## Lack of fit  3  23.922   7.974 10.4461 0.088623
    ## Pure error   2   1.527   0.763                 
    ## 
    ## Stationary point of response surface:
    ##         X1         X2 
    ##   9.920527 239.406876 
    ## 
    ## Eigenanalysis:
    ## eigen() decomposition
    ## $values
    ## [1]  0.02454460 -0.01337914
    ## 
    ## $vectors
    ##          [,1]       [,2]
    ## X1 -0.8969851 -0.4420607
    ## X2  0.4420607 -0.8969851

second-order rsm을 수행하였다. 이제는 lack of fit이 0.088623으로 alpha(=0.05)보다 커 더이상 유의하지 않다. 따라서, 이 모형으로 반응표면분석을 수행해도 될 것 같다.
second-order moel의 summary는 표면의 정준분석의 결과를 제공한다. 이 분석은 적합된 표면의 정상점이 코드화된 단위 (9.920527, 239.406876)로 나타나며, 실험 영역 이내에 있는 것으로 나타났다.
또한, 둘 eigenvalue의 부호가 서로 달라 stationary point는 saddel point이다.
contour plot을 마지막으로 확인해보자.

``` r
contour(rsm2,~X1+X2, image = TRUE)
```

<img src="response-surface-analysis,-WLSE,-Box-Cox_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

2.Make your own dataset based on data in Example 6.6(p.254). Let X1&lt;-X1+e, Y&lt;-Y+e, where e~N(0,0.1^2). Compute the WLSE using the same method as the one in text.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

### set the data

``` r
x<-c(1.15,1.90,3,3,3,3,3,5.34,5.38,5.4,5.4,5.45,7.7,7.8,7.81,7.85,7.87,7.91,
     7.94,9.03,9.07,9.11,9.14,9.16,9.37,10.17,10.18,10.22,10.22,10.22,10.18,10.50,10.23,10.03,10.23)

y<-c(0.99,0.98,2.6,2.67,2.66,2.78,2.8,5.92,5.35,4.33,4.89,5.21,7.68,9.81,6.52,9.71,9.82,9.81,
     8.5,9.47,11.45,12.14,11.5,10.65,10.64,9.78,12.39,11.03,8,11.9,8.68,7.25,13.46,10.19,9.93)

old_w<-c(1.24028,2.18224,7.84930,7.84930,7.84930,7.84930,7.84930,7.43652,6.99309,6.78574,6.78574,6.30514,0.89204,0.84420,0.83963,0.82171,0.81296,0.79588,
     0.78342,0.47385,0.46621,0.45878,0.45327,0.44968,0.41435,0.31182,0.31079,0.30672,0.30672,0.30672,0.31079,0.28033,0.30571,0.32680,0.30571)
```

35개의 관측치로 이루어진 자료이다. 여기서 w는 weight를 의미한다.
선형과 비선형 least squares regression을 포함한 대부분 모델링을 처리하는 방법에서의 일반적인 가정 중 하나는 각 데이터 포인트들은 동일한 precise information을 제공한다는 것이다. 즉, 오차항의 분산은 예측 변수의 모든 값에 대해 일정하다.
하지만, 이 가정은 모든 모형에 대해 적용하는 것이 불가능하다. 그러므로 weight가 주어진 상황에서의 분석을 해보려고 한다.

### add the error terms.

``` r
set.seed(2019)
X<-x+rnorm(n=1,mean=0,sd=0.1)
Y<-y+rnorm(n=1,mean=0,sd=0.1)

data2<-data.frame(X,Y,old_w)
head(data2)
```

    ##          X        Y   old_w
    ## 1 1.223852 0.938524 1.24028
    ## 2 1.973852 0.928524 2.18224
    ## 3 3.073852 2.548524 7.84930
    ## 4 3.073852 2.618524 7.84930
    ## 5 3.073852 2.608524 7.84930
    ## 6 3.073852 2.728524 7.84930

오차항을 넣어준 뒤 X,Y의 변수를 재할당하였다. 그리고 데이터프레임으로 만들어 앞 6개 행만 확인해보았다.

### Get the weights

``` r
g1<-c(X[1:2])
g2<-c(X[3:7])
g3<-c(X[8:12])
g4<-c(X[13:19])
g5<-c(X[20:25])
g6<-c(X[26:35])

h1<-c(Y[1:2])
h2<-c(Y[3:7])
h3<-c(Y[8:12])
h4<-c(Y[13:19])
h5<-c(Y[20:25])
h6<-c(Y[26:35])

group_data_X<-data.frame(cbind(g1,g2,g3,g4,g5,g6))
group_data_Y<-data.frame(cbind(h1,h2,h3,h4,h5,h6))
```

X값들이 비슷한 것들끼리 그룹화 하였다. 총 6개의 그룹으로 나누었다.

``` r
options("scipen" = 100)     # 소수점 100자리까지 표현.

apply(group_data_X,2,mean)  # X그룹별 평균
```

    ##        g1        g2        g3        g4        g5        g6 
    ##  1.598852  3.073852  5.467852  7.892852  9.196852 10.291852

``` r
apply(group_data_Y,2,var)   # 반응변수 Y그룹별 표본분산
```

    ##            h1            h2            h3            h4            h5 
    ## 0.00002777778 0.00641777778 0.30577777778 1.94071555556 0.93569888889 
    ##            h6 
    ## 3.89641000000

각 그룹내에 속하는 X값들의 표본 평균과 반응변수 값들의 표본분산을 계산하였다.

``` r
sj2<-c(0.00002777778, 0.00641777778,0.30577777778,1.94071555556,0.93569888889,3.89641000000)
xbarj<-c(1.598852,3.073852,5.467852,7.892852,9.196852,10.291852)

temp_data<-data.frame(temp_y=sj2,temp_x=xbarj)

temp_lm<-lm(temp_y~temp_x, data=temp_data)
summary(temp_lm)
```

    ## 
    ## Call:
    ## lm(formula = temp_y ~ temp_x, data = temp_data)
    ## 
    ## Residuals:
    ##        1        2        3        4        5        6 
    ##  0.47863 -0.04081 -0.59491  0.17551 -1.29438  1.27596 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)  -1.0486     0.8952  -1.171   0.3065  
    ## temp_x        0.3565     0.1277   2.791   0.0493 *
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.9898 on 4 degrees of freedom
    ## Multiple R-squared:  0.6607, Adjusted R-squared:  0.5759 
    ## F-statistic:  7.79 on 1 and 4 DF,  p-value: 0.04926

이 표본평균을 설명변수로 반응변수의 표본분산을 반응변수로 하는 적절한 회귀모형을 찾았다. p-value가 0.05보다 작아 적절한 모형이 적합되었음을 알 수 있다.

``` r
pre_result<-predict(temp_lm, newdata=data.frame(temp_x=data2$X))

library(tidyverse)
final_data<-data.frame(pre_result) %>% transmute(new_w = 1/pre_result) %>% cbind(data2)
final_data$new_w[1:2]<-0   # 음수의 값인 가중치에 대해 0을 부여하여 대체

final_data
```

    ##         new_w         X         Y   old_w
    ## 1   0.0000000  1.223852  0.938524 1.24028
    ## 2   0.0000000  1.973852  0.928524 2.18224
    ## 3  21.1721026  3.073852  2.548524 7.84930
    ## 4  21.1721026  3.073852  2.618524 7.84930
    ## 5  21.1721026  3.073852  2.608524 7.84930
    ## 6  21.1721026  3.073852  2.728524 7.84930
    ## 7  21.1721026  3.073852  2.748524 7.84930
    ## 8   1.1345057  5.413852  5.868524 7.43652
    ## 9   1.1164439  5.453852  5.298524 6.99309
    ## 10  1.1076269  5.473852  4.278524 6.78574
    ## 11  1.1076269  5.473852  4.838524 6.78574
    ## 12  1.0861819  5.523852  5.158524 6.30514
    ## 13  0.5804571  7.773852  7.628524 0.89204
    ## 14  0.5686890  7.873852  9.758524 0.84420
    ## 15  0.5675384  7.883852  6.468524 0.83963
    ## 16  0.5629822  7.923852  9.658524 0.82171
    ## 17  0.5607314  7.943852  9.768524 0.81296
    ## 18  0.5562833  7.983852  9.758524 0.79588
    ## 19  0.5529933  8.013852  8.448524 0.78342
    ## 20  0.4551817  9.103852  9.418524 0.47385
    ## 21  0.4522462  9.143852 11.398524 0.46621
    ## 22  0.4493484  9.183852 12.088524 0.45878
    ## 23  0.4471992  9.213852 11.448524 0.45327
    ## 24  0.4457779  9.233852 10.598524 0.44968
    ## 25  0.4313813  9.443852 10.588524 0.41435
    ## 26  0.3841228 10.243852  9.728524 0.31182
    ## 27  0.3835975 10.253852 12.338524 0.31079
    ## 28  0.3815106 10.293852 10.978524 0.30672
    ## 29  0.3815106 10.293852  7.948524 0.30672
    ## 30  0.3815106 10.293852 11.848524 0.30672
    ## 31  0.3835975 10.253852  8.628524 0.31079
    ## 32  0.3675147 10.573852  7.198524 0.28033
    ## 33  0.3809924 10.303852 13.408524 0.30571
    ## 34  0.3916309 10.103852 10.138524 0.32680
    ## 35  0.3809924 10.303852  9.878524 0.30571

이 회귀식에 x\_bar대신 각 관측값 *X*<sub>*i*</sub>들을 대입하여 $\\hat{s\_i^2}$을 계산하였다. 가중치 *w*<sub>*i*</sub>는 $\\hat{s\_i^2}$의 역수로 주어지므로 tidyverse패키지안의 dplyr패키지의 transmute함수를 이용해 전처리 했고, 기존의 데이터와 합하여 확인하였다. 음수가 나오는 weight가 있었는데 이 경우 0으로 대체한 후 진행하였다. 이를 바탕으로 새로운 weight에 대해 가중최소제곱법을 적용한 결과는 아래와 같이 주어진다.

``` r
lm(Y~X,data=final_data, weights=new_w)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X, data = final_data, weights = new_w)
    ## 
    ## Coefficients:
    ## (Intercept)            X  
    ##     -0.9305       1.1645

따라서 가중최소제곱법을 적용할 시에 $\\hat{y} = -0.9305+1.1645X$ 이 된다.

### linear model with weight

``` r
lm(y~x, data=data2,weights = old_w)
```

    ## 
    ## Call:
    ## lm(formula = y ~ x, data = data2, weights = old_w)
    ## 
    ## Coefficients:
    ## (Intercept)            x  
    ##     -0.8891       1.1648

오차항이 없을때, weight가 주어진 linear model이다. 이 식은 책의 식과 같음을 확인할 수 있다.
이제 오차항이 주어졌을 때의 모형을 살펴보자.

``` r
summary(lm(Y~X, data=final_data, weights = new_w))
```

    ## 
    ## Call:
    ## lm(formula = Y ~ X, data = final_data, weights = new_w)
    ## 
    ## Weighted Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.5369 -0.4496  0.0000  0.6745  1.5579 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value             Pr(>|t|)    
    ## (Intercept) -0.93053    0.21281  -4.373             0.000128 ***
    ## X            1.16455    0.05194  22.420 < 0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.008 on 31 degrees of freedom
    ## Multiple R-squared:  0.9419, Adjusted R-squared:   0.94 
    ## F-statistic: 502.7 on 1 and 31 DF,  p-value: < 0.00000000000000022

절편과 기울기의 p-value는 모두 충분히 작다. 또한 F-statistic은 매우 크고 p-value는 거의 0이므로 모형은 적절하다고 판단할 수 있다.

3.Make your own dataset based on data in Example 6.7(p.259). Let Y&lt;-Y+e, where e~N(0,1).
-------------------------------------------------------------------------------------------

### (1)Fit the data to the multiple linear regression model.

#### set the data

``` r
y<-c(26,38,50,76,108,157,
     17,26,37,53,83,124,
     13,20,27,37,57,87,
     NA,15,22,27,41,63)
x1<-c(rep(0,6),rep(10,6),rep(20,6),rep(30,6))
x2<-c(rep(seq(0,60,12),4))
```

#### add the error terms

``` r
set.seed(2019)
Y<-y+rnorm(n=1,mean=0,sd=1)

data3<-data.frame(x1,x2,Y)
data3
```

    ##    x1 x2         Y
    ## 1   0  0  26.73852
    ## 2   0 12  38.73852
    ## 3   0 24  50.73852
    ## 4   0 36  76.73852
    ## 5   0 48 108.73852
    ## 6   0 60 157.73852
    ## 7  10  0  17.73852
    ## 8  10 12  26.73852
    ## 9  10 24  37.73852
    ## 10 10 36  53.73852
    ## 11 10 48  83.73852
    ## 12 10 60 124.73852
    ## 13 20  0  13.73852
    ## 14 20 12  20.73852
    ## 15 20 24  27.73852
    ## 16 20 36  37.73852
    ## 17 20 48  57.73852
    ## 18 20 60  87.73852
    ## 19 30  0        NA
    ## 20 30 12  15.73852
    ## 21 30 24  22.73852
    ## 22 30 36  27.73852
    ## 23 30 48  41.73852
    ## 24 30 60  63.73852

y에 error terms을 더한 후 x1, x2와 함께 데이터프레임으로 만들었다. 특이사항으로는 Y값에 결측치가 한개 보인다는 것이다.

``` r
multi_lm<-lm(Y~x1+x2,data=data3)
summary(multi_lm)
```

    ## 
    ## Call:
    ## lm(formula = Y ~ x1 + x2, data = data3)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -15.592  -9.695  -3.722   6.713  35.296 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value       Pr(>|t|)    
    ## (Intercept)  28.9222     6.3322   4.568       0.000187 ***
    ## x1           -1.7166     0.2640  -6.502 0.000002443071 ***
    ## x2            1.5587     0.1452  10.735 0.000000000948 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 13.82 on 20 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.8793, Adjusted R-squared:  0.8673 
    ## F-statistic: 72.87 on 2 and 20 DF,  p-value: 0.0000000006543

multiple linear regression model을 적합시켰다. 그 후, adjusted r square을 확인해보았다.
F-statistic이 충분히 크고 p-value는 0에 가까워 모형은 잘 적합이 되었음을 알 수 있고, 수정된 결정계수는 0.8673이다.

### (2)Fit the data to the Box-Cox transformation model.

``` r
library(MASS) #using boxcox function in r
box_cox<-boxcox(multi_lm)  #log transformation
```

<img src="response-surface-analysis,-WLSE,-Box-Cox_files/figure-markdown_github/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

``` r
lambda<-box_cox$x
likeli_value<-box_cox$y

order_table<-cbind(lambda,likeli_value)
sorted<-order_table[order(-likeli_value),] 
```

boxcox함수를 통해 람다가 언제일 때 최대가능도 함수가 최대가 되는지 그림으로 살펴보았다. 대략 0 근처인 것 같다. 자세한 람다값을 살펴보자. 가능도 함수의 값이 큰 순서대로 정렬을 한 뒤, 람다값을 확인해 보았다.

``` r
head(sorted)  # maximum when lambda is about -0.05.
```

    ##           lambda likeli_value
    ## [1,] -0.06060606     35.25923
    ## [2,] -0.10101010     35.09827
    ## [3,] -0.02020202     34.54317
    ## [4,] -0.14141414     34.02224
    ## [5,]  0.02020202     33.06571
    ## [6,] -0.18181818     32.26908

*λ* = −0.06060606 일 때, 최대가능도 함수는 최대가 된다. 이 값은 거의 0에 가까우므로 log transformation을 이용해보도록 한다.

#### log transformation model

``` r
log_trans<-lm(log(Y)~x1+x2,data=data3)
summary(log_trans)
```

    ## 
    ## Call:
    ## lm(formula = log(Y) ~ x1 + x2, data = data3)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.08522 -0.03485 -0.00536  0.04050  0.08090 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value            Pr(>|t|)    
    ## (Intercept)  3.2441608  0.0234713  138.22 <0.0000000000000002 ***
    ## x1          -0.0309299  0.0009785  -31.61 <0.0000000000000002 ***
    ## x2           0.0302873  0.0005382   56.27 <0.0000000000000002 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.05123 on 20 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.9949, Adjusted R-squared:  0.9943 
    ## F-statistic:  1934 on 2 and 20 DF,  p-value: < 0.00000000000000022

먼저, F-statistic이 충분히 크고, p-value가 거의 0이므로 모형은 적절하다고 할 수 있다. 또한 수정된 결정계수는 0.9943으로 변환하기 전보다 높다. 변환시키지 않은 원래의 모형이 box-cox 변환모형보다 훨씬 적합도가 떨어짐을 알 수 있다.

### (3)Compare two models in (1) and (2) by using the Q-Q plot of residuals in each model.

``` r
library(car) #using qqPlot function.
qqPlot(rstandard(multi_lm))
```

<img src="response-surface-analysis,-WLSE,-Box-Cox_files/figure-markdown_github/unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

    ##  6 20 
    ##  6 19

``` r
qqPlot(rstandard(log_trans))
```

<img src="response-surface-analysis,-WLSE,-Box-Cox_files/figure-markdown_github/unnamed-chunk-22-2.png" style="display: block; margin: auto;" />

    ## 22 16 
    ## 21 16

변환하기 전의 Q-Q plot은 6번 잔차가 신뢰대 밖으로 나가는 모습을 보이고 있다.
또한, 로그변환 한 모형의 Q-Q plot의 잔차들이 조금더 직선위에 잘 모여있는 모습이다. 그러므로, 로그변환한 모형이 정규분포에 더 가까움을 알 수 있다. car package의 함수를 이용하지 않고 기본함수로도 Q-Q plot은 그려볼 수 있다.

``` r
par(mfrow=c(1,2))
qqnorm(multi_lm$residuals)
qqline(multi_lm$residuals)

qqnorm(log_trans$residuals)
qqline(log_trans$residuals)
```

<img src="response-surface-analysis,-WLSE,-Box-Cox_files/figure-markdown_github/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

Reference - click the hyperlink!
--------------------------------

### Response Surface Analysis

[contour](https://rdrr.io/cran/rsm/man/contour.lm.html)

[download the paper](https://www.jstatsoft.org/article/view/v032i07)

[rsm in R for response-surface-analysis analysis](https://www.rdocumentation.org/packages/rsm/versions/1.0/topics/rsm)

### WLSE

[weighted linear regression in r](https://www.datasciencecentral.com/profiles/blogs/weighted-linear-regression-in-r)

### BOX-COX

[box cox transformation](https://rpubs.com/bskc/288328)

### 참고 문헌 : 회귀분석 제 2판 (김충락, 강근석)

DAUM MOVIE GPA CRAWLING
================
Jae Kwan Koo

-   [daum movie crawling](#daum-movie-crawling)
    -   [exercise](#exercise)
    -   [Work](#work)

daum movie crawling
===================

``` r
library(dplyr)
library(rvest)

library(stringr)
library(data.table)
```

``` r
url<-"https://movie.daum.net/boxoffice/yearly"
```

daum 영화 사이트의 연간 영화순위를 뽑아보자.

``` r
library(knitr)
include_graphics("https://raw.github.com/koojaekwan/jaekwan-s-R/master/진행중/daum/daumsite.PNG")
```

<img src="https://raw.github.com/koojaekwan/jaekwan-s-R/master/진행중/daum/daumsite.PNG" style="display: block; margin: auto;" />

``` r
a<-read_html(url) %>% html_nodes("#mArticle ul li") 
a<-gsub("\t","",a)

length(a)
```

    ## [1] 53

``` r
head(a)
```

    ## [1] "<li><a href=\"/boxoffice/weekly\" class=\"link_tab #weekly\">주간</a></li>"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
    ## [2] "<li><a href=\"/boxoffice/monthly\" class=\"link_tab #monthly\">월간</a></li>"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    ## [3] "<li class=\"on\"><a href=\"/boxoffice/yearly\" class=\"link_tab #yearly\">연간</a></li>"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
    ## [4] "<li>\n<a href=\"/moviedb/main?movieId=122091\" class=\"link_boxthumb thumb_noimage bg_noimage #list #yearly @1\"><!-- 모바일 유동형일때 'thumb_noimage' 추가 -->\n<img src=\"\" data-original=\"//img1.daumcdn.net/thumb/R338x491/?fname=http://t1.daumcdn.net/movie/2b07e0c665de4482b9a0094f66bc11761578025532290\" class=\"lazy thumb_photo\" alt=\"남산의 부장들\" onload=\"noImageOnLoad($(this))\"><span class=\"num_ranking2 rank_1\">1위</span><!-- POPCORN-1033 추가 -->\n</a>\n<div class=\"desc_boxthumb\">\n<strong class=\"tit_join\"><a href=\"/moviedb/main?movieId=122091\" class=\"link_g #list #yearly @1\">남산의 부장들</a></strong>\n<div class=\"raking_grade\">\n<span class=\"bg_star star_grade\"><span class=\"bg_star inner_star\" style=\"width:86.4%\">평점</span></span> <!-- 116px이 100%, % 계산에서 width값에 적용-->\n<em class=\"emph_grade\">8.4</em><span class=\"txt_grade\">/10</span>\n</div>\n<dl class=\"list_state\">\n<dt>개봉일</dt>\n<dd>2020.01.22 개봉</dd>\n</dl>\n</div>\n</li>"
    ## [5] "<li>\n<a href=\"/moviedb/main?movieId=131909\" class=\"link_boxthumb thumb_noimage bg_noimage #list #yearly @2\"><!-- 모바일 유동형일때 'thumb_noimage' 추가 -->\n<img src=\"\" data-original=\"//img1.daumcdn.net/thumb/R338x491/?fname=http://t1.daumcdn.net/movie/23837d99576d43ba8e594f91497fea981579656331276\" class=\"lazy thumb_photo\" alt=\"히트맨\" onload=\"noImageOnLoad($(this))\"><span class=\"num_ranking2 rank_2\">2위</span><!-- POPCORN-1033 추가 -->\n</a>\n<div class=\"desc_boxthumb\">\n<strong class=\"tit_join\"><a href=\"/moviedb/main?movieId=131909\" class=\"link_g #list #yearly @2\">히트맨</a></strong>\n<div class=\"raking_grade\">\n<span class=\"bg_star star_grade\"><span class=\"bg_star inner_star\" style=\"width:69.9%\">평점</span></span> <!-- 116px이 100%, % 계산에서 width값에 적용-->\n<em class=\"emph_grade\">6.9</em><span class=\"txt_grade\">/10</span>\n</div>\n<dl class=\"list_state\">\n<dt>개봉일</dt>\n<dd>2020.01.22 개봉</dd>\n</dl>\n</div>\n</li>"              
    ## [6] "<li>\n<a href=\"/moviedb/main?movieId=133855\" class=\"link_boxthumb thumb_noimage bg_noimage #list #yearly @3\"><!-- 모바일 유동형일때 'thumb_noimage' 추가 -->\n<img src=\"\" data-original=\"//img1.daumcdn.net/thumb/R338x491/?fname=http://t1.daumcdn.net/movie/c8555d7906ba4559a1290c616e416c4c1576742973513\" class=\"lazy thumb_photo\" alt=\"백두산\" onload=\"noImageOnLoad($(this))\"><span class=\"num_ranking2 rank_3\">3위</span><!-- POPCORN-1033 추가 -->\n</a>\n<div class=\"desc_boxthumb\">\n<strong class=\"tit_join\"><a href=\"/moviedb/main?movieId=133855\" class=\"link_g #list #yearly @3\">백두산</a></strong>\n<div class=\"raking_grade\">\n<span class=\"bg_star star_grade\"><span class=\"bg_star inner_star\" style=\"width:65.65%\">평점</span></span> <!-- 116px이 100%, % 계산에서 width값에 적용-->\n<em class=\"emph_grade\">6.4</em><span class=\"txt_grade\">/10</span>\n</div>\n<dl class=\"list_state\">\n<dt>개봉일</dt>\n<dd>2019.12.19 개봉</dd>\n</dl>\n</div>\n</li>"

영화이름을 뽑으려고 클래스로 바로 접근하다보니 잘 되지가 않는다. 따라서 위에서부터 차례대로 접근을 해보자.
`li`까지 접근하고 뽑아보자. 페이지상에 있는 안보이던 더 세부적인 태그들이 보이기 시작한다. 여기서 더 접근을 해보자.

``` r
library(knitr)
include_graphics("https://raw.github.com/koojaekwan/jaekwan-s-R/master/진행중/daum/daumsite2.PNG")
```

<img src="https://raw.github.com/koojaekwan/jaekwan-s-R/master/진행중/daum/daumsite2.PNG" style="display: block; margin: auto;" />

### exercise

``` r
read_html(url) %>% html_nodes("#mArticle ul li") %>% html_nodes(".txt_grade") %>% html_text()
```

    ##  [1] "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10"
    ## [12] "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10"
    ## [23] "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10"
    ## [34] "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10" "/10"
    ## [45] "/10" "/10" "/10" "/10" "/10" "/10"

연습으로 뽑아봤다. 모두 10점 중에 몇 점이라는 것을 알 수 있다.

``` r
read_html(url) %>% html_nodes("#mArticle ul li") %>% html_nodes(".link_boxthumb") %>% html_text()
```

    ##  [1] "\n\t\t\t\t\t\t\t\t\t\t1위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" 
    ##  [2] "\n\t\t\t\t\t\t\t\t\t\t2위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" 
    ##  [3] "\n\t\t\t\t\t\t\t\t\t\t3위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" 
    ##  [4] "\n\t\t\t\t\t\t\t\t\t\t4위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" 
    ##  [5] "\n\t\t\t\t\t\t\t\t\t\t5위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" 
    ##  [6] "\n\t\t\t\t\t\t\t\t\t\t6위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" 
    ##  [7] "\n\t\t\t\t\t\t\t\t\t\t7위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" 
    ##  [8] "\n\t\t\t\t\t\t\t\t\t\t8위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" 
    ##  [9] "\n\t\t\t\t\t\t\t\t\t\t9위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t" 
    ## [10] "\n\t\t\t\t\t\t\t\t\t\t10위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [11] "\n\t\t\t\t\t\t\t\t\t\t11위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [12] "\n\t\t\t\t\t\t\t\t\t\t12위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [13] "\n\t\t\t\t\t\t\t\t\t\t13위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [14] "\n\t\t\t\t\t\t\t\t\t\t14위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [15] "\n\t\t\t\t\t\t\t\t\t\t15위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [16] "\n\t\t\t\t\t\t\t\t\t\t16위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [17] "\n\t\t\t\t\t\t\t\t\t\t17위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [18] "\n\t\t\t\t\t\t\t\t\t\t18위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [19] "\n\t\t\t\t\t\t\t\t\t\t19위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [20] "\n\t\t\t\t\t\t\t\t\t\t20위\n\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
    ## [21] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [22] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [23] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [24] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [25] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [26] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [27] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [28] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [29] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [30] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [31] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [32] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [33] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [34] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [35] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [36] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [37] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [38] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [39] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [40] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [41] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [42] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [43] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [44] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [45] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [46] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [47] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [48] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [49] "\n\t\t\t\t\t\t\t\t\t\t"                                            
    ## [50] "\n\t\t\t\t\t\t\t\t\t\t"

20위까지만 나오므로 그냥 `1:50`으로 만드는게 나아보인다.

Work
----

``` r
title<-read_html(url) %>% html_nodes("#mArticle ul li") %>% html_nodes(".desc_boxthumb .tit_join") %>% html_text()

title
```

    ##  [1] "남산의 부장들"                             
    ##  [2] "히트맨"                                    
    ##  [3] "백두산"                                    
    ##  [4] "닥터 두리틀"                               
    ##  [5] "해치지않아"                                
    ##  [6] "천문: 하늘에 묻는다"                       
    ##  [7] "미드웨이"                                  
    ##  [8] "시동"                                      
    ##  [9] "미스터 주: 사라진 VIP"                     
    ## [10] "나쁜 녀석들: 포에버"                       
    ## [11] "스타워즈: 라이즈 오브 스카이워커"          
    ## [12] "스파이 지니어스 "                          
    ## [13] "겨울왕국 2"                                
    ## [14] "신비아파트 극장판 하늘도깨비 대 요르문간드"
    ## [15] "포드 V 페라리"                             
    ## [16] "나이브스 아웃"                             
    ## [17] "타오르는 여인의 초상"                      
    ## [18] "클로젯"                                    
    ## [19] "21 브릿지: 테러 셧다운 "                   
    ## [20] "인셉션"                                    
    ## [21] "프린스 코기"                               
    ## [22] "눈의 여왕4"                                
    ## [23] "버즈 오브 프레이(할리 퀸의 황홀한 해방)"   
    ## [24] "파바로티"                                  
    ## [25] "피아니스트의 전설"                         
    ## [26] "캣츠"                                      
    ## [27] "타발루가와 얼음공주"                       
    ## [28] "오즈의 마법사: 요술구두와 말하는 책"       
    ## [29] "고흐, 영원의 문에서"                       
    ## [30] "울지마 톤즈 2 : 슈크란 바바"               
    ## [31] "하이큐!! 땅 VS 하늘"                       
    ## [32] "슈퍼 베어 "                                
    ## [33] "사마에게"                                  
    ## [34] "두 교황"                                   
    ## [35] "기생충"                                    
    ## [36] "핑크퐁 시네마 콘서트 : 우주대탐험"         
    ## [37] "정직한 후보"                               
    ## [38] "목격자-눈이 없는 아이"                     
    ## [39] "나이트 헌터"                               
    ## [40] "미안해요, 리키"                            
    ## [41] "스파이즈 "                                 
    ## [42] "갱"                                        
    ## [43] "페인 앤 글로리"                            
    ## [44] "레플리카"                                  
    ## [45] "조조 래빗"                                 
    ## [46] "작은 아씨들"                               
    ## [47] "몽마르트 파파"                             
    ## [48] "아내를 죽였다"                             
    ## [49] "차일드 인 타임"                            
    ## [50] "에릭 클랩튼: 기타의 신"

이제 50개의 연간 영화에 대한 타이틀을 뽑았다.

``` r
grade<-read_html(url) %>% html_nodes("#mArticle ul li") %>% html_nodes(".emph_grade") %>% html_text()
grade<-as.numeric(grade)

grade
```

    ##  [1] 8.4 6.9 6.4 6.6 6.5 8.3 7.7 7.6 5.7 6.6 6.9 8.6 7.4 7.7 8.7 8.2 8.3
    ## [18] 8.4 7.7 8.7 7.3 7.9 7.1 8.8 9.1 5.6 5.7 4.0 8.3 9.7 7.4 6.6 9.8 9.2
    ## [35] 7.8 7.1 8.5 6.3 5.4 9.3 6.9 8.4 7.1 7.6 8.8 8.4 8.4 6.2 8.8 7.6

50개의 연간 평점을 뽑아냈다.

``` r
data<-data.table(title,grade,rank = paste0(1:50,"위"))
```

합쳐서 순위를 한눈에 확인해보자.

``` r
data
```

    ##                                          title grade rank
    ##  1:                              남산의 부장들   8.4  1위
    ##  2:                                     히트맨   6.9  2위
    ##  3:                                     백두산   6.4  3위
    ##  4:                                닥터 두리틀   6.6  4위
    ##  5:                                 해치지않아   6.5  5위
    ##  6:                        천문: 하늘에 묻는다   8.3  6위
    ##  7:                                   미드웨이   7.7  7위
    ##  8:                                       시동   7.6  8위
    ##  9:                      미스터 주: 사라진 VIP   5.7  9위
    ## 10:                        나쁜 녀석들: 포에버   6.6 10위
    ## 11:           스타워즈: 라이즈 오브 스카이워커   6.9 11위
    ## 12:                           스파이 지니어스    8.6 12위
    ## 13:                                 겨울왕국 2   7.4 13위
    ## 14: 신비아파트 극장판 하늘도깨비 대 요르문간드   7.7 14위
    ## 15:                              포드 V 페라리   8.7 15위
    ## 16:                              나이브스 아웃   8.2 16위
    ## 17:                       타오르는 여인의 초상   8.3 17위
    ## 18:                                     클로젯   8.4 18위
    ## 19:                    21 브릿지: 테러 셧다운    7.7 19위
    ## 20:                                     인셉션   8.7 20위
    ## 21:                                프린스 코기   7.3 21위
    ## 22:                                 눈의 여왕4   7.9 22위
    ## 23:    버즈 오브 프레이(할리 퀸의 황홀한 해방)   7.1 23위
    ## 24:                                   파바로티   8.8 24위
    ## 25:                          피아니스트의 전설   9.1 25위
    ## 26:                                       캣츠   5.6 26위
    ## 27:                        타발루가와 얼음공주   5.7 27위
    ## 28:        오즈의 마법사: 요술구두와 말하는 책   4.0 28위
    ## 29:                        고흐, 영원의 문에서   8.3 29위
    ## 30:                울지마 톤즈 2 : 슈크란 바바   9.7 30위
    ## 31:                        하이큐!! 땅 VS 하늘   7.4 31위
    ## 32:                                 슈퍼 베어    6.6 32위
    ## 33:                                   사마에게   9.8 33위
    ## 34:                                    두 교황   9.2 34위
    ## 35:                                     기생충   7.8 35위
    ## 36:          핑크퐁 시네마 콘서트 : 우주대탐험   7.1 36위
    ## 37:                                정직한 후보   8.5 37위
    ## 38:                      목격자-눈이 없는 아이   6.3 38위
    ## 39:                                나이트 헌터   5.4 39위
    ## 40:                             미안해요, 리키   9.3 40위
    ## 41:                                  스파이즈    6.9 41위
    ## 42:                                         갱   8.4 42위
    ## 43:                             페인 앤 글로리   7.1 43위
    ## 44:                                   레플리카   7.6 44위
    ## 45:                                  조조 래빗   8.8 45위
    ## 46:                                작은 아씨들   8.4 46위
    ## 47:                              몽마르트 파파   8.4 47위
    ## 48:                              아내를 죽였다   6.2 48위
    ## 49:                             차일드 인 타임   8.8 49위
    ## 50:                     에릭 클랩튼: 기타의 신   7.6 50위
    ##                                          title grade rank

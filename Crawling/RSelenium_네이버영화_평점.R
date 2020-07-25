library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)

# chrome homepage

remDr = remoteDriver(
  remoteServerAddr="localhost",
  port=4445L,
  browserName="chrome")

Sys.sleep(2)
remDr$open() # open

# go to the page we want

remDr$navigate('https://movie.naver.com/movie/point/af/list.nhn?&page=1')

remDr$screenshot(display = T) # just comfirm the page


# 1페이지 작동 확인 --------------------------------------------------------------


url <- "https://movie.naver.com/movie/point/af/list.nhn?&page=1"
remDr$navigate(url) 

body <- remDr$getPageSource()[[1]]
body <- body %>% read_html()

movie_name <- body %>% 
  html_nodes("a.movie.color_b") %>%
  html_text()

grade <- 
body %>% 
  html_nodes("div.list_netizen_score em") %>% 
  html_text()

data.frame(movie_name, grade)



# 50페이지까지 영화 & 평점 ---------------------------------------------------------
i <- 1
url <- glue("https://movie.naver.com/movie/point/af/list.nhn?&page={i}")

temp_movie <- NULL
temp_grade <- NULL


for (i in 1:50){
  url <- glue("https://movie.naver.com/movie/point/af/list.nhn?&page={i}")
  
  remDr$navigate(url) 
  
  Sys.sleep(1)
  
  body <- remDr$getPageSource()[[1]]
  body <- body %>% read_html()
  
  temp_movie <- c(temp_movie,body %>% 
    html_nodes("a.movie.color_b") %>%
    html_text())
  
  temp_grade <- c(temp_grade,
    body %>% 
    html_nodes("div.list_netizen_score em") %>% 
    html_text())

}



df <- data.frame(temp_movie, temp_grade)
colnames(df); str(df)

df$temp_grade <- as.numeric(df$temp_grade)


# 상위 갯수 10개 영화 평점 확인

df %>% 
  dplyr::group_by(temp_movie) %>%
  summarise(n=n(),mean_grade = mean(temp_grade)) %>% 
  arrange(-n) %>% 
  head(10)

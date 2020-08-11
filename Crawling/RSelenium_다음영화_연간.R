library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)

library(tidyverse)
library(stringr)
library(lubridate)


remDr = remoteDriver(
  remoteServerAddr="localhost",
  port=4445L,
  browserName="chrome")

Sys.sleep(2)
remDr$open()

remDr$navigate('https://movie.daum.net/boxoffice/yearly')

remDr$screenshot(display = T)

body <- remDr$getPageSource()[[1]]  ## URL page Source를 가져옴
body <- body %>% read_html()  ## html를 읽어들임


# movie -------------------------------------------------------------------

movie_name <- 
body %>% 
  html_nodes("div.info_tit a") %>% 
  html_text()



score <- 
body %>% 
  html_nodes("span.info_grade") %>%
  html_nodes("span.wrap_grade.grade_netizen") %>% 
  html_text() %>% 
  gsub(x = ., pattern = "\t|\n", replacement = "") %>% 
  str_extract(string = ., pattern = "\\d+.\\d+") %>% 
  str_sub(string = ., start = 2, end = 5) %>% 
  sub(x = ., pattern = "0", replacement = "")

# sub : replace only first match of a word


open_date <- 
body %>% 
  html_nodes("span.info_state") %>% 
  html_text() %>% 
  gsub(x = ., pattern = "\t|\n|개봉", replacement = "") %>% 
  str_trim(string = ., side = "both") %>% 
  ymd()





# combine the data ---------------------------------------------------------

yearly_movie <- data.frame(movie_name, score, open_date)


# arrange date 
yearly_movie %>% arrange(desc(open_date))


# arrange score
yearly_movie %>% arrange(desc(score))

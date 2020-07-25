library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)

library(stringr)


remDr = remoteDriver(
  remoteServerAddr="localhost",
  port=4445L,
  browserName="chrome")

remDr$open()

Sys.sleep(2)

remDr$navigate('http://prod.danawa.com/list/?cate=112758&15main_11_02')

Sys.sleep(2)

# Asus button
co_btn  <- remDr$findElement(using="xpath",
                             value='//*[@id="searchMakerRep2869"]')

co_btn$clickElement()

Sys.sleep(2)

# 라이젠5-3세대 botton

co_btn2 <- remDr$findElement(using="xpath",
                             value='//*[@id="searchAttributeValueRep662507"]')

co_btn2$clickElement()


Sys.sleep(2)

# 상품평 많은순으로 보기

order_btn <- remDr$findElement(using="xpath",
                               value='//*[@id="productListArea"]/div[2]/div[1]/ul/li[6]/a')

order_btn$clickElement()

Sys.sleep(2)

# image & product name 

body <- remDr$getPageSource()[[1]]  ## URL page Source를 가져옴
body <- body %>% read_html()  ## html를 읽어들임

img <- 
body %>% 
  html_nodes('div.prod_main_info') %>% 
  html_nodes('div.thumb_image') %>% 
  html_nodes('img') %>% 
  html_attr('data-original')
  
# img 1~4 is NA
# 
setwd("D:/")

library(glue)
for(i in 5:30){
  download.file(img[i],glue("test_image{i}.jpg"), mode = 'wb')
  Sys.sleep(1)
}
# cd C:\Rselenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445


library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)



remDr = remoteDriver(
  remoteServerAddr="localhost",
  port=4445L,
  browserName="chrome")

remDr$open()

remDr$navigate('https://everytime.kr/')



remDr$screenshot(display = T)
# remDr$getCurrentUrl()
# remDr$getTitle()
# remDr$refresh()

## enter the log in display
co_btn  <- remDr$findElement(using="xpath",
                             value='/html/body/aside/div[1]/a[2]')

co_btn$clickElement()

Sys.sleep(time = 3)  
## ID, password
txt_id <- remDr$findElement(using="xpath",
                            value='//*[@id="container"]/form/p[1]/input')

txt_password <- remDr$findElement(using="xpath",
                                  value='//*[@id="container"]/form/p[2]/input')

txt_id$sendKeysToElement(list("your ID"))
txt_password$sendKeysToElement(list("your PASSWORD"))

Sys.sleep(time = 3)  
## log in!

login_btn <- remDr$findElement(using="xpath",
                               value='//*[@id="container"]/form/p[3]/input')

login_btn$clickElement()

Sys.sleep(time = 3)  
## enter the hot board

st_2_btn <- remDr$findElement(using="xpath",
                              value= '//*[@id="container"]/div[3]/div[2]/div/h3/a')

## Mouse button location
remDr$mouseMoveToLocation(webElement = st_2_btn)

## Mouse click
remDr$click(buttonId = 'LEFT')

Sys.sleep(time = 3)  


# working -----------------------------------------------------------------

# pre processing

url <- "https://everytime.kr/hotarticle/p/1"
remDr$navigate(url)          ## 할당된 URL를 탐색

body <- remDr$getPageSource()[[1]]  ## URL page Source를 가져옴
body <- body %>% read_html()  ## html를 읽어들임

# 1페이지 본문

tmp <- body %>%
  html_nodes("p.medium") %>% 
  html_text()


# 1페이지 vote
vote <- body %>% 
  html_nodes(css = 'li.vote') %>% 
  .[1:20] %>% 
  html_text()
vote


# 10페이지까지 hot board 본문 # 10페이지까지 hot board vote
tmp<-NULL
tmp2 <- NULL
i<-1
url <- paste0("https://everytime.kr/hotarticle/p/",i)

for(i in 1:10){
  url <- paste0("https://everytime.kr/hotarticle/p/",i)
  
  remDr$navigate(url)       
  
  
  body <- remDr$getPageSource()[[1]]  
  
  Sys.sleep(time = 1) 
  
  body <- body %>% read_html()  
  
  
  
  tmp <- c(tmp,body %>%
    html_nodes("p.medium") %>% 
    html_text())
  
  tmp2 <- c(tmp2,body %>% 
              html_nodes(css = 'li.vote') %>% 
              .[1:20] %>% 
              html_text())
  
  
  Sys.sleep(time = 1) 
  
}


setwd("D:/")
df <- data.frame(vote = tmp2, body = tmp)


write.csv(x = df,
          file = "everytime.csv",
          row.names = F)

# cd C:\Rselenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445

setwd("D:/")

library(webshot)

library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)



remDr = remoteDriver(
  remoteServerAddr="localhost",
  port=4445L,
  browserName="chrome")

Sys.sleep(3)
remDr$open()

remDr$navigate('https://www.instagram.com/')

Sys.sleep(3)
# number password

number <- remDr$findElement(using="xpath",
                            value='//*[@id="react-root"]/section/main/article/div[2]/div[1]/div/form/div[2]/div/label/input')

password <- remDr$findElement(using="xpath",
                              value='//*[@id="react-root"]/section/main/article/div[2]/div[1]/div/form/div[3]/div/label/input')


number$sendKeysToElement(list("Your ID"))
password$sendKeysToElement(list("Your PASSWORD"))

Sys.sleep(3)
# login

login_button <- remDr$findElement(using="xpath",
                                  value='//*[@id="react-root"]/section/main/article/div[2]/div[1]/div/form/div[4]/button/div')

login_button$clickElement()

Sys.sleep(3)

# 나중에버튼 1,2
later_button <- remDr$findElement(using="xpath",
                                  value='//*[@id="react-root"]/section/main/div/div/div/div/button')

later_button$clickElement()

Sys.sleep(3)

later_button2 <- remDr$findElement(using="xpath",
                                   value='/html/body/div[4]/div/div/div/div[3]/button[2]')

later_button2$clickElement()

Sys.sleep(5)


# 프로필 클릭

profile <- remDr$findElement(using="xpath",
                             value='//*[@id="react-root"]/section/nav/div[2]/div/div/div[3]/div/div[5]/span/img')

profile$clickElement()

Sys.sleep(5)

# 저장됨 클릭 

saved <- remDr$findElement(using="css",
                           value='a.-qQT3')

saved$clickElement()

Sys.sleep(5)


saved2 <- remDr$findElement(using="css",
                            value='span.W9_iZ')

saved2$clickElement()

Sys.sleep(5)


# 현재 페이지 스크린샷
# webshot::webshot(remDr$getCurrentUrl()[[1]])

# 스크롤 맨 아래로 내리기

webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

Sys.sleep(3)

webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))

Sys.sleep(3)

webElem <- remDr$findElement("css", "body")
webElem$sendKeysToElement(list(key = "end"))



Sys.sleep(5)



# 저장된 이미지 주소 긁어오기

body <- remDr$getPageSource()[[1]]
body <- 
body %>% 
  read_html()

image <- 
body %>%
  html_nodes('div.v1Nh3.kIKUG._bz0w') %>% 
  html_nodes('div.KL4Bh') %>%  
  html_nodes('img') %>% 
  html_attr('src')  

Sys.sleep(5)


# 이미지 다운로드

paste0("image",1,".jpg")
for(i in 1:length(image)){
  download.file(url = image[i],paste0("image",i,".jpg") ,mode = "wb")  
}

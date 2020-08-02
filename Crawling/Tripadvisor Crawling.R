# cd C:\Rselenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445



# Library -----------------------------------------------------------------

library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)

library(stringr)




# Open the Chrome ---------------------------------------------------------

remDr = remoteDriver(
  remoteServerAddr="localhost",
  port=4445L,
  browserName="chrome")

Sys.sleep(2)

remDr$open()



# Go into the page --------------------------------------------------------

remDr$navigate('https://www.tripadvisor.co.kr/Restaurants-g294197-Seoul.html')
# remDr$screenshot(display = T)

Sys.sleep(2)


# Click the Gangnamgu -----------------------------------------------------

button_gangnam  <- 
  remDr$findElement(using="xpath",
                    value='//*[@id="component_47"]/div/div[10]/div[2]/div[1]/div/label')

button_gangnam$clickElement()

Sys.sleep(5)


# page Source -------------------------------------------------------------

body <- remDr$getPageSource()[[1]]  
body <- body %>% read_html()       # read the html



# 1 page test -------------------------------------------------------------

# name
name <- 
body %>% 
  html_nodes("div.wQjYiB7z") %>%
  html_nodes("a") %>% 
  html_text()

# review count

review <- 
body %>% 
  html_nodes("span.w726Ki5B") %>% 
  html_text() %>% 
  str_extract(string = ., pattern = "[0-9]+") %>% 
  as.numeric


# grade (fail) -> we have to go inside the each html address 
# body %>% 
#   html_nodes("span._141TBKA-") %>% 
#   html_nodes("span")



# tripadvisor.co.kr + address
address <- 
body %>% 
  html_nodes("div.wQjYiB7z") %>%
  html_nodes("a") %>% 
  html_attr("href")

url <- str_c("https://www.tripadvisor.co.kr", address)

result <- NULL
for(i in 1:length(url)){
  result[i] <-
    read_html(url[i]) %>% 
    html_nodes("span.r2Cf69qf") %>%
    html_text() %>% 
    str_trim(side = "both") %>% 
    as.numeric
}


data.frame(name, review, result)

str_split_fixed(name, "\\.",2) %>% data.frame %>% .[2]


# test by 50 page  ---------------------------------------------------------
name <- NULL
review <- NULL
url <- NULL; result <- NULL; grade <- NULL

for(i in 1:50){
  
  body <- remDr$getPageSource()[[1]]  
  body <- body %>% read_html()    
  
  #
  name <- append(name,
    body %>% 
    html_nodes("div.wQjYiB7z") %>%
    html_nodes("a") %>% 
    html_text())
  
  #
  review <- append(review,
    body %>% 
    html_nodes("span.w726Ki5B") %>% 
    html_text() %>% 
    str_extract(string = ., pattern = "[0-9]+") %>% 
    as.numeric)
  
  #
  address <- 
    body %>% 
    html_nodes("div.wQjYiB7z") %>%
    html_nodes("a") %>% 
    html_attr("href")
  
  url <- append(url, str_c("https://www.tripadvisor.co.kr", address))
  
  
  button_next  <- 
    remDr$findElement(using="css",
                      value='a.nav.next.rndBtn.ui_button.primary.taLnk')
  
  button_next$clickElement()
  
  Sys.sleep(5)
}




# process grade-----------------------------------------------------------


grade <- NULL
kkk <- NULL
for(i in 784:length(url)){
  cat(glue("\r==== Progress: {i} /{length(url)} ===="))
  
  kkk[i] <-
    read_html(url[i]) %>% 
    html_nodes("span.r2Cf69qf") %>%
    html_text() %>% 
    str_trim(side = "both") %>% 
    as.numeric
}


# dataset -----------------------------------------------------------------


trip_dat <- 
data.frame(str_split_fixed(name, "\\.",2) , review, grade) %>% 
  filter(X2!="") %>% 
  select(-X1)

colnames(trip_dat)[1] <- c("name")
trip_dat$name <- str_trim(trip_dat$name,side = "both")



setwd("D:/")
write.csv(x = trip_dat,file = "tripadvisor.csv",
          fileEncoding = "EUC-KR",row.names = F)




# 필요없는 내용 ------------------------------------------------------------


# temp = "http://google.com"
# download.file(temp, destfile = "scrapedpage.html", quiet=TRUE)
# content <- read_html("scrapedpage.html")

# library(glue)
# n <- 1
# for(n in 1:10000){
#   cat(glue("\r==== Progress: {n} /10000 ===="))
#   Sys.sleep(0.001) #잘 돌아가는지 확인용 코드가 있을 경우 sleep 삭제 후 작성
# }


# parallel process --------------------------------------------------------


# library(foreach)
# library(doSNOW)
# library(doParallel)
# numCores <- detectCores() - 1
# cl <- makeCluster( numCores )
# registerDoParallel(cl) 
# getDoParWorkers()
# stopCluster(cl)


# system.time(
# foreach(i=1:100000, .combine = c) %dopar% i^2
# )
# system.time(
#   foreach(i=1:100000, .combine = c) %do% i^2
# )

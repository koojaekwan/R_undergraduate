library(RSelenium)
library(seleniumPipes)
library(rvest)
library(httr)



remDr = remoteDriver(
  remoteServerAddr="localhost",
  port=4445L,
  browserName="chrome")

remDr$open()
remDr$navigate('https://finance.naver.com/item/coinfo.nhn?code=005930&target=finsum_more')





# ID 찾아내기
frames = remDr$findElements(using = "id",
                            value = 'coinfo_cp')

print(frames)


# Frame 안으로 접근
remDr$switchToFrame(frames[[1]])


# 연간 클릭
remDr$findElement(using = 'xpath',
                  value ='//*[@id="cns_Tab21"]')$clickElement()

page_parse = remDr$getPageSource()[[1]]
page_html = page_parse %>% read_html()


Sys.setlocale('LC_ALL', 'English')
table = page_html %>% html_table(fill = TRUE)
Sys.setlocale('LC_ALL', 'Korean')



df = table[[13]]
head(df)



library(stringr)
library(magrittr)

rownames(df) = df[, 1]
df = df[, -1]

colnames(df) = df[1, ]
df = df[-1, ]
colnames(df) = str_sub(colnames(df), 1, 7)

df = sapply(df, function(x) {
  str_replace_all(x, ',', '') %>%
    as.numeric()
}) %>%
  data.frame(., row.names = rownames(df))

head(df)

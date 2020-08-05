library(rvest)
library(httr)
library(stringr)
library(tidyverse)


url <- "https://www.mangoplate.com/search/%EA%B0%95%EB%B6%81%EA%B5%AC?keyword=%EA%B0%95%EB%B6%81%EA%B5%AC&page=1"

url %>% 
  read_html() %>% 
  html_nodes("div.info") %>% 
  html_nodes("a") %>%
  html_text %>% 
  gsub(pattern = "\n",replacement = "") %>% 
  gsub(pattern = "\t",replacement = "") %>% 
  str_trim(side = "both")

url %>% 
  read_html %>% 
  html_nodes("strong.point.search_point ") %>% 
  html_text %>% 
  as.numeric

url %>% 
  read_html %>% 
  html_nodes("p.etc_info") %>% 
  html_nodes("span.view_count")



# 1-10 page ---------------------------------------------------------------


name <- NULL
grade <- NULL
view_count <- NULL

for(i in 1:10){
  url <- glue('https://www.mangoplate.com/search/%EA%B0%95%EB%B6%81%EA%B5%AC?keyword=%EA%B0%95%EB%B6%81%EA%B5%AC&page={i}')
  
  
  name <- append(name,
                   url %>% 
                     read_html() %>% 
                     html_nodes("div.info") %>% 
                     html_nodes("a") %>%
                     html_text %>% 
                     gsub(pattern = "\n",replacement = "") %>% 
                     gsub(pattern = "\t",replacement = "") %>% 
                     str_trim(side = "both"))
  
  grade <- append(grade,
                  url %>% 
                    read_html %>% 
                    html_nodes("strong.point.search_point ") %>% 
                    html_text %>% 
                    as.numeric)
  
  view_count <- append(view_count,
                       url %>% 
                         read_html %>% 
                         html_nodes("p.etc_info") %>% 
                         html_nodes("span.view_count") %>% 
                         html_text)
}


substore <- str_match(string = name, pattern = "\\(.+\\)") %>%
  str_trim(side = "both") %>% 
  str_replace_na(string = .,replacement = "") %>% 
  str_replace(string = ., pattern = "\\)",replacement = "") %>% 
  str_replace(string = ., pattern = "\\(",replacement = "")

store <- str_remove(string = name, pattern = "\\(.+\\)") %>% 
  str_trim(side = "both")


data <- data.frame(store, substore, grade, view_count)

setwd("D:/")
write.csv(x = data, file = "gangbukgu_food.csv", row.names = F)

# JMJPFU # 27 Jan 2016 
# Webscrapper for movie reviews ##
# Lord help me #

library(rvest)
library(dplyr)

## 

url <- "http://indianexpress.com/article/entertainment/regional/charlie-movie-review-starring-dulquer-salmaan-it-is-a-feel-good-movie/"

mov1 <- read_html(url)

## Getting the text

mov_txt <- mov1 %>% html_nodes("p") %>% html_text() ## This is better format as it gives only the relevant
mov_rev <- mov1 %>% html_nodes("#vuukle_proxy .itemprop span") %>% html_text() 

mov_rev <- mov1 %>% html_nodes("table") %>% .[1] %>% html_table() ## Did not work
mov_rev <- mov1 %>% html_nodes(".story-details") %>% html_text()

## 

url <- "http://www.ncaa.com/rankings/basketball-men/d1/ncaa-mens-basketball-rpi"

ranking <- read_html(url)
rank_RPI <- ranking %>% html_nodes("td") %>% html_text()

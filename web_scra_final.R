## JMJPFU - Oct 14
## JMJPFU ## 14 Oct 2015 ##
# Some neater version of webscrapping


library(rvest)
library(dplyr)



## Some Successful attempts in scrapping

## Site 1 - IMDB

# The below site gets you all the details of the movies, which needs to be further cleaned and sorted

url <- "http://www.imdb.com/search/title?languages=ml&title_type=feature&sort=moviemeter,asc"

mov1 <- read_html(url)

# Getting the Details of the movies

mov_imdb <- mov1 %>% html_nodes("tr td.title") %>% html_text() ## This works

## Site 2 - ### Site now running .com###################################

# For movies and ratings

url <- "http://www.nowrunning.com/malayalam/"
mov1 <- read_html(url)

# Getting the ratings of the movies

sitenow_movrat <- mov1 %>% html_nodes("div div#ctl00_ContentPlaceHolderMainContent_HomeFull1_BestMoviesContainer") %>% html_text() ## Needs Cleaning

## For Reviews of the movies which needs to be cleaned and features extracted

url <- "http://www.nowrunning.com/malayalam-movie-reviews/"

mov1 <- read_html(url)

# Getting the names of the movies

sitenow_reviews <- mov1 %>% html_nodes("div div#ListArea") %>% html_text() ## This needs to be further cleaned up


## Site 3: Times now ###########################

url <- "http://timesofindia.indiatimes.com/entertainment/malayalam/movie-reviews"
mov1 <- read_html(url)

# Getting the Details of the movies

timesnow_movdet <- mov1 %>% html_nodes("div div.mr_listing_brd") %>% html_text() ## comes in a good format

## site 4 India Blitz

url <- "http://www.indiaglitz.com/malayalam-movie-reviews"
mov1 <- read_html(url)

# Getting the Details of the movies

mov1 %>% html_nodes("h1 a") %>% html_text() ## Good format. Not much details. Need to explore more


## 
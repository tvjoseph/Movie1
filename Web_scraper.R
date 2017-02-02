## JMJPFU ## 13 Oct  2015 
## The webscraper for Movie reviews

# Let us attempt with rvest package
library(rvest)
library(dplyr)

## IMDB Site

url <- "http://www.imdb.com/title/tt4888834/"

mov1 <- read_html(url)

# Getting the title of the movies

mov1 %>% html_nodes("h1 span.itemprop") %>% html_text() ## This works

# Getting the ratings from the movies

mov1 %>% html_nodes("strong span") %>% html_text() %>% as.numeric() ## This works

# Getting the title cast

mov1 %>% html_nodes(".itemprop span") %>% html_text() ## This works

# Getting some reviews in a table form

mov1 %>% html_nodes("table") %>% .[[3]] %>% html_table() ## Dosent works

# Getting the credits

mov1 %>% html_nodes(".credit") %>% html_text()

# Getting the Genre

mov1 %>% html_nodes(".genre") %>% html_text()

# Getting the Runtime

mov1 %>% html_nodes(".runtime") %>% html_text()


# Another configuration with IMDB site #


url <- "http://www.imdb.com/search/title?languages=ml&title_type=feature&sort=moviemeter,asc"

mov1 <- read_html(url)

# Getting the Details of the movies

mov_imdb <- mov1 %>% html_nodes("tr td.title") %>% html_text() ## This works

## Another page of IMDB ##

url <- "http://www.imdb.com/search/title?at=0&languages=ml%7C1&sort=year,desc&title_type=feature"

mov1 <- read_html(url)

# Getting the Details of the movies

mov_imdb_yr <- mov1 %>% html_nodes("tr td.title") %>% html_text()


### Site now running .com

url <- "http://www.nowrunning.com/malayalam-movie-reviews/"

mov1 <- read_html(url)

# Getting the names of the movies

mov1 %>% html_nodes("div div#ListArea") %>% html_text() ## Gives a lot of junk

# Another method for the same site

url <- "http://www.nowrunning.com/malayalam/"
mov1 <- read_html(url)

# Getting the ratings of the movies

mov1 %>% html_nodes("#ctl00_ContentPlaceHolderMainContent_HomeFull1_BestMoviesContainer") %>% html_text() ## Gives a lot of junk


# Getting the names of the movies

mov1 %>% html_nodes(".med") %>% html_text()
mov1 %>% html_nodes("#ctl00_ContentPlaceHolderMainContent_HomeFull1_ctl01_NowInTheatersRepeater1_ctl04_MovieTitle") %>% html_text()

# Getting the ratings from the movies

mov1 %>% html_nodes("strong span") %>% html_text() %>% as.numeric()

# Getting the credits

mov1 %>% html_nodes(".credit") %>% html_text()

# Getting the Genre

mov1 %>% html_nodes(".genre") %>% html_text()

# Getting the Runtime

mov1 %>% html_nodes(".runtime") %>% html_text()

# Getting the Title

mov1 %>% html_nodes("hl span") %>% html_text()



##### Times now ##########

url <- "http://timesofindia.indiatimes.com/entertainment/malayalam/movie-reviews"
mov1 <- read_html(url)

# Getting the Details of the movies

mov1 %>% html_nodes("div div.mr_listing_brd") %>% html_text() ## comes in a good format

######## India Blitz

url <- "http://www.indiaglitz.com/malayalam-movie-reviews"
mov1 <- read_html(url)

# Getting the Details of the movies

mov1 %>% html_nodes("h1 a") %>% html_text() ## Good format. Not much details


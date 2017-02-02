# JMJPFU
# 23 July 2016

######## Slide - Packages ##################

install.packages("rvest") # Installs packages
library(rvest) # Loads packages in the environment

######## Loading Data ##############

mov1 <- read_html("https://www.rottentomatoes.com/m/the_secret_life_of_pets/reviews/")

# ( Other data input forms - csv,text,excel, SPSS, SAS, HDFS, RDBMS)

mov1

# (selectorgadget)

mov_trn <- mov1 %>% html_nodes(".the_review") %>% html_text() ## To extract data from the original object

################### Data structures #############

mov_df <- data.frame(mov_trn) # Making a data frame from the vector. Most popular

mov_mtr <- matrix(mov_trn) # Making a matrix from the vector

temp <- c(1,2,3,4) # A vector : Can have various objects


mov_mtr2 <- list(mov_trn,temp) # list : collection of various objects

################### Cleaning data ##################

library(tm)

# user defined Function for cleaning 

cleansamp <- function(corpus) 
{
  #strip whitespace
  corpus <- gsub("\\s+"," ",corpus) 
  #remove punctuation
  corpus <- gsub("[[:punct:]]", "", corpus)
  #remove numbers
  corpus <- gsub("[[:digit:]]+", "", corpus)
  #remove profanity
  #corpus <- gsub(x = corpus, pattern = paste(profanity, collapse = "|"), replacement = "") 
  #lowercase
  corpus <- tolower(corpus)
  corpus
}

mov_trn2 <- cleansamp(mov_trn)

#############  Feature Engineering #####################################################

library(wordcloud)
library(randomForest)
library(ngram)
library(stringr)
library(rvest)
library(dplyr)
library(googleVis) ## For visualisation
library(shiny)
library(dplyr)

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# function : cleansamp
# files : positive_unigram , negative_unigram_sort, Positive_bigram_sort,

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

moviefeat <- function(movie,movname) { # N denots the position of movies there will be in total
  moviedf <- data.frame(movies = NA,unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE)
  mov_cln <- cleansamp(movie) # First Clean the movie
  
  mov_rev <- concat(mov_cln,collapse= " ") # Make movie rev for ngram creation
  
  unilen <- length(get.ngrams(ngram(mov_rev,n=1))) # Take length of unigrams for % calculation
  bilen  <- length(get.ngrams(ngram(mov_rev,n=2))) ## Take length of bigram for % calculation
  trilen  <- length(get.ngrams(ngram(mov_rev,n=3))) ## Take length of trigram for % calculation
  
  ## Finding values for positive unigram ###
  
  
  temp <- data.frame(positive_unigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  moviedf[1,1] <- movname
  moviedf[1,2] <- sum(temp[,2])/unilen
  
  ## Finding values for Negative unigram ###
  
  
  temp <- data.frame(negative_unigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  
  moviedf[1,5] <- sum(temp[,2])/unilen
  
  ## Finding values for Positive bigram ###
  
  
  temp <- data.frame(Positive_bigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  
  moviedf[1,3] <- sum(temp[,2])/bilen # For movie N what is the percentage of good bigram reviews
  
  ## Finding values for Negative bigram ###
  
  
  temp <- data.frame(negative_bigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  moviedf[1,6] <- sum(temp[,2])/bilen ## For Negative Bigrams
  
  ## Finding values for Positive Trigram ###
  
  
  temp <- data.frame(positive_trigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  
  moviedf[1,4] <- sum(temp[,2])/trilen # For Positive trigrams
  
  ## Finding values for Negative Trigram ###
  
  
  temp <- data.frame(negative_tri_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  
  moviedf[1,7] <- sum(temp[,2])/trilen
  
  return(moviedf[1,])
  
  
  
} # End of function

@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# Creating a data frame for storing the values

demo_df <- data.frame(movies = NA,unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE)

# Storing the value of the feature extraction in the data frame

demo_df[1,] <- moviefeat(mov_trn,"life_of_pets") # Extracting features out of text

# Creating a full training set

head(movie_df_back) # Training set

# Building a classification model using random forest

library(randomForest)

movie_df2 <- movie_df_back # Creating another 

movie_df2$V8 <- as.factor(movie_df2$V8) ## Converting the labels to factor column

rf <- randomForest(movie_df2[,2:7], movie_df2$V8, ntree=200, imp=TRUE, sampsize=50,mtry=4, do.trace=TRUE)

# Predicting using the model on the test set

pred <- predict(rf,demo_df[,2:7]) # Doing predictions

demo_df$pred <- pred # Adding the prediction on to the dataframe



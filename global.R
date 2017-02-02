## JMJPFU ##
# 8 Feb 2016 # This is the golobal file for the recommender

library(tm)
library(wordcloud)
library(randomForest)
library(ngram)
library(stringr)
library(rvest)
library(dplyr)
library(googleVis) ## For visualisation
library(shiny)
library(dplyr)


model1 <- readRDS("rf.rds")
movie_master <- readRDS("movies_df.rds") # The movies_df.rds file has been stored in the 
# folder with the Ui and server files. The dataframe is read and is loaded to this variable name

positive_unigram_sort <- readRDS('positive_unigram_sort.rds')
Positive_bigram_sort <- readRDS('Positive_bigram_sort.rds')
positive_trigram_sort <- readRDS('positive_trigram_sort.rds')

negative_unigram_sort <- readRDS('negative_unigram_sort.rds')
negative_bigram_sort <- readRDS('negative_bigram_sort.rds')
negative_tri_sort <- readRDS('negative_tri_sort.rds')
#upload_template <- readRDS('upload_template.rds')


## This is the function to create Termdocumentmatrix #######

termmatrix <- function(movie){
  
  mov_corpus <- Corpus(VectorSource(movie))
  mov_corpus <- tm_map(mov_corpus,content_transformer(tolower))
  mov_corpus <- tm_map(mov_corpus,removePunctuation)
  mov_corpus <- tm_map(mov_corpus,removeNumbers)
  mov_corpus <- tm_map(mov_corpus,removeWords,stopwords('english'))
  mov_corpus <- tm_map(mov_corpus,PlainTextDocument)
  mov_dtm <- TermDocumentMatrix(mov_corpus,control=list(minWordLength=1))
  m <- as.matrix(mov_dtm)
  sort(rowSums(m),decreasing = TRUE)
  
} 

########### Function termmatrix ends ######################

################### Function for rating matrix starts ##############

### JMJPFU ##
# 15 feb 2016 # Function for Rating comments of movies #####

Decision_mov <- function(rating){

  rating_test <- as.numeric(rating) ## Loads the output of the text analysis as rating


  if(rating_test==5){" This a great movie. It should preferably be watched in a Theatre"}
  else if(rating_test==4){" A Good Movie. If you are in town and can spare some time to go to nearby theatre, it wouldnt be a waste of time"}
  else if(rating_test==3){"Fairly Good movie. It could be a good watch in DVD"}
  else if(rating_test==2){"Moderate Movie. If you have nothing else to do at home, then you can watch this on DVD"}
  else if(rating_test==1){"Pathetic Movie. Dont even harbour the idea to watch it, even if you have all the time in the world"}

} # End of the Decision_mov function

############### Function Rating matrix ends ################

############# Function to clean text #####################

cleansamp <- function(corpus){
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

 
        

############ Cleansamp function Ends ####################



## Function to analyse input text #######################

moviefeat1 <- function(movietxt) { # This is the function to 
  
  moviedf <- data.frame(unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE)
  
  
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
  
  moviedf[1,1] <- (sum(temp[,2])/unilen)*100 ## 100 to be deleted
  
  ## Finding values for Negative unigram ###
  
  
  temp <- data.frame(negative_unigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  
  moviedf[1,4] <- (sum(temp[,2])/unilen)*50 ## 50 to be deleted
  
  ## Finding values for Positive bigram ###
  
  
  temp <- data.frame(Positive_bigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  
  moviedf[1,2] <- (sum(temp[,2])/bilen) * 200 # For movie N what is the percentage of good bigram reviews
  
  ## Finding values for Negative bigram ###
  
  
  temp <- data.frame(negative_bigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  moviedf[1,5] <- sum(temp[,2])/bilen ## For Negative Bigrams
  
  ## Finding values for Positive Trigram ###
  
  
  temp <- data.frame(positive_trigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  
  moviedf[1,3] <- sum(temp[,2])/trilen # For Positive trigrams
  
  ## Finding values for Negative Trigram ###
  
  
  temp <- data.frame(negative_tri_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- max(temp1) # 
    
    
  }
  
  moviedf[1,6] <- sum(temp[,2])/trilen
  
  return(moviedf[1,])
  
  
  
} # End of function



############### Code Ends Here ###############


# JMJPFU # Lord Help in closing the model and cleaning up
# 4 Mar 2016

# Experiments

write.table(star_wars,'star_wars.txt')

########## The below is the wordcl reactive function for analysing the wordcloud for the movies selected ###########


wordcl <- reactive({
  
  temp <-  input$movie_l ## Getting the input movie name
  temp <- paste0(temp,".rds") # Pasting the movie name with .rds for reading the matrix
  movie1 <- readRDS(temp) # Storing the charachter text
  
  # if(is.null(movie1)){return()} # This was to try and suppress the error message. Have not worked yet
  
  isolate({
    withProgress({
      setProgress(message = "Processing Corpus ......") # This is for showing the message on the UI that some activity is happening
      termmatrix(movie1) # Creating the term document matrix using the function in global.R
      
      
    }) # End of progress function
    
    
  }) # End of isolate function
  
  
})## End of wordcl reactive function

################## End of the function ###################################


wordcl <- reactive({
  
  temp <-  input$movie_l ## Getting the input movie name
  temp <- paste0(temp,".rds") # Pasting the movie name with .rds for reading the matrix
  movie1 <- readRDS(temp) # Storing the charachter text
  
  mov_cln <- cleansamp(movie1) # First Clean the movie
  
  
  ############ The below is the portion for rating the movie based on an RF model ####################
  #                           moviedf <- data.frame(unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE) # Makes an empty dataframe for storing the values
  #                           
  #                           ####### Process 1 - Cleaning and getting the important variables ready ##########################
  #                           
  #                           
  #                           mov_rev <- concat(mov_cln,collapse= " ") # Concatinating the text for ngram creation
  #                           
  #                           unilen <- length(get.ngrams(ngram(mov_rev,n=1))) # Take length of unigrams for % calculation
  #                           bilen  <- length(get.ngrams(ngram(mov_rev,n=2))) ## Take length of bigram for % calculation
  #                           trilen  <- length(get.ngrams(ngram(mov_rev,n=3))) ## Take length of trigram for % calculation
  #                           
  ## Process 2 -  Finding the % of key words by comparison with templates###
  
  # Process 2, Step 1 - % of Positive bigram ###
  
  
  temp <- data.frame(Positive_bigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- temp1 
    
    
  }
  
  # moviedf[1,2] <- as.numeric(sum(temp[,2])/bilen) ## Loading the % value in the data frame 
  moviedf_pos <- temp %>% filter(V2 > 1) # This is for storing the positive bigrams for cloud analysis
  pos_sum <- sum(temp[,2]) # This is to get the sum of all positive bigram terms
  
  
  
  # Process 2, Step 4 - % of Negative bigram ###
  
  
  temp <- data.frame(negative_bigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- temp1
    
    
  }
  #moviedf[1,5] <- as.numeric(sum(temp[,2])/bilen) ## For Negative Bigrams
  moviedf_neg <- temp %>% filter(V2 > 1) # This is for storing the negative bigrams for cloud analysis
  neg_sum <- sum(temp[,2]) # This is to get the sum of all negative bigram terms
  
  
  ################################### End of Test ########
  
  #                           pred <- predict(model1,moviedf[1,1:6]) # Prediction function is working.
  #                           
  #                           list(pred=pred,moviedf = moviedf,pos_cloud = moviedf_pos,pos_sum=pos_sum,neg_cloud = moviedf_neg,neg_sum=neg_sum) # Listing down all the outputs
  
  isolate({
    withProgress({
      setProgress(message = "Processing Corpus ......") # This is for showing the message on the UI that some activity is happening
      termmatrix(movie1) # Creating the term document matrix using the function in global.R
      pos_terms_mov <- termmatrix(moviedf_pos[,1]) # Creating the term document matrix using the function in global.R
      neg_terms_mov <- termmatrix(moviedf_neg[,1]) # Creating the negative term document matrix
      
    }) # End of progress function
    
    
  }) # End of isolate function
  
  plotdf <- data.frame(positive= as.numeric(pos_sum),negative = as.numeric(neg_sum)) # Creating a datframe for later plotting
  
  list(pos_terms_mov = pos_terms_mov,neg_terms_mov=neg_terms_mov,plotdf_mov=plotdf)
  
})## End of wordcl reactive function

################## Lord Thank you for the success so far and future success ###############

################ Function to make word cloud #################

wordcloud_maker <- function(movie_txt){
  
  # First clean the movie
  mov_cln <- cleansamp(movie_txt)
  # Make a temporary container for positive bigrams
  temp <- data.frame(Positive_bigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- temp1 
    
    
  }
  
  
  moviedf_pos <- temp %>% filter(V2 > 1) # This is for storing the positive bigrams for cloud analysis
  pos_sum <- sum(temp[,2]) # This is to get the sum of all positive bigram terms
  
  # Process 2, Step 4 - % of Negative bigram ###
  
  
  temp <- data.frame(negative_bigram_sort[,1])
  
  # The below for loop calculates the number of times each of the template word occurs in the whole text
  
  for(i in 1:nrow(temp)){
    
    wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
    
    temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
    
    temp[i,2] <- temp1
    
  }
  
  moviedf_neg <- temp %>% filter(V2 > 1) # This is for storing the negative bigrams for cloud analysis
  neg_sum <- sum(temp[,2]) # This is to get the sum of all negative bigram terms
  
  
  ################################### End of Test ########
    
      
      pos_terms_mov <- termmatrix(moviedf_pos[,1]) # Creating the term document matrix using the function in global.R
      neg_terms_mov <- termmatrix(moviedf_neg[,1]) # Creating the negative term document matrix
      
    
  
  plotdf <- data.frame(positive= as.numeric(pos_sum),negative = as.numeric(neg_sum)) # Creating a datframe for later plotting
  
  list(pos_terms = pos_terms_mov,neg_terms=neg_terms_mov,plotdf=plotdf)
  
  
}

star_wars_pos <- wordcloud_maker(star_wars)$pos_terms
star_wars_neg <- wordcloud_maker(star_wars)$neg_terms
star_wars_df <- wordcloud_maker(star_wars)$plotdf

saveRDS(star_wars_pos,'star_wars_pos.rds')
saveRDS(star_wars_neg,'star_wars_neg.rds')
saveRDS(star_wars_df,'star_wars_df.rds')

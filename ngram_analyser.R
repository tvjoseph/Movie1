# JMJPFU # Lord Help this venture and pray for it
# 1 March 2016
# This is the script for creating templates for various genre of movies

# Some great movies

# Step 1 : First go through the site and scrap some text through the below function

deadpool <- mov_txt("the_martian",15,52)

# Step 2 : clean up the text and create unigrams, bigrams and trigrams

mov_cln <- cleansamp(deadpool) # Cleaning the movie
mov_rev <- concat(mov_cln,collapse= " ") # Concatinating for making into various ngrams

mov_uni <- ngram(mov_rev,n=1) # Making into unigrams
mov_uni <- get.ngrams(mov_uni)
mov_uni <- data.frame(mov_uni)


mov_bi <- ngram(mov_rev,n=2) # Making into bigrams
mov_bi <- get.ngrams(mov_bi)
mov_bi <- data.frame(mov_bi)


mov_tri <- ngram(mov_rev,n=3) # Making into trigrams
mov_tri <- get.ngrams(mov_tri)
mov_tri <- data.frame(mov_tri)

# Next step is to make some hexagram so as to find frequencies of the various ngrams

mov_hex <- ngram(mov_rev,n=6) # Making into hexagrams
mov_hex <- get.ngrams(mov_hex)

# Next step is to filter with the master and take only the ones which have not been analysed yet

# Lets compare it with the master and eliminate those bigrams which have already been compared

y_bi <- mov_bi %>% select(mov_bi) # Select only the relevant column related to the bigrams
colnames(y_bi) <- "V1" # Need to have the same column name as x-bi
x_bi <- master_bi %>% select(V1) # Selecting the relevant column
colnames(x_bi) <- "V1"


y_bi <- data.frame(lapply(y_bi, as.character), stringsAsFactors=FALSE) ## This is required to convert all the factors as charachters in order to merge
x_bi <- data.frame(lapply(x_bi, as.character), stringsAsFactors=FALSE) 

# now to find the difference and find the bi-grams to be classified

temp_consol <- setdiff(y_bi,x_bi) ## This is the latest ngram to sort as on 16 feb 2016


# NExt step, run each of the words of the  respective ngrams to get the frequencies

freq_finder <- function(ngram) {
  
  for(i in 1:nrow(ngram)){
  
    wd1 <- paste(ngram[i,1]) # Getting each term from the list of positive terms
  
    temp1 <- length(mov_hex[grep(wd1,mov_hex)]) # finding how many times the word appears in the hexagram
  
    ngram[i,2] <- temp1 # Storing the value in the data frame as second column
  
    print(i) # Only for iteration
  
  }

  ngram <- ngram %>% filter(V2>8) %>% arrange(desc(V2)) # filter only the ngrams which are more frequent and arranging 
  ngram

} # End of the function

mov_bi <- freq_finder(temp_consol)



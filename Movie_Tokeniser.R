## JMJPFU ##
## Jan 27 2016 ## 

# Library Files
library(tm)
library(ngram)
library(stringr)


# Running some functions

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

trigram_clean2 <- apply(trigram_clean1,2,function(x) gsub("T","",x))

# Step 1 # Scrap the web and get the text



# Step 2 # Consolidate the text and make it into tokens to analyse

mov_rev <- mov_txt[1:8] ## Taking only the relevant sections of the text

mov_cln <- cleansamp(mov_rev) ## Applying the function to clean up the text a bit
mov_cln1 <- gsub("â€","",mov_cln) ## Cleaning up the data from unwanted charachters

## Step 3 # Let us crete a corpus and test it out

mov_corpus <- Corpus(VectorSource(mov_cln1))

# let us now concatinate the text into one to create n-gram models

mov_rev <- concat(mov_cln1,collapse= " ") ## This worked pretty well

# let us now create some ngrams 

mov_bigram <- ngram(mov_rev,n=2) 
mov_bigram1 <- get.ngrams(mov_bigram)

# let us do some explorations on the same

length(mov_cln[grep("love",mov_cln)]) ## This is to get a sense of how many times the word love appears in the text

# Bigrams are good, but it is not proper to make sense if we want to get emotions

# Let us now try some trigrams

mov_trigram <- ngram(mov_rev,n=3) 
mov_trigram1 <- get.ngrams(mov_trigram)

# Let us now try some hexagrams

mov_hexagram <- ngram(mov_rev,n=6) 
mov_hexagram1 <- get.ngrams(mov_hexagram)

length(mov_hexagram1[grep("entertains",mov_hexagram1)]) # This can be done to find proportion of words for frequencies

## So to start the movie feature engineering, here is the procedure

# Create a unigram and find out the words which can mean the type of class

mov_ungram <- ngram(mov_rev,n=1)
mov_ungram <- get.ngrams(mov_ungram)

# Create a data set of positive Unigram, bigram and trigram

pos1 <- data.frame(readLines("positive.txt"))

# No of each of the Unigrams in the bigrams

wd1 <- paste(pos1[12,])

length(mov_bigram1[grep(wd1,mov_bigram1)]) # Finding the length of each key word in bigrams
mov_bigram1[grep(wd1,mov_bigram1)]

length(mov_trigram1[grep(wd1,mov_trigram1)]) # Finding the length of each key word in bigrams
mov_bigram1[grep(wd1,mov_bigram1)]

# Creating unique words data set

pos_uni <- data.frame(pos1[3:19,])
pos_bi <- data.frame(pos1[23:48,])
pos_tri <- data.frame(pos1[52:71,])

# Feature F1 - % of +ve words as percentage of total words

# Finding the number of positive words in the corpora

tot_corpora <- tm_map(mov_corpus,removeWords,stopwords('english'))
mov_plain <- tm_map(tot_corpora,PlainTextDocument) ## Convert into a plain document
mov_dtm <- DocumentTermMatrix(mov_plain) ## Converts the plain document into a term document matrix

inspect(mov_dtm) ## just to inspect the corpora

mov_tot <- findFreqTerms(mov_dtm,lowfreq=0) ## Total number of words in the corpora. This will help in finding the % of good words in the corpora

for(i in 1:nrow(pos_uni)){
  
  wd1 <- paste(pos_uni[i,1]) # Getting each term from the list of positive terms
   temp1 <- length(mov_bigram1[grep(wd1,mov_bigram1)])/2 # Finding the frequency of each +ve word in the corpora
   temp2 <- length(mov_trigram1[grep(wd1,mov_trigram1)])/3
   temp3 <- length(mov_hexagram1[grep(wd1,mov_hexagram1)])/6
  
  pos_uni[i,2] <- max(temp1,temp2,temp3)
  pos_total <- sum(pos_uni[,2]) 
  
  
}

F1 <- (pos_total/length(mov_tot))*100 # This is the first fetures. The percentage of good words in the total corpora


# Feature - F2 : % of +ve bigrams as percentage of total hexagrams

hexa_tot <- length(mov_hexagram1)

for(i in 1:nrow(pos_bi)){
  
  wd1 <- paste(pos_bi[i,1]) # Getting each term from the list of positive terms
   
  temp1 <- length(mov_trigram1[grep(wd1,mov_trigram1)])/2 ## Only count of two will appear for trigram
  temp2 <- length(mov_hexagram1[grep(wd1,mov_hexagram1)])/5
  
  pos_bi[i,2] <- max(temp1,temp2)
  pos_total <- sum(pos_bi[,2]) 
  
  
}

F2 <- (pos_total/hexa_tot)*100

# Feature - F3 : % of +ve trigrams as percentage of total hexagrams


for(i in 1:nrow(pos_tri)){
  
  wd1 <- paste(pos_tri[i,1]) # Getting each term from the list of positive terms
  
  
  temp1 <- length(mov_hexagram1[grep(wd1,mov_hexagram1)])/4
  
  pos_tri[i,2] <- temp1
  pos_total <- sum(pos_tri[,2]) 
  
  
}

F3 <- (pos_total/hexa_tot)*100

## This script is to demonstrate the word prediction algorithm

## Step1 : Let us first sample data

setwd("E:/Capstone/final/en_US")
library(tm)
library(ngram)
library(stringr)

## Run the function for sampling



samp <- function(textfile){
  
  fileln <- length(readLines(textfile)) ## Calculate the length of the file
  sampfile <- readLines(textfile) ## Reading all the lines of each data set
  sampsiz <- fileln * 0.01 ## Taking only 1% sample
  txtsamp <- sample(sampfile,sampsiz)  
  return(txtsamp)
  
}

## Sampling the news file

news_samp <- samp("en_US.news.txt")

## Creating tokens and cleaning the sample

news_tokn <- clean_tokn(news_samp)

## Cleaning the data set without converting into tokens

news_clean <- clean_tokn(news_samp)

## Let us first create a Corpus

news_corpus <- Corpus(VectorSource(news_clean))

## Let us create a token for the file

news_tokn <- 
  
  ## Let us now create some ngram models out of the corpus
  
  news_ngrammod <- concat(news_clean)

news_bigram <- ngram(news_ngrammod,n=2)

news_bigram1 <- get.ngrams(news_bigram)

## Sample explorations
length(news_clean[grep("but in",news_clean)])
length(news_clean[grep("but",news_clean)])

news_bigram1[grep("^but in$",news_bigram1)]

## Let us now make a matrix of conditional probabilities for the bi-gram model

condprob_tab <- function(text_mod,text_clean){
  
  len <- length(text_mod)
  tempdf <- data.frame(bigram1=NA,bigram2=NA,prob=NA,stringsAsFactors=FALSE)
  
  for(i in 1:len){
    temp1 <- text_mod[i]
    word1 <- word(temp1,1)
    word2 <- word(temp1,2)
    len1 <- length(text_clean[grep(temp1,text_clean)])
    len2 <- length(text_clean[grep(word1,text_clean)])
    tempdf[i,1] <- word1
    tempdf[i,2] <- word2
    tempdf[i,3] <- len1/len2
    p <- print(i)
    p
  }
  
  return(tempdf)  
  
  
}

## Let us make the first bigram probability tables

news_conprob1 <- condprob_tab(news_bigram1,news_clean)

## some exploration of the conditional probability data frame

temp2 <- news_conprob1
temp3 <- temp2[order(temp2$bigram1,)]
temp4 <- temp3[grep(" the ",temp3$bigram1),]
temp4 <- temp4[order(temp4$prob,decreasing=TRUE),]

## Defining the prediction function

predictnxt <- function(inp){
  input <- "[^inp]"
  input <- tolower(input)  
  ##inlist <- blog_freq_df[grep(input,blog_freq_df$ST),]
  ##predwords <- inlist[order(inlist$Freq,decreasing=TRUE),][1:3,1]
  ##pred1 <- predwords[[1]]
  ##pred2 <- predwords[[2]]
  ##pred3 <- predwords[[3]]
  
  ##return(predwords)
  return(input)
  
  
}

############
server.r function for Shiny

function(input,output) {
  
  output$text1 <- renderText({
    
    tepre <- input$text
    
    tepre <- tolower(tepre)
    tepre <- paste0(" ",tepre)
    
    
    
    inlist <- blog_freq_df[grep(tepre,blog_freq_df$ST),]
    
    predlist <- inlist[order(inlist$Freq,decreasing=TRUE),][1:3,1]
    
    predlist
    
  }     
  )
}


)
#######################
Let us now create the n-gram models from the datasets

## Let us first create a big sample of all the data sets

news_samp2 <- samp("en_US.news.txt")
blog_samp2 <- samp("en_US.blogs.txt")
twit_samp2 <- samp("en_US.twitter.txt")
## Combining all sample files

comb_samp1 <- c(news_samp2,blog_samp2,twit_samp2)

## Let us first clean this sample file

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

comb_clean2 <- cleansamp(comb_samp1)

## Let us now create a bigram out of this sample

comb_conclean2 <- concat(comb_clean2)


comb_trigram1 <- ngram(comb_conclean2,n=3) ## Creating a bigram model

comb_trigram1 <- get.ngrams(comb_trigram1) ## getting the bigram from the model

## Making a data frame out of the bigram model

bigram_df1 <- data.frame(v1=as.vector(names(table(unlist(comb_bigram2)))),v2=as.numeric(table(unlist(comb_bigram2))))
names(bigram_df1) <- c("words","freq") ## changing the column names

bigram_df1 <- bigram_df1[with(bigram_df1,order(-bigram_df1$freq)),]

## Let us now run the conditional probability dataframe created

## Since the process is time consuming let us go chunk by chunk

comb_bichunk1 <- comb_bigram2[1:100]
comb_bichunk2 <- comb_bigram2[101:1000]
comb_bichunk3 <- comb_bigram2[1001:5000]
comb_bichunk4 <- comb_bigram2[5001:10000]
comb_bichunk5 <- comb_bigram2[10001:15000]
comb_bichunk6 <- comb_bigram2[15001:25000]
comb_bichunk5 <- comb_bigram2[25001:35000]
comb_bichunk5 <- comb_bigram2[35001:45000]
comb_bichunk5 <- comb_bigram2[45001:55000]




bigram_df2 <- condprob_tab(comb_bichunk1,comb_clean1)
bigram_df3 <- condprob_tab(comb_bichunk2,comb_clean1)
bigram_df4 <- condprob_tab(comb_bichunk3,comb_clean1)
bigram_df5 <- condprob_tab(comb_bichunk4,comb_clean1)
bigram_df6 <- condprob_tab(comb_bichunk5,comb_clean1)







## Let us now combine the data frames and do some cleaning up

bigram_conpro1 <- rbind(bigram_df2,bigram_df3,bigram_df4)
bigram_conpro2 <- rbind(bigram_conpro1,bigram_df5,bigram_df6)

bigram_consort1 <- bigram_conpro1[order(bigram_conpro1$prob,decreasing=TRUE),]
bigram_consort2 <- bigram_conpro2[order(bigram_conpro2$prob,decreasing=TRUE),]

## With the existing data frame, let us first attempt to do a prediction algorithm

wordpred <- function(texstri){
  
  len <- length(strsplit(texstri,' ')[[1]])
  
  if(len >= 2){
    word1 <- word(texstri,len-1)
    word1 <- paste0("^",word1,"$")
    word2 <- word(texstri,len)
    word2 <- paste0("^",word2,"$")
    temp <- bigram_consort2[grep(word1,bigram_consort2$bigram1),]
    temp1 <- temp[grep(word2,temp$bigram2),3][1:3]
  }
  
  else{
    word1 <- word(texstri,1)
    word1 <- paste0("^",word1,"$")
    
    temp1 <- bigram_consort2[grep(word1,bigram_consort2$bigram1),2][1:3]
    
  }
  
  return(temp1)
  
  
}

######### Let us now create multiple samples of the trigram model and work on the final model for the
### project

## Sampling the data

## Let us first create a big sample of all the data sets

news_samp2 <- samp("en_US.news.txt")
blog_samp2 <- samp("en_US.blogs.txt")
twit_samp2 <- samp("en_US.twitter.txt")
## Combining all sample files

comb_samp2 <- c(news_samp2,blog_samp2,twit_samp2)

## Let us first clean this sample file

cleansamp <- function(corpus) 
{
  #strip whitespace
  corpus <- gsub("\\s+"," ",corpus) 
  #remove punctuation
  corpus <- gsub("[[:punct:]]", "", corpus)
  #remove numbers
  corpus <- gsub("[[:digit:]]+", "", corpus)
  #remove special charachters
  corpus <- gsub("????","",corpus)
  #remove profanity
  #corpus <- gsub(x = corpus, pattern = paste(profanity, collapse = "|"), replacement = "") 
  #lowercase
  corpus <- tolower(corpus)
  corpus
}

comb_clean2 <- cleansamp(comb_samp2)

comb_conclean2 <- concat(comb_clean2)

## Let us clean some of the special charachters within the corpus

comb_conclean2 <- gsub("????","",comb_conclean2)




comb_trigram1 <- ngram(comb_conclean2,n=3) ## Creating a bigram model

comb_trigram1 <- get.ngrams(comb_trigram1) ## getting the bigram from the model

## Let us now create the dataframe for the trigram

## Let us now run the conditional probability dataframe created

## Since the process is time consuming let us go chunk by chunk

condprob_tab <- function(text_mod,text_clean){
  
  len <- length(text_mod)
  tempdf <- data.frame(trigram1=NA,trigram2=NA,trigram3=NA,prob=NA,stringsAsFactors=FALSE)
  
  for(i in 1:len){
    
    temp1 <- text_mod[i]
    word1 <- word(temp1,1:2)
    word1 <- paste(word1,collapse=" ")
    word2 <- word(temp1,3)
    nword1 <- word(word1,1)
    nword2 <- word(word1,2)
    len1 <- length(text_clean[grep(temp1,text_clean)])
    len2 <- length(text_clean[grep(word1,text_clean)])
    tempdf[i,1] <- nword1
    tempdf[i,2] <- nword2
    tempdf[i,3] <- word2
    tempdf[i,4] <- len1/len2
    
    
    
  }
  
  return(tempdf)  
  
  
}


comb_trichunk1 <- comb_trigram1[1:10000]
comb_trichunk2 <- comb_trigram1[10001:20000]
comb_trichunk3 <- comb_trigram1[20001:30000]
comb_trichunk4 <- comb_trigram1[30001:40000]
comb_trichunk5 <- comb_trigram1[40001:50000]
comb_trichunk6 <- comb_trigram1[50001:60000]
comb_trichunk7 <- comb_trigram1[60001:70000]
comb_trichunk8 <- comb_trigram1[70001:80000]
comb_trichunk9 <- comb_trigram1[80001:90000]
comb_trichunk10 <- comb_trigram1[90001:100000]
comb_trichunk11 <- comb_trigram1[100001:110000]
comb_trichunk12 <- comb_trigram1[110001:120000]
comb_trichunk13 <- comb_trigram1[120001:130000]
comb_trichunk14 <- comb_trigram1[130001:140000]
comb_trichunk15 <- comb_trigram1[140001:150000]
comb_trichunk16 <- comb_trigram1[150001:160000]
comb_trichunk17 <- comb_trigram1[160001:170000]
comb_trichunk18 <- comb_trigram1[170001:180000]
comb_trichunk19 <- comb_trigram1[180001:190000]
comb_trichunk20 <- comb_trigram1[190001:200000]
comb_trichunk21 <- comb_trigram1[200001:210000]
comb_trichunk22 <- comb_trigram1[210001:220000]
comb_trichunk23 <- comb_trigram1[220001:230000]
comb_trichunk24 <- comb_trigram1[230001:240000]
comb_trichunk25 <- comb_trigram1[240001:250000]
comb_trichunk26 <- comb_trigram1[250001:260000]
comb_trichunk27 <- comb_trigram1[260001:270000]
comb_trichunk28 <- comb_trigram1[270001:280000]
comb_trichunk29 <- comb_trigram1[280001:290000]
comb_trichunk30 <- comb_trigram1[290001:300000]
comb_trichunk31 <- comb_trigram1[300001:310000]
comb_trichunk32 <- comb_trigram1[310001:320000]
comb_trichunk33 <- comb_trigram1[320001:330000]
comb_trichunk34 <- comb_trigram1[330001:340000]
comb_trichunk35 <- comb_trigram1[340001:350000]
comb_trichunk36 <- comb_trigram1[360001:370000]
comb_trichunk37 <- comb_trigram1[370001:380000]
comb_trichunk38 <- comb_trigram1[380001:390000]
comb_trichunk39 <- comb_trigram1[390001:400000]
comb_trichunk40 <- comb_trigram1[400001:410000]
comb_trichunk41 <- comb_trigram1[410001:420000]
comb_trichunk42 <- comb_trigram1[420001:430000]
comb_trichunk43 <- comb_trigram1[430001:440000]
comb_trichunk44 <- comb_trigram1[440001:450000]
comb_trichunk45 <- comb_trigram1[450001:460000]
comb_trichunk46 <- comb_trigram1[460001:470000]
comb_trichunk47 <- comb_trigram1[470001:480000]
comb_trichunk48 <- comb_trigram1[480001:490000]
comb_trichunk49 <- comb_trigram1[490001:500000]
comb_trichunk50 <- comb_trigram1[500001:510000]
comb_trichunk51 <- comb_trigram1[510001:520000]
comb_trichunk52 <- comb_trigram1[520001:530000]
comb_trichunk53 <- comb_trigram1[530001:540000]
comb_trichunk54 <- comb_trigram1[540001:550000]
comb_trichunk55 <- comb_trigram1[550001:560000]
comb_trichunk56 <- comb_trigram1[560001:573547]




trigram_df1 <- condprob_tab(comb_trichunk1,comb_clean2)
trigram_df2 <- condprob_tab(comb_trichunk2,comb_clean2)
trigram_df3 <- condprob_tab(comb_trichunk3,comb_clean2)
trigram_df4 <- condprob_tab(comb_trichunk4,comb_clean2)
trigram_df5 <- condprob_tab(comb_trichunk5,comb_clean2)
trigram_df6 <- condprob_tab(comb_trichunk6,comb_clean2)
trigram_df7 <- condprob_tab(comb_trichunk7,comb_clean2)
trigram_df8 <- condprob_tab(comb_trichunk8,comb_clean2)
trigram_df9 <- condprob_tab(comb_trichunk9,comb_clean2)
trigram_df10 <- condprob_tab(comb_trichunk10,comb_clean2)
trigram_df11 <- condprob_tab(comb_trichunk11,comb_clean2)
trigram_df12 <- condprob_tab(comb_trichunk12,comb_clean2)
trigram_df13 <- condprob_tab(comb_trichunk13,comb_clean2)
trigram_df14 <- condprob_tab(comb_trichunk14,comb_clean2)
trigram_df15 <- condprob_tab(comb_trichunk15,comb_clean2)
trigram_df16 <- condprob_tab(comb_trichunk16,comb_clean2)
trigram_df17 <- condprob_tab(comb_trichunk17,comb_clean2)
trigram_df18 <- condprob_tab(comb_trichunk18,comb_clean2)
trigram_df19 <- condprob_tab(comb_trichunk19,comb_clean2)
trigram_df20 <- condprob_tab(comb_trichunk20,comb_clean2)
trigram_df21 <- condprob_tab(comb_trichunk21,comb_clean2)
trigram_df22 <- condprob_tab(comb_trichunk22,comb_clean2)
trigram_df23 <- condprob_tab(comb_trichunk23,comb_clean2)
trigram_df24 <- condprob_tab(comb_trichunk24,comb_clean2)
trigram_df25 <- condprob_tab(comb_trichunk25,comb_clean2)
trigram_df26 <- condprob_tab(comb_trichunk26,comb_clean2)
trigram_df27 <- condprob_tab(comb_trichunk27,comb_clean2)
trigram_df28 <- condprob_tab(comb_trichunk28,comb_clean2)
trigram_df29 <- condprob_tab(comb_trichunk29,comb_clean2)
trigram_df30 <- condprob_tab(comb_trichunk30,comb_clean2)
trigram_df31 <- condprob_tab(comb_trichunk31,comb_clean2)
trigram_df32 <- condprob_tab(comb_trichunk32,comb_clean2)
trigram_df33 <- condprob_tab(comb_trichunk33,comb_clean2)
trigram_df34 <- condprob_tab(comb_trichunk34,comb_clean2)
trigram_df35 <- condprob_tab(comb_trichunk35,comb_clean2)
trigram_df36 <- condprob_tab(comb_trichunk36,comb_clean2)
trigram_df37 <- condprob_tab(comb_trichunk37,comb_clean2)
trigram_df38 <- condprob_tab(comb_trichunk38,comb_clean2)
trigram_df39 <- condprob_tab(comb_trichunk39,comb_clean2)
trigram_df40 <- condprob_tab(comb_trichunk40,comb_clean2)
trigram_df41 <- condprob_tab(comb_trichunk41,comb_clean2)
trigram_df42 <- condprob_tab(comb_trichunk42,comb_clean2)
trigram_df43 <- condprob_tab(comb_trichunk43,comb_clean2)
trigram_df44 <- condprob_tab(comb_trichunk44,comb_clean2)
trigram_df45 <- condprob_tab(comb_trichunk45,comb_clean2)
trigram_df46 <- condprob_tab(comb_trichunk46,comb_clean2)
trigram_df47 <- condprob_tab(comb_trichunk47,comb_clean2)
trigram_df48 <- condprob_tab(comb_trichunk48,comb_clean2)
trigram_df49 <- condprob_tab(comb_trichunk49,comb_clean2)
trigram_df50 <- condprob_tab(comb_trichunk50,comb_clean2)
trigram_df51 <- condprob_tab(comb_trichunk51,comb_clean2)
trigram_df52 <- condprob_tab(comb_trichunk52,comb_clean2)
trigram_df53 <- condprob_tab(comb_trichunk53,comb_clean2)
trigram_df54 <- condprob_tab(comb_trichunk54,comb_clean2)
trigram_df55 <- condprob_tab(comb_trichunk55,comb_clean2)
trigram_df56 <- condprob_tab(comb_trichunk56,comb_clean2)


## Let us now combine the data frames and do some cleaning up

trigram_conpro1 <- rbind(trigram_df1,trigram_df2,trigram_df3,trigram_df4,trigram_df5,trigram_df6,trigram_df7)
trigram_conpro1 <- rbind(trigram_conpro1,trigram_df48,trigram_df49,trigram_df50,trigram_df51,trigram_df52,trigram_df53,trigram_df54,trigram_df55,trigram_df56)

## Saving the file to a RDS file

saveRDS(trigram_conpro1,"trigram.rds")
saveRDS(bigram_consort2,"bigram.rds")

## Let us now do some exploration and some manipulation in the trigram_Rds data

trigram_clean1 <- readRDS("trigram.rds")

# Let us do some more cleaning
trigram_clean1 <- gsub("T","",trigram_clean1[,1:3])

## Let us try to get some unique charachters from the data frame so that we can do substitution
stringdf <- apply(trigram_clean2,2,function(x) unique(substring((x),1:30,1:30)))

egstri <- trigram_clean2[grep("[a-z][?][a-z]",trigram_clean2)]
egstri <- grep("T",comb_clean2)
egstri
## Let us create function to do the fine tuning

trigram_clean2 <- apply(trigram_clean1,2,function(x) gsub("T","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("o","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","o",x))  
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","o",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","a",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?"," ",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?"," ",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("???"," ",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("z","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("?","",x))
trigram_clean2 <- apply(trigram_clean2,2,function(x) gsub("s","",x))

trigram_clean3 <- data.frame(trigram_clean2,stringsAsFactors=FALSE)
trigram_clean3$prob <- as.numeric(trigram_clean3$prob)


stringdf <- apply(temp,2,function(x) substring(x),1:nchar(x),1:nchar(x))

stringdf <- apply(ndf,2,function(x) unique(substring((x),1:15,1:15)))
## Let us now save the new trigram to a new RDS file

saveRDS(trigram_conpro1,"trigram3.rds")

## Let us now substitute all NA's in the probability table to value 0

testdf <- trigram_clean3[1:50,]
clean_prob(testdf$prob)
length(trigram_clean3$prob)

for(i in 1:563547){
  if(is.na(trigram_clean3$prob[i])){trigram_clean3$prob[i] <- (0)}
}

## Reducing the probabilities of the terms with 1 to lesser probabilities

for(i in 1:563547){
  if(trigram_conpro1$prob[i]==1){trigram_conpro1$prob[i] <- (0)}
}

## Let us now attempt to find the probabilities of unknow words

## finding words as a novel continuation
temptri <- readRDS("trigram3.rds")

temp <- trigram_conpro1

temp1 <- temptri[grep("^to$",temptri$trigram3),2:3]

unique(temp1$trigram2)

tab1 <- table(temp1$trigram2)
tab2 <- tab1[order(tab1,decreasing=TRUE)]
tab2 <- data.frame(tab2)

## the steps for finding the unigram for the Kneyser 

## cleaning up the bigram

comb_clean2 <- gsub("T","",comb_clean2)
comb_clean2 <- gsub("o","",comb_clean2)
comb_clean2 <- gsub("?","o",comb_clean2)  
comb_clean2 <- gsub("?","o",x))
comb_clean2 <- gsub("?","",comb_clean2)
comb_clean2 <- gsub("?","a",comb_clean2)
comb_clean2 <- gsub("?","",comb_clean2)
comb_clean2 <- gsub("?","",comb_clean2)
comb_clean2 <- gsub("?"," ",comb_clean2)
comb_clean2 <- gsub("?"," ",comb_clean2)
comb_clean2 <- gsub("?","",comb_clean2)
comb_clean2 <- gsub("???"," ",comb_clean2)
comb_clean2 <- gsub("?","",comb_clean2)
comb_clean2 <- gsub("z","",comb_clean2)
comb_clean2 <- gsub("?","",comb_clean2)
comb_clean2 <- gsub("?","",comb_clean2)
comb_clean2 <- gsub("?","",comb_clean2)
comb_clean2 <- gsub("s","",comb_clean2)

## let us now create some bigrams 

comb_conclean3 <- concat(comb_clean2,collapse= " ") # Concatinating the text

comb_bigram2 <- ngram(comb_conclean3,n=2) ## Creating a bigram model

comb_bigram3 <- get.ngrams(comb_bigram2) ## getting the bigram from the model



## Let us now calculate the probabilities
comb_bichunk1 <- comb_bigram3[1:10000]
comb_bichunk2 <- comb_bigram3[10001:20000]
comb_bichunk3 <- comb_bigram3[20001:30000]
comb_bichunk4 <- comb_bigram3[30001:40000]
comb_bichunk5 <- comb_bigram3[40001:50000]
comb_bichunk6 <- comb_bigram3[50001:60000]
comb_bichunk7 <- comb_bigram3[60001:70000]
comb_bichunk8 <- comb_bigram3[70001:80000]
comb_bichunk9 <- comb_bigram3[80001:90000]
comb_bichunk10 <- comb_bigram3[90001:100000]
comb_bichunk11 <- comb_bigram3[100001:110000]
comb_bichunk12 <- comb_bigram3[110001:120000]
comb_bichunk13 <- comb_bigram3[120001:130000]
comb_bichunk14 <- comb_bigram3[130001:140000]
comb_bichunk15 <- comb_bigram3[140001:150000]
comb_bichunk16 <- comb_bigram3[150001:160000]
comb_bichunk17 <- comb_bigram3[160001:170000]
comb_bichunk18 <- comb_bigram3[170001:180000]
comb_bichunk19 <- comb_bigram3[180001:190000]
comb_bichunk20 <- comb_bigram3[190001:200000]
comb_bichunk21 <- comb_bigram3[200001:210000]
comb_bichunk22 <- comb_bigram3[210001:220000]
comb_bichunk23 <- comb_bigram3[220001:230000]
comb_bichunk24 <- comb_bigram3[240001:250000]
comb_bichunk25 <- comb_bigram3[250001:260000]
comb_bichunk26 <- comb_bigram3[260001:270000]
comb_bichunk27 <- comb_bigram3[270001:280000]
comb_bichunk28 <- comb_bigram3[280001:290000]
comb_bichunk29 <- comb_bigram3[290001:300000]
comb_bichunk30 <- comb_bigram3[300001:310000]
comb_bichunk31 <- comb_bigram3[310001:320000]
comb_bichunk32 <- comb_bigram3[320001:334568]






bigram_df1 <- condprob_tab(comb_bichunk1,comb_clean2)
bigram_df2 <- condprob_tab(comb_bichunk2,comb_clean2)
bigram_df3 <- condprob_tab(comb_bichunk3,comb_clean2)
bigram_df4 <- condprob_tab(comb_bichunk4,comb_clean2)
bigram_df5 <- condprob_tab(comb_bichunk5,comb_clean2)
bigram_df6 <- condprob_tab(comb_bichunk6,comb_clean2)
bigram_df7 <- condprob_tab(comb_bichunk7,comb_clean2)
bigram_df8 <- condprob_tab(comb_bichunk8,comb_clean2)
bigram_df9 <- condprob_tab(comb_bichunk9,comb_clean2)
bigram_df10 <- condprob_tab(comb_bichunk10,comb_clean2)
bigram_df11 <- condprob_tab(comb_bichunk11,comb_clean2)
bigram_df12 <- condprob_tab(comb_bichunk12,comb_clean2)
bigram_df13 <- condprob_tab(comb_bichunk13,comb_clean2)
bigram_df14 <- condprob_tab(comb_bichunk14,comb_clean2)
bigram_df15 <- condprob_tab(comb_bichunk15,comb_clean2)
bigram_df16 <- condprob_tab(comb_bichunk16,comb_clean2)
bigram_df17 <- condprob_tab(comb_bichunk17,comb_clean2)
bigram_df18 <- condprob_tab(comb_bichunk18,comb_clean2)
bigram_df19 <- condprob_tab(comb_bichunk19,comb_clean2)
bigram_df20 <- condprob_tab(comb_bichunk20,comb_clean2)
bigram_df21 <- condprob_tab(comb_bichunk21,comb_clean2)
bigram_df22 <- condprob_tab(comb_bichunk22,comb_clean2)
bigram_df23 <- condprob_tab(comb_bichunk23,comb_clean2)
bigram_df24 <- condprob_tab(comb_bichunk24,comb_clean2)
bigram_df25 <- condprob_tab(comb_bichunk25,comb_clean2)
bigram_df26 <- condprob_tab(comb_bichunk26,comb_clean2)
bigram_df27 <- condprob_tab(comb_bichunk27,comb_clean2)
bigram_df28 <- condprob_tab(comb_bichunk28,comb_clean2)
bigram_df29 <- condprob_tab(comb_bichunk29,comb_clean2)
bigram_df30 <- condprob_tab(comb_bichunk30,comb_clean2)

bigram_df31 <- condprob_tab(comb_bichunk31,comb_clean2)
bigram_df32 <- condprob_tab(comb_bichunk32,comb_clean2)


## Making the consolidated bigram

bigram_con1 <- rbind(bigram_con1,bigram_df29,bigram_df30,bigram_df31,bigram_df32)

saveRDS(bigram_con1,"bigram3.rds")

## finding words as a novel continuation

word2 <- bigram_df1[5,2]

word2 <- paste0("^",word2,"$")

temp1 <- length(unique(bigram_df1[grep(word2,bigram_df1$bigram2),1]))



tab1 <- table(temp1$trigram2)
tab2 <- tab1[order(tab1,decreasing=TRUE)]
tab2 <- data.frame(tab2)

## Finding Ney Kneyser probability of words from trigram

trikneyser <- function(trigram1,trigram){
  
  len <- nrow(trigram)
  len2 <- nrow(trigram1)
  
  for(i in 1:len2){
    
    word1 <- trigram1[i,1]
    word1 <- paste0("^",word1,"$")
    word2 <- trigram1[i,2]
    word2 <- paste0("^",word2,"$")
    word3 <- trigram1[i,3]
    word3 <- paste0("^",word3,"$")
    
    temp1 <- trigram[grep(word1,trigram$trigram1),]
    c1 <- nrow(temp1[grep(word2,temp1$trigram2),])
    c2 <- max(((trigram[i,4]*c1)-0.75),0)
    pr1 <- c2/c1
    lamb <- 0.75/c1*(trigram[i,4]*c1)
    c3 <- nrow(trigram[grep(word3,trigram$trigram3),])
    c4 <- c3/len
    pr2 <- lamb*c4
    pr3 <- pr1 + pr2
    
    trigram1[i,5] <- pr3
    
    
  }
  
  
  return(trigram1)
  
  
  
}

## Let us now calculate the kneeyser probabilities of the trigrams

temptri <- readRDS("trigram3.rds")

knetri_chunk1 <- temptri[1:100,]
knetri_chunk2 <- temptri[101:10000,]
knetri_chunk3 <- temptri[10001:20000,]
knetri_chunk4 <- temptri[20001:30000,]
knetri_chunk5 <- temptri[30001:40000,]
knetri_chunk6 <- temptri[40001:50000,]


kney_trigram1 <- trikneyser(knetri_chunk1,temptri)
kney_trigram2 <- trikneyser(knetri_chunk2,temptri)
kney_trigram3 <- trikneyser(knetri_chunk3,temptri)
kney_trigram4 <- trikneyser(knetri_chunk4,temptri)
kney_trigram5 <- trikneyser(knetri_chunk5,temptri)
kney_trigram6 <- trikneyser(knetri_chunk6,temptri)


## Let us now consolidate the dataframes

kney_trigramcon <- rbind(kney_trigram1,kney_trigram2)
saveRDS(kney_trigramcon,"trigram_kney2.rds")

## let us now make the new function for making the kneyser probability

uni_kney <- temptri[,3:4]
uni_kney2 <- temptri[,2]
uni_kney1 <- unique(uni_kney$trigram3)
uni_kney1 <- data.frame(uni_kney1,stringsAsFactors=FALSE)
names(uni_kney1) <- c("unigram")
uni_kney2 <- unique(uni_kney2)
uni_kney2 <- data.frame(uni_kney2,stringsAsFactors=FALSE)
names(uni_kney2) <- c("unigram")
uni_kneycom <- rbind(uni_kney1,uni_kney2)
uni_kneycom <- unique(uni_kneycom)
uni_kneycom1 <- with(uni_kneycom,uni_kneycom[order(unigram),])
uni_kneycom1 <- data.frame(uni_kneycom1)
uni_kneycom2 <- uni_kneycom1[301:67403,]
uni_kneycom2 <- data.frame(uni_kneycom2,stringsAsFactors=FALSE)

## Let us re-define the kneyser function again

trikneyser <- function(trigram1,trigram){
  
  len <- nrow(trigram)
  len2 <- nrow(trigram1)
  
  for(i in 1:len2){
    
    word1 <- trigram1[i,1]
    word1 <- paste0("^",word1,"$")
    temp1 <- trigram[grep(word1,trigram$trigram3),] ## Get all combinations which end with the given word
    
    c3 <- nrow(temp1)
    if(c3==0){trigram[i,2] <- (0)
    next
    }
    temp2 <- temp1[order(temp1$prob,decreasing=TRUE),][1,]
    
    prob1 <- temp2[1,4]
    
    
    word2 <- temp2[1,1]
    word2 <- paste0("^",word2,"$")
    word3 <- temp2[1,2]
    word3 <- paste0("^",word3,"$")
    
    temp3 <- trigram[grep(word2,trigram$trigram1),]
    c1 <- nrow(temp3[grep(word3,temp3$trigram2),])
    c2 <- max(((prob1*c1)-0.75),0)
    pr1 <- c2/c1
    lamb <- (0.75/c1)*(prob1*c1)
    
    c4 <- c3/len
    pr2 <- lamb*c4
    pr3 <- pr1 + pr2
    
    trigram1[i,2] <- pr3
    
    print(i)
    
  }
  
  
  return(trigram1)
  
  
  
}

## let us now redefine the Kneyser trigram again
knetri_chunk1 <- uni_kneycom2[1:100,]
knetri_chunk1 <- data.frame(knetri_chunk1,stringsAsFactors=FALSE)
knetri_chunk2 <- uni_kneycom2[101:10000,]
knetri_chunk2 <- data.frame(knetri_chunk2,stringsAsFactors=FALSE)
knetri_chunk3 <- uni_kneycom2[10001:20000,]
knetri_chunk3<- data.frame(knetri_chunk3,stringsAsFactors=FALSE)
knetri_chunk4 <- uni_kneycom2[20001:30000,]
knetri_chunk4<- data.frame(knetri_chunk4,stringsAsFactors=FALSE)
knetri_chunk5 <- uni_kneycom2[30001:40000,]
knetri_chunk5<- data.frame(knetri_chunk5,stringsAsFactors=FALSE)
knetri_chunk6 <- uni_kneycom2[40001:50000,]
knetri_chunk6<- data.frame(knetri_chunk6,stringsAsFactors=FALSE)
knetri_chunk7 <- uni_kneycom2[50001:60000,]
knetri_chunk7<- data.frame(knetri_chunk7,stringsAsFactors=FALSE)
knetri_chunk8 <- uni_kneycom2[60001:67103,]
knetri_chunk8<- data.frame(knetri_chunk8,stringsAsFactors=FALSE)

kney_trigram1 <- trikneyser(knetri_chunk1,temptri)
kney_trigram2 <- trikneyser(knetri_chunk2,temptri)
kney_trigram3 <- trikneyser(knetri_chunk3,temptri)
kney_trigram4 <- trikneyser(knetri_chunk4,temptri)
kney_trigram5 <- trikneyser(knetri_chunk5,temptri)
kney_trigram6 <- trikneyser(knetri_chunk6,temptri)
kney_trigram7 <- trikneyser(knetri_chunk7,temptri)
kney_trigram8 <- trikneyser(knetri_chunk8,temptri)
## Dataframe

names(kney_trigram5) <- c("unigram","prob")
names(kney_trigram6) <- c("unigram","prob")
names(kney_trigram7) <- c("unigram","prob")
names(kney_trigram8) <- c("unigram","prob")

## Let us create the dataframe for Kney

kney_tricomb <- rbind(kney_trigram1,kney_trigram2,kney_trigram3,kney_trigram4,kney_trigram5,kney_trigram6,kney_trigram7,kney_trigram8)
saveRDS(kney_tricomb,"kneycomb.rds")

## Let us now load each of the language models and then do some clean up

test_tri <- readRDS("trigram3.rds")
test_bi <- readRDS("bigram3.rds")
test_kney <- readRDS("kneycomb.rds")
# Sorting the n-grams according to the order
test_tri <- test_tri[order(test_tri$prob,decreasing=TRUE),]
test_bi <- test_bi[order(test_bi$prob,decreasing=TRUE),]
test_kney <- test_kney[order(test_kney$prob,decreasing=TRUE),]

## Reducing the probabilities of the terms with 1 to lesser probabilities

for(i in 1:324568){
  if(test_bi$prob[i]==1){test_bi$prob[i] <- (0)}
}

## Looking at the kney df and cleaning

for(i in 1:67103){
  if(is.na(test_kney$prob[i])){test_kney$prob[i] <- (0)}
}

## Let us now create some unigrams and find their frequencies

comb_conclean3 <- concat(comb_clean2,collapse= " ") # Concatinating the text

comb_unigram <- ngram(comb_conclean3,n=1) ## Creating a unigram model

comb_unigram1 <- get.ngrams(comb_unigram) ## getting the bigram from the model

## Let us now create a function to find the count of unigrams


unigram_corpus <- Corpus(VectorSource(comb_clean2))

unigram_dtm <- DocumentTermMatrix(unigram_corpus)

## Storing the words and its frequencies in a dataframe for frequency analysis

unigram_df1 <- data.frame(apply(unigram_dtm[,1:1000],2,sum))
unigram_df1 <- data.frame(ST=row.names(unigram_df1),Freq = unigram_df1[,1])

unigram_df2 <- data.frame(apply(unigram_dtm[,1001:5000],2,sum))
unigram_df2 <- data.frame(ST=row.names(unigram_df2),Freq = unigram_df2[,1])

unigram_df3 <- data.frame(apply(unigram_dtm[,5001:9000],2,sum))
unigram_df3 <- data.frame(ST=row.names(unigram_df3),Freq = unigram_df3[,1])

unigram_df4 <- data.frame(apply(unigram_dtm[,9001:13000],2,sum))
unigram_df4 <- data.frame(ST=row.names(unigram_df4),Freq = unigram_df4[,1])

unigram_df5 <- data.frame(apply(unigram_dtm[,13001:17000],2,sum))
unigram_df5 <- data.frame(ST=row.names(unigram_df5),Freq = unigram_df5[,1])

unigram_df6 <- data.frame(apply(unigram_dtm[,17001:21000],2,sum))
unigram_df13 <- data.frame(ST=row.names(unigram_df13),Freq = unigram_df13[,1])

unigram_df7 <- data.frame(apply(unigram_dtm[,21001:25000],2,sum))

unigram_df8 <- data.frame(apply(unigram_dtm[,25001:29000],2,sum))

unigram_df9 <- data.frame(apply(unigram_dtm[,29001:33000],2,sum))

unigram_df10 <- data.frame(apply(unigram_dtm[,33001:37000],2,sum))

unigram_df11 <- data.frame(apply(unigram_dtm[,37001:41000],2,sum))

unigram_df12 <- data.frame(apply(unigram_dtm[,41001:45000],2,sum))

unigram_df13 <- data.frame(apply(unigram_dtm[,45001:46498],2,sum))

unigram_comb <- rbind(unigram_df1,unigram_df2,unigram_df3,unigram_df4,unigram_df5,unigram_df6,unigram_df7,unigram_df8,unigram_df9,unigram_df10,unigram_df11,unigram_df12,unigram_df13)
unigram_comb1 <- unigram_comb[23:46498,]

saveRDS(testuni,"unigram3.rds")

testuni2 <- readRDS("unigram3.rds")
testuni <- testuni[-c(88,147,133,162,164,189:207,218:234,308,372,404,413,414,416,520,614,818,860:900,911,913,937,943),]

testuni <- testuni[order(testuni$Freq,decreasing=TRUE),]

## Reading the files

test_tri <- readRDS("trigram3.rds")
test_bi <- readRDS("bigram3.rds")
test_uni <- readRDS("unigram3.rds")
test_kney <- readRDS("kneycomb.rds")

## Making the Leaning function

## First function to create n-gram list

make_trigram <- function(text1){
  
  text1 <- tolower(text1)
  #strip whitespace
  text1 <- gsub("\\s+"," ",text1) 
  #remove punctuation
  text1 <- gsub("[[:punct:]]", "", text1)
  #remove numbers
  text1 <- gsub("[[:digit:]]+", "", text1)
  
  ## Step 1 -  Getting the current trigram list
  
  trigram_list <- ngram(text1,n=3)
  trigram_list <- get.ngrams(trigram_list)
  trigram_list <- data.frame(trigram_list,stringsAsFactors=FALSE)
  names(trigram_list) <- c("trigram_list")
  
  return(trigram_list)
}

## Second function to create Data frame

tri_freq <- function(master_list){
  
  len <- nrow(master_list)
  tempdf <- data.frame(trigram1=NA,trigram2=NA,trigram3=NA,prob=NA,stringsAsFactors=FALSE)
  
  for(i in 1:len){
    
    temp1 <- master_list[i,1]
    word1 <- word(temp1,1)   
    word2 <- word(temp1,2)
    word3 <- word(temp1,3)
    
    tempdf[i,1] <- word1
    tempdf[i,2] <- word2
    tempdf[i,3] <- word3
    tempdf[i,4] <- master_list[i,2]
    
    
    
  }
  
  return(tempdf)  
  
  
}

## Steps - updating the master list
master_list <- trigram_table
master_list <- data.frame(master_list[,1])
names(master_list) <- c("trigram_list")
current_list <- make_trigram(text1)

master_list <- rbind(master_list,current_list)

master_list1 <- data.frame(table(master_list),stringsAsFactors=FALSE)

freq_df <- tri_freq(master_list1)

saveRDS(master_list,"freq_tri.rds")
saveRDS(freq_df,"master_freq.rds")

## Reference for 3 gram

Davies, Mark. (2011) N-grams data from the Corpus of Contemporary American English (COCA). Downloaded from http://www.ngrams.info on April 25, 2015

## reading the downloaded corpus

web_trigram <- read.table("w3_.txt")
web_bigram <- read.table("w2_.txt")

web_bigram1 <- subset(web_bigram,web_bigram$V1>280)

probtable <- data.frame(web_trigram1$V1)
biprotab <- data.frame(web_bigram1$V1)

web_trigram2 <- apply(probtable,1,function(x) x/500)
web_trigram2 <- data.frame(web_trigram2)

web_bigram2 <- apply(biprotab,1,function(x) x/1900)
web_bigram2 <- data.frame(web_bigram2)

## Joining the data frame

web_trigram3 <- cbind(web_trigram1,web_trigram2)
web_bigram3 <- cbind(web_bigram1,web_bigram2)

trigram_supp1 <- data.frame(web_trigram3[,2:5])
bigram_supp1 <- data.frame(web_bigram3[,2:4])


names(trigram_supp1) <- c("trigram1","trigram2","trigram3","prob")
names(bigram_supp1) <- c("bigram1","bigram2","prob")

tri_own1 <- readRDS("trigram3.rds")
bi_own1 <- readRDS("bigram3.rds")


tri_own <- rbind(tri_own,trigram_supp1)
bi_own <- rbind(bi_own1,bigram_supp1)

saveRDS(tri_own,"trigram3.rds")
saveRDS(bi_own,"bigram3.rds")

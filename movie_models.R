## JMJPFU ## 
# Feb 5 2016 # Model creation with Random forest # 

## model 1 # Random Forest

library(randomForest)
library(readr)
library(caret)

## Random Forest Model

# The labels column has to be converted to factor. Otherwise, RF will treat this as a regression problem.

movie_df$V8 <- as.factor(movie_df$V8) ## Converting the labels to factor column

rf <- randomForest(movie_df[,2:6], movie_df$V8, ntree=200, imp=TRUE, sampsize=50,mtry=4, do.trace=TRUE)

predict(rf,movie_test[,2:7])


############### Saving files using RDS ############################

## For saving specific files as RDs files

#  saveRDS(movie_df,"movies_df.rds") # This will load the dataframe into an rds object
#  temp <- readRDS("movies_df.rds") # This will read the rds file which is a dataframe and store it back into the variable

# By the above methods, we can save models, dataframes etc for deploying in the shiny server

saveRDS(star_wars,'star_wars.rds')
saveRDS(rf,'rf.rds')
saveRDS(positive_unigram_sort,'positive_unigram_sort.rds')
saveRDS(Positive_bigram_sort,'Positive_bigram_sort.rds')
saveRDS(positive_trigram_sort,'positive_trigram_sort.rds')

saveRDS(movie_df,'movies_df.rds')

saveRDS(negative_unigram_sort,'negative_unigram_sort.rds')
saveRDS(negative_bigram_sort,'negative_bigram_sort.rds')
saveRDS(negative_tri_sort,'negative_tri_sort.rds')
saveRDS(samp,'upload_template.rds')
saveRDS(star_wars,'star_wars.rds') ## This is the word cloud
write(star_wars,'star_wars.txt')
######################### End of RDS save code ###################

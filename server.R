## JMJPFU ##
# 7 Feb 2016 # With my Lords help I am trying my visualisation part of the recommender

library(shiny)
library(dplyr)



shinyServer(function(input,output){
  
  
    lang <- reactive({
      
                      switch(input$langsel,
                              "eng" = movie_master[,1], # The list of movies should come here
                              "hind" = c("Under Construction")
                        
                        
                            ) # End of switch function
      
      
      
                      }) # End of lang() reactive function
    
    output$movie_list <- renderUI({
                                  
                                    mov <- lang() # Stores the selected list of movies in this variable
                                    
                                    selectInput("movie_l","Choose your movies:",choices = mov) # In this step
                                    # a new dropdown is created in the UI side and the list of movies output from lang()
                                    # function is stored in the "movies" variable.
                                    # The movies variable is now available for call elsewhere in the program
      
      
      
                                  }) # End of movie_list renderUI output mode
    
                     
  
    output$rating <- renderGvis({
      
                                  temp <-  input$movie_l # This is the movie selected from the drop down in the side panel after language is selected.
                                  # By saving the movie selection we can do other activities like plotting the sentiment. Getting a word cloud etc
                                  
                                  mov_class <- paste0(movie_master[movie_master[,1]==temp,8]) # In the renderUI, it only accepts a charachter. If the output is a number it dosent accept
                                  # so paste0 was done
                                  
                                  
                                  df <- data.frame(Label = "Rating", Value = as.numeric(mov_class))
                                  gplot <- gvisGauge(df, options=list(min=1, max=5, greenFrom=4,greenTo=5, yellowFrom=2, yellowTo=4,redFrom=0, redTo=2, width=150, height=150))
                                 
                                                          
                                  }) # End of output$rating rendeGvis output mode
  
    output$textrating1 <- renderUI({
                                  temp <-  input$movie_l # This is the movie selected from the drop down in the side panel after language is selected.
      # By saving the movie selection we can do other activities like plotting the sentiment. Getting a word cloud etc
      
                                mov_class <- as.numeric(paste0(movie_master[movie_master[,1]==temp,8])) # In the renderUI, it only accepts a charachter. If the output is a number it dosent accept
      # so paste0 was done
                                
      
                                            
                              rating_decision <- Decision_mov(mov_class) # Calling the function to get the decision class of the movie
      
                                  })
    
    
    
    
    
  
   output$classplot <- renderPlot({
                                    
                                  temp <- input$movie_l # Select the movie
                                  mov_class <- movie_master[movie_master[,1]==temp,8] # Selects the class fo the movie
                                  x = as.numeric(mov_class)
                                  y = as.numeric(mov_class)
                                  plot(x,y,type="p",col="red",ylim=c(6,1),xlim=c(1,6)) # Do a simple plot for trial
     
                                 
     
     
                                  }) # End of the renderplot function
   
   
      wordcl <- reactive({ 
        
                          temp <-  input$movie_l ## Getting the input movie name
                          temp1 <- paste0(temp,"_","pos",".rds") # Pasting the movie name with .rds for reading the positive terms
                          temp2 <- paste0(temp,"_","neg",".rds") # Pasting the movie name with .rds for reading the negative terms
                          temp3 <- paste0(temp,"_","df",".rds") # Pasting the movie name with .rds for reading the data frame of words
                          
                          pos_terms_mov <- readRDS(temp1)
                          neg_terms_mov <- readRDS(temp2)
                          plotdf <- readRDS(temp3)
                          
                          list(pos_terms_mov = pos_terms_mov,neg_terms_mov=neg_terms_mov,plotdf_mov=plotdf)
     
                          })## End of wordcl reactive function
      
      
      
      
      
    output$wordplot_pos <- renderPlot({
                                  
                                  # if(is.null(movie1)){return()} # This was to try and suppress the error message. Have not worked yet
      
                                  cloud1 <- wordcl()$pos_terms_mov 
                                  
                                  pos_cloud_mov <- wordcloud(names(cloud1),cloud1, scale=c(4,0.5),min.freq = 4, max.words=50,colors=brewer.pal(8, "Dark2")) # making the wordcloud
                                  
                                  
                                  if(is.null(pos_cloud_mov)){return("No Positive terms")}else{return(pos_cloud_mov)}
                                 
                                     })  ## End of wordplot_pos ouput function
    
    
    output$wordplot_neg <- renderPlot({
      
      
                                  cloud1 <- wordcl()$neg_terms_mov 
      
                                  neg_cloud_mov <- wordcloud(names(cloud1),cloud1, scale=c(4,0.5),min.freq = 4, max.words=50,colors=brewer.pal(8, "Dark2")) # making the wordcloud
      
      
                                  if(is.null(neg_cloud_mov)){return("No Positive terms")}else{return(neg_cloud_mov)}
      
      
      
                                  })  ## End of wordplot_neg ouput function
    
    
    output$wordplot_mov <- renderGvis({
      
                          df <- wordcl()$plotdf_mov
                          df1 <- data.frame(Sentiment=NA,val=NA)
                          denom <- as.numeric(df[1,1] + df[1,2] )
                          df1[1,1] <- "% Positive"
                          df1[1,2] <- round(as.numeric((df[1,1]/denom)*100))
                          df1[2,1] <- "% Negative"
                          df1[2,2] <- round(as.numeric((df[1,2]/denom)*100))
                          
                          gplot <- gvisGauge(df1, options=list(min=0, max= 100, greenFrom=4,greenTo=5, yellowFrom=2, yellowTo=4,redFrom=0, redTo=2, width=300, height=300))
      
      
      
      
    })  ## End of wordplot_mov ouput function
    
    
    
    
#################################### The below reactive function is to clean the dataset #########
    
    textclean <- reactive({
      
                              text1 <- input$movtext # This is loading the uploaded file 
                              #movtxt1 <- scan(text1$datapath,character(0),sep=".") # This is split into multiple sentences
                              movtxt1 <- readLines(text1$datapath) # This is to read the text into a variable
      
                              mov_cln <- cleansamp(movtxt1) # First Clean the movie
      
                          }) # End of textclean reactive function
    
    
    
    textanalysis <- reactive({
      ########################### This is the reactive function which produces sentiments ################
      
      mov_cln <- textclean() # Storing the cleaning output in a variable
      
      
      ############ The below is the portion for rating the movie based on an RF model ####################
      moviedf <- data.frame(unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE) # Makes an empty dataframe for storing the values
      
      ####### Process 1 - Cleaning and getting the important variables ready ##########################
      
      
      mov_rev <- concat(mov_cln,collapse= " ") # Concatinating the text for ngram creation
      
      unilen <- length(get.ngrams(ngram(mov_rev,n=1))) # Take length of unigrams for % calculation
      bilen  <- length(get.ngrams(ngram(mov_rev,n=2))) ## Take length of bigram for % calculation
      trilen  <- length(get.ngrams(ngram(mov_rev,n=3))) ## Take length of trigram for % calculation
      
      ## Process 2 -  Finding the % of key words by comparison with templates###
      
      # Process 2, Step 1 - % of positive unigrams #########
      
      temp <- data.frame(positive_unigram_sort[,1]) # Loading the ngram template
      
      # The below for loop calculates the number of times each of the template word occurs in the whole text
      
      for(i in 1:nrow(temp)){
        
        wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
        
        temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
        
        temp[i,2] <- max(temp1) # 
        
        
      }
      
      moviedf[1,1] <- as.numeric(sum(temp[,2])/unilen) ## Loading the % value in the data frame
      
      # Process 2, Step 2 - % of Negative unigram ###
      
      
      temp <- data.frame(negative_unigram_sort[,1]) # Loading the ngram template
      
      # The below for loop calculates the number of times each of the template word occurs in the whole text
      
      for(i in 1:nrow(temp)){
        
        wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
        
        temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
        
        temp[i,2] <- max(temp1) # 
        
        
      }
      
      moviedf[1,4] <- as.numeric(sum(temp[,2])/unilen) ## Loading the % value in the data frame 
      
      # Process 2, Step 3 - % of Positive bigram ###
      
      
      temp <- data.frame(Positive_bigram_sort[,1])
      
      # The below for loop calculates the number of times each of the template word occurs in the whole text
      
      for(i in 1:nrow(temp)){
        
        wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
        
        temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
        
        temp[i,2] <- max(temp1) # Not a necessary step
        
        
      }
      
      moviedf[1,2] <- as.numeric(sum(temp[,2])/bilen) ## Loading the % value in the data frame 
      moviedf_pos <- temp %>% filter(V2 > 1) # This is for storing the positive bigrams for cloud analysis
      pos_sum <- sum(temp[,2]) # This is to get the sum of all positive bigram terms
      
      
      
      # Process 2, Step 4 - % of Negative bigram ###
      
      
      temp <- data.frame(negative_bigram_sort[,1])
      
      # The below for loop calculates the number of times each of the template word occurs in the whole text
      
      for(i in 1:nrow(temp)){
        
        wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
        
        temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
        
        temp[i,2] <- max(temp1) # 
        
        
      }
      moviedf[1,5] <- as.numeric(sum(temp[,2])/bilen) ## For Negative Bigrams
      moviedf_neg <- temp %>% filter(V2 > 1) # This is for storing the negative bigrams for cloud analysis
      neg_sum <- sum(temp[,2]) # This is to get the sum of all negative bigram terms
      
      
      
      ## Finding values for Positive Trigram ###
      
      
      temp <- data.frame(positive_trigram_sort[,1])
      
      # The below for loop calculates the number of times each of the template word occurs in the whole text
      
      for(i in 1:nrow(temp)){
        
        wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
        
        temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
        
        temp[i,2] <- max(temp1) # 
        
        
      }
      
      moviedf[1,3] <- as.numeric(sum(temp[,2])/trilen) # For Positive trigrams
      
      ## Finding values for Negative Trigram ###
      
      
      temp <- data.frame(negative_tri_sort[,1])
      
      # The below for loop calculates the number of times each of the template word occurs in the whole text
      
      for(i in 1:nrow(temp)){
        
        wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
        
        temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
        
        temp[i,2] <- max(temp1) # 
        
        
      }
      
      moviedf[1,6] <-  as.numeric(sum(temp[,2])/trilen)
      
      moviedf[1,1:6]
      ################################### End of Test ########
      
      pred <- predict(model1,moviedf[1,1:6]) # Prediction function is working.
      
      list(pred=pred,moviedf = moviedf,pos_cloud = moviedf_pos,pos_sum=pos_sum,neg_cloud = moviedf_neg,neg_sum=neg_sum) # Listing down all the outputs
      
      
      
      
      
    }) # End of textanalysis reactive function
    
    
    
      
      
      
      
      output$textrating <- renderUI({
        
                                      rating_test <- textanalysis()$pred ## Loads the output of the text analysis as rating
                                       #                                       
                                      rating_decision <- Decision_mov(rating_test)
                                      
                                    })
      
      output$googview <- renderGvis({
        
        val <- paste(textanalysis()$pred)
        
        df <- data.frame(Label = "Rating", Value = as.numeric(val))
        gplot <- gvisGauge(df, options=list(min=1, max=5, greenFrom=4,greenTo=5, yellowFrom=2, yellowTo=4,redFrom=0, redTo=2, width=150, height=150))
        
      })
      
     
      ########################### This is the reactive function for aggregating positive and negative sentiments ################
      
      textcloud1 <- reactive({
        
        
        ## Creating the positive & Negative clouds #####
        pos_cloud <- textanalysis()$pos_cloud ## Loading the positive terms
        neg_cloud <- textanalysis()$neg_cloud ## Loading the negative terms
        
        isolate({
          withProgress({
            setProgress(message = "Processing Corpus ......") # This is for showing the message on the UI that some activity is happening
            pos_terms <- termmatrix(pos_cloud[,1]) # Creating the term document matrix using the function in global.R
            neg_terms <- termmatrix(neg_cloud[,1]) # Creating the negative term document matrix
            
                      }) # End of progress function
          
                }) # End of isolate function
        
            plotdf <- data.frame(positive= as.numeric(textanalysis()$pos_sum),negative = as.numeric(textanalysis()$neg_sum)) # Creating a datframe for later plotting
        
            list(pos_terms = pos_terms,neg_terms=neg_terms,plotdf=plotdf)
        
        
                }) # End of textcloud1 reactive function
      
  
     output$wordplot2 <- renderPlot({
        
        # if(is.null(movie1)){return()} # This was to try and suppress the error message. Have not worked yet
        
        cloud1 <- textcloud1()$pos_terms ## Loading the positive terms
        pos_cloud <- wordcloud(names(cloud1),cloud1, scale=c(4,0.5),min.freq = 1, max.words=50,colors=brewer.pal(8, "Dark2")) # making the wordcloud
        
        if(is.null(pos_cloud)){return("No Positive terms")}else{return(pos_cloud)}
        
      })  ## End of wordplot2 ouput function
     
     
      
     output$wordplot3 <- renderPlot({
       
       # if(is.null(movie1)){return()} # This was to try and suppress the error message. Have not worked yet
       
       
       cloud2 <- textcloud1()$neg_terms ## Loading the negative terms
       
       neg_cloud <- wordcloud(names(cloud2),cloud2, scale=c(4,0.5),min.freq = 1, max.words=50,colors=brewer.pal(8, "Dark2")) # making the wordcloud
       if(is.null(neg_cloud)){return("No Negative terms")}else{return(neg_cloud)}
      
       
     })  ## End of wordplot3 ouput function
      
     output$wordplot4 <- renderGvis({
       
       df <- textcloud1()$plotdf
       df1 <- data.frame(Sentiment=NA,val=NA)
       denom <- as.numeric(df[1,1] + df[1,2] )
       df1[1,1] <- "% Positive"
       df1[1,2] <- round(as.numeric((df[1,1]/denom)*100))
       df1[2,1] <- "% Negative"
       df1[2,2] <- round(as.numeric((df[1,2]/denom)*100))
       
       
       #df <- data.frame(Label = "Rating", Value = as.numeric(val))
       gplot <- gvisGauge(df1, options=list(min=0, max= 100, greenFrom=4,greenTo=5, yellowFrom=2, yellowTo=4,redFrom=0, redTo=2, width=300, height=300))
       
       
     
       #barplot(df[,1],df[,2])
       
       
     })  ## End of wordplot4 ouput function
     
     
     
      
  
            }) # End of Shinyserver (input,output) function
  
  
  








  
  
  
  
  
  
  
            
            
         
## JMJPFU ## 3-nov-2015 ##

## This is the new data cleaning Script for the second movie site

mov_imbd <- sitenow_movrat

write(mov_imbd,file="movie.txt")

tempclean <- strsplit(mov_imbd,split="[0-9][.][0-9]") ## not very good format
tempclean <- strsplit(mov_imbd,split="[0-9][\r]") ## This gets all the movie names
ml <- length(mov_imbd)

tempclean <- gsub("[^[:blank:]]", "", mov_imbd) ## not a good format
tempclean <- gsub("\t","",tempclean) ## Not good


tempclean <- gsub("\\n","",mov_imbd) ## not good
tempclean <- gsub("[[\r][\n][\t]]","",mov_imbd) ## Not good
tempclean <- gsub("\\s","",tempclean) ## not good
tempclean
templean = str_trim(mov_imbd,side="left")

tempclean = str_split(mov_imbd,pattern = "\\n") ## Split it according to all new lines
length(tempclean[[1]]) ## This has around 344 lines
tempclean2 = c() ## Defining a null placeholder
## Below code is to clean up the whole line and make it into a matrix form with various elements
for(i in 1:length(tempclean[[1]])){
  
  tempclean2[i] = str_trim(tempclean[[1]][i]) ## removes the whitespaces from each line
  
}

tempclean3 = c()
cou = 1
## The below format makes it into a good matrix form with 
for(i in 1:length(tempclean2)){
  
  if(tempclean2[i] != ""){
    
  
  tempclean3[cou] = str_trim(tempclean2[i])
  cou = cou+1
  }
}







movie_dat <- function(mov_imdb){
  mov_imbd <- sitenow_movrat
  library(stringr)
  ml <- length(mov_imdb)
  
  for(mov in 1:ml){
    
    tempclean <- mov_imdb[mov] ## Takes the text of the ratings
    
    tempclean <- gsub("\\n","",tempclean) ## Removes all the new lines
    tempclean <- strsplit(tempclean,split="[()]") ## Splits the text so that the name, year and the rest of text is generated
    movie_rev_dat[mov,1] <- tempclean[[1]][1] # extracting the name of the movie
    movie_rev_dat[mov,2] <- tempclean[[1]][2] # extracting the year of the movie
    
    ## Extracting the ratings from the movie
    tempbal <- strsplit(tempclean[[1]][[3]],split="[[1-9][.][1-9][/]]")
    tempbal <- strsplit(tempbal[[1]][[1]],split="/10")
    rating_str <- str_extract_all(tempbal[[1]][1],pattern="[[1-9][.][1-9]]")
    le <- length(rating_str[[1]])
    movie_rev_dat[mov,3] <- as.numeric(paste0(rating_str[[1]][(le-2):le],collapse=""))
    
    ## Extracting the Director from the movie trials
    
    tempbal <- strsplit(tempbal[[1]][[2]],split="With:")
    dir_str <- strsplit(tempbal[[1]][[1]],split="Dir")
    dir_str <- str_extract_all(dir_str,pattern="([:][\\w\\s]+)") # Extracts all words after the semicolon along with the spaces
    dir_str <- gsub(":","",dir_str[[1]][1])
    dir_str <- gsub("^[\\s]","",dir_str[[1]][1])
    movie_rev_dat[mov,4] <- gsub("[\\s]$","",dir_str[[1]][1])
    
    ## Extracting cast
    tempbal <- strsplit(tempbal[[1]][2],split=",")
    le <- length(tempbal[[1]])
    if((le-1) >= 2){movie_rev_dat[mov,5] <- tempbal[[1]][1]
    movie_rev_dat[mov,6] <- tempbal[[1]][2]}
    
    ## Extracting genre
    tempbal <- strsplit(tempbal[[1]][le],split="([\\w]\\s[|])\\s\\[\\w])")
    tempbal <- strsplit(tempbal[[1]][1],split="\\s")
    y <- str_detect(tempbal[[1]],pattern="[|]") ## looking for the pipe sign for split
    
    #   for(i in 1:length(y)){if(y[i] =="TRUE"){a <- i 
    #       movie_rev_dat[mov,7] <- tempbal[[1]][(a-1)]
    #       movie_rev_dat[mov,8] <- tempbal[[1]][(a+1)]
    #       break}else{a <- "NULL"} ## The for loop has to break once the TRUE condition is met otherwise the below loop will execute
    #     }
    
    ## If there is only one genre present then the pipe will not be there so the below loop will take care
    
    #   if(a=="NULL"){
    #     y <- str_detect(tempbal[[1]],pattern="\\D")
    #     cont = c()
    #   for(i in 1:length(y)){
    #     if(y[i]=="TRUE"){
    #       cont = c(cont,i) ## making a counter to find all the non digit values. The genre will be penultimate value
    #           }
    #   } ## Ending of inner for loop
    #   
    #     
    #     
    # } #ending the second If loop
    
    a <- length(cont)
    a <- cont[(a-1)]
    
    movie_rev_dat[mov,7] <- tempbal[[1]][(a)]
    
    
    ## Extracting length
    y <- str_detect(tempbal[[1]],pattern="mins.") ## Looking for digits within the string
    # for(i in 1:length(y)){
    #   if(y[i] == "TRUE"){a <- i}
    # }
    
    movie_rev_dat[mov,9] <- as.numeric(tempbal[[1]][(a-1)])
    
  } ## For loop ends
  
  movie_rev_dat
  
} ## Function ends


# JMJPFU # 9-Dec 2015

library(stringr)

ynew <- strsplit(mov_imbd,split=c("[0-9][.][0-9]","\\s+[0-9]\\s+")) ## Not great format

ynew <- strsplit(mov_imbd,split = "\\d") ## not good format either

## Strategy for extracting the ratings
# Look for a line starting with a digit.
#Find the index of such line and make it the starting index
# Find the index of the next non character space
## make it the end index
## Extract all charachters between the starting index and ending index

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
    
    
    tempclean3[cou] = str_trim(tempclean2[i]) ## This is to remove any whitespaces 
    cou = cou+1
  }
}

## Once the dataframe is properly arranged, we can go through the dataframe and find 
# those data frames which start with a number

library(stringr)

tempclean2 = matrix(0,nrow=40,ncol=2)
cou = 0
for(i in 1:46){
  
  if(str_detect(tempclean3[i],"^[0-9]")){
    
    cou = cou+1
    tempclean2[cou,1] = tempclean3[i]
    tempclean2[cou,2] = tempclean3[i+1]
    
  }
  
  
}

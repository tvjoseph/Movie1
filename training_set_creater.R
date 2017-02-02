# JMJPFU # 
# 28 Jan 2016
# Let us now build a training set for various categories of movies

library(rvest)
library(dplyr)
library(tm)
library(ngram)
library(stringr)


## Genre Action & Adventure

# The below is a for loop to loop over the critics reviews and get all reviews concatinated

mov_txt <- function(mname,j,k){

  mov_txt <- NULL
  
  mov_name <- paste0(mname)
  
  fir <- paste0("http://www.rottentomatoes.com/m/",mov_name,"/reviews/")


  # Loop 1 - This is for the first page
  
 

  ###################### Critics First Page Reviews ##############
  url <- paste0(fir)

  mov1 <- read_html(url)
  
  mov_trn <- mov1 %>% html_nodes(".the_review") %>% html_text() ## for critics
  
  mov_txt <- c(mov_txt,mov_trn,mov_trn1)
  
##################### End of code ##################
  
############# For Critics ##################
  if(j>1){

    for(i in 2:j){
  
    url <- paste0(fir,"?page=",i,"&sort=") # For Critic
  
    mov1 <- read_html(url)

    mov_trn <- mov1 %>% html_nodes(".the_review") %>% html_text() ## for critics
  

    mov_txt <- c(mov_txt,mov_trn)

    } # End of for loop
    
  } # End of first if loop

################## End of Code #################
  
  ###################### Users First Page Reviews ##############
  url <- paste0(fir,"?type=user")
  
  mov1 <- read_html(url)
 
  mov_trn1 <- mov1 %>% html_nodes(".user_review") %>% html_text() ## For users
  
  mov_txt <- c(mov_txt,mov_trn,mov_trn1)
  
  ##################### End of code ################## 
  
  
################## For Users #################  
  
  if(k>1){

    for(i in 2:k){
 
    url <- paste0(fir,"?page=",i,"&type=user&sort=") # For user
  
    mov1 <- read_html(url)
  
  
    mov_trn1 <- mov1 %>% html_nodes(".user_review") %>% html_text() ## For users
  
    mov_txt <- c(mov_txt,mov_trn1)
  
    } # End of second for loop
    
  } # End of second if loop

############# End of Code ####################

  return(mov_txt)  
  
}









# Class 1 #######
star_wars <- mov_txt
revnant <- mov_txt("the_revenant_2015",14,52)
panda <- mov_txt("kung_fu_panda_3",5,17)
bigshort <- mov_txt("the_big_short",12,52)
brooklyn <- mov_txt("brooklyn",10,50)
room <- mov_txt("room_2015",10,46)
spotlight <- mov_txt("spotlight_2015",12,52)
ipman <- mov_txt("ip_man_3",2,5)
years45 <- mov_txt("45_years",7,8)
carol <- mov_txt("carol",10,31)
anomalisa <- mov_txt("anomalisa",8,16)
ladyinvan <- mov_txt("the_lady_in_the_van",4,5)
creed <- mov_txt("creed_2015",11,52)
martian <- mov_txt("the_martian",15,52)
bridgespies<- mov_txt("bridge_of_spies",13,52)
sonofsaul <- mov_txt("son_of_saul",6,4)
peanut <- mov_txt("the_peanuts_movie",8,52)
mustang <- mov_txt("mustang_2015",4,3)
hubble <- mov_txt("hubble_3d",2,16)
macbeth<- mov_txt("macbeth_2015",8,15)
chiraq <- mov_txt("chi_raq",5,9)
straightoutta <- mov_txt("straight_outta_compton",10,52)
thediary <- mov_txt("the_diary_of_a_teenage_girl",7,10)
 sicario <- mov_txt("sicario_2015",11,52)
thewalk <- mov_txt("the_walk_2015",11,46)
hitman <- mov_txt("hitman_agent_47",6,38)
bone <- mov_txt("bone_tomahawk",3,18)
missionim <- mov_txt("mission_impossible_rogue_nation",14,52)
antman <- mov_txt("antman",13,52)
amy<- mov_txt("amy_2015",9,31)
goodnight<- mov_txt("goodnight_mommy",5,17)
mississippi<- mov_txt("mississippi_grind",5,7)
brilliantyount<- mov_txt("a_brilliant_young_mind",4,7)
patel<- mov_txt("meet_the_patels_2014",3,3)
girlfriend<- mov_txt("the_new_girlfriend",4,2)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)


movie_df[2,] <- moviefeat(revnant,"revnant")
movie_df[3,] <- moviefeat(panda,"panda")
movie_df[4,] <- moviefeat(bigshort,"bigshort")
movie_df[5,] <- moviefeat(brooklyn,"brooklyn")
movie_df[6,] <- moviefeat(room,"room")
movie_df[7,] <- moviefeat(spotlight,"spotlight")
movie_df[8,] <- moviefeat(ipman,"ipman")
movie_df[9,] <- moviefeat(years45,"years45")
movie_df[10,] <- moviefeat(carol,"carol")
movie_df[11,] <- moviefeat(anomalisa,"anomalisa")
movie_df[12,] <- moviefeat(ladyinvan,"ladyinvan")
movie_df[13,] <- moviefeat(creed,"creed")
movie_df[14,] <- moviefeat(martian,"martian")
movie_df[15,] <- moviefeat(bridgespies,"bridgespies")
movie_df[16,] <- moviefeat(sonofsaul,"sonofsaul")
movie_df[17,] <- moviefeat(peanut,"peanut")
movie_df[18,] <- moviefeat(mustang,"mustang")
movie_df[19,] <- moviefeat(hubble,"hubble")
movie_df[20,] <- moviefeat(macbeth,"macbeth")
movie_df[21,] <- moviefeat(chiraq,"chiraq")
movie_df[22,] <- moviefeat(straightoutta,"straightoutta")
movie_df[23,] <- moviefeat(thediary,"thediary")
movie_df[24,] <- moviefeat(sicario,"sicario")
movie_df[25,] <- moviefeat(thewalk,"thewalk")
movie_df[26,] <- moviefeat(hitman,"hitman")
movie_df[27,] <- moviefeat(bone,"bone")
movie_df[28,] <- moviefeat(missionim,"missionim")
movie_df[29,] <- moviefeat(antman,"antman")
movie_df[30,] <- moviefeat(amy,"amy")
movie_df[31,] <- moviefeat(goodnight,"goodnight")
movie_df[32,] <- moviefeat(mississippi,"mississippi")
movie_df[33,] <- moviefeat(brilliantyount,"brilliantyount")
movie_df[34,] <- moviefeat(patel,"patel")
movie_df[35,] <- moviefeat(girlfriend,"girlfriend")


# Class 2 ########

finesthours <- mov_txt("the_finest_hours",6,10)
hateful <- mov_txt("the_hateful_eight",12,52)
sister<- mov_txt("sisters_2015",7,47)
joy <- mov_txt("joy_2014",11,52)
gooddino <- mov_txt("the_good_dinosaur",9,52)
hungergames<- mov_txt("the_hunger_games_mockingjay_part_2",12,52)
danishgirl <- mov_txt("the_danish_girl_2015",9,31)
concussion <- mov_txt("concussion_2015",7,33)
spectre <- mov_txt("spectre_2015",15,52)
trumbo <- mov_txt("trumbo",6,19)
goosebum <- mov_txt("goosebumps_2015",6,49)
youth <- mov_txt("youth_2015",8,13)
suffrergate <- mov_txt("suffragette",9,16)
manup <- mov_txt("man_up_2015",4,9)
truth <- mov_txt("truth_2015",6,8)
everest <- mov_txt("everest_2015",10,52)
visit <- mov_txt("the_visit_2015",10,50)
infinitepolar <- mov_txt("infinitely_polar_bear",6,7)
sleepingwith<- mov_txt("sleeping_with_other_people",6,7)
blackmass<- mov_txt("black_mass_2015",12,52)
pawnsacrifice <- mov_txt("pawn_sacrifice",5,12)
malala <- mov_txt("he_named_me_malala",6,4)
prophet <- mov_txt("the_prophet_2014",3,2)
assassin<- mov_txt("the_assassin_2015",5,5)
rosencraz<- mov_txt("rosencrantz_and_guildenstern_are_dead",2,52)

movie_df[36,] <- moviefeat(finesthours,"finesthours")
movie_df[37,] <- moviefeat(hateful,"hateful")
movie_df[38,] <- moviefeat(sister,"sister")
movie_df[39,] <- moviefeat(joy,"joy")
movie_df[40,] <- moviefeat(gooddino,"gooddino")
movie_df[41,] <- moviefeat(hungergames,"hungergames")
movie_df[42,] <- moviefeat(danishgirl,"danishgirl")
movie_df[43,] <- moviefeat(concussion,"concussion")
movie_df[44,] <- moviefeat(spectre,"spectre")
movie_df[45,] <- moviefeat(trumbo,"trumbo")
movie_df[46,] <- moviefeat(goosebum,"goosebum")
movie_df[47,] <- moviefeat(youth,"youth")
movie_df[48,] <- moviefeat(suffrergate,"suffrergate")
movie_df[49,] <- moviefeat(manup,"manup")
movie_df[50,] <- moviefeat(truth,"truth")
movie_df[51,] <- moviefeat(everest,"everest")
movie_df[52,] <- moviefeat(visit,"visit")
movie_df[53,] <- moviefeat(infinitepolar,"infinitepolar")
movie_df[54,] <- moviefeat(sleepingwith,"sleepingwith")
movie_df[55,] <- moviefeat(blackmass,"blackmass")
movie_df[56,] <- moviefeat(pawnsacrifice,"pawnsacrifice")
movie_df[57,] <- moviefeat(malala,"malala")
movie_df[58,] <- moviefeat(prophet,"prophet")
movie_df[59,] <- moviefeat(assassin,"assassin")
movie_df[60,] <- moviefeat(rosencraz,"rosencraz")


# Class 3 ########

secretsold <- mov_txt("13_hours_the_secret_soldiers_of_benghazi",8,52)
lazer <- mov_txt("lazer_team",2,5)
heartsea <- mov_txt("in_the_heart_of_the_sea",10,37)
hoteltrans <- mov_txt("hotel_transylvania_2",5,52)
inern <- mov_txt("the_intern",8,52)
irrational <- mov_txt("irrational_man",8,10)
walkinwoods <- mov_txt("a_walk_in_the_woods_2015",8,22)
ted2 <- mov_txt("ted_2",10,52)
mazerunner<- mov_txt("maze_runner_the_scorch_trials",7,52)
minions <- mov_txt("minions",10,52)
cooties<- mov_txt("cooties",2,10)
Freeheld<- mov_txt("771417802",5,3)
learningtodri <- mov_txt("learning_to_drive",4,5)
samba<- mov_txt("samba_2014",3,2)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)


movie_df[61,] <- moviefeat(secretsold,"secretsold")
movie_df[62,] <- moviefeat(lazer,"lazer")
movie_df[63,] <- moviefeat(heartsea,"heartsea")
movie_df[64,] <- moviefeat(hoteltrans,"hoteltrans")
movie_df[65,] <- moviefeat(inern,"inern")
movie_df[66,] <- moviefeat(irrational,"irrational")
movie_df[67,] <- moviefeat(walkinwoods,"walkinwoods")
movie_df[68,] <- moviefeat(ted2,"ted2")
movie_df[69,] <- moviefeat(mazerunner,"mazerunner")
movie_df[70,] <- moviefeat(minions,"minions")
movie_df[71,] <- moviefeat(cooties,"cooties")
movie_df[72,] <- moviefeat(Freeheld,"Freeheld")
movie_df[73,] <- moviefeat(learningtodri,"learningtodri")
movie_df[74,] <- moviefeat(samba,"samba")




# Class 4 ########

boy <- mov_txt("the_boy",2,14)
dadhome <- mov_txt("daddys_home_2014",5,44)
janegot<- mov_txt("jane_got_a_gun",2,3)
alvinchip <- mov_txt("alvin_and_the_chipmunks_the_road_chip",3,20)
burnt <- mov_txt("burnt",6,22)
ourbrand <- mov_txt("our_brand_is_crisis_2016",7,7)
greeninferno <- mov_txt("the_green_inferno",4,28)
scout <- mov_txt("scouts_guide_to_the_zombie_apocalypse",4,18)
captive<- mov_txt("captive_2015",3,4)
perfectguy <- mov_txt("the_perfect_guy",2,20)
pan <- mov_txt("pan_2015",9,49)
warroom <- mov_txt("war_room_2015",2,52)
knock<- mov_txt("knock_knock_2015",3,19)
minutes90<- mov_txt("90_minutes_in_heaven",2,5)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)

movie_df[75,] <- moviefeat(boy,"boy")
movie_df[76,] <- moviefeat(dadhome,"dadhome")
movie_df[77,] <- moviefeat(janegot,"janegot")
movie_df[78,] <- moviefeat(alvinchip,"alvinchip")
movie_df[79,] <- moviefeat(burnt,"burnt")
movie_df[80,] <- moviefeat(ourbrand,"ourbrand")

movie_df[81,] <- moviefeat(greeninferno,"greeninferno")
movie_df[82,] <- moviefeat(scout,"scout")
movie_df[83,] <- moviefeat(captive,"captive")
movie_df[84,] <- moviefeat(perfectguy,"perfectguy")
movie_df[85,] <- moviefeat(pan,"pan")
movie_df[86,] <- moviefeat(warroom,"warroom")
movie_df[87,] <- moviefeat(knock,"knock")
movie_df[88,] <- moviefeat(minutes90,"minutes90")

# Class 5 ##########

ridealong <- mov_txt("ride_along_2",5,18)
dirtygrad <- mov_txt("dirty_grandpa",5,20)  
fifthwave <- mov_txt("the_fifth_wave",5,25)
normofnorth <- mov_txt("norm_of_the_north",3,10)
forest <- mov_txt("the_forest_2016",5,1)
pointbreak <- mov_txt("point_break_2013",4,23)
lastwitch <- mov_txt("the_last_witch_hunter",6,32)  
rockthe<- mov_txt("rock_the_kasbah",5,5)
sinister <- mov_txt("sinister_2",4,27)
paranormal <- mov_txt("paranormal_activity_the_ghost_dimension",4,27)
fantasticfour <- mov_txt("fantastic_four_2015",11,52)
transporter<- mov_txt("the_transporter_refueled",5,18)
stonewall<- mov_txt("stonewall_2015",4,4) 
gem<- mov_txt("jem_and_the_holograms_2015",2,11)
shanghai <- mov_txt("shanghai",2,9)

<- mov_txt("the_finest_hours",6,10)


movie_df[89,] <- moviefeat(ridealong,"ridealong")
movie_df[90,] <- moviefeat(dirtygrad,"dirtygrad")
movie_df[91,] <- moviefeat(fifthwave,"fifthwave")
movie_df[92,] <- moviefeat(normofnorth,"normofnorth")
movie_df[93,] <- moviefeat(pointbreak,"pointbreak")
movie_df[94,] <- moviefeat(lastwitch,"lastwitch")
movie_df[95,] <- moviefeat(rockthe,"rockthe")
movie_df[96,] <- moviefeat(sinister,"sinister")
movie_df[97,] <- moviefeat(paranormal,"paranormal")
movie_df[98,] <- moviefeat(fantasticfour,"fantasticfour")
movie_df[99,] <- moviefeat(transporter,"transporter")
movie_df[100,] <- moviefeat(stonewall,"stonewall")
movie_df[101,] <- moviefeat(gem,"gem")
movie_df[102,] <- moviefeat(shanghai,"shanghai")


AcAd_good <- c(AcAd_good,mov_txt) # A list of good action and adventure movies
mov_40less <- mov_txt
Rom_good <- mov_txt


# http://www.rottentomatoes.com/browse/dvd-all/?services=amazon;amazon_prime;flixster;hbo_go;itunes;netflix_iw;vudu#



## Training set - Analyser - This will create corpus and analyse these docs as per frequency of words

############################## Corpus Analyser ############
mov_cln <- cleansamp(AcAd_good) # Cleaning up the movies
mov_corpus <- Corpus(VectorSource(bad_trigram)) # Creating a corpus for further analysis
tot_corpora <- tm_map(mov_corpus,removeWords,stopwords('english')) ## Removing stop words
tot_corpora <- tm_map(tot_corpora, stripWhitespace) # Removing all white spaces
tot_corpora <- tm_map(tot_corpora, PlainTextDocument) # Making them into plain documents

tdm <- TermDocumentMatrix(tot_corpora) ## Creating a term document matrix out of the corpora for frequency analysis
dtm <- DocumentTermMatrix(tot_corpora) ## Terms are in the rows in this matrix
freq <- sort(colSums(as.matrix(dtm)),decreasing = TRUE) # Finding the frequencies of key words
wf <- data.frame(word=names(freq), freq=freq) # This gives the words and its frequencies as a matrix  

findFreqTerms(dtm, lowfreq=10) # This is an alternate method for finding frequencies

############## End of Code ###########################

# Let us now create trigrams and see if we can create corpus and frequency terms

mov_rev <- concat(mov_cln,collapse= " ")

bad_trigram <- ngram(mov_rev,n=3) 
bad_trigram <- get.ngrams(bad_trigram)
bad_trigram <- data.frame(bad_trigram)

# Creating templates of bigrams 

temp_bigram <- ngram(mov_rev,n=2)
temp_bigram <- get.ngrams(temp_bigram)
temp_bigram <- data.frame(temp_bigram)

# Let us now create a hexagram

temp_hexagram <- ngram(mov_rev,n=6)
temp_hexagram <- get.ngrams(temp_hexagram)


# Let us find the frequency through a grep function

for(i in 1:nrow(bad_trigram)){
  
  wd1 <- paste(bad_trigram[i,1]) # Getting each term from the list of positive terms
  
  temp1 <- length(temp_hexagram[grep(wd1,temp_hexagram)])
  
  bad_trigram[i,2] <- max(temp1)
 
  
  
}

temp_trigram <- bad_trigram %>% filter(V2>5) %>% arrange(desc(V2))


########################## Code for merging Key words #############
# The below piece of code is for converting factor to characters and then finding the difference 

## JMJPFU ## 
# 3 Feb 2016 

y_bi <- temp_trigram %>% select(bad_trigram)

x_bi <- bad_tri_freq %>% select(bad_trigram)



y_bi <- data.frame(lapply(y_bi, as.character), stringsAsFactors=FALSE) ## This is required to convert all the factors as charachters in order to merge
x_bi <- data.frame(lapply(x_bi, as.character), stringsAsFactors=FALSE) 

## Let us try a diff function in Dplyr to find those values which are unique in temp_bigram

temp_consol <- setdiff(y_bi,x_bi) ## This is the latest trigram to sort as on 16 feb 2016

## JMJPFU ## 
# 16 Feb 2016 # Finding difference with positive terms

y_bi <- temp_consol
x_bi <- Positive_bigram_sort %>% select(V1)

temp_consol1 <- setdiff(y_bi,x_bi) ##

## We need to define a master of all bigrams which have been sorted so that we dont refer this any more

master_bi <- bad_bi_freq %>% select(V1,V2)
temp_bigram <- temp_bigram %>% select(V1,V2)

master_bi <- rbind(master_bi,temp_bigram) ## This is the master bigram which has been sorted so far on 16th feb 2016

master_tri <- bad_tri_freq %>% select(bad_trigram)

master_tri <- rbind(master_tri,temp_consol) ## This is the master trigram as of 16 Feb 2016


################################## End of Code for adding new terms ##################

# let us filter out the major terms

bad_tri_freq <- bad_trigram %>% filter(V2 > 4)
bad_tri_freq <- bad_tri_freq %>% arrange(desc(V2))

bad_bi_freq <- bad_bigram %>% filter(V2>5) %>% arrange(desc(V2))


## Storing the genre terms

wf_ac_ad_good <- wf %>% filter(freq>9) # This gives a consolidated list. This needs to be further sorted
wf_ac_ad_total <- wf # Just storing the values for later use

bad_rat <- wf %>% filter(freq>9) # Storing the bad ratings in a 
bad_tot <- wf

############# Template creation #############
# Update on Templates #########
Positive_bigram_sort <- Positive_bigram # Update on 16 Feb 2016 
rm(Positive_bigram)
negative_bigram_sort <- negative_bigram # update on 16 Feb 2016
rm(negative_bigram)










#####################




## JMJPFU # 
# 30 Jan 2016 # Lets create the movie templates

write.csv(wf_ac_ad_good,"Action_good.csv")
write.csv(bad_rat,"bad_rat.csv")
write.csv(bad_tri_freq,"bad_tri.csv")
write.csv(bad_bi_freq,"bad_bigram2.csv")
write.csv(temp_consol,"Bigrams_tosort2.csv") # 16 feb sort 2
write.csv(temp_consol,"Trigram_tosort2.csv") # 16 Feb sort 2

## JMJPFU #
# 2 Feb 2016 # Lets start loading the data

################# Feature Engineering ##########

# The process to follow is find the percentage of these critical words as 

## Process

# 1 - Get the reviews of a movie as charachter and clean it. Store in movie_cln
# 2 - Make unigram, bigram and trigram out of the same
# 2 - Make a new dataframe for each movie with each feature
# 3 - The values of the features should come by comparison with the templates
    # % of total positive
    # % of total Negative
    # % of total pos bigram
    # % of total neg bigram
    # % of total pos trigram
    # % of total neg trigram 
# 4 - Create a master dataframe with each movie as row and fetures
# 5 - Use this for creating the training set and also for final classification

# Below is the dataframe to hold all movies data

moviedf <- data.frame(movies = NA,unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE)

# Finding lenght of the bigrams and trigrams
mov_rev <- concat(mov_cln,collapse= " ")

bilen  <- length(get.ngrams(ngram(mov_rev,n=2))) 
trilen  <- length(get.ngrams(ngram(mov_rev,n=3))) 

# Finding the values for each movies

temp <- data.frame(Positive_bigram_sort[,1])


# The below for loop calculates the number of times each of the template word occurs in the whole text

for(i in 1:nrow(temp)){
  
  wd1 <- paste(temp[i,1]) # Getting each term from the list of positive terms
  
  temp1 <- length(mov_cln[grep(wd1,mov_cln)]) ## mov_cln is unique for each movie. It is the sum total of all the reviews for the movie. Each review should be one record
  
  temp[i,2] <- max(temp1) # 
  
  
  
}


moviedf[2,3] <- sum(temp[,2])/bilen # For movie 1 what is the percentage of good bigram reviews
moviedf[2,4] <- sum(temp[,2])/trilen
moviedf[2,6] <- sum(temp[,2])/bilen
moviedf[2,7] <- sum(temp[,2])/trilen

#### Creating the whole training set ################# 3 Feb 2016 #########

# In this attempt, we need to create the training set for every movie. The classes will be divided according
# to the rating in rotten tomatoes
# 100 - 80 : Class 1
# 60 - 80 : Class 2
# 40-60: Class 3
# 20-40: Class 4
# 0-20: Class 5


#################### JMJPFU ##############
# Feb 5 2016 ########
# Steps for making a DF for all movies ############

movie_df <- data.frame(movies = NA,unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE)
iter <- 1 # Iterations to go over the movie data frame



############## Function for storing movies into training set ################ 



# Function to start here

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



############### Code Ends Here ###############


#### Start training ################

# To check the models, we will first go as per the classes we have defined

movie_df[1:35,8] <- 5
movie_df[36:60,8] <- 4
movie_df[61:74,8] <- 3
movie_df[75:88,8] <- 2
movie_df[89:102,8] <- 1

write.csv(movie_df,"movie_df.csv")

## Now that the training set has been created let us now generate some test sets for the model

movie_test <- data.frame(movies = NA,unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE)

# Class 1
queen_of_earth<- mov_txt("queen_of_earth",3,3)
Finderskeeper <- mov_txt("finders_keepers_2015",2,1)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)

movie_test[3,] <- moviefeat(queen_of_earth,"queen_of_earth")
movie_test[4,] <- moviefeat(Finderskeeper,"Finderskeeper")

# Class 2
fat <- mov_txt("fat",1,1)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)

movie_test[5,] <- moviefeat(fat,"fat")

# Class 3
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)

# Class 4

Heist <- mov_txt("heist_2015",2,4)
dragonblade <- mov_txt("dragon_blade",2,4)
strongest <- mov_txt("the_strongest_man",1,1)
zoolander <- mov_txt("zoolander_2",5,4)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)


movie_test[1,] <- moviefeat(Heist,"Heist")
movie_test[2,] <- moviefeat(dragonblade,"dragonblade")
movie_test[6,] <- moviefeat(strongest,"strongest")

# Class 5
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)
<- mov_txt("the_finest_hours",6,10)

movie_test[6,] <- moviefeat(strongest,"strongest")
movie_test[7,] <- moviefeat(strongest,"zoolander")


############### JMJPFU ###################
# 9 Feb 2016  ## 
# Experiments to work with text

sampfile <- readLines("movie_reco.txt") # This will read in the text in seperate objects
sampfile <- concat(sampfile,collapse = " ") # This will collapse it into one big paragraph

sampfile <- strsplit(sampfile,"[.]") # This will split it according to various sentences
sampfile <- cleansamp(sampfile[[1]]) # This cleans and splits it into various sentenses
sampfile[grep(wd1,sampfile)]

# JMJPFU # Lord help this venture. This is our venture 
# 4 Mar 2016

################# Regular Movie updater ############### 

# This is the place where the movies have to be updated regularly

library(rvest)
library(dplyr)
library(tm)
library(ngram)
library(stringr)

# First task is to create the movie classes as per the Rotten Tomatoes classes

# Category 5 #######

Zootopia <- mov_txt("zootopia",10,52)
The_Wave <- mov_txt("the_wave_2016",3,1)
Emelie <- mov_txt("emelie",1,1)
Ten_cloverfield_lane<- mov_txt("10_cloverfield_lane",11,52)
Deadpool<- mov_txt("deadpool",13,52)
Hello_my_name_is_doris<- mov_txt("hello_my_name_is_doris",4,4)
Eye_in_the_sky<- mov_txt("eye_in_the_sky",5,4)
Star_wars_episode_vii_the_force_awakens<- mov_txt("star_wars_episode_vii_the_force_awakens",18,52)
Kung_fu_panda_3<- mov_txt("kung_fu_panda_3",7,47)
The_revenant_2015<- mov_txt("the_revenant_2015",15,52)
The_lady_in_the_van<- mov_txt("the_lady_in_the_van",6,12)
The_witch_2016<- mov_txt("the_witch_2016",10,52)
Spotlight_2015<- mov_txt("spotlight_2015",14,52)
Midnight_special_2015<- mov_txt("midnight_special_2015",4,3)
City_of_gold_2016<- mov_txt("city_of_gold_2016",2,1)
Brooklyn<- mov_txt("brooklyn",12,52)
The_big_short<- mov_txt("the_big_short",13,52)
Born_to_be_blue<- mov_txt("born_to_be_blue",2,1)
Krisha_2016<- mov_txt("krisha_2016",3,1)
Fourtyfive_years<- mov_txt("45_years",8,14)
Room_2015<- mov_txt("room_2015",12,79)




# Category 4 #######
The_boy_and_the_beast<- mov_txt("the_boy_and_the_beast",1,1)
Whiskey_Tango_Foxtrot<- mov_txt("whiskey_tango_foxtrot",7,19)
Eddie_the_eagle<- mov_txt("eddie_the_eagle",6,16)
Race_2016<- mov_txt("race_2016",6,11)
Remember_2016<- mov_txt("remember_2016",3,3)
Where_to_invade_next<- mov_txt("where_to_invade_next",6,13)
Marguerite<- mov_txt("marguerite",3,1)
Spectre_2015<- mov_txt("spectre_2015",15,52)
April_and_the_extraordinary_world_2016<- mov_txt("april_and_the_extraordinary_world_2016",1,1)
Rams<- mov_txt("rams",4,1)
Mustang_2015<- mov_txt("mustang_2015",5,4)
Janis_little_girl_blue<- mov_txt("janis_little_girl_blue",3,1)
Everybody_wants_some<- mov_txt("everybody_wants_some",1,1)
 


# Category 3 #######

Knight_of_Cups<- mov_txt("knight_of_cups",6,7)
The_Other_Side_of_the_door<- mov_txt("the_other_side_of_the_door",1,1)
Risen_2016<- mov_txt("risen_2016",6,26)
The_young_messiah<- mov_txt("the_young_messiah",2,7)
Triple_9<- mov_txt("triple_9",7,16)
Lolo_2016<- mov_txt("lolo_2016",2,1)
The_dark_horse_2016<- mov_txt("the_dark_horse_2016",2,2)
Miles_ahead_2016<- mov_txt("miles_ahead_2016",2,1)
Kill_your_friends<- mov_txt("kill_your_friends",2,2)
Thank_you_for_playing<- mov_txt("thank_you_for_playing",1,1)





# Category 2 #######
London_Has_Fallen <- mov_txt("london_has_fallen",8,44)
Batman_v_superman_dawn_of_justice <- mov_txt("batman_v_superman_dawn_of_justice",14,52)
My_big_fat_greek_wedding_2<- mov_txt("my_big_fat_greek_wedding_2",6,11)
The_perfect_match_2016<- mov_txt("the_perfect_match_2016",1,2)
Daddys_home_2014<- mov_txt("Daddys_home_2014",6,52)
The_brothers_grimsby<- mov_txt("the_brothers_grimsby",6,17)
Zoolander_2<- mov_txt("zoolander_2",9,35)
The_girl_in_the_photographs<- mov_txt("the_girl_in_the_photographs",1,1)
<- mov_txt("zootopia",5,13)




# Category 1 #######
The_divergent_series_allegiant<- mov_txt("the_divergent_series_allegiant",7,34)
Gods_of_egypt<- mov_txt("gods_of_egypt",6,38)
Ride_along_2<- mov_txt("ride_along_2",5,28)
I_saw_the_light<- mov_txt("i_saw_the_light",2,1)
The_choice<- mov_txt("the_choice",4,8)
<- mov_txt("zootopia",5,13)
<- mov_txt("zootopia",5,13)
<- mov_txt("zootopia",5,13)
<- mov_txt("zootopia",5,13)
<- mov_txt("zootopia",5,13)
<- mov_txt("zootopia",5,13)
<- mov_txt("zootopia",5,13)
<- mov_txt("zootopia",5,13)

## Updating the test set

movie_test[57,] <- moviefeat(The_choice,"The_choice")

########## Making a Test set of based on the latest Movies

## JMJPFU ## Lord Bless this venture
# 29 March 2016

movie_test <- data.frame(movies = NA,unigood = NA,bigood=NA,trigood=NA,unibad=NA,bibad=NA,tribad=NA,stringsAsFactors=FALSE)

## After all the test sets are updated need to sort the dataset

library(dplyr)
library(randomForest)

movie_test_sort <- movie_test %>% arrange(movies) # This is sorted

# Next to do predictions

movie_test_sort$pred <- predict(rf,movie_test_sort[,2:7])

# Taking a backup of the existing movie_df
movie_df_back <- movie_df

movie_df <- movie_test_sort

## Now to create wordclouds for all the movies
# Changing the working directory to another directory



Zootopia_pos <- wordcloud_maker(Zootopia)$pos_terms
Zootopia_neg <- wordcloud_maker(Zootopia)$neg_terms
Zootopia_df <- wordcloud_maker(Zootopia)$plotdf

saveRDS(Zootopia_pos,'Zootopia_pos.rds')
saveRDS(Zootopia_neg,'Zootopia_neg.rds')
saveRDS(Zootopia_df,'Zootopia_df.rds')
#Ethan Moore's Pitch Quality Model "expected Run Value"

#Version 3.0


# Overview ----------------------------------------------------------------


#This script contains the final version of my pitch-level model to predict
# the value of individual MLB pitches on a runs scale. This script is 
# separated into several sections and is commented to give some idea of 
# what the code is doing. Enjoy!


# Libraries --------------------------------------------------------------

library(tidyverse)
require(caTools)
library(baseballr)
library(ggplot2)
library(psych)
library(lubridate)
library(zoo)
library(randomForest)
library(Boruta)



# Data Acquisition from BaseballSavant.com -------------------------------

# baseballR function scrape_statcast_savant() only returns 40,000 pitches 
# at a time, so we must break the 2020 season up into several date ranges 
# to acquire all the data 

#you may also want to load in savant data from previous seasons as well

data1 = scrape_statcast_savant(start_date = "2020-07-23",
                               end_date = "2020-08-01",
                               player_type = "pitcher")

data2 = scrape_statcast_savant(start_date = "2020-08-02",
                               end_date = "2020-08-10",
                               player_type = "pitcher")

data3 = scrape_statcast_savant(start_date = "2020-08-11",
                               end_date = "2020-08-19",
                               player_type = "pitcher")

data4 = scrape_statcast_savant(start_date = "2020-08-20",
                               end_date = "2020-08-29",
                               player_type = "pitcher")

data5 = scrape_statcast_savant(start_date = "2020-08-30",
                               end_date = "2020-09-06",
                               player_type = "pitcher")

data6 = scrape_statcast_savant(start_date = "2020-09-07",
                               end_date = "2020-09-14",
                               player_type = "pitcher")

data7 = scrape_statcast_savant(start_date = "2020-09-15",
                               end_date = "2020-09-22",
                               player_type = "pitcher")

data8 = scrape_statcast_savant(start_date = "2020-09-23",
                               end_date = "2020-09-29",
                               player_type = "pitcher")

mlbraw = rbind(data1, data2, data3, data4, data5, data6, data7, data8) #bind all dataframes into one


rm(data1, data2, data3, data4, data5, data6, data7, data8) #remove individual dataframes


# Data Cleaning and Feature Creation -------------------------------------

#Honestly my comments here are going to be sparse because I wrote this code a long time ago.
#Essentially, we are just creating our response variable here. Skip to the next section if you
# don't particularly care how this is done!

#Derive Linear Weights for Balls in Play
mlbraw$des2 = NA
mlbraw$des2[grepl("single", mlbraw$des)] = "single"
mlbraw$des2[grepl("doubles", mlbraw$des)] = "double"
mlbraw$des2[grepl("ground-rule double", mlbraw$des)] = "double"
mlbraw$des2[grepl("triple", mlbraw$des)] = "triple"
mlbraw$des2[grepl("homer", mlbraw$des)] = "home_run"
mlbraw$des2[grepl("grand slam", mlbraw$des)] = "home_run"
mlbraw$des2[grepl("home run", mlbraw$des)] = "home_run"
mlbraw$des2[grepl("reaches on a throwing error", mlbraw$des)] = "field_error"
mlbraw$des2[grepl("reaches on a fielding error", mlbraw$des)] = "field_error"
mlbraw$des2[grepl("reaches on a missed catch error", mlbraw$des)] = "field_error"
mlbraw$des2[grepl("hit by pitch", mlbraw$des)] = "hit_by_pitch"
mlbraw$des2[grepl("walk", mlbraw$des)] = "walk"
mlbraw$des2[grepl("strikes out", mlbraw$des)] = "strikeout"
mlbraw$des2[grepl("on strikes", mlbraw$des)] = "strikeout"
mlbraw$des2[grepl("sacrifice fly", mlbraw$des)] = "sac_fly"
mlbraw$des2[grepl("fielder's choice", mlbraw$des)] = "fielders_choice"
mlbraw$des2[grepl("force out", mlbraw$des)] = "fielders_choice"
mlbraw$des2[grepl("double play", mlbraw$des)] = "double_play"

mlbraw$des2[grepl("flies out", mlbraw$des) | 
              grepl("grounds out", mlbraw$des) |
              grepl("lines out", mlbraw$des) |
              grepl("pops out", mlbraw$des) |
              grepl("out on a sacrifice bunt", mlbraw$des)] = "field_out"

des_subset = mlbraw[!is.na(mlbraw$des2),]

des_subset$on_1b = ifelse(!is.na(des_subset$on_1b), 1, 0)
des_subset$on_2b = ifelse(!is.na(des_subset$on_2b), 1, 0)
des_subset$on_3b = ifelse(!is.na(des_subset$on_3b), 1, 0)

a = str_count(des_subset$des, "score")
b = str_count(des_subset$des, "homer")
c = str_count(des_subset$des, "grand slam")
a[is.na(a)] = 0
b[is.na(b)] = 0
c[is.na(c)] = 0

des_subset$runs_scored = a + b + c

des_subset$date1 = as.Date(des_subset$game_date)

des_subset = des_subset%>%
  arrange(date1, home_team, away_team, inning, desc(inning_topbot), at_bat_number, pitch_number)


#get Linear Weights table using baseballR
mlbraw2 = mlbraw%>%
  run_expectancy_code()

re_table <- mlbraw2%>%
  run_expectancy_table()

Season_RE = data.frame(matrix(ncol = 5, nrow = nrow(re_table)))
names(Season_RE) = c("outs_when_up", "on_1b", "on_2b", "on_3b", "RE")
library(stringr)

for (i in 1:nrow(re_table)){
  
  Season_RE$outs_when_up[i] = str_split(
    re_table$base_out_state, " ")[[i]][[1]]
  
  Season_RE$on_1b[i] = ifelse(str_split(
    re_table$base_out_state, " ")[[i]][[5]] == "1b", 1, 0)
  
  Season_RE$on_2b[i] = ifelse(str_split(
    re_table$base_out_state, " ")[[i]][[6]] == "2b", 1, 0)
  
  Season_RE$on_3b[i] = ifelse(str_split(
    re_table$base_out_state, " ")[[i]][[7]] == "3b", 1, 0)
  
}

Season_RE$RE = re_table$avg_re
Season_RE$outs_when_up = as.integer(Season_RE$outs_when_up)

mlb_re <- left_join(des_subset, Season_RE, by=c("on_1b", "on_2b", "on_3b", "outs_when_up"))

mlb_re$RE_after = ifelse(mlb_re$inning_topbot == lead(mlb_re$inning_topbot), lead(mlb_re$RE, 1), 0)
mlb_re$RE_diff = mlb_re$RE_after - mlb_re$RE

mlb_re$playRE = mlb_re$RE_diff + mlb_re$runs_scored

mlb_re%>%
  select(on_1b, on_2b, on_3b, outs_when_up, events, RE, RE_after, RE_diff, runs_scored, playRE)%>%
  head(15) #makes sure it worked

mlb_LW <- mlb_re%>%
  group_by(des2)%>%
  summarise(lin_weight = mean(playRE, na.rm = T), count = n())%>%
  arrange(desc(lin_weight))

#print(mlb_LW, n=50)

#Derive Linear Weights for Non-Balls in Play

mlb2 = mlbraw
mlb2$des2[grepl("strike", mlbraw$description)] = "strike"
mlb2$des2[grepl("missed bunt", mlbraw$description)] = "strike"
mlb2$des2[grepl("ball", mlbraw$description)] = "ball"
mlb2$des2[grepl("pitchout", mlbraw$description)] = "ball"
#mlb2$des2[grepl("foul", mlbraw$description)] = "foul"  #treat foul balls the same as strikes
mlb2$des2[grepl("foul", mlbraw$description)] = "strike"
# mlb2$des2[grepl("walk", mlbraw$des)] = "walk"
# mlb2$des2[grepl("strikes out", mlbraw$des)] = "strikeout"

mlb2$des2[grepl("walk", mlbraw$des)] = "ball" #treat pitches ending in walks as balls
mlb2$des2[grepl("strikes out", mlbraw$des)] = "strike" #treat pitches ending in strikeouts as strikes


mlb2$on_1b = ifelse(!is.na(mlb2$on_1b), 1, 0)
mlb2$on_2b = ifelse(!is.na(mlb2$on_2b), 1, 0)
mlb2$on_3b = ifelse(!is.na(mlb2$on_3b), 1, 0)

a = str_count(mlb2$des, "score")
b = str_count(mlb2$des, "homer")
c = str_count(mlb2$des, "grand slam")
a[is.na(a)] = 0
b[is.na(b)] = 0
c[is.na(c)] = 0

mlb2$runs_scored = a + b + c

mlb2$date1 = as.Date(mlb2$game_date)

mlb2 = mlb2%>%
  arrange(date1, home_team, away_team, inning, desc(inning_topbot), at_bat_number, pitch_number)

mlb_re2 <- left_join(mlb2, Season_RE, by=c("on_1b", "on_2b", "on_3b", "outs_when_up"))

mlb_re2$RE_after = ifelse(mlb_re2$inning_topbot == lead(mlb_re2$inning_topbot), lead(mlb_re2$RE, 1), 0)
mlb_re2$RE_diff = mlb_re2$RE_after - mlb_re2$RE

mlb_re2$playRE = mlb_re2$RE_diff + mlb_re2$runs_scored

mlb_re2$playRE[mlb_re2$playRE == 0] = NA

mlb_re2= mlb_re2%>%fill(playRE, .direction = "up")

COUNT_RE_Matrix = mlb_re2%>%
  filter(balls != 4)%>%
  group_by(balls, strikes)%>%
  summarise(count_RE = mean(playRE, na.rm=T)) #Run Expectancy by count

COUNT_RE_Matrix%>%
  arrange(desc(count_RE))

#Finding linear weights of balls and strikes

mlb_re3 = left_join(mlb_re2, COUNT_RE_Matrix, by=c("balls", "strikes"))

mlb_re3$count_RE_after = ifelse(mlb_re3$at_bat_number == lead(mlb_re3$at_bat_number), lead(mlb_re3$count_RE, 1), 0)
mlb_re3$count_RE_diff = mlb_re3$count_RE_after - mlb_re3$count_RE

mlb_re3$count_playRE = mlb_re3$count_RE_diff + mlb_re3$runs_scored

BALL_STRIKE_LW = mlb_re3%>%
  filter(des2 %in% c("ball", "strike"))%>%
  group_by(des2)%>%
  summarise(lin_weight = mean(count_playRE, na.rm = T), count = n())%>%
  arrange(desc(lin_weight))

mlb_LW = rbind(mlb_LW, BALL_STRIKE_LW)

season_mlb = left_join(mlb2, mlb_LW[,1:2], by = "des2")

#give pitcher credit for only 1 out on situation-based outcomes
season_mlb$lin_weight[season_mlb$des2 == "field_error"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]
season_mlb$lin_weight[season_mlb$des2 == "double_play"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]
season_mlb$lin_weight[season_mlb$des2 == "fielders_choice"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]
season_mlb$lin_weight[season_mlb$des2 == "sac_fly"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]

season_mlb = season_mlb%>%
  filter(!is.na(des2))

head(season_mlb)

rm(a,b,c,mlb_LW, mlb_re, mlb_re2, mlb_re3, mlb2, mlbraw, mlbraw2, BALL_STRIKE_LW)

#Creation of variables like velo_diff and movement diff based on a pitcher's average fastball.
# These variables did not improve the model, so I will not use them, but here they are if you want to use them.

p_avgs <- season_mlb%>%
  filter(pitch_type %in% c("FF", "SI"))%>%
  group_by(player_name)%>%
  summarise(avg_velo = mean(release_speed,na.rm=T),
            avg_vmov = mean(pfx_z,na.rm=T),
            avg_hmov = mean(pfx_x,na.rm=T))

#get fb averages columns
season_mlb3 <- left_join(p_avgs, season_mlb,  by = "player_name")

#get pitch differences
season_mlb4 <- season_mlb3%>%
  mutate(
    velo_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      release_speed - avg_velo, NA),
    hmov_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      pfx_x - avg_hmov, NA),
    vmov_diff = ifelse(
      pitch_type %in% c("SL", "CB", "FS", "KC", "CH", "FC"), 
      pfx_z - avg_vmov, NA)
  )

names(season_mlb4)

rm(season_mlb, season_mlb3, des_subset)  #get rid of dataframes we don't need anymore

#flip horizontal release position and horizontal movement measurements for LHP
#remove NA values, Random Forest does not like those!
season_mlb5 <- season_mlb4%>%
  filter(!is.na(lin_weight),
         !is.na(p_throws),
         !is.na(stand),
         !is.na(release_spin_rate),
         !is.na(release_extension))%>%
  mutate(release_pos_x_adj = ifelse(p_throws == "R", release_pos_x, -release_pos_x),
         pfx_x_adj = ifelse(p_throws == "R", pfx_x, -pfx_x)) 


rm(season_mlb4)

set.seed(1)

# Feature Selection for Each Pitch Type-----------------------------------

#Hey look, a function!

feature_selection <- function(pitch_data){
  
  #The Boruta feature selection algorithm takes a long time with all the data, so we have to
  #subset down a lot. I took a 25% random sample of RvR pitches from the dataframe that we passed in
  
  rr_data = pitch_data%>%
    filter(p_throws == "R",
           stand =="R",
           !is.na(release_spin_rate),
           !is.na(release_extension)) 
  
  rr_data_sampled = rr_data[sample(nrow(rr_data), size = nrow(rr_data)*.25),]
  
  Boruta_PitchType <- Boruta(lin_weight ~ release_speed + release_pos_x_adj + release_extension+
                               release_pos_z + pfx_x_adj + pfx_z + plate_x + plate_z + release_spin_rate, # + velo_diff + hmov_diff + vmov_diff, #Potential extra variables to consider 
                             data = rr_data_sampled)
  
  #print(Boruta_PitchType)
  #plot(Boruta_PitchType)
  
  feature_importance <- data.frame(attStats(Boruta_PitchType))
  
  feature_importance%>%
    select(meanImp, decision)%>%
    arrange(desc(meanImp)) 
  
  Features <- getSelectedAttributes(Boruta_PitchType, withTentative = F) #return the list of confirmed important variables for use in the model
  
  return(Features)
  
}

#I like to time how long this next part takes, but you don't have to
#The rest of the code takes an hour or two to run on average

# #timing!
# start <- proc.time()

#Get the features for our models for each pitch type group using our function

#Fastballs- 4 Seam
ff_data <- season_mlb5%>%
  filter(pitch_type == "FF")

FF_features <- feature_selection(ff_data)

#Sinkers

si_data <- season_mlb5%>%
  filter(pitch_type == "SI")

Si_features <- feature_selection(si_data)

#Cutters

fc_data <- season_mlb5%>%
  filter(pitch_type == "FC")

FC_features <- feature_selection(fc_data)

#CHANGEUPS AND SPLITTERS

ch_data <- season_mlb5%>%
  filter(pitch_type %in% c("CH", "FS"))

CH_features <- feature_selection(ch_data)

#SLIDERS
sl_data <- season_mlb5%>%
  filter(pitch_type == "SL")

SL_features <- feature_selection(sl_data)

#CURVEBALLS AND KNUCKLE CURVES

cb_data <- season_mlb5%>%
  filter(pitch_type %in% c("CU", "KC"))

CB_features = feature_selection(cb_data)

# Model Validation -------------------------------------------------------

rmse = function(m, o){ #function for our model evaluaiton metric
  sqrt(mean((m - o)^2))
}

rf_model <- function(df, features) { #our model function with dataframe and features as parameters
  
  features1 <- append(features, c("lin_weight"))
  df1 <- df[,features1]
  
  randomForest(lin_weight ~ ., data = df1, importance = T)
  
}

#Another function! This one for validation!
#This code heavily borrows from the following page:
# https://stackoverflow.com/questions/45857247/r-predict-new-values-for-groups

validate <- function(data, features){ #(ff_data, FF_features)
  feats_only <- data[,c("p_throws", "stand", "lin_weight", features)] 
  data_shuffled <- feats_only[sample(nrow(data), replace = F),]
  
  # train
  suppressWarnings(nested <- 
                     data_shuffled[1:round(nrow(data_shuffled)*.7),]%>% 
                     nest(-p_throws, -stand)%>% 
                     rename(myorigdata = data))
  
  # test
  suppressWarnings(new_nested <- 
                     data_shuffled[1:round(nrow(data_shuffled)*.3),]%>%
                     nest(-p_throws, -stand)%>% 
                     rename(mynewdata = data))
  
  featurelist <- rep(list(features),length(nested$myorigdata))
  
  suppressWarnings(one <- nested %>% 
                     mutate(my_model = map2(myorigdata, featurelist, rf_model)))
  
  two <- one%>%
    full_join(new_nested, by = c("p_throws", "stand"))
  
  two$my_new_pred = NA
  for(i in 1:nrow(two)){
    
    two$my_new_pred[i] = as_tibble(predict(two$my_model[[i]], two$mynewdata[[i]][-1]))
    
  }
  
  name <- deparse(substitute(data))
  
  assign(paste(name, "_models_val", sep=""), two, envir = globalenv()) #save the validation models in case we need them later
  
  three = two
  
  four <- three%>%
    select(p_throws,stand, mynewdata, my_new_pred)
  
  five<- four%>%
    unnest(c(mynewdata, my_new_pred))
  
  six <- five%>%
    rename(preds = my_new_pred)
  
  predictions = six
  
  print("RMSE:")
  print(round(rmse(predictions$preds, predictions$lin_weight), digits = 5)) #print validation RMSE
  
  return(predictions) #return dataframe of all predictions 
  
}

#run validation on all 6 pitch type groups

ff_w_preds_val <- validate(ff_data, FF_features)
si_w_preds_val <- validate(si_data, Si_features)
fc_w_preds_val <- validate(fc_data, FC_features)
ch_w_preds_val <- validate(ch_data, CH_features)
sl_w_preds_val <- validate(sl_data, SL_features)
cb_w_preds_val <- validate(cb_data, CB_features)

# #evaluating feature importance and accessing individual models sample code
# importance(ff_data_models_val$my_model[[1]])
# varImpPlot(ff_data_models_val$my_model[[1]])
# ff_data_models_val$my_model[1]

#Get total validation RMSE
needed_cols = c("lin_weight", "preds")

total_preds_val <- rbind(ff_w_preds_val[,needed_cols], si_w_preds_val[,needed_cols], fc_w_preds_val[,needed_cols], 
                         ch_w_preds_val[,needed_cols], sl_w_preds_val[,needed_cols], cb_w_preds_val[,needed_cols])

#TOTAL VALIDATION RMSE 
rmse(total_preds_val$lin_weight, total_preds_val$preds)


# Application To All Data ------------------------------------------------


application <- function(data, features){
  
  feats_only <- data[,c("p_throws", "stand", "lin_weight", features)] 
  
  # train
  suppressWarnings(nested <- feats_only%>% 
                     nest(-p_throws, -stand)%>% 
                     rename(myorigdata = data))
  
  # test
  suppressWarnings(new_nested <- feats_only%>% #train and test are now the same and contain all data
                     nest(-p_throws, -stand)%>% 
                     rename(mynewdata = data))
  
  featurelist <- rep(list(features),length(nested$myorigdata))
  
  suppressWarnings(one <- nested %>% 
                     mutate(my_model = map2(myorigdata, featurelist, rf_model)))
  
  two <- one%>%
    full_join(new_nested, by = c("p_throws", "stand")) #make a separate model for each platoon combination
  
  two$my_new_pred = NA
  for(i in 1:nrow(two)){
    
    two$my_new_pred[i] = as_tibble(predict(two$my_model[[i]], two$mynewdata[[i]][-1]))
    
  }
  
  name <- deparse(substitute(data))
  
  assign(paste(name, "_models_total", sep=""), two, envir = globalenv())
  
  three = two
  
  four <- three%>%
    select(p_throws, stand, mynewdata, my_new_pred)
  
  five<- four%>%
    unnest(c(mynewdata, my_new_pred))
  
  six <- five%>%
    rename(preds = my_new_pred)
  
  predictions = six
  
  print("RMSE:")
  print(round(rmse(predictions$preds, predictions$lin_weight), digits = 5)) #print overall RMSE for data
  
  return(predictions) #return predictions
  
}

#Apply our models to every pitch type group

ff_w_preds <- application(ff_data, FF_features)
si_w_preds <- application(si_data, Si_features)
fc_w_preds <- application(fc_data, FC_features)
ch_w_preds <- application(ch_data, CH_features)
sl_w_preds <- application(sl_data, SL_features)
cb_w_preds <- application(cb_data, CB_features)

#find features that exist in EVERY model so we can rbind

needed_cols = intersect(FF_features, Si_features)
needed_cols = intersect(needed_cols, FC_features)
needed_cols = intersect(needed_cols, CH_features)
needed_cols = intersect(needed_cols, SL_features)
needed_cols = intersect(needed_cols, CB_features)
needed_cols = c(needed_cols, "lin_weight", "preds")
needed_cols

total_preds <- rbind(ff_w_preds[,needed_cols], si_w_preds[,needed_cols], fc_w_preds[,needed_cols], 
                     ch_w_preds[,needed_cols], sl_w_preds[,needed_cols], cb_w_preds[,needed_cols])


#Get total RMSE for all pitches
rmse(total_preds$lin_weight, total_preds$preds) 


#Join predictions back into original data
join_cols = needed_cols[needed_cols != "preds"]
final_mlb <- left_join(season_mlb5, total_preds, by = join_cols) 

#make sure we didn't accidentally mess up the join!
nrow(final_mlb) == nrow(season_mlb5)


# use these for timing your code if you wish
# end <- proc.time()
# end-start


#Analysis (Add your own!) ------------------------------------------------

#Find NA predictions by pitcher
final_mlb%>%
  filter(is.na(release_spin_rate))%>%
  group_by(player_name)%>%
  summarise(n=n())%>%
  arrange(desc(n))
# #these are due to missing release spin rate data

#Overall Leaderboard
final_mlb%>%
  group_by(player_name)%>%
  summarise(n=n(), xRV = 100*sum(preds,na.rm=T)/n)%>%
  filter(n>300)%>%
  arrange((xRV))%>%
  print(n=50)

#Top Curveballs Leaderboard
final_mlb%>%
  filter(pitch_type %in% c("CU", "KC"))%>%
  group_by(player_name, pitch_type)%>%
  summarise(pitches=n(), xRV = 100*sum(preds,na.rm=T)/pitches,
            RV = 100*sum(lin_weight)/pitches)%>%
  filter(pitches>150)%>%
  arrange((xRV))%>%
  print(n=20)

#Player Specific Query
final_mlb%>%
  filter(player_name == "Walker Buehler", pitch_type == "FF")%>%
  group_by(player_name)%>%
  summarise(n = n(), qop = 100*sum(preds,na.rm=T)/n)


#Best Pitches
final_mlb%>%
  arrange(preds)%>%
  select(player_name, inning, strikes, balls, outs_when_up, game_date, preds, lin_weight, pitch_type, des2)%>%
  head(15) 


#Worst Pitches
final_mlb%>%
  arrange(desc(preds))%>%
  select(player_name, inning, strikes, balls, outs_when_up, game_date, preds, lin_weight, pitch_type, des2)%>%
  head(15)


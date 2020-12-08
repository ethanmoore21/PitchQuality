#Ethan Moore
#Pitch Quality Model

#This version is not intended to match results with my blog post but instead contains all of the data from the 2020 season.

library(tidyverse)
require(caTools)
library(baseballr)
library(ggplot2)
library(psych)
library(lubridate)
library(zoo)
library(randomForest)
library(Boruta)

###Data acquisition from BaseballSavant.com

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

mlbraw = rbind(data1, data2, data3, data4, data5, data6, data7, data8)

rm(data1, data2, data3, data4, data5, data6, data7, data8)

###Data cleaning and feature creation

#Balls in Play Linear Weights
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

#sanity check
#des_subset$des[des_subset$runs_scored == 4]
# des_subset%>%
#   group_by(runs_scored)%>%
#   summarise(n())

des_subset$date1 = as.Date(des_subset$game_date)

des_subset = des_subset%>%
  arrange(date1, home_team, away_team, inning, desc(inning_topbot), at_bat_number, pitch_number)


#get Linear Weights table
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

#BALL and STRIKE LINEAR WEIGHTS

mlb2 = mlbraw
mlb2$des2[grepl("strike", mlbraw$description)] = "strike"
mlb2$des2[grepl("missed bunt", mlbraw$description)] = "strike"
mlb2$des2[grepl("ball", mlbraw$description)] = "ball"
mlb2$des2[grepl("pitchout", mlbraw$description)] = "ball"
#mlb2$des2[grepl("foul", mlbraw$description)] = "foul"
mlb2$des2[grepl("foul", mlbraw$description)] = "strike"
# mlb2$des2[grepl("walk", mlbraw$des)] = "walk"
# mlb2$des2[grepl("strikes out", mlbraw$des)] = "strikeout"

mlb2$des2[grepl("walk", mlbraw$des)] = "ball"
mlb2$des2[grepl("strikes out", mlbraw$des)] = "strike"


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
season_mlb$lin_weight[season_mlb$des2 == "double play"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]
season_mlb$lin_weight[season_mlb$des2 == "fielder's choice"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]
season_mlb$lin_weight[season_mlb$des2 == "force out"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]
season_mlb$lin_weight[season_mlb$des2 == "sacrifice fly"] = mlb_LW$lin_weight[mlb_LW$des2 == "field_out"]

season_mlb = season_mlb%>%
  filter(!is.na(des2))

head(season_mlb)

rm(a,b,c,mlb_LW, mlb_re, mlb_re2, mlb_re3, mlb2, mlbraw, mlbraw2, BALL_STRIKE_LW)

#Further cleaning

season_mlb2 <- season_mlb%>%
  mutate(grouped_pitch_type = ifelse(
    pitch_type %in% c("FF","FT","FC","SI"), "FB", ifelse(
      pitch_type %in% c("CH", "FS"), "CH", ifelse(
        pitch_type %in% c("CU", "KC"), "CB", ifelse(
          pitch_type %in% c("SL"), "SL", NA
        )
      )
    )
  ))%>%
  filter(!is.na(grouped_pitch_type))

p_avgs <- season_mlb2%>%
  filter(grouped_pitch_type %in% c("FB"))%>%
  group_by(player_name)%>%
  summarise(avg_velo = mean(release_speed,na.rm=T),
            avg_vmov = mean(pfx_z,na.rm=T),
            avg_hmov = mean(pfx_x,na.rm=T))

#get fb averages columns
season_mlb3 <- left_join(p_avgs, season_mlb2,  by = "player_name")

#get pitch differences
season_mlb4 <- season_mlb3%>%
  mutate(
    velo_diff = ifelse(
      grouped_pitch_type %in% c("SL", "CB", "CH"), 
      release_speed - avg_velo, NA),
    hmov_diff = ifelse(
      grouped_pitch_type %in% c("SL", "CB", "CH"), 
      pfx_x - avg_hmov, NA),
    vmov_diff = ifelse(
      grouped_pitch_type %in% c("SL", "CB", "CH"), 
      pfx_z - avg_vmov, NA)
  )

names(season_mlb4)

rm(season_mlb, season_mlb2, season_mlb3, des_subset)

###Feature selection
#memory.limit(size=100000)

#Fastballs
fb_data <- season_mlb4%>%
  filter(grouped_pitch_type == "FB",
         !is.na(lin_weight),
         !is.na(p_throws),
         !is.na(stand),
         !is.na(release_spin_rate))

rr_fb_data = fb_data%>%
  filter(p_throws == "R",
         stand =="R",
         !is.na(release_spin_rate))

rr_fb_data_sampled = rr_fb_data[sample(nrow(rr_fb_data), size = nrow(rr_fb_data)*.1),]

Boruta_FS <- Boruta(lin_weight ~ release_speed + release_pos_x + release_pos_y+
                      release_pos_z + pfx_x + pfx_z + plate_x + plate_z + balls + strikes + outs_when_up +
                      release_spin_rate,
                    data = rr_fb_data_sampled)

print(Boruta_FS)
plot(Boruta_FS)

fb_fs <- data.frame(attStats(Boruta_FS))

fb_fs%>%
  select(meanImp, decision)%>%
  arrange(desc(meanImp))

#Offspeed
os_data <- season_mlb4%>%
  filter(grouped_pitch_type %in% c("CH", "CB", "SL"),
         !is.na(lin_weight),
         !is.na(p_throws),
         !is.na(stand),
         !is.na(release_spin_rate))

rr_os_data = os_data%>%
  filter(p_throws == "R",
         stand=="R",
         !is.na(release_spin_rate))

rr_os_data_sampled = rr_os_data[sample(nrow(rr_os_data), size = nrow(rr_os_data)*.1),]

Boruta_OS <- Boruta(lin_weight ~ release_speed + release_pos_x + release_pos_y+
                      release_pos_z + pfx_x + pfx_z + plate_x + plate_z + balls + strikes + outs_when_up +
                      release_spin_rate + velo_diff + hmov_diff + vmov_diff,
                    data = rr_os_data_sampled)

print(Boruta_OS)
plot(Boruta_OS)

os_fs <- data.frame(attStats(Boruta_OS))

os_fs%>%
  select(meanImp, decision)


###Validation
rmse = function(m, o){
  sqrt(mean((m - o)^2))
}

set.seed(69)

#Fastball Model
fb_data_shuffled <- fb_data[sample(nrow(fb_data), replace = F),]

# train
fb_nested <- 
  fb_data_shuffled[1:round(nrow(fb_data_shuffled)*.7),]%>% 
  nest(-p_throws, -stand, -grouped_pitch_type)%>% 
  rename(myorigdata = data)

# test
new_fb_nested <- 
  fb_data_shuffled[1:round(nrow(fb_data_shuffled)*.3),]%>%
  nest(-p_throws, -stand, -grouped_pitch_type)%>% 
  rename(mynewdata = data)

# make the model function
rf_model_fb <- function(df) {
  randomForest(lin_weight ~ release_speed + release_pos_x + release_pos_y+
                 release_pos_z + pfx_x + pfx_z + plate_x + plate_z + release_spin_rate,
               data = df)
}

one <- fb_nested %>% 
  mutate(my_model = map(myorigdata, rf_model_fb))

two <- one%>%
  full_join(new_fb_nested, by = c("p_throws", "stand", "grouped_pitch_type"))

three <- two%>%
  mutate(my_new_pred = map2(my_model, mynewdata, predict))

four <- three%>%
  select(p_throws,stand,grouped_pitch_type, mynewdata, my_new_pred)

five<- four%>%
  unnest(c(mynewdata, my_new_pred))

six <- five%>%
  rename(preds = my_new_pred)

fbs_predictions = six

rmse(fbs_predictions$preds, fbs_predictions$lin_weight)


#Off speed model
os_data_shuffled <- os_data[sample(nrow(os_data), replace = F),]

# train
os_nested <- 
  os_data_shuffled[1:round(nrow(os_data_shuffled)*.7),]%>% 
  nest(-p_throws, -stand, -grouped_pitch_type)%>% 
  rename(myorigdata = data)

# test
new_os_nested <- 
  os_data_shuffled[1:round(nrow(os_data_shuffled)*.3),]%>%
  nest(-p_throws, -stand, -grouped_pitch_type)%>% 
  rename(mynewdata = data)

# make model function

rf_model_os <- function(df) {
  randomForest(lin_weight ~ release_speed + release_pos_x + 
                 release_pos_z + pfx_x + pfx_z + plate_x + plate_z + release_spin_rate,
               data = df)
}

one <- os_nested %>% 
  mutate(my_model = map(myorigdata, rf_model_os)) 

two <- one%>%
  full_join(new_os_nested, by = c("p_throws", "stand", "grouped_pitch_type"))

three <- two%>%
  mutate(my_new_pred = map2(my_model, mynewdata, predict))

four <- three%>%
  select(p_throws,stand,grouped_pitch_type, mynewdata, my_new_pred)

five<- four%>%
  unnest(c(mynewdata, my_new_pred))

six <- five%>%
  rename(preds = my_new_pred)

os_predictions = six

rmse(os_predictions$preds, os_predictions$lin_weight)


#Application to all data

#fastball model
# fb_data <- season_mlb4%>%
#   filter(grouped_pitch_type == "FB")

#fb_data_shuffled <- fb_data[sample(nrow(fb_data), replace = F),]

#train
#already exists above
# fb_nested <- 
#   fb_data_shuffled[1:round(nrow(fb_data_shuffled)*.7),]%>% 
#   nest(-p_throws, -stand, -grouped_pitch_type)%>% 
#   rename(myorigdata = data)

#test, now includes all data
new_fb_nested <- fb_data%>% #[1:round(nrow(fb_data_shuffled)),]%>%
  nest(-p_throws, -stand, -grouped_pitch_type)%>% 
  rename(mytotaldata = data)

# make model function
#already exists from above
# rf_model_fb <- function(df) {
#   randomForest(lin_weight ~ release_speed + release_pos_x + release_pos_y+
#                  release_pos_z + pfx_x + pfx_z + plate_x + plate_z + release_spin_rate,
#                data = df)
# }

#same operations as earlier but now in a pipe
fbs_predictions <- fb_nested %>% 
  mutate(my_model = map(myorigdata, rf_model_fb))%>%
  full_join(new_fb_nested, by = c("p_throws", "stand", "grouped_pitch_type"))%>%
  mutate(my_new_pred = map2(my_model, mytotaldata, predict))%>%
  select(p_throws,stand,grouped_pitch_type, mytotaldata, my_new_pred)%>%
  unnest(c(mytotaldata, my_new_pred))%>%
  rename(preds = my_new_pred)

fbs_predictions <- fbs_predictions%>%
  filter(!is.na(preds),
         !is.na(lin_weight))

rmse(fbs_predictions$preds, fbs_predictions$lin_weight)

#FB Leaderboard for sanity
fbs_predictions%>%
  group_by(player_name)%>%
  summarise(pitches=n(), qop = -100*sum(preds,na.rm=T)/pitches)%>%
  filter(pitches>300)%>%
  arrange(desc(qop))%>%
  print(n=Inf)


#off speed model
# os_data <- season_mlb4%>%
#   filter(grouped_pitch_type %in% c("CH", "CB", "SL"))

#os_data_shuffled <- os_data[sample(nrow(os_data), replace = F),]

#train
#already exists from above
# os_nested <- 
#   os_data_shuffled[1:round(nrow(os_data_shuffled)*.7),]%>% 
#   nest(-p_throws, -stand, -grouped_pitch_type)%>% 
#   rename(myorigdata = data)

# test
new_os_nested <- os_data%>% #og_data_shuffled[1:round(nrow(os_data_shuffled)*.3),]%>%
  nest(-p_throws, -stand, -grouped_pitch_type)%>% 
  rename(mytotaldata = data)

# make a model function
# rf_model_os <- function(df) {
#   randomForest(lin_weight ~ release_speed + release_pos_x + 
#                  release_pos_z + pfx_x + pfx_z + plate_x + plate_z + release_spin_rate,
#                data = df)
# }

#same operations as earlier but now in a pipe
os_predictions <- os_nested %>% 
  mutate(my_model = map(myorigdata, rf_model_os))%>% 
  full_join(new_os_nested, by = c("p_throws", "stand", "grouped_pitch_type"))%>%
  mutate(my_new_pred = map2(my_model, mytotaldata, predict))%>%
  select(p_throws,stand,grouped_pitch_type, mytotaldata, my_new_pred)%>%
  unnest(c(mytotaldata, my_new_pred))%>%
  rename(preds = my_new_pred)

os_predictions <- os_predictions%>%
  filter(!is.na(preds),
         !is.na(lin_weight))

rmse(os_predictions$preds, os_predictions$lin_weight)

#Offspeed Leaderboard for sanity
os_predictions%>%
  #filter(grouped_pitch_type == "CB")%>%
  group_by(player_name, grouped_pitch_type)%>%
  summarise(n=n(), qop = -100*sum(preds,na.rm=T)/n)%>%
  filter(n>50)%>%
  arrange(desc(qop))


#get all predictions together

predictions <- rbind(fbs_predictions, os_predictions)

predictions <- predictions%>%
  filter(!is.na(preds),
         !is.na(lin_weight))

rmse(predictions$preds, predictions$lin_weight) #Final RMSE

#overall leaderboard for sanity
predictions%>%
  #filter(grouped_pitch_type == "CH")%>%
  group_by(player_name)%>%
  summarise(n=n(), qop = -100*sum(preds,na.rm=T)/n)%>%
  filter(n>100)%>%
  arrange(desc(qop))%>%
  print(n=50)

#Rescale, display leaderboard
predictions%>%
  group_by(player_name)%>%
  summarise(pitches = length(na.omit(preds)), xRV = 100*sum(preds, na.rm=T)/pitches)%>%
  mutate(QOP_plus = as.numeric(rescale(-xRV, mean = 100, sd=10, df = F)))%>%
  select(player_name, pitches, QOP_plus)%>%
  filter(pitches > 500)%>%
  arrange(desc(QOP_plus))%>%
  print(n=50)

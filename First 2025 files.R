setwd("C:/New Data/Data")
library(httr)
directory <- paste0(getwd(),"/NFLBDB2025")
#fileURL <- "https://www.dropbox.com/scl/fi/spy2limdm8kqleswa1pqo/nfl-big-data-bowl-2025.zip?rlkey=e4cz61xvmzlv3itdli1oee3dw&raw=1"
#GET(url=fileURL, write_disk("NFLBDB2025.zip", overwrite = TRUE))
#unzip("nfl-big-data-bowl-2025.zip")
source('https://raw.githubusercontent.com/ptallon/SportsAnalytics_Fall2024/main/SharedCode.R')
load_packages(c("data.table","dplyr","ggplot2", "ggalt", "ggforce","hms", "gganimate", "RColorBrewer", "nflfastR", "ggimage", "png", "gifski"))
df <- load_data_for_one_week(directory, 1, TRUE)
#NEw Test here

library(data.table)
library(dplyr)
library(ggplot2)
library(ggalt)
library(ggforce)
library(hms)
library(gganimate)
library(data.table)
library(dplyr)
library(RColorBrewer)
library(nflfastR)
library(ggimage)
library(png)
library(gifski)

list.files(path = "NFLBDB2025")

t1 <- fread("NFLBDB2025/tracking_week_1.csv")
games <- fread("NFLBDB2025/games.csv")
plays <- fread("NFLBDB2025/plays.csv")
players <- fread("NFLBDB2025/players.csv")
player_play <- fread("NFLBDB2025/player_play.csv")

df <- left_join(t1, games,  by = c("gameId"))
df <- left_join(df, plays,  by = c("gameId", "playId"))
df <- left_join(df, players,  by = c("nflId"))
df <- left_join(df, player_play,  by = c("gameId","playId", "nflId"))





# Number of plays in week 1
length(unique(df$playId))

#How long was each play in week 1
length_of_plays_df <- df %>%
  group_by(playId)%>% 
  summarise(n = max(frameId), .groups = "keep") %>%
  arrange(-n) %>%
  
  data.frame() %>% head(10)

#What are the different outcomes of the snap and how many of each
table(plays$passResult)

table(plays$passResult != "")

plays[plays$passResult == "", "passResult"] <- NA #sets all empty strings to NA

table(plays$passResult, useNA = "ifany")

plays$passLength

x<- table(plays$passResult,plays$passLength)
x <- data.frame(x)
colnames(x) <- c("Pass_Result", "Pass_Length", "Count")


ggplot(data = x, aes(x = Pass_Length, y = Count, group = Pass_Result, fill = Pass_Result, colour = Pass_Result)) +
  geom_area(alpha = 0.5)



#Doing work on motion



df2 <-player_play [player_play$shiftSinceLineset == TRUE]
df3 <-player_play [player_play$motionSinceLineset == TRUE]


all_motion_df <- rbind(df1, df2, df3)
all_motion_df <- all_motion_df %>% distinct() %>% data.frame()


#insert new column called team using hometeamabbr and visitorteamabbr
df <- df %>%
  mutate(team = ifelse(club == homeTeamAbbr, "home", ifelse(club == "football", "football", "away")  ) )%>%
  data.frame()


#append a week 
plays <- plays %>%
  left_join(games %>% select(gameId, week), by = c("gameId"))

player_play <- player_play %>%
  left_join(games %>% select(gameId, week), by = c("gameId"))


#add week and jerseyNumber to the right of player_play

player_play <- player_play %>%
  left_join(df %>% select(nflId, jerseyNumber, week) %>% distinct(), by = c("nflId"))

df1 <- player_play [player_play$inMotionAtBallSnap == TRUE]
df2 <-player_play [player_play$shiftSinceLineset == TRUE]
df3 <-player_play [player_play$motionSinceLineset == TRUE]

all_motion_df <- rbind(df1, df2, df3)
all_motion_df <- all_motion_df %>% distinct() %>% data.frame()
#Which games and plays have motion

df1 %>% 
  select(gameId,playId) %>%
  distinct()

game_df <- df %>%
  filter(gameId == 2022090800, playId == 80) %>%
  data.frame()


visualize_single_play(game_df)

#who is in motion
df %>%
  filter(gameId == 2022090800, playId == 80, inMotionAtBallSnap == TRUE) %>%
  select(displayName.x, jerseyNumber) %>%
  distinct()



plays_with_multiple_players_in_motion <- df1 %>%
  select(gameId, playId, nflId, jerseyNumber.x, week.x) %>%
  group_by(gameId,playId,week.x) %>%
  summarise(n = n(), .groups = 'keep') %>%
  arrange(-n) %>%
  filter(week.x == 1, n > 1)%>%
  data.frame() %>% print()

visualize_single_play(df %>%  filter(gameId == 2022091112, playId == 62), highlight_players_in_motion = TRUE)















#measure the distance traveled by the player or players in motion
game_df <- df %>% 
  filter(gameId == 2022091112, playId == 62,frameType == "BEFORE_SNAP", inMotionAtBallSnap == TRUE)%>%
  
  # reduce columns to what we need to use
  select(displayName.x, jerseyNumber, x, y, dis, frameId, frameType, time, gameId, playId, week) %>%
  #in order to generate a cumulative sum sort by name and frame
  arrange(displayName.x, frameId) %>%
  group_by(displayName.x) %>%
  # set the direction of the motion
  mutate(motion_direction = ifelse(y- lag(y) < 0, 1, ifelse(y-lag(y)> 0, 2, NA)), 
         dy = ifelse(y == lag(y), NA, y -lag(y))) %>%
  
  #Compute Cumalative totals over course of a play
  group_by(displayName.x, motion_direction)%>%
  mutate(sum_dy = cumsum(dy),
         sum_dy_d1 = ifelse(dy <0, cumsum(dy), NA),
         sum_dy_d2 = ifelse(dy > 0, cumsum(dy), NA)) %>%
  
  group_by(displayName.x) %>%
  
  mutate(sum_abs_dy = cumsum(abs(dy)), na.rm = TRUE,
         max_sum_dy_d1 = min(sum_dy_d1, na.rm = TRUE),
         max_sum_dy_d2 = max(sum_dy_d2, na.rm = TRUE),
         max_time = max(time),
         min_time = min(time),
         interval = difftime(max_time, min_time, units = "secs"),
         speed_yrds_second = sum_abs_dy/ as.double(interval)) %>%
  slice(n()) %>%
  select(displayName.x, sum_abs_dy, max_sum_dy_d1, max_sum_dy_d2,interval,speed_yrds_second, gameId, playId, week) %>%
  data.frame %>% print()

n <- motion_stats_single_week(df, player_play)
  

viz_df <- df %>%
  filter(gameId == 2022091112, playId == 62) %>%
  left_join(player_play %>% select(nflId,jerseyNumber.x) %>% distinct(),
            by = c("pff_primaryDefensiveCoverageMatchupNflId" = "nflId"))  %>%
  rename("matchup_jerseyNumber1" = "jerseyNumber.x") %>%
  left_join(player_play %>% select(nflId,jerseyNumber.x) %>% distinct(),
            by = c("pff_secondaryDefensiveCoverageMatchupNflId" = "nflId")) %>%
    rename("matchup_jerseyNumber2" = "jerseyNumber.x") %>%
    data.frame()


  
  
  
  
  
  
  
  
  
  #to do cumulative sum you must fort first
  arrange(displayName.x, frameId) %>%
  #Groupby based on presnap criteria
  group_by(displayName.x, frameType == "BEFORE_SNAP", inMotionAtBallSnap == TRUE)%>%
  #Calculate the total motion distance
  #mutate(totalMotionPresnap = cumsum(ifelse(frameType == "BEFORE_SNAP" & inMotionAtBallSnap == TRUE, dis, NA))) %>%
  ungroup() %>%
  #which columns do I want to present in the results
  select(displayName, jerseyNumber, totalMotionPresnap) %>%
  filter(!is.na(totalMotionPresnap)) %>%
  data.frame() %>% tail(1)%>% print()

  
  visualize_single_frame(viz_df, highlight_players_in_motion = TRUE, highlight_matchup = TRUE, show_Matchup =  TRUE, frame_number =  66)

df2 = df %>%
  select( gameId, playId, preSnapHomeTeamWinProbability, preSnapVisitorTeamWinProbability, preSnapVisitorScore, preSnapHomeScore, yardlineNumber, yardsGained, passResult, pff_runConceptPrimary, pff_runConceptSecondary, pff_manZone, pff_passCoverage) %>%
  distinct(gameId, playId, .keep_all = TRUE)
  
  



  










plays <- plays %>%
  left_join(games %>% select(gameId, week), by = c("gameId")) %>%


player_play1 <- player_play %>%
  left_join(plays by = c("gameId"))

df1 <- player_play [player_play$inMotionAtBallSnap == TRUE]
df2 <-player_play [player_play$shiftSinceLineset == TRUE]
df3 <-player_play [player_play$motionSinceLineset == TRUE]

all_motion_df <- rbind(df1, df2, df3)



dfMatchup <- all_motion_df %>% select(gameId, playId, week.x, jerseyNumber.x,   )








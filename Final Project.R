setwd("C:/New Data/Data")
library(httr)
directory <- paste0(getwd(),"/NFLBDB2025")
#fileURL <- "https://www.dropbox.com/scl/fi/spy2limdm8kqleswa1pqo/nfl-big-data-bowl-2025.zip?rlkey=e4cz61xvmzlv3itdli1oee3dw&raw=1"
#GET(url=fileURL, write_disk("NFLBDB2025.zip", overwrite = TRUE))
#unzip("nfl-big-data-bowl-2025.zip")
source('https://raw.githubusercontent.com/ptallon/SportsAnalytics_Fall2024/main/SharedCode.R')
load_packages(c("data.table","dplyr","ggplot2", "ggalt", "ggforce","hms", "gganimate", "RColorBrewer", "nflfastR", "ggimage", "png", "gifski"))
df <- load_data_for_one_week(directory, 1, TRUE)
t1 <- fread("NFLBDB2025/tracking_week_1.csv")
games <- fread("NFLBDB2025/games.csv")
plays <- fread("NFLBDB2025/plays.csv")
players <- fread("NFLBDB2025/players.csv")
player_play <- fread("NFLBDB2025/player_play.csv")

df <- left_join(t1, games,  by = c("gameId"))
df <- left_join(df, plays,  by = c("gameId", "playId"))
df <- left_join(df, players,  by = c("nflId"))
df <- left_join(df, player_play,  by = c("gameId","playId", "nflId"))

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






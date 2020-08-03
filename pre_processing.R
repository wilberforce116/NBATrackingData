library(tidyverse)
library(janitor)
library(trajr)
library(lubridate)
library(hms)

source("_functions.R")
source("fullcourt.R")


#### --- Load raw data sets --- ####

moments <- sportvu_convert_json("data/0021500440.json")
pbp <- read_csv("data/LAC_at_LAL_events.csv") %>% 
  clean_names()


#### --- Merge data --- ####
# Remove frames that don't have 11 observations
# Clean the team and game_clock variables
# Arrange in chronological order

moments_with_pbp <- moments %>% 
  left_join(pbp, by = c("event.id" = "eventnum", "quarter" = "period")) %>% 
  mutate(team = case_when(team_id == 1610612747 ~ "Lakers",
                          team_id == 1610612746 ~ "Clippers",
                          is.na(team_id) ~ "ball"),
         game_clock = hms::as_hms(game_clock),
         est_time_of_play = hms::as_hms(pctimestring/60)) %>% 
  group_by(quarter, event.id, game_clock) %>% 
  mutate(count = n()) %>% 
  ungroup() %>% 
  filter(count == 11) %>% 
  select(-matches("\\d")) %>% 
  select(-pctimestring) %>% 
  select(homedescription, visitordescription, everything()) %>% 
  arrange(quarter, event.id, desc(game_clock), desc(shot_clock)) 


#### --- Get rid of duplicated camera frames --- ####
# Create ID variable for camera frames
# Create ball handler variable

full_game <- moments_with_pbp %>% 
  select(-contains(c("description", "event", "timestring", "score", "game_id"))) %>% 
  unique() %>% 
  arrange(quarter, desc(game_clock), desc(shot_clock)) %>% 
  mutate(frame_num = rep(1:(nrow(.)/11), each = 11),
         dist_to_ball = unlist(lapply(1:nrow(.), function(x) dist_to_ball(., x)))) %>% 
  group_by(frame_num) %>%
  mutate(ball_handler = ifelse(length(minN(dist_to_ball, 2)) > 1,
                               lag(ball_handler),
                               which(dist_to_ball == minN(dist_to_ball, 2)))) %>%
  ungroup()

ball_handler_vec <- (full_game %>%
                       group_by(frame_num) %>%
                       slice(ball_handler) %>%
                       unique())$lastname

full_game <- full_game %>% 
  mutate(ball_handler = rep(ball_handler_vec, each = 11))


#### --- Look at the movement of the ball --- ####
# Fix clock stoppage errors
# Create displacement variable

all_ball_frames <- full_game %>%
  filter(lastname == "ball")

runs <- all_ball_frames %>% 
  group_by(run = rep(1:length(rle(all_ball_frames$displacement > 3)$values),
                     rle(all_ball_frames$displacement > 3)$lengths)) %>% 
  group_nest() %>% 
  pull(data)

for (i in seq(2, length(runs), by = 2)) {
  runs[[i]] <- runs[[i]] %>% 
    arrange(row_number() %% 2 == 0)
}

all_ball_frames <- bind_rows(runs) %>%
  mutate(displacement = c(sqrt(diff(x_loc)^2 + diff(y_loc)^2 + diff(radius)^2), NA)) %>% 
  filter(lag(displacement) != 0)

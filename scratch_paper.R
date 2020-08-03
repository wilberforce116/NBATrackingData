library(tidyverse)
library(janitor)
library(trajr)
library(lubridate)
library(hms)

source("_functions.R")
source("fullcourt.R")

# Gantt Chart of Moments

gantt_moments <- function(range){
  moments_with_pbp %>% 
    filter(event.id %in% range) %>% 
    group_by(event.id) %>% 
    summarise(start_time = max(game_clock),
              end_time = min(game_clock)) %>% 
    ggplot() + 
    geom_segment(aes(x = start_time, xend = end_time, y = event.id, yend = event.id),
                 size = 100/length(range)) + 
    scale_x_reverse(breaks = as.numeric(ms("12:00", "11:00", "10:00", "09:00",
                                           "08:00", "07:00", "06:00", "05:00",
                                           "04:00", "03:00", "02:00", "01:00", "00:00")),
                    labels = function(l) strftime(hms(l), "%M:%S")) +
    scale_y_reverse() +
    labs(x = "Game Clock",
         y = "Moment") +
    theme_bw()
}

gantt_moments(1:20)

# Identify time when cameras still roll while the game_clock is stopped

gc_stoppages <- moments_with_pbp %>% 
  group_by(quarter, event.id, game_clock) %>% 
  summarise(count = n()) %>% 
  filter(count != 11) %>% View()

prob_gc_stoppages <- gc_stoppages %>% 
  filter(count > 22) %>% 
  pull(game_clock)

sc_stoppages <- moments_with_pbp %>% 
  group_by(quarter, event.id, shot_clock) %>% 
  summarise(count = n()) %>% 
  filter(count != 11)

prob_sc_stoppages <- sc_stoppages %>% 
  filter(count > 22) %>% 
  pull(shot_clock)


### 


fullcourt() +
  geom_point(data = nodes,
             aes(x_loc, y_loc), color = "red") +
  scale_color_manual(values = c("Lakers" = "#fdb927",
                                "Clippers" = "#1D428A",
                                "ball" = "#FA8320")) + 
  scale_y_reverse()
  

nodes %>%
  filter(shot_clock != 24) %>%
  group_by(ball_handler) %>% 
  summarise(num_passes = n()) %>% 
  View()

shot_df <- shot_df %>% 
  mutate(quarter = as.numeric(quarter))

home_shot_df <- shot_df[1:77,] %>% 
  rename(home_desc = shot_description)
away_shot_df <- shot_df[78:162,] %>% 
  rename(away_desc = shot_description)

shot_df <- bind_rows(home_shot_df, away_shot_df) %>% 
  select(home_desc, away_desc, everything())






home_time <- pbp %>% 
  filter(eventmsgtype %in% 1:2,
         !is.na(homedescription),
         !str_detect(homedescription, "BLK")) %>% 
  pull(pctimestring)

away_time <- pbp %>% 
  filter(eventmsgtype %in% 1:2,
         !is.na(visitordescription),
         !str_detect(visitordescription, "BLK")) %>% 
  pull(pctimestring)

home_shot_df$time <- home_time
away_shot_df$time <- away_time

pbp <- pbp %>% 
  left_join(shot_df, by = c("homedescription" = "home_desc",
                            "visitordescription" = "away_desc",
                            "period" = "quarter",
                            "pctimestring" = "time"))

nodes_annotated <- read_csv("nodes.csv") %>% 
  select(-1)

nodes_annotated$is_pass %>% table()

nodes_annotated %>% 
  filter(is_pass == F) %>% 
  group_by(description) %>% 
  summarise_if(is.numeric, mean) %>% View()

nodes_annotated %>% 
  group_by(is_pass) %>% 
  summarise_if(is.numeric, mean) %>% View()


nodes_annotated %>% 
  filter(dir_change > 1500) %>% 
  group_by(is_pass) %>% 
  summarise(count = n())


nodes_annotated %>% 
  filter(!(description %in% c("Traffic", "Shot"))) %>%
  group_by(is_pass) %>% 
  summarise(count = n())

passes_df <- nodes_annotated %>% 
  filter(is_pass == T)

fullcourt() +
  geom_point(data = passes_df, aes(x_loc, y_loc))

passes_df %>% 
  filter(description != "Inbound" | is.na(description)) %>% 
  group_by(ball_handler) %>% 
  summarise(count = n())

all_ball_frames %>% 
  group_by(ball_handler) %>% 
  summarise(count = n()) %>% 
  ggplot() +
  geom_bar(aes(reorder(ball_handler, count), count/25), stat = "identity") +
  coord_flip() +
  labs(x = "Ball Handler",
       y = "Seconds of Posession",
       title = "How much time each player spent as the ball handler",
       subtitle = "Christmas Day, 2015 - Lakers vs. Clippers") +
  theme_bw()

full_game %>% 
  filter(frame_num %in% passes_df$frame_num) %>% View()




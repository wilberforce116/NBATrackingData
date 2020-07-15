library(tidyverse)
library(janitor)
library(trajr)
library(lubridate)
library(hms)

source("_functions.R")
source("fullcourt.R")

moments <- sportvu_convert_json("data/0021500440.json")
pbp <- read_csv("data/LAC_at_LAL_events.csv") %>% 
  clean_names()

moments_with_pbp <- moments %>% 
  left_join(pbp, by = c("event.id" = "eventnum", "quarter" = "period")) %>% 
  mutate(team = case_when(team_id == 1610612747 ~ "Lakers",
                          team_id == 1610612746 ~ "Clippers",
                          is.na(team_id) ~ "ball"),
         game_clock = hms::as_hms(game_clock),
         est_time_of_play = hms::as_hms(pctimestring/60)) %>% 
  select(-matches("\\d")) %>% 
  select(-pctimestring) %>% 
  select(homedescription, visitordescription, everything()) %>% 
  arrange(quarter, event.id, desc(game_clock), desc(shot_clock))



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

moments_with_pbp %>% 
  group_by(quarter, event.id, game_clock) %>% 
  summarise(n()) %>% 
  filter(`n()` != 11) %>% 
  pull(`n()`) %>% 
  sum()

### Identify Passes

ball_distance <- player_dist_matrix(moments, 25) %>%
  select(starts_with("ball_"))

selected_play <- moments_with_pbp %>% 
  filter(event.id == 25, lastname == "ball") %>% 
  mutate(min_ball_dist = apply(ball_distance, 1, min),
         ball_carrier = apply(ball_distance, 1, which.min))

coords <- selected_play %>% 
  select(x_loc, y_loc, game_clock) %>% 
  TrajFromCoords(fps = 25)

traj_data <- coords %>% 
  mutate(inst_speed = c(NA, TrajDerivatives(coords)[[1]]),
         inst_acceleration = c(NA, NA, TrajDerivatives(coords)[[3]]),
         dir_change = c(NA, NA, TrajDirectionalChange(coords)))


nodes <- selected_play %>% 
  left_join(traj_data, by = c("x_loc" = "x", "y_loc" = "y", "game_clock" = "game_clock")) %>% 
  unique() %>% 
  filter(abs(dir_change) > 200 | inst_acceleration > 250) %>% 
  mutate(next_node_dist = c(sqrt(diff(x_loc)^2 + diff(y_loc)^2), NA))

# nodes <- nodes %>% 
#   mutate(next_node_height = unlist(lapply(1:nrow(nodes), function(x) nodes[x+1,]$radius))) %>% 
#   filter(next_node_dist > 3,
#          next_node_height< 8)
  
nodes <- nodes %>% 
  mutate(next_node_same_carrier = unlist(
    lapply(1:nrow(nodes), function(x) nodes[x,]$ball_carrier == nodes[x+1,]$ball_carrier))) %>% 
  filter(next_node_same_carrier == F,
         radius < 8,
         game_clock < 720)

fullcourt() +
  geom_point(data = selected_play,
             aes(x_loc, y_loc, color = team)) + 
  geom_point(data = nodes,
             aes(x_loc, y_loc), color = "red") +
  scale_color_manual(values = c("Lakers" = "#fdb927",
                                "Clippers" = "#1D428A",
                                "ball" = "#FA8320")) +
  scale_y_reverse()



####

traj_data %>% 
  ggplot() +
  geom_point(aes(game_clock, dir_change)) +
  theme_bw()




traj_data <- coords %>%
  select(x,y, game_clock) %>% 
  mutate(inst_disp = c(NA, sqrt(diff(x)^2 + diff(y)^2)),
         inst_ang_disp = c(NA, NA, (TrajAngles(coords)*180)/pi),
         ang_disp_per_disp = c(NA, NA, na.omit(inst_ang_disp)/diff(na.omit(inst_disp))),
         inst_dir = c(NA, diff(y)/diff(x)),
         inst_dir_change = c(NA, NA, diff(na.omit(inst_dir))/as.numeric(diff(game_clock)[-1])))


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

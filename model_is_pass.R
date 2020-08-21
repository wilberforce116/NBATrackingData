nodes_temp <- nodes_annotated %>%  
  select(ball_handler, game_clock, shot_clock, frame_num, is_pass,
         x_loc, y_loc, radius,
         displacement, inst_speed, inst_acceleration, dir_change) %>% 
  mutate(is_pass = as.factor(is_pass),
         displacement = start_nodes$displacement,
         ends_near_basket = ifelse(row_number() %in% shots_all_baskets, T, F),
         fold = sample(1:5, nrow(nodes_annotated), replace = T))

get_logit_preds <- function(){
  nodes_with_fits <- list()
  for (i in 1:5){
    logit_mod <- glm(is_pass ~ x_loc + y_loc + radius + displacement + inst_speed + inst_acceleration + dir_change + ends_near_basket, family = binomial, data = filter(nodes_temp, fold != i))
    nodes_with_fits[[i]] <- nodes_temp %>% 
      filter(fold == i) %>% 
      mutate(prob_logit = predict(logit_mod, newdata = filter(nodes_temp, fold == i), type = "response"),
             pred_logit = as.factor(ifelse(prob_logit > 0.6, T, F)))
  }
  nodes_with_fits <- bind_rows(nodes_with_fits) %>% 
    arrange(frame_num)
  return(nodes_with_fits)
}

nodes_temp <- get_logit_preds()

caret::confusionMatrix(nodes_temp$pred_logit, nodes_temp$is_pass)








init_logit_mod <- glm(is_pass ~ x_loc + y_loc + radius + displacement + inst_speed + inst_acceleration + dir_change + near_basket, family = binomial, data = nodes_temp)
init_logit_mod %>% summary()



# EDA

nodes_temp %>%
  ggplot(aes(x = is_pass, fill = ends_near_basket)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(x = "Was it a pass?",
       fill = "Ends near basket?") +
  scale_x_discrete(limits = c(TRUE, FALSE, NA), 
                   labels = c("Yes", "No", "Unknown")) +
  scale_fill_manual(breaks = c(TRUE, FALSE), values = c("darkgrey", "darkred")) +
  theme_bw()

nodes_temp %>%
  ggplot(aes(x = is_pass, y = radius)) +
  geom_violin(aes(fill = is_pass), show.legend = F) +
  geom_boxplot(width = 0.2) +
  labs(x = "Was it a pass?",
       y = "Height") +
  scale_x_discrete(limits = c(TRUE, FALSE, NA), 
                   labels = c("Yes", "No", "Unknown")) +
  scale_fill_manual(breaks = c(TRUE, FALSE), values = c("darkred", "darkgrey")) +
  theme_bw()

nodes_temp %>%
  ggplot(aes(x = is_pass, y = displacement)) +
  geom_violin(aes(fill = is_pass), show.legend = F) +
  geom_boxplot(width = 0.2) +
  labs(x = "Was it a pass?",
       y = "Displacement") +
  scale_x_discrete(limits = c(TRUE, FALSE, NA), 
                   labels = c("Yes", "No", "Unknown")) +
  scale_fill_manual(breaks = c(TRUE, FALSE), values = c("darkred", "darkgrey")) +
  ylim(0,3) + 
  theme_bw()


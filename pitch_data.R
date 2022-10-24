#import .csv file
pitcherdata <- read.csv("pitch_data.csv", header = TRUE, sep = ",")

library(tidyverse)

x <- c(1.55, 3.45, 3.45, 1.55, 1.55)
z <- c(-0.9, -0.9, 1, 1, -0.9)

sz <- tibble(x,z)

pitcherdata %>% 
  filter(pitcher_id == "player_a") %>% 
  ggplot(aes(x=plate_loc_height, y=plate_loc_side))+
  geom_path(data = sz, aes(x=x, y=z))+
  coord_equal()+
  geom_point(alpha = 0.3)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  facet_grid(~ pitch_type)+
  xlab("")+
  ylab("")+
  ggtitle("Pitcher B: All Pitches")

pitcherdata %>% 
  filter(pitch_call == "in_play", pitcher_id == "player_b") %>% 
  ggplot(aes(x=plate_loc_height, y=plate_loc_side))+
  geom_path(data = sz, aes(x=x, y=z))+
  coord_equal()+
  geom_point(alpha = 0.4)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  facet_grid(~ pitch_type)+
  xlab("")+
  ylab("")+
  ggtitle("Pitcher B: Balls in Play")

pitcherdata %>% 
  filter(pitch_call == "strike_swinging", pitcher_id == "player_b") %>% 
  ggplot(aes(x=plate_loc_height, y=plate_loc_side))+
  geom_path(data = sz, aes(x=x, y=z))+
  coord_equal()+
  geom_point(alpha = 0.4)+
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())+
  facet_grid(~ pitch_type)+
  xlab("")+
  ylab("")+
  ggtitle("Pitcher B: Swing and Misses")
  

pitcherdata %>%
  filter(pitcher_id =="player_a") %>% 
  mutate(First_Pitch_Strike = ifelse((pitch_of_pa == 1)&((pitch_call == "foul_ball")|(pitch_call == "in_play")|(pitch_call == "strike_called")|(pitch_call == "strike_swinging")), 1,0),
         strike = ifelse(((pitch_call == "foul_ball")|(pitch_call == "in_play")|(pitch_call == "strike_called")|(pitch_call == "strike_swinging")), 1,0),
         swing = ifelse((pitch_call == "foul_ball")|(pitch_call == "in_play")|(pitch_call == "strike_swinging"), 1,0),
         BIP = ifelse(hit_type == "", 0, 1)) %>% 
  group_by(pitch_type) %>% 
  summarise(FirstPitchStrikePercent= mean(First_Pitch_Strike), StrikePercent = mean(strike), SwingPercent = mean(swing), BIP_Percent = mean(BIP))

pitcherdata %>% 
  filter(pitcher_id =="player_b") %>% 
  filter(pitch_call %in% c("in_play", "strike_swinging", "foul_ball")) %>% 
  mutate(whiff = ifelse(pitch_call == "strike_swinging", 1, 0)) %>% 
  group_by(pitch_type) %>% 
  summarise(whiffPercent = mean(whiff))

pitcherdata %>% 
  filter(pitcher_id =="player_b") %>% 
  filter(pitch_call %in% c("in_play", "strike_swinging", "foul_ball")) %>% 
  mutate(whiff = ifelse(pitch_call == "strike_swinging", 1, 0)) %>% 
  group_by(pitch_type) %>% 
  summarise(whiffPercent = mean(whiff))

HH_stats = pitcherdata %>% 
  filter(pitcher_id =="player_b") %>%
  filter(pitch_call == "in_play") %>% 
  mutate(HH = ifelse(exit_speed >= 95, 1, 0),
         GB = ifelse(((hit_type == "soft_ground_ball")|(hit_type == "medium_ground_ball"|(hit_type == "hard_ground_ball")| hit_type == "bunt")), 1, 0),
         LD = ifelse(((hit_type == "soft_line_drive")|(hit_type == "medium_line_drive"|(hit_type == "hard_line_drive"))), 1, 0),
         FB = ifelse(((hit_type == "soft_fly_ball")|(hit_type == "medium_fly_ball"|(hit_type == "hard_fly_ball"|(hit_type == "pop_up")))), 1, 0)) %>% 
  group_by(pitch_type) %>% 
  summarise(HHPercent = mean(HH), GB_Percent = mean(GB), LD_Percent = mean(LD), FB_Percent = mean(FB))
  
  
write.csv(summary_stats, "summary_stats.csv")
write.csv(whiff_stats, "whiff_stats")
write.csv(HH_stats, "HH_stats.csv")

  
  
  
  
  



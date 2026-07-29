library(devtools)
library(baseballr)
library(tidyverse)
packageVersion("baseballr")

options(max.print = 100000)

nov_pitcher_data = statcast_search(start_date = "2025-11-01",
                                end_date = "2025-12-01",
                               player_type = "pitcher")

oct_pitcher_data = statcast_search(start_date = "2025-10-01",
                                   end_date = "2025-11-01",
                                   player_type = "pitcher")

sep_pitcher_data = statcast_search(start_date = "2025-09-01",
                                  end_date = "2025-10-01",
                                  player_type = "pitcher")

oct_batter_data = statcast_search(start_date = "2025-10-01",
                                  end_date = "2025-11-01",
                                  player_type = "batter")

oct_pitcher_data_clean = oct_pitcher_data %>%
#  drop_na(release_pos_x) %>%
#  drop_na(release_pos_z) %>%
#  drop_na(release_speed) %>%
#  drop_na(release_spin_rate) %>%
#  drop_na(release_extension) %>%
  mutate(team = ifelse(inning_topbot == "Top", home_team, away_team)) %>%
  mutate(pitchtype = ifelse(pitch_type == "FF", "Fastball", 
                                      ifelse(pitch_type == "SL", "Slider",
                                             ifelse(pitch_type == "CU", "Curveball",
                                                    ifelse(pitch_type == "CH", "Changeup",
                                                           ifelse(pitch_type == "FS", "Splitter",
                                                                  ifelse(pitch_type == "ST", "Sweeper",
                                                                         ifelse(pitch_type == "SI", "Sinker",
                                                                                ifelse(pitch_type == "FC", "Cutter",
                                                                                       ifelse(pitch_type == "KC", "Knuckle-Curve",
                                                                                              ifelse(pitch_type == "FO", "Forkball",
                                                                                                     ifelse(pitch_type == "SV", "Slurve", "Slow Curve")))))))))))) %>%
  mutate(description_adj = ifelse(events == "double", "hit",
                              ifelse(events == "single", "hit",
                                     ifelse(events == "triple", "hit",
                                            ifelse(events == "home_run", "hit",
                                                   ifelse(events == "field_error", "error_or_fielders_choice",
                                                          ifelse(events == "fielders_choice", "error_or_fielders_choice",
                                                                 ifelse(description == "hit_into_play", "out", description))))))))

median(oct_pitcher_data_hits$launch_speed)

oct_pitcher_data_hits_prev = oct_pitcher_data_clean %>%
#  drop_na(launch_speed) %>%
#  drop_na(launch_angle) %>%
#  drop_na(bat_speed) %>%
#  drop_na(hit_distance_sc) %>%
  filter(description == "hit_into_play") %>%
  mutate(team_hitter = ifelse(inning_topbot == "Top", away_team, home_team))

batter_names = oct_batter_data %>%
  distinct(player_name, .keep_all = TRUE) %>%
  select(player_name, batter)

oct_pitcher_data_hits = oct_pitcher_data_hits_prev %>%
  left_join(batter_names, by = "batter") %>%
  mutate(player_name_hitter = player_name.y) %>%
  mutate(events = ifelse(events == "double_play", "grounded_into_double_play", 
                ifelse(events == "fielders_choice", "field_error",
                       ifelse(events == "fielders_choice_out", "field_out",
                              ifelse(events == "force_out", "field_out", events)))))

median(oct_pitcher_data_hits$bat_speed)


test = oct_pitcher_data_clean %>%
  filter(description == "hit_into_play")
  table(test$events)

pitcher_table_pre = oct_pitcher_data_clean %>%
  filter(!is.na(pitchtype), pitchtype != "") %>%
  count(player_name, team, pitchtype, description_adj) %>%
  pivot_wider(names_from = description_adj,
              values_from = n,
              values_fill = 0) %>%
  mutate(strike = called_strike + swinging_strike + swinging_strike_blocked) %>%
  mutate(surname = sub(",.*", "", player_name)) %>%
  mutate(total = ball + called_strike + foul + foul_tip + out + swinging_strike + hit + blocked_ball + error_or_fielders_choice + hit_by_pitch + swinging_strike_blocked + automatic_ball + automatic_strike + foul_bunt + missed_bunt + bunt_foul_tip)

totals = oct_pitcher_data_clean %>%
  count(player_name)

pitcher_table_joined = left_join(pitcher_table_pre, totals, by = "player_name")

pitcher_table = pitcher_table_joined %>%
  mutate(swinging_strike_rate = swinging_strike / n) %>%
  mutate(strike_rate = strike / n) %>%
  mutate(walk_rate = ball / n)

library(tidyverse)
library(baseballr)
library(Lahman)

data_2025 = baseballr::bref_daily_batter("2025-01-01", "2025-12-31")

data_2025 = data_2025 %>%
  mutate(woba = ((0.69 * BB) + (0.72 * HBP) + (0.87 * X1B) + (1.24 * X2B) + (1.56 * X3B) + (2.03 * HR)) / (AB + BB + HBP + SF))

data_2025 = data_2025 %>%
  mutate(batting_runs = ((woba - .310) / 1.221) * PA) %>%
  mutate(wSB = (SB * .2) - (CS * .4)) %>%
  mutate(ubr_proxy = ((X3B - (PA * 0.0019)) * 0.3) + ((GDP - (PA * 0.0062)) * -0.4))

fielding_2025 = baseballr::fg_fielder_leaders(
  startseason = 2025,
  endseason = 2025,
  pos = "all"
)

fielding_filtered = fielding_2025 %>%
  mutate(Name = PlayerName)

fielding_filtered = fielding_filtered %>%
  mutate(Defense = ifelse(is.na(Defense) == TRUE, 0, Defense)) %>%
  select(Name, Pos, Defense)

fielding_filtered = fielding_filtered %>%
  group_by(Name) %>%
  summarize(Defense = sum(Defense))

total_data_2025 = left_join(data_2025, fielding_filtered, by = "Name")

total_data_2025 = total_data_2025 %>%
  mutate(Defense = ifelse(is.na(Defense) == TRUE, 0, Defense)) %>%
  mutate(replacement_runs = 570*(PA/24300)) %>%
  mutate(runs_per_win = (9 * ((21343 + 21343)/43105.1)) + 1.5) %>%
  mutate(bsr_proxy = wSB + ubr_proxy)

total_data_2025 = total_data_2025 %>%
  mutate(war = (batting_runs + bsr_proxy + Defense + replacement_runs) / runs_per_win)

total_data_2025 = left_join(total_data_2025, batter_id_x, by = "Name")

write_csv(total_data_2025, "2025.csv")

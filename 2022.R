library(tidyverse)
library(baseballr)
library(Lahman)

data_2022 = baseballr::bref_daily_batter("2022-01-01", "2022-12-31")

data_2022 = data_2022 %>%
  mutate(woba = ((0.69 * BB) + (0.72 * HBP) + (0.87 * X1B) + (1.24 * X2B) + (1.56 * X3B) + (2.03 * HR)) / (AB + BB + HBP + SF))

data_2022 = data_2022 %>%
  mutate(batting_runs = ((woba - .310) / 1.221) * PA) %>%
  mutate(wSB = (SB * .2) - (CS * .4)) %>%
  mutate(ubr_proxy = ((X3B - (PA * 0.0019)) * 0.3) + ((GDP - (PA * 0.0062)) * -0.4))

fielding_2022 = baseballr::fg_fielder_leaders(
  startseason = 2022,
  endseason = 2022,
  pos = "all"
)

fielding_filtered = fielding_2022 %>%
  mutate(Name = PlayerName)

fielding_filtered = fielding_filtered %>%
  mutate(Defense = ifelse(is.na(Defense) == TRUE, 0, Defense)) %>%
  select(Name, Pos, Defense)

fielding_filtered = fielding_filtered %>%
  group_by(Name) %>%
  summarize(Defense = sum(Defense))

total_data_2022 = left_join(data_2022, fielding_filtered, by = "Name")

total_data_2022 = total_data_2022 %>%
  mutate(Defense = ifelse(is.na(Defense) == TRUE, 0, Defense)) %>%
  mutate(replacement_runs = 570*(PA/24300)) %>%
  mutate(runs_per_win = (9 * ((21343 + 21343)/43105.1)) + 1.5) %>%
  mutate(bsr_proxy = wSB + ubr_proxy)

total_data_2022 = total_data_2022 %>%
  mutate(war = (batting_runs + bsr_proxy + Defense + replacement_runs) / runs_per_win)

write_csv(total_data_2022, "2022.csv")

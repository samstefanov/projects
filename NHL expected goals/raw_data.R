library(nhlscraper)
library(tidyverse)

raw24 = nhlscraper::gc_play_by_plays(season = 20232024)
raw25 = nhlscraper::gc_play_by_plays(season = 20242025)
raw26 = nhlscraper::gc_play_by_plays(season = 20252026)

raw = rbind(raw24, raw25)
raw = rbind(raw, raw26)

lagged = raw %>%
  mutate(periodslagged = lag(periodNumber)) %>%
  mutate(secs_lagged = lag(secondsElapsedInPeriod)) %>%
  mutate(game_event_lagged = lag(eventTypeDescKey)) %>%
  mutate(distance_lagged = lag(distance))

lagged = lagged %>%
  mutate(timelastevent = (secondsElapsedInPeriod - secs_lagged)) %>%
  mutate(distance_lagged = ifelse(is.na(distance_lagged), 0, distance_lagged))

justshots = lagged %>%
  filter(eventTypeDescKey %in% c("shot-on-goal", "missed-shot", "goal"))

justshots = justshots %>%
  mutate(angle_lagged = lag(angle)) %>%
  mutate(goalcheck = ifelse(eventTypeDescKey == "goal", 1, 0))

justshots = justshots %>%
  mutate(num = 1:nrow(justshots)) %>%
  mutate(rebqual = ifelse(isRebound == TRUE, (abs(angle - angle_lagged) / (timelastevent / 60)), -10))

justshots = justshots %>%
  mutate(high_slot = ifelse((69 <= xCoordNorm & xCoordNorm < 83 & -15 <= yCoordNorm & yCoordNorm <= 15), 1, 0)) %>%
  mutate(low_slot = ifelse((50 <= xCoordNorm & xCoordNorm < 69 & -15 <= yCoordNorm & yCoordNorm <= 15), 1, 0)) %>%
  mutate(crease = ifelse((83 <= xCoordNorm & xCoordNorm <= 89 & -4 <= yCoordNorm & yCoordNorm <= 4), 1, 0)) %>%
  mutate(left_side_of_net = ifelse((83 <= xCoordNorm & xCoordNorm <= 89 & -15 <= yCoordNorm & yCoordNorm < -4), 1, 0)) %>%
  mutate(right_side_of_net = ifelse((83 <= xCoordNorm & xCoordNorm <= 89 & 4 < yCoordNorm & yCoordNorm <= 15), 1, 0)) %>%
  mutate(top = ifelse((25 <= xCoordNorm & xCoordNorm < 50 & -15 <= yCoordNorm & yCoordNorm <= 15), 1, 0)) %>%
  mutate(left_elbow = ifelse((25 <= xCoordNorm & xCoordNorm < 50 & yCoordNorm < -15), 1, 0)) %>%
  mutate(right_elbow = ifelse((25 <= xCoordNorm & xCoordNorm < 50 & 15 < yCoordNorm), 1, 0)) %>%
  mutate(right_flank = ifelse((50 <= xCoordNorm & xCoordNorm <= 89 & 15 < yCoordNorm), 1, 0)) %>%
  mutate(left_flank = ifelse((50 <= xCoordNorm & xCoordNorm <= 89 & yCoordNorm < -15), 1, 0)) %>%
  mutate(behind_net = ifelse((89 < xCoordNorm & xCoordNorm <= 100 & -15 <= yCoordNorm & yCoordNorm <= 15), 1, 0)) %>%
  mutate(left_corner = ifelse((89 < xCoordNorm & xCoordNorm <= 100 & yCoordNorm < -15), 1, 0)) %>%
  mutate(right_corner = ifelse((89 < xCoordNorm & xCoordNorm <= 100 & 15 < yCoordNorm), 1, 0)) %>%
  mutate(rebqual = ifelse(rebqual == Inf, (abs(angle - angle_lagged)) / 0.5, rebqual))

justshots = justshots %>%
  mutate(shotzone = ifelse(high_slot == 1, "high slot", 
                           ifelse(low_slot == 1, "low slot",
                                  ifelse(crease == 1, "crease",
                                         ifelse(left_side_of_net == 1, "left side of net",
                                                ifelse(right_side_of_net == 1, "right side of net",
                                                       ifelse(top == 1, "top",
                                                              ifelse(left_elbow == 1, "left elbow",
                                                                     ifelse(right_elbow == 1, "right elbow",
                                                                            ifelse(right_flank == 1, "right flank",
                                                                                   ifelse(left_flank == 1, "left flank",
                                                                                          ifelse(behind_net == 1, "behind net",
                                                                                                 ifelse(left_corner == 1, "left corner",
                                                                                                        ifelse(right_corner == 1, "right corner", 0))))))))))))))
norebounds = justshots %>%
  filter(isRebound == FALSE)

justrebounds = justshots %>%
  filter(isRebound == TRUE)

norebounds = norebounds %>%
  mutate(number = 1:nrow(norebounds))

write.csv(raw, "raw.csv")
write.csv(norebounds, "norebounds.csv")
write.csv(justrebounds, "justrebounds.csv")

library(tidyverse)
library(nhlscraper)

justrebounds = read.csv("justrebounds.csv")

justrebounds = justrebounds %>%
  filter(gameTypeId == 2)

justrebounds = justrebounds %>%
  mutate(number = 1:nrow(justrebounds))

justrebounds = justrebounds %>%
  filter(homeSkaterCount == 5) %>%
  filter(awaySkaterCount == 5) %>%
  filter(strengthState == "even-strength")

training = justrebounds %>%
  filter(seasonId == 20242025)

testing = justrebounds %>%
  filter(seasonId == 20252026)

training = training %>%
  drop_na(shotType) %>%
  filter(xCoordNorm >= 25) %>%
  select(-number)

testing = testing %>%
  drop_na(shotType) %>%
  filter(xCoordNorm >=25) %>%
  select(-number)

model = glm(goalcheck ~ distance_lagged + angle_lagged + timelastevent + shotzone + angle + 
              distance + shotType, data = training, family = binomial)
summary(model)

testing = testing %>%
  filter(shotType != "between-legs")

testing$predicted_prob = predict(model, type = "response", newdata = testing)

testing = testing %>%
  mutate(category = ifelse(predicted_prob < 0.05, "category 1",
                           ifelse(predicted_prob < 0.10, "category 2",
                                  ifelse(predicted_prob < 0.15, "category 3",
                                         ifelse(predicted_prob < 0.20, "category 4",
                                                ifelse(predicted_prob < 0.25, "category 5",
                                                       ifelse(predicted_prob < 0.30, "category 6",
                                                              ifelse(predicted_prob < 0.35, "category 7",
                                                                     ifelse(predicted_prob < 0.4, "category 8",
                                                                            "category 9")))))))))

just_to_see = testing %>%
  select(goalcheck, angle, angle_lagged, timelastevent, rebqual, predicted_prob, shotType, isRebound, shotzone, isEmptyNetAgainst) %>%
  filter(isEmptyNetAgainst == FALSE)

just_to_test = testing %>%
  mutate(rebcat = ifelse(rebqual < 0, "category 1",
                         ifelse(rebqual < 10, "category 2",
                                ifelse(rebqual < 20, "category 3",
                                       ifelse(rebqual < 30, "category 4",
                                              ifelse(rebqual < 40, "category 5",
                                                     ifelse(rebqual < 50, "category 6",
                                                            ifelse(rebqual < 60, "category 7",
                                                                   ifelse(rebqual < 70, "category 8",
                                                                          ifelse(rebqual < 80, "category 9",
                                                                                 ifelse(rebqual < 90, "category 10",
                                                                                        ifelse(rebqual < 100, "category 11",
                                                                                               "category 12")))))))))))) %>%
  mutate(ang = (abs(angle - angle_lagged)))

grouped = testing %>%
  group_by(category) %>%
  summarize(total = sum(goalcheck))

grouped2 = testing %>%
  group_by(category) %>%
  summarize(n = n())

grouped = left_join(grouped, grouped2, by = "category")

grouped = grouped %>%
  mutate(likelihood = total / n)

ggplot(grouped, aes(x = category, y = likelihood)) + stat_summary(geom = "bar")

training$predicted_prob = predict(model, type = "response", newdata = training)

testing = testing %>%
  select(-category)

grouped3 = testing %>%
  group_by(skater1PlayerIdFor) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater1PlayerIdFor)

grouped4 = testing %>%
  group_by(skater2PlayerIdFor) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater2PlayerIdFor)

grouped5 = testing %>%
  group_by(skater3PlayerIdFor) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater3PlayerIdFor)

grouped6 = testing %>%
  group_by(skater4PlayerIdFor) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater4PlayerIdFor)

grouped7 = testing %>%
  group_by(skater5PlayerIdFor) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater5PlayerIdFor)

grouped8 = testing %>%
  group_by(skater6PlayerIdFor) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater6PlayerIdFor)

grouped9 = testing %>%
  group_by(skater1PlayerIdAgainst) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater1PlayerIdAgainst)

grouped10 = testing %>%
  group_by(skater2PlayerIdAgainst) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater2PlayerIdAgainst)

grouped11 = testing %>%
  group_by(skater3PlayerIdAgainst) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater3PlayerIdAgainst)

grouped12 = testing %>%
  group_by(skater4PlayerIdAgainst) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater4PlayerIdAgainst)

grouped13 = testing %>%
  group_by(skater5PlayerIdAgainst) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater5PlayerIdAgainst)

grouped14 = testing %>%
  group_by(skater6PlayerIdAgainst) %>%
  summarize(total = sum(predicted_prob)) %>%
  mutate(playerId = skater6PlayerIdAgainst)

grouped_all = full_join(grouped3, grouped4, by = "playerId")
grouped_all = full_join(grouped_all, grouped5, by = "playerId")
grouped_all = full_join(grouped_all, grouped6, by = "playerId")
grouped_all = full_join(grouped_all, grouped7, by = "playerId")
grouped_all = full_join(grouped_all, grouped8, by = "playerId")
grouped_all = full_join(grouped_all, grouped9, by = "playerId")
grouped_all = full_join(grouped_all, grouped10, by = "playerId")
grouped_all = full_join(grouped_all, grouped11, by = "playerId")
grouped_all = full_join(grouped_all, grouped12, by = "playerId")
grouped_all = full_join(grouped_all, grouped13, by = "playerId")
grouped_all = full_join(grouped_all, grouped14, by = "playerId")

grouped_all = grouped_all %>%
  mutate(total.x = ifelse(is.na(total.x), 0, total.x)) %>%
  mutate(total.y = ifelse(is.na(total.y), 0, total.y)) %>%
  mutate(total.x.x = ifelse(is.na(total.x.x), 0, total.x.x)) %>%
  mutate(total.y.y = ifelse(is.na(total.y.y), 0, total.y.y)) %>%
  mutate(total.x.x.x = ifelse(is.na(total.x.x.x), 0, total.x.x.x)) %>%
  mutate(total.y.y.y = ifelse(is.na(total.y.y.y), 0, total.y.y.y)) %>%
  mutate(total.x.x.x.x = ifelse(is.na(total.x.x.x.x), 0, total.x.x.x.x)) %>%
  mutate(total.y.y.y.y = ifelse(is.na(total.y.y.y.y), 0, total.y.y.y.y)) %>%
  mutate(total.x.x.x.x.x = ifelse(is.na(total.x.x.x.x.x), 0, total.x.x.x.x.x)) %>%
  mutate(total.y.y.y.y.y = ifelse(is.na(total.y.y.y.y.y), 0, total.y.y.y.y.y)) %>%
  mutate(total.x.x.x.x.x.x = ifelse(is.na(total.x.x.x.x.x.x), 0, total.x.x.x.x.x.x)) %>%
  mutate(total.y.y.y.y.y.y = ifelse(is.na(total.y.y.y.y.y.y), 0, total.y.y.y.y.y.y)) %>%
  mutate(total = total.x + total.y + total.x.x + total.y.y + total.x.x.x + total.y.y.y) %>%
  mutate(total_vs = total.x.x.x.x + total.y.y.y.y + total.x.x.x.x.x + total.y.y.y.y.y + total.x.x.x.x.x.x + total.y.y.y.y.y.y)

grouped_all = grouped_all %>%
  mutate(xgonice = (total - total_vs)) %>%
  select(playerId, total, total_vs, xgonice)

players = nhlscraper::players()

players = players %>%
  select(playerId, playerFullName)

grouped_all = left_join(grouped_all, players, by = "playerId")

grouped_all_rebs = grouped_all

grouped_all_rebs = grouped_all_rebs %>%
  mutate(total_for = total)

grouped_all_rebs = grouped_all_rebs %>%
  select(playerFullName, total_for, total_vs)

write.csv(grouped_all_rebs, "grouped_all_rebs.csv")

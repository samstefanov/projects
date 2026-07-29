library(caret)
library(glmnet)

data2024 = read_csv("2024.csv")
data2025 = read_csv("2025.csv")

data2024 = data2024 %>%
  mutate(war2024 = (war/G) * 162) %>%
  mutate(batting_runs2024 = batting_runs) %>%
  mutate(bsr_proxy2024 = bsr_proxy) %>%
  mutate(defense2024 = Defense) %>%
  filter(G >= 30)

data2025 = data2025 %>%
  mutate(war2025 = (war/G) * 162) %>%
  mutate(batting_runs2025 = batting_runs) %>%
  mutate(bsr_proxy2025 = bsr_proxy) %>%
  mutate(defense2025 = Defense) %>%
  filter(G >= 30)

data2025 = data2025 %>%
  select(Name, war2025, batting_runs2025, bsr_proxy2025, defense2025)

data2023 = read_csv("2023.csv")
data2022 = read_csv("2022.csv")

data2023 = data2023 %>%
  mutate(war2023 = (war/G)*162) %>%
  mutate(batting_runs2023 = batting_runs) %>%
  mutate(bsr_proxy2023 = bsr_proxy) %>%
  mutate(defense2023 = Defense) %>%
  filter(G >= 30)

data2022 = data2022 %>%
  mutate(war2022 = (war/G)*162) %>%
  mutate(batting_runs2022 = batting_runs) %>%
  mutate(bsr_proxy2022 = bsr_proxy) %>%
  mutate(defense2022 = Defense) %>%
  filter (G >= 30)

data2023 = data2023 %>%
  select(Name, war2023, batting_runs2023, bsr_proxy2023, defense2023)

data2022 = data2022 %>%
  select(Name, war2022, batting_runs2022, bsr_proxy2022, defense2022)

combined = left_join(data2024, data2025, by = "Name")

combined = left_join(combined, data2023, by = "Name")
combined = left_join(combined, data2022, by = "Name")

combined = combined %>%
  drop_na(war2024) %>%
  drop_na(war2025) %>%
  drop_na(war2023) %>%
  drop_na(war2022) %>%
  mutate(krate = (SO / PA)) %>%
  mutate(walkrate = (BB / PA))

baseballid = read_csv("baseballid.csv")

baseballid = baseballid %>%
  mutate(bbref_id = key_bbref) %>%
  mutate(id = key_mlbam)

baseballid = baseballid %>%
  select(bbref_id, id)

combined_with_id = left_join(combined, baseballid, by = "bbref_id")

combined_with_id = combined_with_id %>%
  filter(PA >= 10)

bat_speed_2024 = read_csv("bat-tracking (3).csv")

bat_speed_2024 = bat_speed_2024 %>%
  mutate(avg_bat_speed_2024 = avg_bat_speed)

bat_speed_2024 = bat_speed_2024 %>%
  select(id, avg_bat_speed_2024)

bat_speed_2023 = read_csv("bat-tracking (5).csv")

bat_speed_2023 = bat_speed_2023 %>%
  mutate(avg_bat_speed_2023 = avg_bat_speed)

bat_speed_2023 = bat_speed_2023 %>%
  select(id, avg_bat_speed_2023)

combined_with_id = left_join(combined_with_id, bat_speed_2024, by = "id")

combined_with_id = left_join(combined_with_id, bat_speed_2023, by = "id")

hit_rates_2024 = read_csv("stats.csv")

hit_rates_2024 = hit_rates_2024 %>%
  mutate(id = player_id) %>%
  mutate(hard_hit_percent_2024 = hard_hit_percent) %>%
  mutate(oz_swing_percent_2024 = oz_swing_percent)

hit_rates_2024 = hit_rates_2024 %>%
  select(id, hard_hit_percent_2024, oz_swing_percent_2024)

hit_rates_2023 = read_csv("stats (3).csv")

hit_rates_2023 = hit_rates_2023 %>%
  mutate(id = player_id) %>%
  mutate(hard_hit_percent_2023 = hard_hit_percent) %>%
  mutate(oz_swing_percent_2023 = oz_swing_percent)

hit_rates_2023 = hit_rates_2023 %>%
  select(id, hard_hit_percent_2023, oz_swing_percent_2023)

hit_rates_2022 = read_csv("stats (4).csv")

hit_rates_2022 = hit_rates_2022 %>%
  mutate(id = player_id) %>%
  mutate(hard_hit_percent_2022 = hard_hit_percent) %>%
  mutate(oz_swing_percent_2022 = oz_swing_percent)

hit_rates_2022 = hit_rates_2022 %>%
  select(id, hard_hit_percent_2022, oz_swing_percent_2022)

combined_with_id = left_join(combined_with_id, hit_rates_2024, by = "id")

combined_with_id = left_join(combined_with_id, hit_rates_2023, by = "id")

combined_with_id = left_join(combined_with_id, hit_rates_2022, by = "id")

combined_with_id = combined_with_id %>%
  drop_na(hard_hit_percent_2022) %>%
  drop_na(hard_hit_percent_2023) %>%
  mutate(avg_bat_speed_2023 = ifelse(is.na(avg_bat_speed_2023), avg_bat_speed_2024, avg_bat_speed_2023))

print(mean(combined_with_id$oz_swing_percent_2022))

model = lm(war2025 ~ Age + war2024 + war2023, data = combined_with_id)
importance = varImp(model)
print(importance)
print(model)

y = combined_with_id$war2025

x = model.matrix(y ~ Age + hard_hit_percent_2024 + hard_hit_percent_2023 + hard_hit_percent_2022 + oz_swing_percent_2024 + oz_swing_percent_2023 + oz_swing_percent_2022 + avg_bat_speed_2024 + avg_bat_speed_2023 + batting_runs2024 + bsr_proxy2024 + defense2024 + batting_runs2023 + bsr_proxy2023 + defense2023 + batting_runs2022 + bsr_proxy2022 + defense2022, data = combined_with_id)[, -1]

cv_model = cv.glmnet(x, y, alpha = 0)

best_coefficients = coef(cv_model, s = "lambda.min")

print(best_coefficients)

cv_lasso = cv.glmnet(x, y, alpha = 1)

final_coefficients = coef(cv_lasso, s = "lambda.min")

print(final_coefficients)

combined_with_id = combined_with_id %>%
  mutate(lmmodelallpredictors = (-2.252056 + (-0.093608 * Age) + 
                                   (0.022409 * hard_hit_percent_2024) + (-0.042186 * hard_hit_percent_2023) + (0.032307 * hard_hit_percent_2022) +
                                   (-0.015252 * oz_swing_percent_2024) + (0.045495 * oz_swing_percent_2023) + (-0.024836 * oz_swing_percent_2022) + 
                                   (0.154881 * avg_bat_speed_2024) + (-0.083196 * avg_bat_speed_2023) + 
                                   (0.050086 * batting_runs2024) + (-0.005237 * bsr_proxy2024) + (0.067589 * defense2024) +
                                   (0.030454 * batting_runs2023) + (0.079705 * bsr_proxy2023) + (0.004902 * defense2023) +
                                   (0.021167 * batting_runs2022) + (0.025818 * bsr_proxy2022) + (0.046854 * defense2022))) %>%
  mutate(lmmodelbackwardsselection = (-3.68730 + (-0.09347 * Age) +
                                        (0.09991 * avg_bat_speed_2024) + (0.05184 * batting_runs2024) + (0.07320 * defense2024) + 
                                        (0.02441 * batting_runs2023) + (0.08602 * bsr_proxy2023) + (0.02680 * batting_runs2022) + (0.04972 * defense2022))) %>%
  mutate(lmmodeljustwars = (-0.2524 + (0.4059 * war2024) + (0.2544 * war2023) + (0.1831 * war2022))) %>%
  mutate(lmmodelwarsallpredictors = (-2.45654 + (-0.09428 * Age) +
                                       (0.03815 * hard_hit_percent_2024) + (-0.04890 * hard_hit_percent_2023) + (0.01461 * hard_hit_percent_2022) +
                                       (-0.01663 * oz_swing_percent_2024) + (0.04945 * oz_swing_percent_2023) + (-0.02087 * oz_swing_percent_2022) +
                                       (0.14145 * avg_bat_speed_2024) + (-0.07770 * avg_bat_speed_2023) +
                                       (0.33479 * war2024) + (0.26934 * war2023) + (0.20735 * war2022))) %>%
  mutate(lmmodelwarsbackwards = (2.6784 + (-0.1007 * Age) + (0.3920 * war2024) + (0.2388 * war2023) + (0.2171 * war2022))) %>%
  mutate(ridgemodelallpreds = (-2.185861792 + (-0.077504826 * Age) + 
                                 (0.027097591 * hard_hit_percent_2024) + (-0.021090585 * hard_hit_percent_2023) + (0.021114575 * hard_hit_percent_2022) +
                                 (-0.007048770 * oz_swing_percent_2024) + (0.021503481 * oz_swing_percent_2023) + (-0.012605523 * oz_swing_percent_2022) + 
                                 (0.059939188 * avg_bat_speed_2024) + (-0.001543654 * avg_bat_speed_2023) + 
                                 (0.040529764 * batting_runs2024) + (0.014944043 * bsr_proxy2024) + (0.056556751 * defense2024) +
                                 (0.024901058 * batting_runs2023) + (0.064990935 * bsr_proxy2023) + (0.015344583 * defense2023) +
                                 (0.022478188 * batting_runs2022) + (0.027135245 * bsr_proxy2022) + (0.035423397 * defense2022))) %>%
  mutate(lassomodelsomepreds = 0.546458737 + (-0.029227867 * Age) + (0.011474019 * hard_hit_percent_2024) + (0.009172416 * avg_bat_speed_2024) + (0.046496870 * batting_runs2024) + (0.057002433 * defense2024) + (0.019084340 * batting_runs2023) + (0.014607420 * bsr_proxy2023) + (0.011275374 * batting_runs2022) + (0.016433304 * defense2022)) %>%
  mutate(lassomodelallpreds = (-1.161267463 + (-0.079268013 * Age) + (0.013913009 * hard_hit_percent_2024) + (-0.003353711 * hard_hit_percent_2023) + (0.012727913 * hard_hit_percent_2022) + 
                                 (0.005186285 * oz_swing_percent_2023) + (0.044555058 * avg_bat_speed_2024) + (0.049501921 * batting_runs2024) + (0.069350577 * defense2024) +
                                 (0.023081321 * batting_runs2023) + (0.072649373 * bsr_proxy2023) + (0.001889488 * defense2023) + (0.021212593 * batting_runs2022) + (0.008362837 * bsr_proxy2022) + (0.040128440 * defense2022))) %>%
  mutate(lmmodelwarsjust23 = (2.17452 + (-0.07996 * Age) + (0.43630 * war2024) + (0.32940 * war2023)))

combined_with_id = combined_with_id %>%
  mutate(gap_allpredictors = round((war2025 - lmmodelallpredictors), digits = 5)) %>%
  mutate(gap_backwards = round((war2025 - lmmodelbackwardsselection), digits = 5)) %>%
  mutate(gap_wars = round((war2025 - lmmodeljustwars), digits = 5)) %>%
  mutate(gap_allpredwars = round((war2025 - lmmodelwarsallpredictors), digits = 5)) %>%
  mutate(gap_backwardswars = round((war2025 - lmmodelwarsbackwards), digits = 5)) %>%
  mutate(gap_ridgeallpreds = round((war2025 = ridgemodelallpreds), digits = 5)) %>%
  mutate(gap_lassosomepreds = round((war2025 - lassomodelsomepreds), digits = 5)) %>%
  mutate(gap_lassoallpreds = round((war2025 - lassomodelallpreds), digits = 5)) %>%
  mutate(gap_warsjust23 = round((war2025 - lmmodelwarsjust23), digits = 5))

combined_with_id = combined_with_id %>%
  mutate(gap_abs_allpredictors = abs(gap_allpredictors)) %>%
  mutate(gap_abs_backwards = abs(gap_backwards)) %>%
  mutate(gap_abs_wars = abs(gap_wars)) %>%
  mutate(gap_abs_allpredwars = abs(gap_allpredwars)) %>%
  mutate(gap_abs_backwardswars = abs(gap_backwardswars)) %>%
  mutate(gap_abs_ridgeallpreds = abs(gap_ridgeallpreds)) %>%
  mutate(gap_abs_lassosomepreds = abs(gap_lassosomepreds)) %>%
  mutate(gap_abs_lassoallpreds = abs(gap_lassoallpreds)) %>%
  mutate(gap_abs_warsjust23 = abs(gap_warsjust23))

combined_with_id_filtered = combined_with_id %>%
  select(Name, Age, war2022, war2023, war2024, war2025, lmmodelallpredictors, lmmodelbackwardsselection, 
         lmmodeljustwars, lmmodelwarsallpredictors, lmmodelwarsbackwards, ridgemodelallpreds, lmmodelwarsjust23,
         lassomodelsomepreds,lassomodelallpreds,
         gap_allpredictors, gap_abs_allpredictors,
         gap_backwards, gap_abs_backwards,
         gap_wars, gap_abs_wars,
         gap_allpredwars, gap_abs_allpredwars,
         gap_backwardswars, gap_abs_backwardswars,
         gap_ridgeallpreds, gap_abs_ridgeallpreds,
         gap_lassosomepreds, gap_abs_lassosomepreds,
         gap_lassoallpreds, gap_abs_lassoallpreds,
         gap_warsjust23, gap_abs_warsjust23)

print(mean(combined_with_id$gap_abs_allpredictors))
print(mean(combined_with_id$gap_abs_backwards))
print(mean(combined_with_id$gap_abs_wars))
print(mean(combined_with_id$gap_abs_allpredwars))
print(mean(combined_with_id$gap_abs_backwardswars))
print(mean(combined_with_id$gap_abs_ridgeallpreds))
print(mean(combined_with_id$gap_abs_lassosomepreds))
print(mean(combined_with_id$gap_abs_lassoallpreds))
print(mean(combined_with_id$gap_abs_warsjust23))


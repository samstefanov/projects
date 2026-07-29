library(tidyverse)

grouped_all_rebs = read.csv("grouped_all_rebs (4).csv")
grouped_all = read.csv("grouped_all (3).csv")

grouped_all = grouped_all %>%
  mutate(playerFullName = ifelse(X == 206, "Elias Pettersson (F)", playerFullName)) %>%
  mutate(playerFullName = ifelse(X == 870, "Elias Pettersson (D)", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Jacob Middleton", "Jake Middleton", playerFullName))

grouped_all_rebs = grouped_all_rebs %>%
  mutate(playerFullName = ifelse(X == 163, "Elias Pettersson (F)", playerFullName)) %>%
  mutate(playerFullName = ifelse(X == 819, "Elias Pettersson (D)", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Jacob Middleton", "Jake Middleton", playerFullName))

grouped = full_join(grouped_all, grouped_all_rebs, by = "playerFullName")

grouped_filter = grouped %>%
  mutate(total_for.x = ifelse(is.na(total_for.x), 0, total_for.x)) %>%
  mutate(total_for.y = ifelse(is.na(total_for.y), 0, total_for.y)) %>%
  mutate(total_vs.x = ifelse(is.na(total_vs.x), 0, total_vs.x)) %>%
  mutate(total_vs.y = ifelse(is.na(total_vs.y), 0, total_vs.y))

grouped_filter = grouped_filter %>%
  mutate(total_for = total_for.x + total_for.y) %>%
  mutate(total_vs = total_vs.x + total_vs.y)

grouped_filter = grouped_filter %>%
  select(playerFullName, total_for, total_vs)

grouped_filter = grouped_filter %>%
  filter(!is.na(playerFullName))

grouped_filter$rank_for = rank(-grouped_filter$total_for)
grouped_filter$rank_vs = rank(-grouped_filter$total_vs)

actual = read.csv("Player Season Totals - Natural Stat Trick (6).csv")

actual = actual %>%
  mutate(playerFullName = Player)

actual = actual %>%
  select(playerFullName, xGF, xGA, Off..Zone.Starts, Def..Zone.Starts, Neu..Zone.Starts, On.The.Fly.Starts, GP, TOI)

actual = actual %>%
  mutate(playerFullName = ifelse(playerFullName == "Tommy Novak", "Thomas Novak", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Olli Määttä", "Olli Maatta", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "A.J. Greer", "Anthony-John (AJ) Greer", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Zack Bolduc", "Zachary Bolduc", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Maxim Tsyplakov", "Maksim Tsyplakov", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Frederic Gaudreau", "Freddy Gaudreau", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Yegor Chinakhov", "Egor Chinakhov", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Joe Veleno", "Joseph Veleno", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "P.O Joseph", "Pierre-Olivier Joseph", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Aatu Raty", "Aatu Räty", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Joshua Mahura", "Josh Mahura", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Alex Petrovic", "Alexander Petrovic", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Jeffrey Viel", "Jeffrey Truchon-Viel", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Zac Jones", "Zachary Jones", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Juuso Valimaki", "Juuso Välimäki", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Jack St. Ivany", "John St. Ivany", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Max Shabanov", "Maksim Shabanov", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Josh Dunne", "Joshua Dunne", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Sammy Blais", "Samuel Blais", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Sam Poulin", "Samuel Poulin", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Alex Barré-Boulet", "Alex Barre-Boulet", playerFullName)) %>%
  mutate(playerFullName = ifelse(xGF == 1.11, "Matej Blümel", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Viking Gustafsson-Nyberg", "Viking Gustafsson Nyberg", playerFullName)) %>%
  mutate(playerFullName = ifelse(xGF == 43.80, "Elias Pettersson (F)", playerFullName)) %>%
  mutate(playerFullName = ifelse(xGF == 34.94, "Elias Pettersson (D)", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Alex Nylander", "Alexander Nylander", playerFullName)) %>%
  mutate(playerFullName = ifelse(playerFullName == "Martin Fehérváry", "Martin Fehervary", playerFullName))

grouped_combined = full_join(grouped_filter, actual, by = "playerFullName")

grouped_combined$rankafor = rank(-grouped_combined$xGF)
grouped_combined$rankavs = rank(-grouped_combined$xGA)

grouped_combined = grouped_combined %>%
  mutate(for_rank_diff = abs(rank_for - rankafor)) %>%
  mutate(vs_rank_diff = abs(rank_vs - rankavs))

grouped_combined = grouped_combined %>%
  mutate(xG_on_ice = total_for - total_vs) %>%
  mutate(xGFp60 = ((total_for / TOI) * 60)) %>%
  mutate(xGAp60 = ((total_vs / TOI) * 60)) %>%
  filter(GP >= 20) %>%
  mutate(xGratio = (total_for / (total_for + total_vs))) %>%
  mutate(o_zone_start_ratio = (Off..Zone.Starts / (Off..Zone.Starts + Def..Zone.Starts + Neu..Zone.Starts + On.The.Fly.Starts))) %>%
  mutate(d_zone_start_ratio = (Def..Zone.Starts / (Off..Zone.Starts + Def..Zone.Starts + Neu..Zone.Starts + On.The.Fly.Starts)))

grouped_combined = grouped_combined %>%
  mutate(zone_start_ratio = o_zone_start_ratio - d_zone_start_ratio) %>%
  mutate(xgvzs = (xGratio - zone_start_ratio)) %>%
  select(playerFullName, total_for, GP, TOI, total_vs, xGFp60, xGAp60, xGratio, o_zone_start_ratio, d_zone_start_ratio, zone_start_ratio, xgvzs)


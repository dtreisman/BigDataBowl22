library(tidyverse)
library(ngscleanR)
library(ggvoronoi)
library(ggrepel)
library(caret)
library(randomForest)
library(xgboost)
library(MLmetrics)
library(ModelMetrics)
library(deldir)
library(patchwork)
library(nflfastR)
library(bayesboot)

fastR <- load_pbp(seasons = c(2018, 2019, 2020)) %>%
  filter(play_type == "punt") %>%
  select(game_id, play_id, contains(c("lateral", "fumble"))) %>%
  filter(lateral_return == 0, fumble == 0) 

clean_and_condense_for_xRY <- function(df_tracking, plays, pff) {
  
  
  df_tracking <- df_tracking %>%
    ngscleanR::clean_and_rotate()
  
  df_Punts <- df_tracking %>%
    filter(play_type_nfl == "PUNT")
  
  # Get Punt Apex ----
  df_apex <- df_Punts %>%
    filter(#game_id == 2018123000, play_id %in% c(892, 1267), 
      display_name == "football") %>%
    mutate(event_group = ifelse(event != "None", event, NA),
           event_group = ifelse(frame_id == 1, "play_start", event_group)) %>%
    fill(event_group, .direction = "down") %>%
    filter(event_group == "punt") %>%
    mutate(event_group = ifelse(s == max(s), event_group, NA)) %>%
    fill(event_group, .direction = "down") %>%
    filter(event_group == "punt") %>%
    group_by(game_id, play_id) %>%
    mutate(hang_frame = row_number(),
           punt_apex = ifelse(hang_frame == round(max(hang_frame)/2), T, F)) %>%
    filter(punt_apex == T) %>%
    mutate(apex_frame = frame_id) %>%
    select(game_id, play_id, apex_frame)
  
  
  # Make Main DF ----
  start = Sys.time()
  df_Main <- df_Punts %>%
    #filter(game_id == 2018123000, play_id %in% c(892, 1267)) %>%
    left_join(plays %>%
                select(game_id, play_id, special_teams_result, game_clock, returner_id),
              by = c("game_id", "play_id")) %>%
    left_join(pff %>%
                select(game_id, play_id, gunners, vises),
              by = c("game_id", "play_id")) %>%
    separate(gunners, into = c("gunner_1", "gunner_2"), sep = "; ") %>%
    separate(vises, into = c("vise_1", "vise_2", "vise_3"), sep = "; ") %>%
    separate(game_clock, into = c("minutes_rem", "seconds_rem", "milisec_rem"), sep = ":") %>%
    mutate(time_remaining = as.numeric(minutes_rem)*60 + as.numeric(seconds_rem)) %>%
    inner_join(df_apex, by = c("game_id", "play_id")) %>%
    mutate(event_group = ifelse(event != "None", event, NA)) %>%
    fill(event_group, .direction = "down") %>%
    #filter(event_group == "punt") %>%
    mutate(punt_frame = (s == max(s) & event_group == "punt")) %>%
    #filter(event_group == "punt") %>%
    filter(frame_id >= punt_frame) %>%
    mutate(is_returner = ifelse(returner_id == nfl_id, 1, 0),
           jersey_number = as.character(jersey_number),
           is_gunner_1 = ifelse((team_name == str_extract(gunner_1, team_name) & 
                                   jersey_number == readr::parse_number(gunner_1)),
                                1, 
                                0),
           is_gunner_2 = ifelse((team_name == str_extract(gunner_2, team_name) & 
                                   jersey_number == readr::parse_number(gunner_2)),
                                1, 
                                0),
           is_vise_1 = ifelse((team_name == str_extract(vise_1, team_name) & 
                                 jersey_number == readr::parse_number(vise_1)),
                              1, 
                              0),
           is_vise_2 = ifelse((team_name == str_extract(vise_2, team_name) & 
                                 jersey_number == readr::parse_number(vise_2)),
                              1, 
                              0))
  
  
  
  Sys.time() - start
  
  return(df_Main)
  
  
} 


tracking2018 <- read_rds("tracking2018.rds")
start <- Sys.time()
df_Punts2018 <- clean_and_condense_for_xRY(tracking2018, plays, pff)
Sys.time() - start
rm(tracking2018)

tracking2019 <- read_rds("tracking2019.rds") 
df_Punts2019 <- clean_and_condense_for_xRY(tracking2019, plays, pff)
rm(tracking2019)

tracking2020 <- read_rds("tracking2020.rds") 
df_Punts2020 <- clean_and_condense_for_xRY(tracking2020, plays, pff)
rm(tracking2020)

df_Main <- bind_rows(df_Punts2018, df_Punts2019, df_Punts2020)

write.csv(df_Main, "Main DF.csv", row.names = F)

save.image()



teams <- plays %>%
  select(game_id, play_id, possession_team) %>%
  left_join(games) %>%
  mutate(return_team = ifelse(possession_team == home_team_abbr, visitor_team_abbr, home_team_abbr),
         kicking_team = ifelse(possession_team == home_team_abbr, home_team_abbr, visitor_team_abbr)) %>%
  select(game_id, play_id, return_team, kicking_team) 

player_teams <- df_returner %>%
  ungroup() %>%
  select(game_id, play_id, nfl_id) %>%
  rename(returner_id = nfl_id) %>%
  unique() %>%
  left_join(teams) %>%
  mutate(season = str_sub(game_id, 1, 4)) %>%
  select(returner_id, return_team, season) %>%
  unique() %>%
  pivot_wider(id_cols = returner_id, names_from = season, 
              values_from = return_team, values_fn = list)

player_names <- df_Main %>%
  ungroup() %>%
  select(game_id, play_id, nfl_id) %>%
  unique() %>%
  left_join(players)

kickers <- plays %>% 
  select(game_id, play_id, kicker_id)


# Get FB LOC ----
df_football <- df_Main %>%
  filter(display_name == "football") %>%
  rename(football_x = x,
         football_y = y,
         football_s = s,
         football_a = a) %>%
  select(game_id, play_id, frame_id, football_x, football_y, football_s, football_a)



# Get Returner LOC ----
df_returner <- df_Main %>%
  filter(!is.na(nfl_id)) %>%
  select(game_id, play_id, frame_id, nfl_id, position, x, y, s, a, o, dir, is_returner) %>%
  left_join(df_football, by = c("game_id", "play_id", "frame_id")) %>%
  
  group_by(game_id, play_id, frame_id) %>%
  # mutate(min_frame = ifelse(frame_id == min(frame_id), 1, 0)) %>%
  # filter(min_frame ==1 ) %>%
  # mutate(dist_to_fb = sqrt((football_x - x)**2 + (football_y - y)**2)) %>%
  # mutate(is_returner_ = ifelse(x == max(x), 1, 0)) %>%
  # filter(is_returner_ == 1) %>% 
  left_join(df_Main %>%
              select(game_id, play_id, frame_id, nfl_id, position, x, y) %>%
              group_by(game_id, play_id) %>%
              filter(frame_id == 1) %>%
              mutate(is_returner_ = ifelse(x == max(x), 1,0)) %>%
              filter(is_returner_ == 1) %>%
              select(game_id, play_id, nfl_id) %>%
              rename(returner_id = nfl_id) %>%
              unique(), by = c("game_id", "play_id")) %>%
  filter(nfl_id == returner_id) %>%
  left_join(df_Mod_puntloc %>%
              select(game_id, play_id, pred_x, pred_y), 
            by = c("game_id", "play_id")) %>%
  mutate(dist_to_punt_xloc = sqrt(((pred_x - x)**2) + ((pred_y - y)**2))) %>%
  rename_at(vars(c(x, y, s, a, o, dir)),function(x) paste0("returner_", x)) %>%
  
  select(game_id, play_id, frame_id, nfl_id, position, returner_x, returner_y, returner_s, returner_a, 
         returner_o, returner_dir, dist_to_punt_xloc)


# Get PFF cols ----
df_pff <- pff %>%
  mutate(kick_direction_error = ifelse(kick_direction_actual != kick_direction_intended, 1, 0)) %>%
  select(game_id, play_id, kick_direction_error)


# Get Situation vars ----
df_situation <- plays %>%
  filter(special_teams_result %in% c("Return", "Downed", "Muffed", "Fair Catch", "Touchback")) %>%
  select(game_id, play_id, kick_return_yardage, special_teams_result, 
         yardline_number) #%>%
  # inner_join(df_Main %>% select(game_id, play_id, time_remaining) %>% unique(), 
  #            by = c("game_id", "play_id"))


# Vise1 DF ----
df_vise1 <- df_Main %>%
  filter(is_vise_1 == 1) %>%
  rename_at(vars(c(x, y, s, a, o, dir)),function(x) paste0("vise_1_", x)) %>%
  select(game_id, play_id, frame_id, vise_1_x, vise_1_y, vise_1_s, vise_1_a, 
         vise_1_o, vise_1_dir)

# Vise 2 DF ----
df_vise2 <- df_Main %>%
  filter(is_vise_2 == 1) %>% 
  rename_at(vars(c(x, y, s, a, o, dir)),function(x) paste0("vise_2_", x)) %>%
  select(game_id, play_id, frame_id, vise_2_x, vise_2_y, vise_2_s, vise_2_a, 
         vise_2_o, vise_2_dir)

# Get Gunner1 vars ----
df_gunner1 <- df_Main %>%
  filter(is_gunner_1 == 1) %>% 
  rename_at(vars(c(x, y, s, a, o, dir)),function(x) paste0("gunner_1_", x)) %>%
  select(game_id, play_id, frame_id, gunner_1_x, gunner_1_y, gunner_1_s, gunner_1_a, 
         gunner_1_o, gunner_1_dir) %>%
  inner_join(df_vise1, by = c("game_id", "play_id", "frame_id")) %>%
  inner_join(df_vise2, by = c("game_id", "play_id", "frame_id")) %>%
  mutate(gun1_dist_to_vise1 = sqrt((gunner_1_x - vise_1_x)**2 + (gunner_1_y - vise_1_y)**2),
         gun1_dist_to_vise2 = sqrt((gunner_1_x - vise_2_x)**2 + (gunner_1_y - vise_2_y)**2),
         gunner1_def_dist = ifelse(gun1_dist_to_vise1 < gun1_dist_to_vise2, 
                                   gun1_dist_to_vise1, gun1_dist_to_vise2),
         gunner1_def_xdist = ifelse(gun1_dist_to_vise1 < gun1_dist_to_vise2,
                                    gunner_1_x - vise_1_x, gunner_1_x - vise_2_x),
         gunner1_def_ydist = ifelse(gun1_dist_to_vise1 < gun1_dist_to_vise2,
                                    vise_1_y - gunner_1_y , vise_2_y - gunner_1_y)) %>%
  select(-contains("vise"))


# Get Gunner2 vars ----
df_gunner2 <- df_Main %>%
  filter(is_gunner_2 == 1) %>% 
  rename_at(vars(c(x, y, s, a, o, dir)),function(x) paste0("gunner_2_", x)) %>%
  select(game_id, play_id, frame_id, gunner_2_x, gunner_2_y, gunner_2_s, gunner_2_a, 
         gunner_2_o, gunner_2_dir) %>%
  inner_join(df_vise1, by = c("game_id", "play_id", "frame_id")) %>%
  inner_join(df_vise2, by = c("game_id", "play_id", "frame_id")) %>%
  mutate(gun2_dist_to_vise1 = sqrt((gunner_2_x - vise_1_x)**2 + (gunner_2_y - vise_1_y)**2),
         gun2_dist_to_vise2 = sqrt((gunner_2_x - vise_2_x)**2 + (gunner_2_y - vise_2_y)**2),
         gunner2_def_dist = ifelse(gun2_dist_to_vise1 < gun2_dist_to_vise2, 
                                   gun2_dist_to_vise1, gun2_dist_to_vise2),
         gunner2_def_xdist = ifelse(gun2_dist_to_vise1 < gun2_dist_to_vise2,
                                    gunner_2_x - vise_1_x, gunner_2_x - vise_2_x),
         gunner2_def_ydist = ifelse(gun2_dist_to_vise1 < gun2_dist_to_vise2,
                                    vise_1_y - gunner_2_y, vise_2_y - gunner_2_y)) %>%
  select(-contains("vise"))


# Full Gunner DF ----
df_gunners <- left_join(df_gunner1, df_gunner2, by = c("game_id", "play_id", "frame_id")) %>%
  left_join(df_returner, by = c("game_id", "play_id", "frame_id")) %>%
  mutate(ret_dist_to_g1 = sqrt((returner_x - gunner_1_x)**2 + (returner_y - gunner_1_y)**2),
         ret_dist_to_g2 = sqrt((returner_x - gunner_2_x)**2 + (returner_y - gunner_2_y)**2)) %>%
  select(-contains("returner"))

colSums(is.na(df_gunners))


# Get Nearest Defender DF ----
df_nearestDefender <- df_Main %>% 
  filter(defense == 0) %>%
  select(game_id, play_id, frame_id, nfl_id, x, y, s) %>%
  left_join(
    df_returner %>%
      select(game_id, play_id, frame_id, nfl_id, returner_x, returner_y) %>%
      rename(returner_id = nfl_id),
    by = c("game_id", "play_id", "frame_id")
  ) %>%
  filter(nfl_id != returner_id, !(is.na(nfl_id))) %>%
  mutate(dist_to_returner = sqrt((returner_x - x)**2 + (returner_y - y)**2)) %>%
  group_by(game_id, play_id, frame_id) %>%
  # mutate(def_loc = ifelse(dist_to_returner == min(dist_to_returner), "nearest" , "other"),
         # def_loc = ifelse(dist_to_returner == min(dist_to_returner[which(dist_to_returner != min(dist_to_returner))]), def_loc , 0)) %>%
  mutate(def_dist_rank = rank(dist_to_returner, ties.method = "first")) %>%
  filter(def_dist_rank <= 2) %>%
  select(-nfl_id) %>%
  pivot_wider(id_cols = game_id:returner_y, names_from = def_dist_rank, values_from = x:s) %>%
  mutate(def1_dist = sqrt((x_1 - returner_x)**2 + (y_1 - returner_y)**2),
         def2_dist = sqrt((x_2 - returner_x)**2 + (y_2 - returner_y)**2)) %>%
  select(-contains("returner"), play_id) %>%
  ungroup()


df_uniquePlays <- df_Main %>%
  select(game_id, play_id, frame_id) %>%
  unique()



# start = Sys.time()
# df_voronoi <- data.frame()
# for (i in c(1:nrow(df_Main %>%
#                    select(game_id, play_id, frame_id) %>%
#                    unique()))) {
#   #for (i in c(1:50)) {
#   df_temp <- df_Main %>%
#     filter(game_id == df_uniquePlays$game_id[i], 
#            play_id == df_uniquePlays$play_id[i],
#            frame_id == df_uniquePlays$frame_id[i]) 
#   
#   #print(paste(i, nrow(df_temp)))
#   print(paste("Row:", i, "-",
#               df_uniquePlays$game_id[i],
#               df_uniquePlays$play_id[i], 
#               df_uniquePlays$frame_id[i]))
#   
#   if (nrow(df_temp) == 23 & sum(df_temp$is_returner, na.rm = T) == 1) {
#     
#     
#     voronoi <- deldir(df_temp$x, df_temp$y)$summary %>%
#       select(dir.area) 
#     
#     if (nrow(voronoi) == 23) {
#       
#       voronoi <- voronoi %>%
#         bind_cols(df_temp) %>%
#         filter(is_returner == 1) %>%
#         select(game_id, play_id, frame_id, is_returner, dir.area) %>%
#         rename(returner_area = dir.area)
#       
#       df_voronoi <- bind_rows(df_voronoi, voronoi)
#       
#     }
#   }
# }
# Sys.time() - start


# Get Voronoi Area ----
# 
# takes about 9 minutes
start <- Sys.time()
df_voronoi_area <- df_Main %>%
  filter(display_name != "football") %>%
  select(game_id, play_id, frame_id, nfl_id, x, y) %>%
  group_by(game_id, play_id, frame_id) %>%
  mutate(n = n()) %>%
  filter(n == 22) %>%
  filter(!(game_id %in% c(2018110407, 2019090812, 2020102200, 
                          2020120604, 2019091505, 2019100609,
                          2020112900, 2020120702) &
             play_id %in% c(3171, 200, 675, 3698, 1581, 4190,
                            4488, 604))) %>%
  mutate(voronoi = deldir(x, y)$summary[8]) 
Sys.time() - start  

df_voronoi_area$voronoi_area <- df_voronoi_area$voronoi$dir.area

df_voronoi_area <- df_voronoi_area %>%
  left_join(df_returner %>%
              select(game_id, play_id, frame_id, nfl_id) %>%
              rename(returner_id = nfl_id),
            by = c("game_id", "play_id", "frame_id")) %>%
  filter(returner_id == nfl_id) %>%
  select(game_id, play_id, frame_id, voronoi_area) 


save.image()


# EDA ---------------------------------------------------------------------

#punt land locations
table(pff$special_teams_result)

results_pct_of_total_plot <- plays %>%
  filter(special_teams_play_type == "Punt") %>%
  group_by(special_teams_result) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(., aes(reorder(special_teams_result, -pct), pct)) +
  geom_col(fill = "#992F4C", alpha = .9) +
  scale_y_continuous(labels = scales::percent_format()) +
  expand_limits(y = .5) +
  theme_bw() +
  labs(x = NULL, y = "% of Total\n", title = "Punt Play Result Share of Total Punts")
ggsave(filename = "results_pct_of_total_plot.png", 
       plot = results_pct_of_total_plot, width = 12, height = 6)


df_Main %>%
  filter(special_teams_result == "Out of Bounds", display_name == "football", 
         event == "punt_land") %>%
  View()

df_Main %>%
  filter(special_teams_result %in% c("Downed", "Touchback")) %>%
  filter(event %in% c("punt", "punt_land"), display_name == "football") %>% 
  group_by(game_id, play_id) %>%
  mutate(n = n()) %>%
  filter(n == 2, frame_id == max(frame_id)) %>%
  select(x, y, special_teams_result) %>%
  ggplot(., aes(x-10, y)) +
  theme_minimal() +
  #xlim(30, 120) +
  ylim(0,55) +
  geom_segment(aes(x = 40, xend = 40, y = 0, yend = 53.3), color = "grey") +
  geom_segment(aes(x = 50, xend = 50, y = 0, yend = 53.3), color = "grey") +
  geom_segment(aes(x = 60, xend = 60, y = 0, yend = 53.3), color = "grey") +
  geom_segment(aes(x = 70, xend = 70, y = 0, yend = 53.3), color = "grey") +
  geom_segment(aes(x = 80, xend = 80, y = 0, yend = 53.3), color = "grey") +
  geom_segment(aes(x = 90, xend = 90, y = 0, yend = 53.3), color = "grey") +
  geom_segment(aes(x = 100, xend = 100, y = 0, yend = 53.3), color = "grey") +
  annotate(geom = "text", x = 40, y = 5, label = "30") +
  annotate(geom = "text", x = 50, y = 5, label = "40") +
  annotate(geom = "text", x = 60, y = 5, label = "50") +
  annotate(geom = "text", x = 70, y = 5, label = "40") +
  annotate(geom = "text", x = 80, y = 5, label = "30") +
  annotate(geom = "text", x = 90, y = 5, label = "20") +
  annotate(geom = "text", x = 100, y = 5, label = "10") +
  geom_segment(aes(x = 30, xend = 120, y = 0, yend = 0)) +
  geom_segment(aes(x = 30, xend = 120, y = 53.3, yend = 53.3)) +
  geom_rect(aes(xmin = 110, xmax = 120, ymin = 0, ymax = 53.3)) +
  geom_point(size = 4, aes(color = special_teams_result), alpha = .8) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
         panel.grid = element_blank()) +
  scale_x_discrete(breaks = c(seq(30, 90, by = 10)), labels = c(seq(30, 90, by = 10))) +
  labs(x = NULL, y = NULL)

df_Results %>%
  mutate(x = round(football_x),
         y = round(football_y)) %>%
  # group_by(x) %>%
  # summarise(pred_touch = mean(avg_pred_touchback, na.rm = T)) %>%
  ggplot(.) +
  geom_smooth(aes(x = x, y = avg_pred_touchback, color = "Touchback")) +
  geom_smooth(aes(x = x, y = avg_pred_downed, color = "Downed")) +
  geom_smooth(aes(x = x, y = avg_pred_return, color = "Return")) +
  geom_smooth(aes(x = x, y = avg_pred_fair_catch, color = "Fair Catch")) +
  ylim(0, 1)



xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- 40#max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- 120 #min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

df_Results %>%
  mutate(x = round(football_x),
         y = round(football_y)) %>%
  ggplot() +
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
  scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = FALSE) + 
  annotate("text", x = df_hash$x[df_hash$x < 55/2], 
           y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2, 
           alpha = .3) + 
  annotate("text", x = df_hash$x[df_hash$x > 55/2], 
           y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2, 
           alpha = .3) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4, 
           alpha = .3) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4, 
           alpha = .3) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  # geom_point(data = example_play, aes(x = (xmax-y), y = x, shape = team,
  #                                     fill = team, group = nflId, size = team, colour = team), alpha = 0.7) + 
  # geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
  #           vjust = 0.36, size = 3.5) + 
  ylim(ymin, ymax) + 
  coord_fixed() +  
  theme_void() + 
  transition_time(frame.id)  +
  ease_aes('linear') + 
  NULL

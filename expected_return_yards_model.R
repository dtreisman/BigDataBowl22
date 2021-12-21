
# Final Modeling DF ----

df_RY <- df_uniquePlays %>%
  inner_join(df_Main  %>%
               inner_join(fastR, by = c("nflfastr_game_id" = "game_id", "play_id" = "play_id")) %>%
               mutate(event_group = ifelse(frame_id <=10 & event_group != "ball_snap", 
                                           "play_start", 
                                           event_group)) %>%
               select(game_id, play_id, frame_id, event_group) %>%
               unique(), 
             by = c("game_id", "play_id", "frame_id")) %>%
  inner_join(df_situation, by = c("game_id", "play_id")) %>%
  #left_join(df_pff, by = c("game_id", "play_id")) %>%
  left_join(df_football, by = c("game_id", "play_id", "frame_id")) %>%
  left_join(df_returner, by = c("game_id", "play_id", "frame_id"))  %>%
  left_join(df_voronoi_area, by = c("game_id", "play_id", "frame_id")) %>%
  left_join(df_gunners, by = c("game_id", "play_id", "frame_id")) %>%
  left_join(df_nearestDefender, by = c("game_id", "play_id", "frame_id")) %>%
  filter(!is.na(event_group)) 


df_Mod_ry <- df_RY %>%
  filter(special_teams_result == "Return")%>%
  filter(!is.na(kick_return_yardage)) %>%
  group_by(game_id, play_id)  %>%
  #filter(frame >= 10) %>%
  select(-special_teams_result, -nfl_id.x, -nfl_id.y,
         -position.x, -position.y) %>%
  filter(!event_group %in% c("play_start", "tackle", "out_of_bounds", "touchdown", "fair_catch", 
                             "punt_downed", "touchback")) %>%
  filter(event_group == "punt") %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  filter(frame_id == max(frame_id)) %>%
  select(-contains("football"), -contains("_a"), voronoi_area, -contains("_o"),
         -gunner_2_s, -gunner_1_s, -dist_to_punt_xloc, -one, -contains("gunner"), 
         -contains("s_"), s_1, s_2, def1_dist, def2_dist, -def3_dist, -def4_dist, 
         -def5_dist, -def6_dist, -def7_dist, -def8_dist, -def9_dist, -def10_dist,
         -def11_dist, -yardline_number)



colSums(is.na(train_ry))

unique(df_Mod_ry$event_group)

# modeling ----------------------------------------------------------------


train_ry <- df_Mod_ry %>%
  filter(str_sub(as.character(game_id), 1, 4) != "2020") %>%
  padr::fill_by_prevalent(kick_direction_error) %>%
  padr::fill_by_function(voronoi_area, 
                         gunner_1_x, gunner_1_y, gunner_1_s, gunner_1_a, 
                         gunner_1_o, gunner_1_dir, gunner1_def_dist, 
                         gunner1_def_xdist, gunner1_def_ydist, gunner_2_x, 
                         gunner_2_y, gunner_2_s, gunner_2_a, gunner_2_o,
                         gunner_2_dir, gunner2_def_dist, gunner2_def_xdist, 
                         gunner2_def_ydist, ret_dist_to_g1, ret_dist_to_g2, mean)
test_ry <- df_Mod_ry %>%
  filter(str_sub(as.character(game_id), 1, 4) == "2020")%>%
  padr::fill_by_prevalent(kick_direction_error) %>%
  padr::fill_by_function(voronoi_area, 
                         gunner_1_x, gunner_1_y, gunner_1_s, gunner_1_a, 
                         gunner_1_o, gunner_1_dir, gunner1_def_dist, 
                         gunner1_def_xdist, gunner1_def_ydist, gunner_2_x, 
                         gunner_2_y, gunner_2_s, gunner_2_a, gunner_2_o,
                         gunner_2_dir, gunner2_def_dist, gunner2_def_xdist, 
                         gunner2_def_ydist, ret_dist_to_g1, ret_dist_to_g2, mean)

pred_naive <- rep(mean(test_ry$kick_return_yardage), nrow(test_ry))


# linear model ------------------------------------------------------------


fit_lm <- lm(kick_return_yardage ~., data = train_ry[,c(-1, -2, -3, -4)])

summary(fit_lm)

pred_lm <- predict(fit_lm, test_ry)


# random forest -----------------------------------------------------------


tuneRF(x = train_ry[,c(-1, -2, -3, -4, -5)], y = train_ry$kick_return_yardage, trace = T,
       ntreeTry = 150)

fit_rf_ry <- randomForest(x = train_ry[,c(-1, -2, -3, -4, -5)],
                          y = train_ry$kick_return_yardage,
                          do.trace = T, ntree = 200, mtry = 2)


plot(fit_rf_ry)

pred_rf_ry <- predict(fit_rf_ry, test_ry)

RMSE(pred_naive, test_ry$kick_return_yardage)
RMSE(pred_lm, test_ry$kick_return_yardage)
RMSE(pred_rf_ry, test_ry$kick_return_yardage)

plot(density(pred_lm))

varImpPlot(fit_rf_ry)



# xgboost -------------------------------------------------------------------

train_xgb <- train_ry %>%
  ungroup() %>%
  mutate(across(where(is.character), .fns = as.factor)) %>%
  mutate(across(where(is.factor), .fns = as.numeric)) %>%
  select(-game_id, -play_id, -frame_id, -event_group, -kick_return_yardage) %>%
  as.matrix()

test_xgb <- test_ry %>%
  ungroup() %>%
  mutate(across(where(is.character), .fns = as.factor)) %>%
  mutate(across(where(is.factor), .fns = as.numeric)) %>%
  select(-game_id, -play_id, -frame_id, -event_group, -kick_return_yardage) %>%
  as.matrix()

df_Mod_xgb <- df_Mod_ry %>%
  ungroup() %>%
  mutate(across(where(is.character), .fns = as.factor)) %>%
  mutate(across(where(is.factor), .fns = as.numeric)) %>%
  select(-game_id, -play_id, -frame_id, -event_group, -kick_return_yardage) %>%
  as.matrix()


grid_tuning <- expand.grid(
  nrounds = c(100, 200, 300, 400, 500, 600),
  max_depth = c(1, 2, 5, 10, 20), 
  eta = 0.01,
  gamma = c(0, 1, 5), 
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = .75
)

train_control <- caret::trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE
)


fit_xgb_cv <- caret::train(
  x = train_xgb,
  y = train_ry$kick_return_yardage,
  trControl = train_control,
  tuneGrid = grid_tuning,
  method = "xgbTree",
  verbose = TRUE,
  maximize = F
)


# taken from somewhere on Google
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

tuneplot(fit_xgb_cv)

fit_xgb_cv$results %>%
  filter(RMSE == min(fit_xgb_cv$results$RMSE))


train_control <- caret::trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE
)

# final set of params
grid_tuned <- expand.grid(
  nrounds = 400,
  max_depth = 1,
  eta = 0.01,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# last fit
fit_xgb <- caret::train(
  x = train_xgb,
  y = train_ry$kick_return_yardage,
  trControl = train_control,
  tuneGrid = grid_tuned,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)

 
xgb.plot.importance(xgb.importance(fit_xgb$finalModel$feature_names, model = fit_xgb$finalModel))



pred_xgb <- predict(fit_xgb, test_xgb)

RMSE(pred_naive, test_ry$kick_return_yardage)
RMSE(pred_lm, test_ry$kick_return_yardage)
RMSE(pred_rf_ry, test_ry$kick_return_yardage)
RMSE(pred_xgb, test_ry$kick_return_yardage)


# Get Final Preds ----
df_RY_imp <- df_RY %>%
  padr::fill_by_prevalent(kick_direction_actual) %>%
  padr::fill_by_function(voronoi_area, 
                         gunner_1_x, gunner_1_y, gunner_1_s, gunner_1_a, 
                         gunner_1_o, gunner_1_dir, gunner1_def_dist, 
                         gunner1_def_xdist, gunner1_def_ydist, gunner_2_x, 
                         gunner_2_y, gunner_2_s, gunner_2_a, gunner_2_o,
                         gunner_2_dir, gunner2_def_dist, gunner2_def_xdist, 
                         gunner2_def_ydist, ret_dist_to_g1, ret_dist_to_g2, mean)  %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  select(-one, -contains("nfl_id"), -contains("position")) %>%
  filter()

colSums(is.na(df_RY_imp))
df_RY$pred_return_yards <- predict(fit_rf_ry, df_RY_imp[, c(-4, -5)])


pred_return <- as.data.frame(predict(fit_xgb_fc, df_RY_imp[, c(-5)], type = "prob"))

df_Pred_fc <- bind_cols(df_Mod_fc, 
                        as.data.frame(predict(fit_xgb_fc, df_Mod_xgb_fc, type = "prob"))) %>%
  select(game_id, play_id, frame_id, Downed:Touchback)


df_RY$pred_return <- pred_return$Returned
df_RY$pred_downed <- pred_return$Downed
df_RY$pred_fair_catch <- pred_return$`Fair Catch`
df_RY$pred_touchback <- pred_return$Touchback


df_RY %>%
  ggplot(.) +
  geom_density(aes(pred_return, fill = "Return"))+
  geom_density(aes(pred_downed, fill = "Downed"))+
  geom_density(aes(pred_touchback, fill = "Touchback"))+
  geom_density(aes(pred_fair_catch, fill = "Fair Catch"))


# Extras (IGNORE THIS SECTION)----
df_ResultsByPlayer %>%
  filter(special_teams_result == "Fair Catch", n > 10) %>%
  arrange(-n) %>%
  View()

df_ResultsByPlayer %>%
  group_by(nfl_id, display_name) %>%
  summarise(n = sum(n),
            ryoe = sum(return_yards_oe),
            avg_ryoe = mean(avg_return_yards_oe),
            adj_ryoe = sum(adj_ryoe),
            avg_adj_ryoe = mean(avg_adj_ryoe)) %>%
  filter(n > 10) %>%
  View()


df_RY %>%
  select(pred_return_yards, special_teams_result) %>%
  ggplot(., aes(pred_return_yards, 
                fill = special_teams_result)) +
  geom_density(alpha = .6) +
  scale_fill_viridis_d() +
  facet_wrap(~special_teams_result, ncol = 1)



unique(df_RY$kick_direction_error)


colSums(is.na(df_RY_imp))
for (i in names(df_Mod_ry)) {
  if (!(i %in% names(df_RY))) {
    print(i)
  }
}

df_Mod_ry$pred_naive <- rep(mean(train_ry$kick_return_yardage), nrow(df_Mod_ry))
df_Mod_ry$pred_lm <- predict(fit_lm, df_Mod_ry)
df_Mod_ry$pred_rf <- predict(fit_rf_ry, df_Mod_ry)
df_Mod_ry$pred_xgb <- predict(fit_xgb, df_Mod_xgb)


pred_return <- as.data.frame(predict(fit_rf_fc, df_RY_imp[, c(-4, -5)], type = "prob"))

df_RY$pred_return <- pred_return$Returned
df_RY$pred_downed <- pred_return$Downed
df_RY$pred_fair_catch <- pred_return$`Fair Catch`





df_Mod_ry %>%
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  filter(str_sub(as.character(game_id), 1, 4) == "2020") %>%
  ggplot(., aes()) +
  geom_point(aes(x = pred_xgb, y = kick_return_yardage)) +
  geom_smooth(aes(x = pred_xgb, y = kick_return_yardage), method = "lm") +
  facet_wrap(~factor(frame), scales = "free")


df_Mod_ry %>%
  # left_join(df_Main %>% 
  #             select(game_id, play_id, frame_id, event_group) %>%
  #             filter(!is.na(event_group)) %>%
  #             head())
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  filter(str_sub(as.character(game_id), 1, 4) == "2020") %>%
  left_join(df_RY %>%
              select(game_id, play_id, frame_id, pred_return), 
            by = c("game_id", "play_id", "frame_id")) %>%
  group_by(frame_id) %>%
  summarise(rmse_naive = RMSE(pred_naive*pred_return, kick_return_yardage),
            rmse_pred_lm = RMSE(pred_lm*pred_return, kick_return_yardage),
            rmse_pred_rf = RMSE(pred_rf*pred_return, kick_return_yardage),
            rmse_pred_xgb = RMSE(pred_xgb*pred_return, kick_return_yardage)) %>%
  ggplot(.) +
  theme_minimal() +
  geom_line(aes(frame_id, rmse_naive, color = "Naive")) +
  geom_line(aes(frame_id, rmse_pred_lm, color = "LM")) +
  geom_line(aes(frame_id, rmse_pred_rf, color = "RF")) +
  #geom_line(aes(frame, rmse_naive), size = 1.1, alpha = .4, color = "darkblue") +
  #geom_point(aes(frame, rmse_naive), size = 3, alpha = .8, color = "darkblue") +
  
  geom_line(aes(frame_id, rmse_pred_xgb, color = "XGB")) +
  #geom_line(aes(frame, rmse_pred), size = 1.1, alpha = .4, color = "darkblue") +
  #geom_point(aes(frame, rmse_pred), size = 3, alpha = .8, color = "darkblue") +
  geom_vline(xintercept = round(mean(df_Main$apex_frame)), lty = 8) +
  geom_text(aes(label = "Punt Apex (approx.)", 
                y = 1,
                x = round(mean(df_Main$apex_frame))), 
            angle = 90, nudge_x = -3, hjust = 0) +
  geom_vline(xintercept = round(mean(df_Main$frame_id[df_Main$event == "punt"])), lty = 8) +
  geom_text(aes(label = "Punt", 
                y = 1,
                x = round(mean(df_Main$frame_id[df_Main$event == "punt"]))), 
            angle = 90, nudge_x = -3, hjust = 0) +
  geom_vline(xintercept = round(mean(df_Main %>%
                                 filter(event_group == "punt", display_name == 'football') %>%
                                 group_by(game_id, play_id) %>%
                                 filter(frame_id == max(frame_id)) %>%
                                 ungroup() %>%
                                 select(frame_id) %>%
                                 deframe()) +1), lty = 8) +
  geom_text(aes(label = "Punt Received", 
                y = 1,
                x = round(mean(df_Main %>%
                           filter(event_group == "punt", display_name == 'football') %>%
                           group_by(game_id, play_id) %>%
                           filter(frame_id == max(frame_id)) %>%
                           ungroup() %>%
                           select(frame_id) %>%
                           deframe()) + 1)), 
            angle = 90, nudge_x = -3, hjust = 0) +
  
  scale_x_continuous(n.breaks = 30)


df_Mod %>%
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  filter(str_sub(as.character(game_id), 1, 4) == "2020") %>%
  group_by(frame) %>%
  summarise(pred = mean(pred_xgb),
            n = n()) %>%
  ggplot(., aes(frame, pred)) +
  theme_minimal() +
  geom_smooth(color = "darkred", alpha = .2) +
  geom_line(size = 1.1, alpha = .4, color = "darkblue") +
  geom_point(aes(size = n), alpha = .8, color = "darkblue") +
  scale_x_continuous(n.breaks = 20)



df_Mod_ry %>%
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  filter(str_sub(as.character(game_id), 1, 4) == "2020") %>%
  ggplot(.) +
  geom_point(aes(x = pred_xgb, y = kick_return_yardage)) +
  scale_x_continuous(n.breaks = 18)



# full fit
df_Mod_xgb_yac <- df_Mod_yac %>%
  mutate(route = as.numeric(as.factor(route))) %>%
  mutate(across(where(is.character), as.factor),
         across(where(is.factor), as.numeric)) %>%
  inner_join(completed_passes) %>%
  filter(complete == 1) %>%
  select(-gameId, -playId, -yac, -complete) %>%
  as.matrix()



df_Mod %>%
  select(game_id, play_id, frame_id, kick_return_yardage, contains("pred")) %>%
  pivot_longer(kick_return_yardage:pred_xgb) %>%
  ggplot(.,aes(x = value, fill = name)) +
  geom_density(alpha = .5)

df_situation2 <- plays %>%
  filter(special_teams_result %in% c("Return", "Muffed", "Fair Catch", "Downed")) %>%
  select(game_id, play_id, kick_return_yardage, quarter, yardline_number) %>%
  inner_join(df_Main %>% select(game_id, play_id, time_remaining) %>% unique(), 
             by = c("game_id", "play_id"))

df_Mod_full <- df_uniquePlays %>%
  left_join(df_situation2, by = c("game_id", "play_id")) %>%
  left_join(df_pff, by = c("game_id", "play_id")) %>%
  left_join(df_football, by = c("game_id", "play_id", "frame_id")) %>%
  left_join(df_returner, by = c("game_id", "play_id", "frame_id"))  %>%
  left_join(df_voronoi, by = c("game_id", "play_id", "frame_id")) %>%
  left_join(df_gunners, by = c("game_id", "play_id", "frame_id")) %>%
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  filter(frame >= 10) %>%
  select(-is_returner, -one) %>%
  padr::fill_by_value(kick_return_yardage, 0) %>%
  ungroup() 






df_yardage <- df_Mod_full %>%
  select(game_id, play_id, kick_return_yardage)

df_Mod_full <- df_Mod_full %>%
  na.omit()


players <- read_csv("players.csv") %>%
  janitor::clean_names()

colSums(is.na(df_Mod_full))
df_Mod_full$pred <- predict(fit_rf, df_Mod_full)

df_Pred <- df_Mod_full %>%  
  select(game_id, play_id, frame, contains("pred")) %>%
  filter(frame == 25) %>%
  left_join(df_Main %>%
              select(game_id, play_id, returner_id) %>%
              mutate(returner_id = as.numeric(returner_id)) %>%
              unique()) %>%
  left_join(players, by = c("returner_id" = "nfl_id")) %>%
  left_join(plays %>% select(game_id, play_id, special_teams_result)) %>%
  left_join(df_yardage)  %>%
  mutate(ryoe = kick_return_yardage - pred)  


df_Pred %>%
  group_by(returner_id, display_name, special_teams_result) %>%
  summarise(ryoe = mean(ryoe),
            n = mean(n)) %>%
  ggplot(., aes(x = display_name, y = ryoe)) +
  geom_col() +
  facet_wrap(~special_teams_result, scales = "free") +
  coord_flip()


# gunner1 speed over time ----
df_Punts2018 %>%
  filter(special_teams_result %in% c("Return", "Fair Catch"), is_gunner_1 == 1) %>%
  
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  ggplot(.) +
  geom_smooth(aes(x = frame_id, y = s, color = special_teams_result)) +
  geom_vline(aes(xintercept = mean(apex_frame))) +
  scale_x_continuous(n.breaks = 9) +
  theme_minimal()

df_Punts2018 %>%
  filter(special_teams_result %in% c("Return", "Fair Catch"), is_gunner_1 == 1) %>%
  
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  ggplot(.) +
  geom_smooth(aes(x = frame_id, y = a, color = special_teams_result)) +
  geom_vline(aes(xintercept = mean(apex_frame))) +
  scale_x_continuous(n.breaks = 30) +
  theme_minimal()





  
df_Main %>%
  filter(is_gunner_1 == 1, 
         game_id %in% plays$game_id[1:1], 
         play_id %in% plays$play_id[1:2]) %>%
  head(24) %>%
  ggplot(.) +
  geom_path(aes(x = x, y = y, color = frame_id), size = 3) +
  theme_minimal() +
  xlim(20, 80) +
  ylim (0, 75)



xgb_caret_full_yac <- caret::train(
  x = df_Mod_xgb_yac,
  y = df_Mod_yac$yac,
  trControl = train_control,
  tuneGrid = grid_tuned_yac,
  method = "xgbTree",
  verbose = TRUE,
  metric = "RMSE",
  maximize = F
)

df_Mod

xmin <- 0
xmax <- 60
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3
ymin = 00
#ymax <- min(round(max(play$x, na.rm = TRUE) + 10, -1), 120)
ymax =120
df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
df_hash <- df_hash %>% filter(y < ymax, y > ymin)

df_Mod %>%
  mutate(across(where(is.numeric), round)) %>% 
  filter(frame %in% c(1, 5, 15, 25, 40)) %>%
  ggplot(.,) +
  scale_size_continuous(range = c(2, 8), guide = FALSE) + 
  scale_shape_manual(values = c(21, 18, 21), guide = FALSE) +
  # scale_fill_manual(values = c("#013369", 
  #                             "#D50A0A",
  #                             "black"),
  #                  guide = T) + 
  #scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = TRUE) + 
  annotate("text", x = df_hash$x[df_hash$x < 55/2], 
           y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df_hash$x[df_hash$x > 55/2], 
           y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),  
           angle = 90, size = 4) + 
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") +
  ylim(ymin, ymax) + 
  coord_fixed() +
  geom_density_2d_filled(aes(x = gunner_1_y, y = gunner_1_x), alpha = .7) +
  scale_fill_viridis_d(option = "B")




library(gganimate)
animate_nfl_play <- function(play) {
  
  colors <- nflfastR::teams_colors_logos
  ## General field boundaries
  xmin <- 0
  xmax <- 53.5
  hash.right <- 38.35
  hash.left <- 12
  hash.width <- 3.3
  
  
  ## Specific boundaries for a given play
  #ymin <- max(round(min(play$x, na.rm = TRUE) - 10, -1), 0)
  ymin = 40
  #ymax <- min(round(max(play$x, na.rm = TRUE) + 10, -1), 120)
  ymax = 120
  df_hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
  df_hash <- df_hash %>% filter(!(floor(y %% 5) == 0))
  df_hash <- df_hash %>% filter(y < ymax, y > ymin)
  
  animate_play <- play %>%
    ggplot(.) + 
    scale_size_continuous(range = c(2, 8), guide = FALSE) + 
    scale_shape_manual(values = c(21, 18, 21), guide = FALSE) +
    # scale_fill_manual(values = c("#013369", 
    #                              "#D50A0A",
    #                              "black"),
    #                   guide = T) + 
    # scale_colour_manual(values = c("black", "#654321", "#c60c30"), guide = TRUE) + 
    annotate("text", x = df_hash$x[df_hash$x < 55/2], 
             y = df_hash$y[df_hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df_hash$x[df_hash$x > 55/2], 
             y = df_hash$y[df_hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "),  
             angle = 90, size = 4) + 
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") +
    geom_density_2d_filled(aes(x = football_y, y = football_x), alpha = .7) +
    scale_fill_viridis_d(option = "B") +
    ylim(ymin, ymax) + 
    coord_fixed() +  
    theme_minimal()+
    transition_time(frame) # +
    #ease_aes('linear') 
  
  ## Ensure timing of play matches 10 frames-per-second
  play.length.ex <- as.numeric(length(unique(example_play$frame)))
  return(animate(animate_play, fps = 10, nframes = play.length.ex, ))
  
}

# tackle made that saved most YAC EPA
example_play <- df_Mod %>% filter(game_id == 2018112200, play_id == 2619)
#example_play <- prepare_play_for_animation(example_play3)
a <- animate_nfl_play(example_play)
image_write(play__most_epa_saved, path="play__most_epa_saved.gif")

# play with worst "missed tackle"
#pred_full %>% filter(gameId == 2018090903, playId == 685)
example_play4 <- df_tracking %>% filter(gameId == 2018122305, playId == 3615)
example_play4 <- prepare_play_for_animation(example_play4)
play__most_epa_lost <- animate_nfl_play(example_play4)
image_write(play__most_epa_lost, path="play__most_epa_lost.gif")







df_Mod %>%
  bind_cols(predict(fit_rf, df_Mod)) %>%
  bind_cols(data.frame(rep(mean(df_Mod$kick_return_yardage), nrow(df_Mod)))) %>%
  select(game_id, play_id, frame_id, kick_return_yardage, ...23, 
         `rep.mean.df_Mod.kick_return_yardage...nrow.df_Mod..`) %>%
  mutate(resid_rf = ...23 - kick_return_yardage,
         resid_naive = `rep.mean.df_Mod.kick_return_yardage...nrow.df_Mod..` - kick_return_yardage) %>%
  ggplot() +
  geom_density(aes(x = resid_rf), fill = "red") +
  geom_density(aes(x = resid_naive), fill = "blue")

df_gunners %>%
  left_join(df_vise1, by = c("game_id", "play_id")) %>%
  left_join(df_vise2, by = c("game_id", "play_id")) %>%
  ggplot() +
  geom_point(aes(x = gunner_1_x, y = gunner_1_y, color = "gunner_1"), size = 3) +
  geom_point(aes(x = gunner_2_x, y = gunner_2_y, color = "gunner_2"), size = 3) +
  geom_point(aes(x = vise_1_x, y = vise_1_y, color = "vise_1"), size = 3) +
  geom_point(aes(x = vise_2_x, y = vise_2_y, color = "vise_2"), size = 3) +
  xlim(0, 100) +
  ylim(0, 50)




plot_play(df_Punts %>%
  filter(game_id == 2018123000, play_id == 892), frame = 54, animated = F)




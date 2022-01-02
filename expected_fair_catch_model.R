df_Mod_fc <- df_Main %>%
  group_by(game_id, play_id) %>%
  mutate(is_last_frame = ifelse(frame_id == max(frame_id), 1, 0)) %>%
  ungroup() %>%
  filter(is_last_frame == 1)

df_Mod_fc %>%
  select()

df_result <- plays %>%
  filter(special_teams_result %in% c("Return", "Muffed", "Fair Catch", "Downed", "Touchback")) %>%
  select(game_id, play_id, special_teams_result)#, quarter, yardline_number) %>%
  # inner_join(df_Main %>% select(game_id, play_id, time_remaining) %>% unique(), 
  #            by = c("game_id", "play_id"))



df_Mod_fc_plays <- df_Main %>%
  #group_by(game_id, play_id) %>%
  #mutate(is_last_frame = ifelse(frame_id == max(frame_id), 1, 0)) %>%
  #ungroup() %>%
  #filter(is_last_frame == 1) %>%
  filter(event_group == "punt") %>%
  select(game_id, play_id, frame_id) %>%
  unique()

df_Mod_fc <- df_Mod_fc_plays %>%
  left_join(df_result, by = c("game_id", "play_id")) %>%
  #left_join(df_pff, by = c("game_id", "play_id")) %>%
  #left_join(df_football, by = c("game_id", "play_id", "frame_id")) %>%
  left_join(df_returner, by = c("game_id", "play_id", "frame_id"))  %>%
  left_join(df_voronoi_area, by = c("game_id", "play_id", "frame_id")) %>%
  left_join(df_gunners, by = c("game_id", "play_id", "frame_id")) %>%
  left_join(df_nearestDefender, by = c("game_id", "play_id", "frame_id")) %>%
  select(-contains("nfl_id"), -contains("position")) %>%
  filter(!is.na(special_teams_result)) %>%
  mutate(special_teams_result = case_when(
    special_teams_result == "Fair Catch" ~ "Fair Catch",
    special_teams_result == "Downed" ~ "Downed",
    special_teams_result == "Touchback" ~ "Touchback",
    special_teams_result %in% c("Muffed", "Return") ~ "Returned"
  )) %>%
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  filter(frame <=57) %>%
  select(-one, -contains("gunner")) %>%
  filter(frame <= max(frame) - 10)


table(df_Mod_fc$frame)

plays %>%
  left_join(pff) %>%
  filter(special_teams_play_type == "Punt") %>%
  View()


colSums(is.na(df_Mod_fc))

write.csv(df_Mod_fc, "xFC DF Mod.csv", row.names = F)
# test <- df_Main %>%
#   filter(display_name != "football", game_id == 2018110407, play_id == 3171, frame_id ==36) #%>%
# nrow(deldir(test$x, test$y)$summary[8])
#   #mutate(voronoi = deldir(x, y)$summary[8]) %>%
#   #View()


colSums(is.na(df_Mod_fc))

# imp = mice::mice(df_Mod_fc, m = 1, method = "cart", maxit = 3)
# df_Mod_fc <- complete(imp) %>%
#   mutate(special_teams_result = case_when(
#     special_teams_result == "Fair Catch" ~ "Fair Catch",
#     special_teams_result == "Downed" ~ "Downed",
#     special_teams_result %in% c("Muffed", "Return") ~ "Returned"
#     
#   ))

table(df_Mod_fc$special_teams_result)

table(df_Mod_fc$frame)

df_Mod_fc %>%
  filter(is.na(special_teams_result)) %>%
  View()



# modeling ----------------------------------------------------------------


train_fc <- df_Mod_fc %>%
  filter(str_sub(as.character(game_id), 1, 4) != "2020") %>%
  padr::fill_by_function(football_x, football_y, football_s, football_a, 
                         returner_x, returner_y, returner_s, returner_a, 
                         returner_o, returner_dir, nfl_id, voronoi_area, 
                         gunner_1_x, gunner_1_y, gunner_1_s, gunner_1_a, 
                         gunner_1_o, gunner_1_dir, gunner1_def_dist, 
                         gunner1_def_xdist, gunner1_def_ydist, gunner_2_x, 
                         gunner_2_y, gunner_2_s, gunner_2_a, gunner_2_o,
                         gunner_2_dir, gunner2_def_dist, gunner2_def_xdist, 
                         gunner2_def_ydist, ret_dist_to_g1, ret_dist_to_g2, mean)

test_fc <- df_Mod_fc %>%
  filter(str_sub(as.character(game_id), 1, 4) == "2020") %>%
  padr::fill_by_function(football_x, football_y, football_s, football_a, 
                         returner_x, returner_y, returner_s, returner_a, 
                         returner_o, returner_dir, nfl_id, voronoi_area, 
                         gunner_1_x, gunner_1_y, gunner_1_s, gunner_1_a, 
                         gunner_1_o, gunner_1_dir, gunner1_def_dist, 
                         gunner1_def_xdist, gunner1_def_ydist, gunner_2_x, 
                         gunner_2_y, gunner_2_s, gunner_2_a, gunner_2_o,
                         gunner_2_dir, gunner2_def_dist, gunner2_def_xdist, 
                         gunner2_def_ydist, ret_dist_to_g1, ret_dist_to_g2, mean)

pred_naive_fc <- bind_cols(rep(sum(df_Mod_fc$special_teams_result == "Fair Catch")/nrow(df_Mod_fc), nrow(test_fc)),
                           rep(sum(df_Mod_fc$special_teams_result == "Downed")/nrow(df_Mod_fc), nrow(test_fc)),
                           rep(sum(df_Mod_fc$special_teams_result == "Returned")/nrow(df_Mod_fc), nrow(test_fc)),
                           rep(sum(df_Mod_fc$special_teams_result == "Touchback")/nrow(df_Mod_fc), nrow(test_fc)))
names(pred_naive_fc) <- c("FairCatch", "Downed", "Returned", "Touchback")



# RF ----------------------------------------------------------------------


tuneRF(x = train_fc[, c(-1, -2, -3, -4)],
       y = as.factor(train_fc$special_teams_result))

fit_rf_fc <- randomForest(x = train_fc[, c(-1, -2, -3, -4)],
                          y = as.factor(train_fc$special_teams_result),
                          do.trace = T, ntree = 100, mtry = 8)

plot(fit_rf_fc)
fit_rf_fc$confusion

varImpPlot(fit_rf_fc)

save.image()


pred_rf_fc <- predict(fit_rf_fc, test_fc, type = "prob")
ModelMetrics::mlogLoss(test_fc$special_teams_result, as.matrix(pred_naive_fc))
ModelMetrics::mlogLoss(test_fc$special_teams_result, as.matrix(pred_rf_fc))

table(test_fc$special_teams_result, predict(fit_rf_fc, test_fc, type = "response")) %>%
  as.data.frame() %>%
  gt::gt()

df_Mod_fc_ <-  df_Mod_fc %>%
  padr::fill_by_function(football_x, football_y, football_s, football_a, 
                         returner_x, returner_y, returner_s, returner_a, 
                         returner_o, returner_dir, nfl_id, voronoi_area, 
                         gunner_1_x, gunner_1_y, gunner_1_s, gunner_1_a, 
                         gunner_1_o, gunner_1_dir, gunner1_def_dist, 
                         gunner1_def_xdist, gunner1_def_ydist, gunner_2_x, 
                         gunner_2_y, gunner_2_s, gunner_2_a, gunner_2_o,
                         gunner_2_dir, gunner2_def_dist, gunner2_def_xdist, 
                         gunner2_def_ydist, ret_dist_to_g1, ret_dist_to_g2, mean)
  

fit_rf_fc_final <- randomForest(x = df_Mod_fc_[, c(-1, -2, -3, -4)],
                                y = as.factor(df_Mod_fc_$special_teams_result),
                                do.trace = T, ntree = 100, mtry = 8)


pred_rf_fc_final <- as.data.frame(predict(fit_rf_fc_final, df_Mod_fc_, type = "prob"))



# xgboost -------------------------------------------------------------------

train_xgb_fc <- train_fc %>%
  ungroup() %>%
  mutate(across(where(is.character), .fns = as.factor)) %>%
  mutate(across(where(is.factor), .fns = as.numeric)) %>%
  select(-game_id, -play_id, -frame_id, -special_teams_result) %>%
  as.matrix()

test_xgb_fc <- test_fc %>%
  ungroup() %>%
  mutate(across(where(is.character), .fns = as.factor)) %>%
  mutate(across(where(is.factor), .fns = as.numeric)) %>%
  select(-game_id, -play_id, -frame_id, -special_teams_result) %>%
  as.matrix()

df_Mod_xgb_fc <- df_Mod_fc %>%
  ungroup() %>%
  mutate(across(where(is.character), .fns = as.factor)) %>%
  mutate(across(where(is.factor), .fns = as.numeric)) %>%
  select(-game_id, -play_id, -frame_id, -special_teams_result) %>%
  as.matrix()


grid_tuning <- expand.grid(
  nrounds = c(100, 120, 200, 300, 400, 500, 600),
  max_depth = c(20, 40, 60, 100),
  eta = 0.1,
  gamma = 0, 
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE,
  classProbs = T,
  summaryFunction = mnLogLoss
)


fit_xgb_cv <- caret::train(
  x = train_xgb_fc,
  y = as.factor(make.names(train_fc$special_teams_result)),
  trControl = train_control,
  tuneGrid = grid_tuning,
  method = "xgbTree",
  metric = "logLoss",
  verbose = TRUE,
  maximize = F
)

?caret::train()
# taken from somewhere on Google
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$logLoss, probs = probs), min(x$results$logLoss))) +
    theme_bw()
}

tuneplot(fit_xgb_cv)

fit_xgb_cv$results %>%
  filter(logLoss == max(fit_xgb_cv$results$logLoss))


train_control <- caret::trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE, # no training log
  allowParallel = TRUE,
  classProbs = T,
  summaryFunction = mnLogLoss
)

# final set of params
grid_tuned <- expand.grid(
  nrounds = 400,
  max_depth = 20,
  eta = 0.01,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# last fit
fit_xgb_fc <- caret::train(
  x = train_xgb_fc,
  y = as.factor(make.names(train_fc$special_teams_result)),
  trControl = train_control,
  tuneGrid = grid_tuned,
  method = "xgbTree",
  verbose = TRUE,
  metric = "logLoss",
  maximize = T
)

fit_xgb_fc


params = list(
  nrounds = c(50, 150, 250),
  max_depth = c(10, 20, 30),
  eta = 0.01,
  objective = c(),
  gamma = 0, 
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = .75,
  objective = "multi:softprob",
  num_class = 4
)


xgb_cv <- xgb.cv(data = train_xgb_fc, label = as.numeric(as.factor(train_fc$special_teams_result))-1, 
       params = params, nfold = 5, metrics = "mlogloss", verbose = T, maximize = F,
       nrounds = 100, early_stopping_rounds = 10)
print(xgb_cv)

xgb_fc_imp <- xgb.importance(fit_xgb_fc$finalModel$feature_names, model = fit_xgb_fc$finalModel)

eFC_imp_plot <- tibble(xgb_fc_imp) %>%
  mutate(Feature = case_when(
    Feature == "returner_x" ~ "Returner X Coordinate",
    Feature == "dist_to_punt_xloc" ~ "Returner Dist. to Expected Punt Land Loc.",
    Feature == "x_2" ~ "2nd Nearest Defender X Location",
    Feature == "y_1" ~ "Nearest Defender Y Coordinate",
    Feature == "y_2" ~ "2nd Nearest Defender Y Coordinate",
    Feature == "returner_y" ~ "Returner Y Location",
    Feature == "returner_dir" ~ "Returner Direction",
    Feature == "returner_o" ~ "Returner Orientation",
    Feature == "returner_s" ~ "Returner Speed",
    Feature == "s_2" ~ "2nd Defender Speed",
    Feature == "s_1" ~ "Nearest Defender Speed",
    Feature == "x_1" ~ "Nearest Defender X Location",
    Feature == "voronoi_area" ~ "Returner Voronoi Area",
    Feature == "returner_a" ~ "Returner Acceleration",
    Feature == "frame" ~ "Frames Since Kick",
    Feature == "def1_dist" ~ "Nearest Defender Dist. to Returner",
    Feature == "def2_dist" ~ "2nd Nearest Defender Dist. to Returner"
  )) %>%
  na.omit() %>%
  ggplot(., aes(reorder(Feature, Gain), Gain)) +
  geom_col(fill = pal[1], alpha = .9) +
  coord_flip() +
  theme_bw() +
  labs(x = NULL,
       title = "Feature Importance for Return Decision XGBoost Classifier")

ggsave(eFC_imp_plot, filename = "eFC_imp_plot.png", width = 10, height = 4)

pred_xgb_fc <- predict(fit_xgb_fc, test_xgb_fc)
pred_xgb_fc_probs <- predict(fit_xgb_fc, test_xgb_fc, type = "prob")


as.matrix(pred_xgb_fc)

ModelMetrics::mlogLoss(test_fc$special_teams_result, as.matrix(pred_naive_fc))
ModelMetrics::mlogLoss(test_fc$special_teams_result, as.matrix(pred_rf_fc))
ModelMetrics::mlogLoss(test_fc$special_teams_result, as.matrix(pred_xgb_fc_probs))




# preds -------------------------------------------------------------------



df_xFC <- df_Mod_fc %>%
  select(game_id, play_id, frame_id, special_teams_result) %>%
  bind_cols(pred_xgb_fc_probs)

write.csv(df_xFC, "Expected Fair Catch Preds.csv", row.names = F)
write_rds(fit_xgb_fc, "Expected Fair Catch Model.rds")

df_xFC %>% 
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  pivot_longer(Downed:Touchback) %>%
  group_by(frame, name) %>%
  summarise(prob = mean(value)) %>%
  #filter(special_teams_result == "Fair Catch") %>%
  #group_by(frame) %>%
  #summarize(fc_prob = mean()) %>%
  ggplot(., aes(frame*.1, prob, color = name)) +
  geom_smooth(size = 1.2) +
  theme_minimal() +
  scale_x_continuous(n.breaks = 20) +
  scale_y_continuous(n.breaks = 10, limits = c(0, 1), 
                     labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Seconds Since the Punt", 
       y = "Probability of a Fair Catch",
       title = "Probability of Each Punt Result while Ball is in the Air",
       subtitle = "Probabilities are Calculated Using Frame-Level Tracking Data",
       color = "Punt Result") +
  scale_color_manual(values = c("darkred", "darkblue", "darkgreen", "orange")) 



df_xFC %>% 
  group_by(game_id, play_id) %>%
  mutate(one = 1,
         frame = cumsum(one)) %>%
  pivot_longer(Downed:Returned) %>%
  #group_by(frame, name) %>%
  #summarise(prob = mean(value)) %>%
  #filter(special_teams_result == "Fair Catch") %>%
  #group_by(frame) %>%
  #summarize(fc_prob = mean()) %>%
  ggplot(., aes(value, fill = name)) +
  geom_density(alpha = .7) +
  theme_minimal() +
  labs(x = "Probability", 
       title = "Probability of Each Punt Result while Ball is in the Air",
       subtitle = "Probabilities are Calculated Using Frame-Level Tracking Data",
       color = "Punt Result") +
  scale_x_continuous(labels = scales::percent_format()) + 
  scale_fill_manual(values = c("darkred", "darkblue", "darkgreen")) 

  
  
df_xFC %>%
  pivot_longer(Downed:Touchback) %>%
  group_by(game_id, play_id, name) %>%
  summarise(pred = mean(value)) %>%
  pivot_wider(names_from = name, values_from = pred)

library(useful)
library(REdaS)

df_xPuntLoc <- df_Main %>%
  select(game_id, play_id, frame_id, event_group, display_name, x, y, s) %>%
  filter(event_group == "punt") %>%
  left_join(pff %>%
              select(game_id, play_id, snap_time, kick_type, kick_direction_actual),
            by = c("game_id", "play_id")) %>%
  left_join(plays %>%
              select(game_id, play_id, yardline_number, quarter, kick_length)) %>%
  filter(display_name == "football") %>%
  group_by(game_id, play_id) %>%
  filter(frame_id == min(frame_id) | frame_id == min(frame_id)+5) %>%
  mutate(first_frame = ifelse(frame_id == min(frame_id), 1, 0),
         second_frame = ifelse(frame_id == min(frame_id)+5, 1, 0),
         lead_x = lead(x),
         lead_y = lead(y),
         punt_angle = atan(
           (lead_y-y)/(lead_x-x)
         )*180/3.14) 


df_Mod_puntloc <- df_xPuntLoc %>%
  filter(first_frame == 1) %>%
  select(game_id, play_id, x, y, s, punt_angle, yardline_number, snap_time, kick_type, 
         kick_direction_actual) %>%
  padr::fill_by_prevalent(kick_type) %>%
  padr::fill_by_prevalent(kick_direction_actual) %>%
  padr::fill_by_function(punt_angle, mean)

colSums(is.na(df_Mod_puntloc))

train_pl <- df_Mod_puntloc %>%
  filter(str_sub(as.character(game_id), 1, 4) != "2020")
test_pl <- df_Mod_puntloc %>%
  filter(str_sub(as.character(game_id), 1, 4) == "2020")

#pred_naive <- rep(mean(train_pl$kick_return_yardage), nrow(test_pl))

fit_rf_plx <- randomForest(x = train_pl[, c(-1,-2, -3, -4)], y = train_pl$x,
                          do.trace = T, ntree = 150)

plot(fit_rf_plx)

pred_rf_plx <- predict(fit_rf_plx, test_pl)

plot(pred_rf_plx, test_pl$x)


fit_rf_ply <- randomForest(x = train_pl[, c(-1,-2, -3, -4)], y = train_pl$y,
                          do.trace = T, ntree = 150)

plot(fit_rf_ply)
pred_rf_ply <- predict(fit_rf_ply, test_pl)

plot(pred_rf_ply, test_pl$y)


df_Mod_puntloc$pred_x <- predict(fit_rf_plx, df_Mod_puntloc)
df_Mod_puntloc$pred_y <- predict(fit_rf_ply, df_Mod_puntloc)

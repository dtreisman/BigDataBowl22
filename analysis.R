library(gt)
library(gtExtras)
library(ggridges)

df_RY <- df_RY %>%
  left_join(df_Pred_fc) %>%
  rename(pred_return = Returned,
         pred_downed = Downed, 
         pred_fair_catch = Fair.Catch,
         pred_touchback = Touchback) %>%
  left_join(teams)


df_puntBounces <- df_Main %>%
  filter(special_teams_result %in% c("Downed", "Touchback")) %>%
  filter(event %in% c("punt_land", "punt_downed", "touchback"), display_name == "football") %>% 
  group_by(game_id, play_id) %>%
  mutate(n = n()) %>%
  filter(n == 2) %>%
  select(game_id, play_id, event, x) %>%
  pivot_wider(id_cols = game_id:play_id, values_from = x, names_from = event, names_prefix = "x_") %>%
  mutate(punt_bounce_dist = x_punt_downed - x_punt_land) %>%
  ungroup() %>%
  select(game_id, play_id, punt_bounce_dist, x_punt_land)

# Results Aggreagtions ----

df_Results <- df_RY %>%
filter(event_group == "punt") %>%
  group_by(game_id, play_id) %>%
  mutate(avg_pred_return = mean(pred_return, na.rm = T),
         avg_pred_fair_catch = mean(pred_fair_catch, na.rm = T),
         avg_pred_downed = mean(pred_downed, na.rm = T),
         avg_pred_touchback = mean(pred_touchback, na.rm = T)) %>%
  filter(frame_id == max(frame_id)) %>%
  padr::fill_by_value(kick_return_yardage, 0) %>%
  left_join(df_puntBounces) %>%
  filter(x_punt_land < 110 | is.na(x_punt_land)) %>%
  mutate(kick_return_yardage = ifelse(special_teams_result == "Downed", 
                                      0 - punt_bounce_dist,
                                      kick_return_yardage),
         kick_return_yardage = ifelse(special_teams_result == "Touchback", 
                                      20 - round(110-x_punt_land),
                                      kick_return_yardage)) %>%
  select(-nfl_id.y) %>%
  rename(nfl_id = nfl_id.x) %>%
  mutate(is_return = ifelse(special_teams_result == "Return", 1, 0),
         is_fair_catch = ifelse(special_teams_result == "Fair Catch", 1, 0),
         is_downed = ifelse(special_teams_result == "Downed", 1, 0),
         is_touchback = ifelse(special_teams_result == "Touchback", 1, 0),
         returns_oe = is_return - avg_pred_return,
         fair_catches_oe = is_fair_catch - avg_pred_fair_catch,
         downed_oe = is_downed - avg_pred_downed,
         touchbacks_oe = is_touchback - avg_pred_touchback,
         return_yards_oe = round(kick_return_yardage - pred_return_yards, 1),
         adj_ryoe = round(kick_return_yardage - pred_return_yards*avg_pred_return, 1),
         w_adj_ryoe = case_when(
           special_teams_result == "Return" ~ adj_ryoe * returns_oe,
           special_teams_result == "Downed" ~ adj_ryoe * downed_oe,
           special_teams_result == "Touchback" ~ adj_ryoe * touchbacks_oe,
           special_teams_result == "Fair Catch" ~ adj_ryoe * fair_catches_oe
         ),
         avg_return_yards_oe_RETURN = ifelse(special_teams_result == "Return",  mean(return_yards_oe, na.rm = T), NA)
         ) %>%
  left_join(kickers)

df_ResultsByPlayer <- df_Results %>%
  group_by(nfl_id) %>%
  summarise(n = n(),
            avg_return_yards_oe = mean(return_yards_oe, na.rm = T),
            return_yards_oe = sum(return_yards_oe, na.rm = T),
            avg_adj_ryoe = mean(adj_ryoe, na.rm = T),
            adj_ryoe = sum(adj_ryoe, na.rm = T),
            avg_return_yards_oe_RETURN = mean(avg_return_yards_oe_RETURN, na.rm = T),
            avg_w_adj_ryoe = mean(w_adj_ryoe, na.rm = T),
            w_adj_ryoe = sum(w_adj_ryoe, na.rm = T),
            xReturns = sum(avg_pred_return, na.rm = T),
            xFairCatches = sum(avg_pred_fair_catch, na.rm = T),
            xDowned = sum(avg_pred_downed, na.rm = T),
            xTouchbacks = sum(avg_pred_touchback, na.rm = T),
            returns_oe = sum(returns_oe, na.rm = T),
            fair_catches_oe = sum(fair_catches_oe, na.rm = T),
            downed_oe = sum(downed_oe, na.rm = T),
            touchbacks_oe = sum(touchbacks_oe, na.rm = T)) %>%
  ungroup() %>%
  left_join(players, by = "nfl_id")

df_ResultsByPlayer_Result <- df_Results %>%
  group_by(special_teams_result, nfl_id) %>%
  summarise(n = n(),
            avg_return_yards_oe = mean(return_yards_oe, na.rm = T),
            return_yards_oe = sum(return_yards_oe, na.rm = T),
            avg_adj_ryoe = mean(adj_ryoe, na.rm = T),
            adj_ryoe = sum(adj_ryoe, na.rm = T),
            avg_w_adj_ryoe = mean(w_adj_ryoe, na.rm = T),
            w_adj_ryoe = sum(w_adj_ryoe, na.rm = T),
            xReturns = sum(avg_pred_return, na.rm = T),
            xFairCatches = sum(avg_pred_fair_catch, na.rm = T),
            xDowned = sum(avg_pred_downed, na.rm = T),
            xTouchbacks = sum(avg_pred_touchback, na.rm = T),
            returns_oe = sum(returns_oe, na.rm = T),
            fair_catches_oe = sum(fair_catches_oe, na.rm = T),
            downed_oe = sum(downed_oe, na.rm = T),
            touchbacks_oe = sum(touchbacks_oe, na.rm = T)) %>%
  ungroup() %>%
  left_join(players, by = "nfl_id")


df_ResultsByReturnTeam <- df_Results %>%
  left_join(teams) %>%
  group_by(return_team) %>%
  summarise(n = n(),
            avg_return_yards_oe = mean(return_yards_oe, na.rm = T),
            return_yards_oe = sum(return_yards_oe, na.rm = T),
            avg_adj_ryoe = mean(adj_ryoe, na.rm = T),
            adj_ryoe = sum(adj_ryoe, na.rm = T),
            avg_w_adj_ryoe = mean(w_adj_ryoe, na.rm = T),
            w_adj_ryoe = sum(w_adj_ryoe, na.rm = T),
            xReturns = sum(avg_pred_return, na.rm = T),
            xFairCatches = sum(avg_pred_fair_catch, na.rm = T),
            xDowned = sum(avg_pred_downed, na.rm = T),
            xTouchbacks = sum(avg_pred_touchback, na.rm = T),
            returns_oe = sum(returns_oe, na.rm = T),
            fair_catches_oe = sum(fair_catches_oe, na.rm = T),
            downed_oe = sum(downed_oe, na.rm = T),
            touchbacks_oe = sum(touchbacks_oe, na.rm = T)) %>%
  ungroup()

df_ResultsByKickingTeam <- df_Results %>%
  left_join(teams) %>%
  group_by(kicking_team)%>%
  summarise(n = n(),
            avg_return_yards_oe = mean(return_yards_oe, na.rm = T),
            return_yards_oe = sum(return_yards_oe, na.rm = T),
            avg_adj_ryoe = mean(adj_ryoe, na.rm = T),
            adj_ryoe = sum(adj_ryoe, na.rm = T),
            avg_w_adj_ryoe = mean(w_adj_ryoe, na.rm = T),
            w_adj_ryoe = sum(w_adj_ryoe, na.rm = T),
            xReturns = sum(avg_pred_return, na.rm = T),
            xFairCatches = sum(avg_pred_fair_catch, na.rm = T),
            xDowned = sum(avg_pred_downed, na.rm = T),
            xTouchbacks = sum(avg_pred_touchback, na.rm = T),
            returns_oe = sum(returns_oe, na.rm = T),
            fair_catches_oe = sum(fair_catches_oe, na.rm = T),
            downed_oe = sum(downed_oe, na.rm = T),
            touchbacks_oe = sum(touchbacks_oe, na.rm = T)) %>%
  ungroup()

# Plots ----

# custom BigDataBowl palette based on BDB colors
pal <- c("#992F4C", "#2A40AC", "#24354C", "#BF8641", "#BF3636", "darkred")

player_values_table <- df_ResultsByPlayer %>%
  filter(n > 30) %>%
  select(display_name, n, avg_return_yards_oe_RETURN, avg_adj_ryoe, avg_w_adj_ryoe) %>%
  mutate(across(where(is.numeric), round, 2)) %>%
  arrange(-avg_w_adj_ryoe) %>%
  gt() %>%
  tab_header(title = "NFL Punt Returner Value and Skill Measures", subtitle = "2018-2020") %>%
  # data_color(
  #   columns = c(avg_return_yards_oe_RETURN, avg_adj_ryoe, avg_w_adj_ryoe),
  #   colors = c("#992F4C", "white", "#24354C")
  # )
  gt_hulk_col_numeric(columns = avg_return_yards_oe_RETURN:avg_w_adj_ryoe)  %>%
  cols_label(
    display_name = "Returner",
    n = "N",
    avg_return_yards_oe_RETURN = "RYOE",
    avg_adj_ryoe = html("RYOE<sub>adj</sub>"),
    avg_w_adj_ryoe = "wRYOE"
  ) %>%
  tab_footnote(
    footnote = "RYOE on returns only",
    locations = cells_column_labels(
      columns = avg_return_yards_oe_RETURN
    )
  ) %>%
  tab_footnote(
    footnote = "Adj. RYOE on all returns, fair catches, downed punts, and touchbacks ",
    locations = cells_column_labels(
      columns = avg_adj_ryoe
    )
  ) %>%
  tab_footnote(
    footnote = "Weighted RYOE, a proxy for represnting the value of a punt play decision",
    locations = cells_column_labels(
      columns = avg_w_adj_ryoe
    )
  ) %>%
  tab_source_note(md("Minimum of 30 plays.<br>All values are *per play*.")) %>%
  tab_style(
    style = list(
      cell_text( weight = "bold")
    ),
    locations = list(
      cells_column_labels(everything())
    )
  ) 
  
gtsave(data = player_values_table, filename = "player_values_table.png")


adj_ryoe_dist_plot <- df_Results %>%
  select(special_teams_result, return_yards_oe, adj_ryoe) %>%
  pivot_longer(cols = c(return_yards_oe, adj_ryoe)) %>%
  mutate(name = fct_rev(name)) %>%
  ggplot(., aes(x = value, y = special_teams_result, fill = name)) +
  geom_density_ridges(alpha = .7, scale = 1.2) +
  geom_vline(xintercept = 0, alpha = .5, lty = 2) +
  #scale_fill_viridis_d()+
  theme_bw() +
  labs(title = "Distribution of Return Yards Over Expected",
       subtitle =  "Downed punts remain a poor choice, touchbacks are worthwhile when possible",
       x = "",
       y = "density\n",
       fill = "Punt Result") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal") +
  scale_x_continuous(limits = c(-35, 35), n.breaks = 8) +
  scale_fill_manual(values = c("#24354C", "#992F4C"), labels = c("RYOE", "Adj. RYOE")) 
ggsave(filename = "adj_ryoe_dist_plot.png", plot = adj_ryoe_dist_plot, width = 10, height = 6)


ryoe_dist_plot <- df_Results %>%
  select(special_teams_result, return_yards_oe) %>%
  ggplot(., aes(x = return_yards_oe, y = special_teams_result)) +
  geom_density_ridges(alpha = .7, fill = "#24354C", scale = 1.2) +
  geom_vline(xintercept = 0, alpha = .5, lty = 2) +
  #scale_fill_viridis_d()+
  theme_bw() +
  labs(title = "Distribution of Return Yards Over Expected",
       subtitle =  "Downed punts and fair catches appear to be poor choices",
       x = "\nReturn Yards Over Expected\n\n",
       y = "density\n",
       fill = "Punt Result") +
  scale_x_continuous(limits = c(-35, 35), n.breaks = 8) 
ggsave(filename = "ryoe_dist_plot.png", plot = ryoe_dist_plot, width = 10, height = 6)


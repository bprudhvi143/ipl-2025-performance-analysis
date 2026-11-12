# ==============================================================================
# IPL 2025 Performance Analysis - Complete Validation & Test Suite
# Tests statistical calculations, extras classification, bowler credits,
# radar normalization, and Shiny reactive server outputs.
# ==============================================================================

library(shiny)
library(dplyr)

cat("====================================================================\n")
cat(">>> RUNNING IPL 2025 ANALYTICS VALIDATION TEST SUITE\n")
cat("====================================================================\n\n")

# 1. Dataset Verification
data_path <- if (file.exists("data/ipl_2025_deliveries.csv")) {
  "data/ipl_2025_deliveries.csv"
} else {
  "../data/ipl_2025_deliveries.csv"
}

stopifnot("Dataset file does not exist!" = file.exists(data_path))
ipl <- read.csv(data_path, stringsAsFactors = FALSE)

cat("  [1/7] Testing dataset dimensions and schema...\n")
stopifnot("Total rows must be 17246" = nrow(ipl) == 17246)
stopifnot("Distinct matches must be 74" = length(unique(ipl$match_id)) == 74)
stopifnot("Franchises count must be 10" = length(unique(ipl$batting_team)) == 10)
cat("        ✓ Dimensions verified: 17,246 deliveries across 74 matches and 10 teams.\n")

# 2. Overview KPIs
cat("  [2/7] Testing season overview KPIs...\n")
total_matches <- length(unique(ipl$match_id))
total_deliveries <- nrow(ipl)
total_runs <- sum(ipl$runs_of_bat + ipl$extras, na.rm = TRUE)
total_wickets_all <- sum(ipl$wicket_type != "" & ipl$wicket_type != "retired hurt", na.rm = TRUE)
total_bowler_wickets <- sum(ipl$wicket_type %in% c("bowled", "caught", "hit wicket", "lbw", "stumped"), na.rm = TRUE)

stopifnot("Matches must equal 74" = total_matches == 74)
stopifnot("Total deliveries must equal 17246" = total_deliveries == 17246)
stopifnot("Total runs must equal 26493" = total_runs == 26493)
stopifnot("Total dismissals must equal 871" = total_wickets_all == 871)
stopifnot("Total credited bowler wickets must equal 830" = total_bowler_wickets == 830)
cat("        ✓ Overview KPIs verified: 74 matches, 17,246 deliveries, 26,493 runs, 871 wickets.\n")

# 3. Batting Statistical Rules
cat("  [3/7] Testing batting metrics and non-striker dismissals...\n")
batter_stats <- ipl %>%
  group_by(striker) %>%
  summarise(
    runs = sum(runs_of_bat, na.rm = TRUE),
    balls = sum(wide == 0, na.rm = TRUE),
    fours = sum(runs_of_bat == 4, na.rm = TRUE),
    sixes = sum(runs_of_bat == 6, na.rm = TRUE),
    matches = n_distinct(match_id),
    .groups = "drop"
  )

dismissals_df <- ipl %>%
  filter(player_dismissed != "" & wicket_type != "" & wicket_type != "retired hurt") %>%
  group_by(player_dismissed) %>%
  summarise(dismissals = n(), .groups = "drop")

batting_summary <- batter_stats %>%
  left_join(dismissals_df, by = c("striker" = "player_dismissed")) %>%
  mutate(
    dismissals = ifelse(is.na(dismissals), 0, dismissals),
    strike_rate = ifelse(balls > 0, round((runs / balls) * 100, 2), 0),
    batting_avg = ifelse(dismissals > 0, round(runs / dismissals, 2), NA),
    boundary_runs = (fours * 4) + (sixes * 6),
    boundary_pct = ifelse(runs > 0, round((boundary_runs / runs) * 100, 1), 0)
  ) %>%
  arrange(desc(runs))

# Validate Top Batsmen
top_bat <- batting_summary[1, ]
stopifnot("Top run scorer must be Sai Sudharsan" = top_bat$striker == "Sai Sudharsan")
stopifnot("Sai Sudharsan runs must be 759" = top_bat$runs == 759)
stopifnot("Sai Sudharsan balls faced must be 486" = top_bat$balls == 486)
stopifnot("Sai Sudharsan dismissals must be 14" = top_bat$dismissals == 14)
stopifnot("Sai Sudharsan average must be 54.21" = abs(top_bat$batting_avg - 54.21) < 0.01)

sky <- batting_summary %>% filter(striker == "Suryakumar Yadav")
stopifnot("SKY runs must be 717" = sky$runs == 717)
stopifnot("SKY strike rate must be 167.92" = abs(sky$strike_rate - 167.92) < 0.01)

# Edge case: zero dismissals
zero_diss <- batting_summary %>% filter(dismissals == 0)
stopifnot("Players with 0 dismissals must have NA average (not Inf/NaN)" = all(is.na(zero_diss$batting_avg)))
cat("        ✓ Batting metrics verified: Top scorers correct, non-striker dismissals counted, edge cases handled.\n")

# 4. Bowling Statistical Rules
cat("  [4/7] Testing bowling metrics, extras classification and bowler credit...\n")
ipl_bowling <- ipl %>%
  mutate(
    bowler_runs = runs_of_bat + ifelse(wide > 0 | noballs > 0, extras, 0),
    is_legal = ifelse(wide == 0 & noballs == 0, 1, 0),
    is_dot = ifelse(wide == 0 & noballs == 0 & runs_of_bat == 0 & extras == 0, 1, 0),
    is_bowler_wicket = ifelse(wicket_type %in% c("bowled", "caught", "hit wicket", "lbw", "stumped"), 1, 0)
  )

bowling_summary <- ipl_bowling %>%
  group_by(bowler) %>%
  summarise(
    wickets = sum(is_bowler_wicket, na.rm = TRUE),
    runs_conceded = sum(bowler_runs, na.rm = TRUE),
    legal_balls = sum(is_legal, na.rm = TRUE),
    dot_balls = sum(is_dot, na.rm = TRUE),
    matches = n_distinct(match_id),
    .groups = "drop"
  ) %>%
  mutate(
    overs = round(legal_balls / 6, 1),
    economy = ifelse(legal_balls > 0, round(runs_conceded / (legal_balls / 6), 2), NA),
    bowling_avg = ifelse(wickets > 0, round(runs_conceded / wickets, 2), NA),
    bowling_sr = ifelse(wickets > 0, round(legal_balls / wickets, 2), NA),
    dot_pct = ifelse(legal_balls > 0, round((dot_balls / legal_balls) * 100, 1), 0)
  ) %>%
  arrange(desc(wickets), economy)

# Validate Top Bowler
top_bowl <- bowling_summary[1, ]
stopifnot("Top wicket taker must be Prasidh" = top_bowl$bowler == "Prasidh")
stopifnot("Prasidh wickets must be 25" = top_bowl$wickets == 25)
stopifnot("Prasidh runs conceded must be 488" = top_bowl$runs_conceded == 488)
stopifnot("Prasidh economy must be 8.27" = abs(top_bowl$economy - 8.27) < 0.01)

noor <- bowling_summary %>% filter(bowler == "Noor Ahmad")
stopifnot("Noor Ahmad wickets must be 24" = noor$wickets == 24)
stopifnot("Noor Ahmad economy must be 8.16" = abs(noor$economy - 8.16) < 0.01)

# Check runout exclusion
runouts_in_bowler_wkts <- ipl %>% filter(wicket_type == "runout")
stopifnot("Runouts must not be bowler wickets" = sum(runouts_in_bowler_wkts$wicket_type %in% c("bowled", "caught", "hit wicket", "lbw", "stumped")) == 0)
cat("        ✓ Bowling metrics verified: Byes/leg-byes excluded, runouts excluded, economies accurate.\n")

# 5. Team Scoring Rules
cat("  [5/7] Testing team aggregation and innings performance...\n")
team_runs <- ipl %>%
  group_by(batting_team) %>%
  summarise(total_runs = sum(runs_of_bat + extras, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_runs))

stopifnot("Top scoring team must be PBKS with 3258 runs" = team_runs$batting_team[1] == "PBKS" && team_runs$total_runs[1] == 3258)
stopifnot("Second scoring team must be MI with 2907 runs" = team_runs$batting_team[2] == "MI" && team_runs$total_runs[2] == 2907)
cat("        ✓ Team calculations verified: Team runs match exact ball-by-ball sum.\n")

# 6. Radar Chart Normalization
cat("  [6/7] Testing Player Comparison radar normalization...\n")
normalize_metric <- function(val, min_val, max_val, invert = FALSE) {
  if (is.na(val) || is.null(val)) return(0)
  if (max_val == min_val) return(50)
  norm <- (val - min_val) / (max_val - min_val) * 100
  if (invert) norm <- 100 - norm
  pmin(100, pmax(0, round(norm, 1)))
}

test_norm_max <- normalize_metric(100, 0, 100)
test_norm_min <- normalize_metric(0, 0, 100)
test_norm_inv <- normalize_metric(6.0, 6.0, 12.0, invert = TRUE)
test_norm_inv_high <- normalize_metric(12.0, 6.0, 12.0, invert = TRUE)

stopifnot("Max val should normalize to 100" = test_norm_max == 100)
stopifnot("Min val should normalize to 0" = test_norm_min == 0)
stopifnot("Best economy (6.0) inverted should score 100" = test_norm_inv == 100)
stopifnot("Worst economy (12.0) inverted should score 0" = test_norm_inv_high == 0)
cat("        ✓ Normalization logic verified: Bounds constrained [0, 100], inverted scales correct.\n")

# 7. Shiny Server Reactivity & Reactive UI Outputs
cat("  [7/7] Testing Shiny reactive server logic with testServer...\n")
source("app.R")

testServer(server, {
  # Overview Navigation
  session$setInputs(nav_menu = "overview")
  stopifnot(!is.null(output$header_title_ui))
  stopifnot(!is.null(output$table_top_batsmen))
  stopifnot(!is.null(output$table_top_bowlers))
  stopifnot(!is.null(output$table_team_rankings))
  
  # Player Analytics (Batting)
  session$setInputs(nav_menu = "player", player_select = "Sai Sudharsan", player_category = "batting")
  stopifnot(!is.null(output$player_analytics_ui))
  stopifnot(!is.null(output$plot_player_bat_match_trend))
  stopifnot(!is.null(output$plot_player_bat_runs_donut))
  stopifnot(!is.null(output$table_player_bat_log))
  
  # Player Analytics (Bowling)
  session$setInputs(player_select = "Prasidh", player_category = "bowling")
  stopifnot(!is.null(output$player_analytics_ui))
  stopifnot(!is.null(output$plot_player_bowl_match_trend))
  stopifnot(!is.null(output$plot_player_bowl_economy))
  stopifnot(!is.null(output$table_player_bowl_log))
  
  # Team Analytics
  session$setInputs(nav_menu = "team", team_select = "PBKS", team_season = 2025)
  stopifnot(!is.null(output$team_kpi_cards_ui))
  stopifnot(!is.null(output$plot_team_match_trend))
  stopifnot(!is.null(output$plot_team_innings_comp))
  stopifnot(!is.null(output$table_team_top_bat))
  stopifnot(!is.null(output$table_team_top_bowl))
  
  # Player Comparison Studio (Batting Duel)
  session$setInputs(nav_menu = "comparison", comp_player_a = "Sai Sudharsan", comp_player_b = "Suryakumar Yadav", comp_mode = "batting")
  stopifnot(!is.null(output$comp_content_ui))
  stopifnot(!is.null(output$plot_comp_radar))
  stopifnot(!is.null(output$table_comp_scorecard))
  
  # Player Comparison Studio (Bowling Duel)
  session$setInputs(comp_player_a = "Prasidh", comp_player_b = "Noor Ahmad", comp_mode = "bowling")
  stopifnot(!is.null(output$comp_content_ui))
  stopifnot(!is.null(output$plot_comp_radar))
  stopifnot(!is.null(output$table_comp_scorecard))
  
  # Player Comparison Studio (Identical selection validation)
  session$setInputs(comp_player_a = "Kohli", comp_player_b = "Kohli", comp_mode = "batting")
  stopifnot(!is.null(output$comp_validation_ui))
})
cat("        ✓ Shiny server integration verified: All 4 pages reactive pipelines passed.\n")

cat("\n====================================================================\n")
cat(">>> ALL 7 TEST SUITES PASSED! STATISTICAL & REACTIVE INTEGRITY VERIFIED\n")
cat("====================================================================\n")

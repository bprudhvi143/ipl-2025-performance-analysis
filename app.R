# ==============================================================================
# IPL 2025 Performance Analytics Dashboard
# Modern Sports Intelligence & Cricket Performance Analytics Platform
# Official IPL Theme: Royal Blue (#1D428A), Electric Blue (#0090DF), Gold (#FFB800)
# ==============================================================================

library(shiny)
library(tidyverse)
library(bslib)
library(plotly)
library(DT)
library(bsicons)
library(shinyWidgets)
library(shinycssloaders)
library(scales)

# ------------------------------------------------------------------------------
# 1. DATA PREPARATION & BASELINE PRECOMPUTATION
# ------------------------------------------------------------------------------
data_path <- if (file.exists("data/ipl_2025_deliveries.csv")) {
  "data/ipl_2025_deliveries.csv"
} else {
  "../data/ipl_2025_deliveries.csv"
}

ipl <- read.csv(data_path, stringsAsFactors = FALSE)
ipl$striker <- ifelse(ipl$striker == "Livingstone 1 run", "Livingstone", ipl$striker)
ipl$bowler <- ifelse(ipl$bowler == "Livingstone 1 run", "Livingstone", ipl$bowler)
ipl$player_dismissed <- ifelse(ipl$player_dismissed == "Livingstone 1 run", "Livingstone", ipl$player_dismissed)

# Annotate delivery phases
ipl <- ipl %>%
  mutate(
    over_phase = case_when(
      over < 6  ~ "Powerplay (1-6)",
      over < 15 ~ "Middle (7-15)",
      TRUE      ~ "Death (16-20)"
    ),
    is_legal = ifelse(wide == 0 & noballs == 0, 1, 0),
    is_dot = ifelse(wide == 0 & noballs == 0 & runs_of_bat == 0 & extras == 0, 1, 0),
    bowler_runs = runs_of_bat + ifelse(wide > 0 | noballs > 0, extras, 0),
    is_bowler_wicket = ifelse(wicket_type %in% c("bowled", "caught", "hit wicket", "lbw", "stumped"), 1, 0),
    is_any_dismissal = ifelse(wicket_type != "" & wicket_type != "retired hurt", 1, 0)
  )

# Precompute Season Overview Totals
total_matches <- n_distinct(ipl$match_id)
total_deliveries <- nrow(ipl)
total_runs <- sum(ipl$runs_of_bat + ipl$extras, na.rm = TRUE)
total_wickets_fallen <- sum(ipl$is_any_dismissal, na.rm = TRUE)
total_bowler_wickets <- sum(ipl$is_bowler_wicket, na.rm = TRUE)

# Precompute Player Batting Summary
batter_raw <- ipl %>%
  group_by(striker) %>%
  summarise(
    runs = sum(runs_of_bat, na.rm = TRUE),
    balls = sum(wide == 0, na.rm = TRUE),
    fours = sum(runs_of_bat == 4, na.rm = TRUE),
    sixes = sum(runs_of_bat == 6, na.rm = TRUE),
    dot_balls_faced = sum(wide == 0 & runs_of_bat == 0, na.rm = TRUE),
    singles = sum(runs_of_bat == 1, na.rm = TRUE),
    doubles = sum(runs_of_bat == 2, na.rm = TRUE),
    triples = sum(runs_of_bat == 3, na.rm = TRUE),
    matches = n_distinct(match_id),
    teams = paste(sort(unique(batting_team)), collapse = ", "),
    .groups = "drop"
  )

# Count dismissals for all batters (striker or non-striker)
dismissals_df <- ipl %>%
  filter(player_dismissed != "" & is_any_dismissal == 1) %>%
  group_by(player_dismissed) %>%
  summarise(dismissals = n(), .groups = "drop")

batting_summary <- batter_raw %>%
  left_join(dismissals_df, by = c("striker" = "player_dismissed")) %>%
  mutate(
    dismissals = ifelse(is.na(dismissals), 0, dismissals),
    strike_rate = ifelse(balls > 0, round((runs / balls) * 100, 2), 0),
    batting_avg = ifelse(dismissals > 0, round(runs / dismissals, 2), NA),
    boundary_runs = (fours * 4) + (sixes * 6),
    boundary_pct = ifelse(runs > 0, round((boundary_runs / runs) * 100, 1), 0),
    display_avg = ifelse(dismissals > 0, as.character(batting_avg), paste0(runs, "*"))
  ) %>%
  arrange(desc(runs))

# Precompute Player Bowling Summary
bowling_summary <- ipl %>%
  group_by(bowler) %>%
  summarise(
    wickets = sum(is_bowler_wicket, na.rm = TRUE),
    runs_conceded = sum(bowler_runs, na.rm = TRUE),
    legal_balls = sum(is_legal, na.rm = TRUE),
    dot_balls = sum(is_dot, na.rm = TRUE),
    matches = n_distinct(match_id),
    teams = paste(sort(unique(bowling_team)), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    overs = round(legal_balls / 6, 1),
    overs_display = paste0(legal_balls %/% 6, ".", legal_balls %% 6),
    economy = ifelse(legal_balls > 0, round(runs_conceded / (legal_balls / 6), 2), NA),
    bowling_avg = ifelse(wickets > 0, round(runs_conceded / wickets, 2), NA),
    bowling_sr = ifelse(wickets > 0, round(legal_balls / wickets, 2), NA),
    dot_pct = ifelse(legal_balls > 0, round((dot_balls / legal_balls) * 100, 1), 0)
  ) %>%
  arrange(desc(wickets), economy)

# Precompute Team Summaries
team_batting_summary <- ipl %>%
  group_by(batting_team) %>%
  summarise(
    runs_scored = sum(runs_of_bat + extras, na.rm = TRUE),
    balls_faced = sum(wide == 0, na.rm = TRUE),
    matches_bat = n_distinct(match_id),
    .groups = "drop"
  )

team_bowling_summary <- ipl %>%
  group_by(bowling_team) %>%
  summarise(
    wickets_taken = sum(is_any_dismissal, na.rm = TRUE),
    runs_conceded = sum(runs_of_bat + extras, na.rm = TRUE),
    matches_bowl = n_distinct(match_id),
    .groups = "drop"
  )

team_summary <- full_join(team_batting_summary, team_bowling_summary, by = c("batting_team" = "bowling_team")) %>%
  rename(team = batting_team) %>%
  mutate(
    matches = pmax(matches_bat, matches_bowl, na.rm = TRUE),
    run_rate = round(runs_scored / (balls_faced / 6), 2)
  ) %>%
  arrange(desc(runs_scored))

# Franchise Color Map for IPL Teams (Lighter, Vibrant, High-Contrast Palette)
team_colors <- c(
  "CSK"  = "#FDE047", # Bright Light Gold
  "DC"   = "#38BDF8", # Light Sky Blue
  "GT"   = "#2DD4BF", # Light Mint Teal
  "KKR"  = "#C084FC", # Light Vivid Purple
  "LSG"  = "#67E8F9", # Light Crisp Cyan
  "MI"   = "#60A5FA", # Light Vibrant Blue
  "PBKS" = "#FB7185", # Light Rose Crimson
  "RCB"  = "#F87171", # Light Bright Coral Red
  "RR"   = "#F472B6", # Light Bright Pink
  "SRH"  = "#FB923C"  # Light Bright Orange
)

# Team Logo Resolver Helper (Checks PNG images in www/ and www/teams/)
get_team_logo_path <- function(team_code) {
  if (is.na(team_code) || is.null(team_code) || team_code == "") return(NULL)
  tc <- tolower(trimws(as.character(team_code)))
  first_team <- tolower(trimws(strsplit(tc, ",")[[1]][1]))
  
  if (file.exists(file.path("www", "teams", paste0(first_team, ".png")))) {
    return(paste0("teams/", first_team, ".png"))
  } else if (file.exists(file.path("www", paste0(first_team, ".png")))) {
    return(paste0(first_team, ".png"))
  } else if (file.exists(file.path("www", "teams", paste0(first_team, ".svg")))) {
    return(paste0("teams/", first_team, ".svg"))
  }
  return(NULL)
}

# Team Logo HTML Helper for DataTables and UI
team_logo_html <- function(team_code, size = 22) {
  if (is.na(team_code) || is.null(team_code) || team_code == "") return("—")
  img_src <- get_team_logo_path(team_code)
  if (!is.null(img_src)) {
    paste0("<span class='team-badge-wrap'><img src='", img_src, "' class='team-logo-icon' style='width:", size, "px; height:", size, "px; object-fit: contain; vertical-align: middle;' alt='", team_code, "' /> <span class='team-code-text'>", team_code, "</span></span>")
  } else {
    paste0("<span class='team-code-text'>", team_code, "</span>")
  }
}

# All unique players for search/dropdowns
all_players <- sort(unique(c(batting_summary$striker, bowling_summary$bowler)))
all_teams <- sort(unique(ipl$batting_team))
all_seasons <- sort(unique(ipl$season))

# ==============================================================================
# Cricbuzz Player Profiles Database (Role & Style Metadata)
# ==============================================================================
cricbuzz_db <- list(
  # Top Indian Batters & WKs
  "Kohli" = list(full_name = "Virat Kohli", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Virat Kohli", "V Kohli")),
  "Rohit" = list(full_name = "Rohit Sharma", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Rohit Sharma", "RG Sharma")),
  "Sai Sudharsan" = list(full_name = "B Sai Sudharsan", role = "Top order Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Sai Sudharsan", "Sudharsan")),
  "Suryakumar Yadav" = list(full_name = "Suryakumar Yadav", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Suryakumar Yadav", "SKY", "Surya")),
  "Shubman Gill" = list(full_name = "Shubman Gill", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Shubman Gill", "S Gill", "Gill")),
  "Jaiswal" = list(full_name = "Yashasvi Jaiswal", role = "Top order Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Yashasvi Jaiswal", "Y Jaiswal")),
  "Gaikwad" = list(full_name = "Ruturaj Gaikwad", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Ruturaj Gaikwad", "R Gaikwad")),
  "Rajat Patidar" = list(full_name = "Rajat Patidar", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Rajat Patidar")),
  "Shreyas Iyer" = list(full_name = "Shreyas Iyer", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Shreyas Iyer", "S Iyer")),
  "Tilak Varma" = list(full_name = "Tilak Varma", role = "Top order Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Tilak Varma")),
  "Rinku Singh" = list(full_name = "Rinku Singh", role = "Middle order Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Rinku Singh")),
  "Riyan Parag" = list(full_name = "Riyan Parag", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Riyan Parag", "Parag")),
  "Padikkal" = list(full_name = "Devdutt Padikkal", role = "Top order Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Devdutt Padikkal", "D Padikkal")),
  "Rahane" = list(full_name = "Ajinkya Rahane", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Ajinkya Rahane", "A Rahane")),
  "Tripathi" = list(full_name = "Rahul Tripathi", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Rahul Tripathi", "R Tripathi")),
  "Prabhsimran" = list(full_name = "Prabhsimran Singh", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Prabhsimran Singh")),
  "Ashutosh Sharma" = list(full_name = "Ashutosh Sharma", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Ashutosh Sharma")),
  "Shashank Singh" = list(full_name = "Shashank Singh", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Shashank Singh")),
  "Nehal Wadhera" = list(full_name = "Nehal Wadhera", role = "Middle order Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Nehal Wadhera")),
  "Naman Dhir" = list(full_name = "Naman Dhir", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Naman Dhir")),
  "Ayush Badoni" = list(full_name = "Ayush Badoni", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Ayush Badoni")),
  "Sameer Rizvi" = list(full_name = "Sameer Rizvi", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Sameer Rizvi")),
  "Shahrukh Khan" = list(full_name = "M Shahrukh Khan", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Shahrukh Khan")),
  "Abhinav Manohar" = list(full_name = "Abhinav Manohar", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Abhinav Manohar")),
  "Angkrish Raghuvanshi" = list(full_name = "Angkrish Raghuvanshi", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Slow Left arm Orthodox", aliases = c("Angkrish Raghuvanshi")),
  
  # Indian WKs
  "Dhoni" = list(full_name = "MS Dhoni", role = "Wicketkeeper Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("MS Dhoni", "M.S. Dhoni", "Mahendra Singh Dhoni")),
  "Pant" = list(full_name = "Rishabh Pant", role = "Wicketkeeper Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Medium", aliases = c("Rishabh Pant", "R Pant")),
  "Rahul" = list(full_name = "KL Rahul", role = "Wicketkeeper Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("KL Rahul", "K.L. Rahul")),
  "Samson" = list(full_name = "Sanju Samson", role = "Wicketkeeper Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Sanju Samson", "S Samson")),
  "Ishan Kishan" = list(full_name = "Ishan Kishan", role = "Wicketkeeper Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Ishan Kishan")),
  "Jitesh Sharma" = list(full_name = "Jitesh Sharma", role = "Wicketkeeper Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Jitesh Sharma")),
  "Dhruv Jurel" = list(full_name = "Dhruv Jurel", role = "Wicketkeeper Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Dhruv Jurel")),
  "Abishek Porel" = list(full_name = "Abishek Porel", role = "Wicketkeeper Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Abishek Porel")),
  
  # Indian Allrounders
  "Hardik Pandya" = list(full_name = "Hardik Pandya", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Hardik Pandya", "H Pandya")),
  "Jadeja" = list(full_name = "Ravindra Jadeja", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Slow Left arm Orthodox", aliases = c("Ravindra Jadeja", "R Jadeja", "Jaddu")),
  "Axar" = list(full_name = "Axar Patel", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Slow Left arm Orthodox", aliases = c("Axar Patel", "A Patel")),
  "Ashwin" = list(full_name = "Ravichandran Ashwin", role = "Bowling Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Ravichandran Ashwin", "R Ashwin")),
  "Abhishek Sharma" = list(full_name = "Abhishek Sharma", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Slow Left arm Orthodox", aliases = c("Abhishek Sharma")),
  "Shivam Dube" = list(full_name = "Shivam Dube", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Right arm Medium", aliases = c("Shivam Dube", "S Dube")),
  "Nitish Reddy" = list(full_name = "Nitish Kumar Reddy", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Nitish Reddy", "NK Reddy")),
  "Krunal Pandya" = list(full_name = "Krunal Pandya", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Slow Left arm Orthodox", aliases = c("Krunal Pandya", "K Pandya")),
  "Washington Sundar" = list(full_name = "Washington Sundar", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Washington Sundar", "W Sundar")),
  "Shahbaz Ahmed" = list(full_name = "Shahbaz Ahmed", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Slow Left arm Orthodox", aliases = c("Shahbaz Ahmed")),
  "Ramandeep Singh" = list(full_name = "Ramandeep Singh", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Ramandeep Singh")),
  "Rahul Tewatia" = list(full_name = "Rahul Tewatia", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Rahul Tewatia")),
  "Harpreet Brar" = list(full_name = "Harpreet Brar", role = "Bowling Allrounder", bat_style = "Left hand Bat", bowl_style = "Slow Left arm Orthodox", aliases = c("Harpreet Brar")),
  
  # Indian Bowlers
  "Bumrah" = list(full_name = "Jasprit Bumrah", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Jasprit Bumrah", "J Bumrah")),
  "Siraj" = list(full_name = "Mohammed Siraj", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Mohammed Siraj", "M Siraj")),
  "Shami" = list(full_name = "Mohammed Shami", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Mohammed Shami", "M Shami")),
  "Prasidh" = list(full_name = "Prasidh Krishna", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Prasidh Krishna", "P Krishna")),
  "Arshdeep Singh" = list(full_name = "Arshdeep Singh", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Left arm Fast Medium", aliases = c("Arshdeep Singh", "A Singh")),
  "Chahal" = list(full_name = "Yuzvendra Chahal", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Yuzvendra Chahal", "Y Chahal")),
  "Kuldeep Yadav" = list(full_name = "Kuldeep Yadav", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Left arm Chinaman", aliases = c("Kuldeep Yadav", "K Yadav")),
  "Varun Chakaravarthy" = list(full_name = "Varun Chakaravarthy", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Mystery Spin", aliases = c("Varun Chakaravarthy", "Varun Chakravarthy")),
  "Mayank Yadav" = list(full_name = "Mayank Yadav", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Express Fast", aliases = c("Mayank Yadav")),
  "Harshit Rana" = list(full_name = "Harshit Rana", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Harshit Rana")),
  "Harshal Patel" = list(full_name = "Harshal Patel", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Harshal Patel")),
  "Avesh Khan" = list(full_name = "Avesh Khan", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Avesh Khan")),
  "Khaleel Ahmed" = list(full_name = "Khaleel Ahmed", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Left arm Fast Medium", aliases = c("Khaleel Ahmed")),
  "Mukesh Kumar" = list(full_name = "Mukesh Kumar", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Mukesh Kumar")),
  "T Natarajan" = list(full_name = "T Natarajan", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Left arm Fast Medium", aliases = c("T Natarajan", "Natarajan")),
  "Yash Dayal" = list(full_name = "Yash Dayal", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Left arm Fast Medium", aliases = c("Yash Dayal")),
  "Mohit Sharma" = list(full_name = "Mohit Sharma", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Mohit Sharma")),
  "Sandeep Sharma" = list(full_name = "Sandeep Sharma", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Sandeep Sharma")),
  "Tushar Deshpande" = list(full_name = "Tushar Deshpande", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Tushar Deshpande")),
  "Vaibhav Arora" = list(full_name = "Vaibhav Arora", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Vaibhav Arora")),
  "Ravi Bishnoi" = list(full_name = "Ravi Bishnoi", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Ravi Bishnoi")),
  "Sai Kishore" = list(full_name = "R Sai Kishore", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Slow Left arm Orthodox", aliases = c("R Sai Kishore", "Sai Kishore")),
  "Karn Sharma" = list(full_name = "Karn Sharma", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Karn Sharma")),
  "Suyash Sharma" = list(full_name = "Suyash Sharma", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Suyash Sharma")),
  "Rahul Chahar" = list(full_name = "Rahul Chahar", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Rahul Chahar")),
  "Bhuvneshwar" = list(full_name = "Bhuvneshwar Kumar", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Bhuvneshwar Kumar", "B Kumar")),
  "Chahar" = list(full_name = "Deepak Chahar", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Deepak Chahar", "D Chahar")),
  "Ishant" = list(full_name = "Ishant Sharma", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Ishant Sharma", "I Sharma")),
  "Yash Thakur" = list(full_name = "Yash Thakur", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Yash Thakur")),
  "Simarjeet Singh" = list(full_name = "Simarjeet Singh", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Simarjeet Singh")),
  
  # Overseas Stars
  "Head" = list(full_name = "Travis Head", role = "Top order Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Travis Head", "T Head")),
  "Klaasen" = list(full_name = "Heinrich Klaasen", role = "Wicketkeeper Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Heinrich Klaasen", "H Klaasen")),
  "Narine" = list(full_name = "Sunil Narine", role = "Bowling Allrounder", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Sunil Narine", "S Narine")),
  "Russell" = list(full_name = "Andre Russell", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Andre Russell", "A Russell")),
  "Rashid Khan" = list(full_name = "Rashid Khan", role = "Bowling Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Rashid Khan", "Rashid")),
  "Cummins" = list(full_name = "Pat Cummins", role = "Bowling Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Pat Cummins", "P Cummins")),
  "Boult" = list(full_name = "Trent Boult", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Left arm Fast", aliases = c("Trent Boult", "T Boult")),
  "Rabada" = list(full_name = "Kagiso Rabada", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Right arm Fast", aliases = c("Kagiso Rabada", "K Rabada")),
  "Noor Ahmad" = list(full_name = "Noor Ahmad", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Left arm Chinaman", aliases = c("Noor Ahmad", "Noor")),
  "Pathirana" = list(full_name = "Matheesha Pathirana", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast (Sling)", aliases = c("Matheesha Pathirana", "M Pathirana")),
  "Pooran" = list(full_name = "Nicholas Pooran", role = "Wicketkeeper Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Nicholas Pooran", "N Pooran")),
  "Miller" = list(full_name = "David Miller", role = "Middle order Batter", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("David Miller", "D Miller")),
  "Maxwell" = list(full_name = "Glenn Maxwell", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Glenn Maxwell", "G Maxwell")),
  "Stoinis" = list(full_name = "Marcus Stoinis", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Marcus Stoinis", "M Stoinis")),
  "Buttler" = list(full_name = "Jos Buttler", role = "Wicketkeeper Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Jos Buttler", "J Buttler")),
  "de Kock" = list(full_name = "Quinton de Kock", role = "Wicketkeeper Batter", bat_style = "Left hand Bat", bowl_style = "Slow Left arm", aliases = c("Quinton de Kock", "Q de Kock")),
  "du Plessis" = list(full_name = "Faf du Plessis", role = "Top order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Faf du Plessis", "F du Plessis")),
  "Phil Salt" = list(full_name = "Phil Salt", role = "Wicketkeeper Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Phil Salt", "P Salt")),
  "Gurbaz" = list(full_name = "Rahmanullah Gurbaz", role = "Wicketkeeper Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Medium", aliases = c("Rahmanullah Gurbaz", "R Gurbaz")),
  "Stubbs" = list(full_name = "Tristan Stubbs", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Tristan Stubbs", "T Stubbs")),
  "Moeen Ali" = list(full_name = "Moeen Ali", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Moeen Ali")),
  "Livingstone" = list(full_name = "Liam Livingstone", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak / Offbreak", aliases = c("Liam Livingstone", "L Livingstone")),
  "Mitchell Marsh" = list(full_name = "Mitchell Marsh", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Mitchell Marsh", "M Marsh")),
  "Marco Jansen" = list(full_name = "Marco Jansen", role = "Bowling Allrounder", bat_style = "Right hand Bat", bowl_style = "Left arm Fast", aliases = c("Marco Jansen", "M Jansen")),
  "Will Jacks" = list(full_name = "Will Jacks", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Will Jacks")),
  "Tim David" = list(full_name = "Tim David", role = "Middle order Batter", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Tim David")),
  "Shepherd" = list(full_name = "Romario Shepherd", role = "Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Romario Shepherd", "R Shepherd")),
  "Theekshana" = list(full_name = "Maheesh Theekshana", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Maheesh Theekshana", "M Theekshana")),
  "Hasaranga" = list(full_name = "Wanindu Hasaranga", role = "Bowling Allrounder", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Wanindu Hasaranga", "W Hasaranga")),
  "Zampa" = list(full_name = "Adam Zampa", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Legbreak", aliases = c("Adam Zampa", "A Zampa")),
  "Mujeeb" = list(full_name = "Mujeeb Ur Rahman", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Offbreak", aliases = c("Mujeeb Ur Rahman")),
  "Mitchell Starc" = list(full_name = "Mitchell Starc", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Left arm Fast", aliases = c("Mitchell Starc", "M Starc")),
  "Hazlewood" = list(full_name = "Josh Hazlewood", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Right arm Fast Medium", aliases = c("Josh Hazlewood", "J Hazlewood")),
  "Gerald Coetzee" = list(full_name = "Gerald Coetzee", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Gerald Coetzee")),
  "Ferguson" = list(full_name = "Lockie Ferguson", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Lockie Ferguson", "L Ferguson")),
  "Nortje" = list(full_name = "Anrich Nortje", role = "Bowler", bat_style = "Right hand Bat", bowl_style = "Right arm Fast", aliases = c("Anrich Nortje", "A Nortje")),
  "Spencer Johnson" = list(full_name = "Spencer Johnson", role = "Bowler", bat_style = "Left hand Bat", bowl_style = "Left arm Fast", aliases = c("Spencer Johnson")),
  "Sam Curran" = list(full_name = "Sam Curran", role = "Allrounder", bat_style = "Left hand Bat", bowl_style = "Left arm Fast Medium", aliases = c("Sam Curran", "S Curran"))
)

# Cricbuzz Metadata Getter with intelligent squad fallback
get_player_cricbuzz_meta <- function(player_code) {
  if (is.null(player_code) || is.na(player_code) || player_code == "") {
    return(list(
      full_name = "Player",
      role = "Player",
      bat_style = "Right hand Bat",
      bowl_style = "Right arm Medium",
      franchise = "—"
    ))
  }
  
  meta <- if (player_code %in% names(cricbuzz_db)) {
    cricbuzz_db[[player_code]]
  } else {
    bat_rec <- batting_summary %>% filter(striker == player_code)
    bowl_rec <- bowling_summary %>% filter(bowler == player_code)
    has_bat <- nrow(bat_rec) > 0 && bat_rec$balls >= 30
    has_bowl <- nrow(bowl_rec) > 0 && bowl_rec$legal_balls >= 30
    
    inferred_role <- if (has_bat && has_bowl) {
      "Allrounder"
    } else if (has_bowl) {
      "Bowler"
    } else {
      "Batter"
    }
    
    list(
      full_name = player_code,
      role = inferred_role,
      bat_style = "Right hand Bat",
      bowl_style = "Right arm Fast Medium",
      aliases = c(player_code)
    )
  }
  
  bat_teams <- ipl %>% filter(striker == player_code) %>% pull(batting_team) %>% unique()
  bowl_teams <- ipl %>% filter(bowler == player_code) %>% pull(bowling_team) %>% unique()
  all_teams <- unique(c(bat_teams, bowl_teams))
  all_teams <- all_teams[all_teams != "" & !is.na(all_teams)]
  p_teams <- if (length(all_teams) > 0) paste(all_teams, collapse = ", ") else "IPL 2025 Squad"
  meta$franchise <- p_teams
  meta
}

# Autocomplete search suggestions (full names + aliases + dataset names)
all_search_suggestions <- sort(unique(c(
  all_players,
  unlist(lapply(cricbuzz_db, function(x) x$full_name)),
  unlist(lapply(cricbuzz_db, function(x) x$aliases))
)))

# Robust Player Name Resolver
resolve_player <- function(query) {
  if (is.null(query) || is.na(query)) return(NULL)
  q <- trimws(tolower(as.character(query)))
  if (q == "") return(NULL)
  
  # 1. Exact match in all_players
  match_exact <- all_players[tolower(all_players) == q]
  if (length(match_exact) > 0) return(match_exact[1])
  
  # 2. Match in Cricbuzz profiles
  for (p in names(cricbuzz_db)) {
    prof <- cricbuzz_db[[p]]
    aliases <- tolower(c(p, prof$full_name, prof$aliases))
    if (q %in% aliases) return(p)
  }
  
  # 3. Word boundary match in all_players
  partial_match <- all_players[grepl(paste0("\\b", q), tolower(all_players))]
  if (length(partial_match) == 1) return(partial_match[1])
  
  # 4. Word boundary match in Cricbuzz profiles
  for (p in names(cricbuzz_db)) {
    prof <- cricbuzz_db[[p]]
    aliases <- tolower(c(p, prof$full_name, prof$aliases))
    if (any(grepl(paste0("\\b", q), aliases))) return(p)
  }
  
  # 5. Substring match
  sub_match <- all_players[grepl(q, tolower(all_players))]
  if (length(sub_match) == 1) return(sub_match[1])
  
  return(NULL)
}

# Radar Normalization Helper
normalize_metric <- function(val, min_val, max_val, invert = FALSE) {
  if (is.na(val) || is.null(val)) return(0)
  if (max_val == min_val) return(50)
  norm <- (val - min_val) / (max_val - min_val) * 100
  if (invert) norm <- 100 - norm
  pmin(100, pmax(0, round(norm, 1)))
}

# Blended, High-Contrast Plotly layout template with Luminous Elements
light_plotly_layout <- function(p, title = "", showlegend = TRUE) {
  p %>%
    layout(
      title = list(
        text = title,
        font = list(color = "#F8FAFC", size = 14, family = "Inter, sans-serif")
      ),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)",
      font = list(color = "#CBD5E1", family = "Inter, sans-serif", size = 11),
      showlegend = showlegend,
      legend = list(
        font = list(color = "#F8FAFC", size = 11),
        bgcolor = "rgba(16, 27, 46, 0.85)",
        bordercolor = "rgba(255, 255, 255, 0.12)",
        borderwidth = 1
      ),
      margin = list(l = 55, r = 30, t = 45, b = 55),
      bargap = 0.35, # Generous spacing between bars - NOT congested!
      hoverlabel = list(
        bgcolor = "#0B111E",
        bordercolor = "#38BDF8",
        font = list(color = "#FFFFFF", family = "Inter, sans-serif")
      ),
      xaxis = list(
        gridcolor = "rgba(255, 255, 255, 0.08)",
        zerolinecolor = "rgba(255, 255, 255, 0.15)",
        tickfont = list(color = "#94A3B8", size = 11),
        titlefont = list(color = "#E2E8F0", size = 12)
      ),
      yaxis = list(
        gridcolor = "rgba(255, 255, 255, 0.08)",
        zerolinecolor = "rgba(255, 255, 255, 0.15)",
        tickfont = list(color = "#94A3B8", size = 11),
        titlefont = list(color = "#E2E8F0", size = 12)
      )
    ) %>%
    config(displayModeBar = FALSE, responsive = TRUE)
}

# ------------------------------------------------------------------------------
# 2. USER INTERFACE (Bootstrap 5 + Custom IPL Theme)
# ------------------------------------------------------------------------------
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bg = "#0B111E",
    fg = "#FFFFFF",
    primary = "#0090DF",
    secondary = "#FFB800",
    base_font = font_google("Inter")
  ),
  title = "IPL 2025 Performance Analysis | Official Cricket Intelligence Platform",
  
  # Inject Custom CSS and Google Fonts
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800;900&display=swap"),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  div(
    class = "app-container",
    
    # ----------------- SIDEBAR NAVIGATION -----------------
    div(
      class = "app-sidebar",
      
      # Brand Header with Official IPL Logo
      div(
        class = "brand-section",
        div(
          class = "brand-logo-container",
          tags$img(src = "ipl_logo.png", alt = "IPL Logo", class = "brand-logo-img")
        ),
        div(
          div(class = "brand-title", "IPL INSIGHTS"),
          div(class = "brand-subtitle", "Season 2025 Official")
        )
      ),
      
      # Navigation Pill Buttons
      div(
        class = "sidebar-nav",
        radioGroupButtons(
          inputId = "nav_menu",
          label = NULL,
          choices = c(
            "<span class='nav-icon'>📊</span> Overview" = "overview",
            "<span class='nav-icon'>👤</span> Player Analytics" = "player",
            "<span class='nav-icon'>🛡️</span> Team Analytics" = "team",
            "<span class='nav-icon'>⚔️</span> Player Comparison" = "comparison"
          ),
          direction = "vertical",
          justified = TRUE,
          selected = "overview"
        )
      ),
      
      # Sidebar Footer
      div(
        class = "sidebar-footer",
        div(class = "season-tag", "⚡ IPL 2025 OFFICIAL DATA"),
        div("74 Matches • 10 Teams • 17,246 Balls"),
        div(style = "margin-top: 6px; color: #64748B;", "Ball-by-Ball Analytics v2.0")
      )
    ),
    
    # ----------------- MAIN WRAPPER -----------------
    div(
      class = "main-wrapper",
      
      # Top Header Bar with IPL Logo Integration
      div(
        class = "top-header",
        div(
          class = "header-titles",
          uiOutput("header_title_ui")
        ),
        div(
          class = "header-badges",
          div(
            class = "header-ipl-badge",
            tags$img(src = "ipl_logo.png", alt = "IPL", class = "header-logo-mini"),
            span(style = "font-size: 0.82rem; font-weight: 700; color: #0090DF;", "IPL 2025")
          ),
          div(
            class = "status-badge",
            span(class = "pulse-dot"),
            "SEASON COMPLETE"
          )
        )
      ),
      
      # Main Content Body
      div(
        class = "content-body",
        
        # Hidden Tabset for Navigation
        tabsetPanel(
          id = "main_tabs",
          type = "hidden",
          
          # ====================================================================
          # PAGE 1: OVERVIEW (Light & Uncongested Visualizations)
          # ====================================================================
          tabPanel(
            "overview",
            # Championship Showcase Bar (Compact, Attractive & High-Impact)
            div(
              class = "championship-showcase-bar",
              # Champions Podium
              div(
                class = "showcase-card showcase-champion",
                div(class = "showcase-badge", span("🏆"), "2025 CHAMPIONS"),
                div(
                  class = "showcase-main",
                  tags$img(src = "teams/rcb.png", class = "showcase-logo", alt = "RCB"),
                  div(
                    class = "showcase-details",
                    tags$h4("Royal Challengers Bengaluru"),
                    tags$p("Defeated PBKS by 6 runs • 1st IPL Trophy")
                  ),
                  span(class = "showcase-score-pill", "190/9")
                )
              ),
              # Runners-Up Podium
              div(
                class = "showcase-card showcase-runner",
                div(class = "showcase-badge", span("🥈"), "RUNNERS-UP"),
                div(
                  class = "showcase-main",
                  tags$img(src = "teams/pbks.png", class = "showcase-logo", alt = "PBKS"),
                  div(
                    class = "showcase-details",
                    tags$h4("Punjab Kings"),
                    tags$p("Grand Finalist • Chased 184/7 in Final")
                  ),
                  span(class = "showcase-score-pill", "184/7")
                )
              ),
              # Tournament Structure
              div(
                class = "showcase-card showcase-summary",
                div(class = "showcase-badge", span("🏟️"), "TOURNAMENT COMPLETE"),
                div(
                  class = "showcase-main",
                  div(class = "showcase-icon-cube", "🏏"),
                  div(
                    class = "showcase-details",
                    tags$h4("74 Matches Total"),
                    tags$p("70 League Games + 4 Playoffs")
                  ),
                  span(class = "showcase-stat-pill", "10 SQUADS")
                )
              )
            ),
            
            # KPI Cards Row
            div(
              class = "kpi-grid",
              div(
                class = "kpi-card kpi-primary",
                div(class = "kpi-header", span(class = "kpi-title", "Total Matches"), div(class = "kpi-icon-wrap", "🏟️")),
                div(class = "kpi-value", format(total_matches, big.mark = ",")),
                div(class = "kpi-subtitle", "70 League Stage + 4 Playoffs")
              ),
              div(
                class = "kpi-card kpi-secondary",
                div(class = "kpi-header", span(class = "kpi-title", "Total Deliveries"), div(class = "kpi-icon-wrap", "⚡")),
                div(class = "kpi-value", format(total_deliveries, big.mark = ",")),
                div(class = "kpi-subtitle", "Full ball-by-ball records")
              ),
              div(
                class = "kpi-card kpi-positive",
                div(class = "kpi-header", span(class = "kpi-title", "Total Runs Scored"), div(class = "kpi-icon-wrap", "🏏")),
                div(class = "kpi-value", format(total_runs, big.mark = ",")),
                div(class = "kpi-subtitle", "Bat runs + team extras")
              ),
              div(
                class = "kpi-card kpi-purple",
                div(class = "kpi-header", span(class = "kpi-title", "Total Wickets"), div(class = "kpi-icon-wrap", "🎯")),
                div(class = "kpi-value", format(total_wickets_fallen, big.mark = ",")),
                div(class = "kpi-subtitle", paste0(total_bowler_wickets, " credited to bowlers"))
              )
            ),
            
            # Row 1: Visualizations (Runs Distribution & Team Scoring)
            fluidRow(
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Runs Distribution by Innings & Phase"),
                      tags$p("Powerplay (1–6), Middle (7–15), and Death (16–20)")
                    ),
                    span(class = "card-action-pill", "Phase Split")
                  ),
                  withSpinner(plotlyOutput("plot_overview_runs_dist", height = "330px"), type = 4, color = "#0090DF")
                )
              ),
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Franchise Total Runs Comparison"),
                      tags$p("Cumulative runs scored across all matches")
                    ),
                    span(class = "card-action-pill", "10 Franchises")
                  ),
                  withSpinner(plotlyOutput("plot_overview_team_scoring", height = "330px"), type = 4, color = "#0090DF")
                )
              )
            ),
            
            # Row 2: Top Batsmen & Top Bowlers (Guarded from Overflow + Team Logos)
            fluidRow(
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Top 5 Batsmen (Orange Cap)"),
                      tags$p("Season leaders by runs, strike rate & average")
                    ),
                    span(class = "card-action-pill", "Orange Cap")
                  ),
                  div(
                    class = "table-responsive-container",
                    DTOutput("table_top_batsmen")
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Top 5 Bowlers (Purple Cap)"),
                      tags$p("Season leaders by credited wickets & economy")
                    ),
                    span(class = "card-action-pill", "Purple Cap")
                  ),
                  div(
                    class = "table-responsive-container",
                    DTOutput("table_top_bowlers")
                  )
                )
              )
            ),
            
            # Row 3: Team Rankings Leaderboard (With Team Logos)
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("IPL 2025 Franchise Performance Standings"),
                  tags$p("Matches, runs scored, scoring run rates, wickets taken and conceded")
                ),
                span(class = "card-action-pill", "Season Standings")
              ),
              div(
                class = "table-responsive-container",
                DTOutput("table_team_rankings")
              )
            )
          ),
          
          # ====================================================================
          # PAGE 2: PLAYER ANALYTICS
          # ====================================================================
          tabPanel(
            "player",
            # Player Search & Category Filter Bar
            div(
              class = "filter-panel",
              div(
                style = "flex: 1; min-width: 320px;",
                tags$label("Search Player (Instant Autocomplete)", style = "font-weight: 700; color: #CBD5E1; margin-bottom: 6px; display: block;"),
                div(
                  class = "player-search-bar-wrap",
                  div(
                    class = "search-input-group",
                    span(class = "search-icon", "🔍"),
                    tags$input(
                      id = "player_search",
                      type = "text",
                      class = "player-search-control",
                      placeholder = "Type any player name (e.g. Virat, Rohit, Bumrah, Klaasen, Stokes)...",
                      value = "Sai Sudharsan",
                      list = "ipl_players_datalist",
                      autocomplete = "off",
                      oninput = "Shiny.setInputValue('player_search', this.value);"
                    ),
                    tags$button(
                      type = "button",
                      class = "search-clear-btn",
                      title = "Clear search",
                      onclick = "document.getElementById('player_search').value = ''; Shiny.setInputValue('player_search', ''); document.getElementById('player_search').focus();",
                      "✕"
                    ),
                    tags$datalist(
                      id = "ipl_players_datalist",
                      lapply(all_search_suggestions, function(p) tags$option(value = p))
                    )
                  ),
                  # Quick Trending Star Chips
                  div(
                    class = "quick-chips-wrap",
                    span(style = "font-size: 0.78rem; font-weight: 700; color: #94A3B8; margin-right: 4px; align-self: center;", "⚡ Popular:"),
                    lapply(
                      c("Sai Sudharsan", "Virat Kohli", "Suryakumar Yadav", "Jasprit Bumrah", "Travis Head", "Heinrich Klaasen", "Rashid Khan", "Rohit Sharma", "Prasidh"),
                      function(star) {
                        tags$button(
                          type = "button",
                          class = "quick-chip-btn",
                          onclick = sprintf("document.getElementById('player_search').value = '%s'; Shiny.setInputValue('player_search', '%s');", star, star),
                          star
                        )
                      }
                    )
                  )
                ),
                # Hidden element for test backwards-compatibility
                tags$div(style = "display: none;", selectizeInput("player_select", label = NULL, choices = all_players, selected = "Sai Sudharsan"))
              ),
              div(
                style = "min-width: 240px;",
                tags$label("Performance Category", style = "font-weight: 700; color: #CBD5E1; margin-bottom: 6px; display: block;"),
                radioGroupButtons(
                  inputId = "player_category",
                  label = NULL,
                  choices = c("Batting Analysis" = "batting", "Bowling Analysis" = "bowling"),
                  selected = "batting",
                  justified = TRUE,
                  status = "primary"
                )
              )
            ),
            
            # Dynamic Player Analytics Output
            uiOutput("player_analytics_ui")
          ),
          
          # ====================================================================
          # PAGE 3: TEAM ANALYTICS
          # ====================================================================
          tabPanel(
            "team",
            # Team & Season Filters
            div(
              class = "filter-panel",
              div(
                style = "flex: 1; min-width: 240px;",
                tags$label("Select Franchise"),
                selectInput(
                  inputId = "team_select",
                  label = NULL,
                  choices = all_teams,
                  selected = "PBKS"
                )
              ),
              div(
                style = "min-width: 180px;",
                tags$label("Season"),
                selectInput(
                  inputId = "team_season",
                  label = NULL,
                  choices = all_seasons,
                  selected = 2025
                )
              )
            ),
            
            # Team KPI Cards with Big Team Crest Banner
            uiOutput("team_kpi_cards_ui"),
            
            # Team Visualizations Row 1
            fluidRow(
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Match-by-Match Scoring Trend"),
                      tags$p("Total runs scored per match across the tournament")
                    ),
                    span(class = "card-action-pill", "Scoring Trend")
                  ),
                  withSpinner(plotlyOutput("plot_team_match_trend", height = "320px"), type = 4, color = "#0090DF")
                )
              ),
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Innings Comparison (1st vs 2nd Innings)"),
                      tags$p("Runs scored and wickets lost setting vs chasing targets")
                    ),
                    span(class = "card-action-pill", "Innings Split")
                  ),
                  withSpinner(plotlyOutput("plot_team_innings_comp", height = "320px"), type = 4, color = "#0090DF")
                )
              )
            ),
            
            # Team Visualizations Row 2: Phase Breakdowns
            fluidRow(
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Batting Runs by Match Phase"),
                      tags$p("Runs distribution across Powerplay, Middle, and Death overs")
                    ),
                    span(class = "card-action-pill", "Batting Phases")
                  ),
                  withSpinner(plotlyOutput("plot_team_bat_phase", height = "280px"), type = 4, color = "#0090DF")
                )
              ),
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Bowling Wickets by Match Phase"),
                      tags$p("Breakthrough wickets taken across Powerplay, Middle, and Death overs")
                    ),
                    span(class = "card-action-pill", "Bowling Phases")
                  ),
                  withSpinner(plotlyOutput("plot_team_bowl_phase", height = "280px"), type = 4, color = "#0090DF")
                )
              )
            ),
            
            # Team Rosters Top 3 with Team Crests
            fluidRow(
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Franchise Top 3 Batsmen"),
                      tags$p("Leading run contributors for the selected team")
                    ),
                    span(class = "card-action-pill", "Top Batters")
                  ),
                  div(
                    class = "table-responsive-container",
                    DTOutput("table_team_top_bat")
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "analytics-card",
                  div(
                    class = "analytics-card-header",
                    div(
                      class = "card-title-group",
                      tags$h2("Franchise Top 3 Bowlers"),
                      tags$p("Leading wicket-takers representing the selected team")
                    ),
                    span(class = "card-action-pill", "Top Bowlers")
                  ),
                  div(
                    class = "table-responsive-container",
                    DTOutput("table_team_top_bowl")
                  )
                )
              )
            )
          ),
          
          # ====================================================================
          # PAGE 4: PLAYER COMPARISON STUDIO (Instant Search & Cricbuzz Profiles)
          # ====================================================================
          tabPanel(
            "comparison",
            # Recommended Featured Rivalries (Instant Duel Matchups)
            div(
              class = "featured-duels-ribbon",
              div(
                class = "featured-duels-header",
                span(style = "font-size: 1.15rem; color: #FFB800;", "⚔️"),
                tags$span(style = "font-weight: 800; font-size: 0.88rem; color: #F8FAFC; letter-spacing: 0.5px;", "FEATURED RIVALRIES:"),
                tags$span(style = "font-size: 0.8rem; color: #94A3B8; margin-left: 4px;", "Click any rivalry to load instant head-to-head comparison")
              ),
              div(
                class = "featured-duels-list",
                tags$button(
                  type = "button",
                  class = "duel-pill-btn",
                  onclick = "document.getElementById('comp_player_a_search').value = 'Virat Kohli'; Shiny.setInputValue('comp_player_a_search', 'Virat Kohli'); document.getElementById('comp_player_b_search').value = 'Shubman Gill'; Shiny.setInputValue('comp_player_b_search', 'Shubman Gill');",
                  tags$span(class = "duel-tag", "👑 RUN MACHINES"),
                  tags$span(class = "duel-names", "Virat Kohli ", tags$b(style = "color: #38BDF8;", "vs"), " Shubman Gill")
                ),
                tags$button(
                  type = "button",
                  class = "duel-pill-btn",
                  onclick = "document.getElementById('comp_player_a_search').value = 'Sai Sudharsan'; Shiny.setInputValue('comp_player_a_search', 'Sai Sudharsan'); document.getElementById('comp_player_b_search').value = 'Suryakumar Yadav'; Shiny.setInputValue('comp_player_b_search', 'Suryakumar Yadav');",
                  tags$span(class = "duel-tag", "💥 ORANGE CAP"),
                  tags$span(class = "duel-names", "Sai Sudharsan ", tags$b(style = "color: #38BDF8;", "vs"), " Suryakumar Yadav")
                ),
                tags$button(
                  type = "button",
                  class = "duel-pill-btn",
                  onclick = "document.getElementById('comp_player_a_search').value = 'Jasprit Bumrah'; Shiny.setInputValue('comp_player_a_search', 'Jasprit Bumrah'); document.getElementById('comp_player_b_search').value = 'Prasidh'; Shiny.setInputValue('comp_player_b_search', 'Prasidh');",
                  tags$span(class = "duel-tag", "⚡ PACE TITANS"),
                  tags$span(class = "duel-names", "Jasprit Bumrah ", tags$b(style = "color: #38BDF8;", "vs"), " Prasidh")
                ),
                tags$button(
                  type = "button",
                  class = "duel-pill-btn",
                  onclick = "document.getElementById('comp_player_a_search').value = 'Rashid Khan'; Shiny.setInputValue('comp_player_a_search', 'Rashid Khan'); document.getElementById('comp_player_b_search').value = 'Noor Ahmad'; Shiny.setInputValue('comp_player_b_search', 'Noor Ahmad');",
                  tags$span(class = "duel-tag", "🌪️ SPIN WIZARDS"),
                  tags$span(class = "duel-names", "Rashid Khan ", tags$b(style = "color: #38BDF8;", "vs"), " Noor Ahmad")
                ),
                tags$button(
                  type = "button",
                  class = "duel-pill-btn",
                  onclick = "document.getElementById('comp_player_a_search').value = 'Travis Head'; Shiny.setInputValue('comp_player_a_search', 'Travis Head'); document.getElementById('comp_player_b_search').value = 'Heinrich Klaasen'; Shiny.setInputValue('comp_player_b_search', 'Heinrich Klaasen');",
                  tags$span(class = "duel-tag", "🚀 POWER DEMOLISHERS"),
                  tags$span(class = "duel-names", "Travis Head ", tags$b(style = "color: #38BDF8;", "vs"), " Heinrich Klaasen")
                ),
                tags$button(
                  type = "button",
                  class = "duel-pill-btn",
                  onclick = "document.getElementById('comp_player_a_search').value = 'Rohit Sharma'; Shiny.setInputValue('comp_player_a_search', 'Rohit Sharma'); document.getElementById('comp_player_b_search').value = 'KL Rahul'; Shiny.setInputValue('comp_player_b_search', 'KL Rahul');",
                  tags$span(class = "duel-tag", "⭐ CLASSIC MASTERS"),
                  tags$span(class = "duel-names", "Rohit Sharma ", tags$b(style = "color: #38BDF8;", "vs"), " KL Rahul")
                )
              )
            ),
            # Selectors Bar
            div(
              class = "filter-panel",
              div(
                style = "flex: 1; min-width: 280px;",
                tags$label("Player A (Type to Search)", style = "font-weight: 700; color: #38BDF8; margin-bottom: 6px; display: block;"),
                div(
                  class = "player-search-bar-wrap",
                  div(
                    class = "search-input-group",
                    span(class = "search-icon", "🏏"),
                    tags$input(
                      id = "comp_player_a_search",
                      type = "text",
                      class = "player-search-control",
                      placeholder = "Type Player A (e.g. Kohli, Rohit, Gill)...",
                      value = "Sai Sudharsan",
                      list = "ipl_comp_datalist_a",
                      autocomplete = "off",
                      oninput = "Shiny.setInputValue('comp_player_a_search', this.value);"
                    ),
                    tags$button(
                      type = "button",
                      class = "search-clear-btn",
                      title = "Clear Player A",
                      onclick = "document.getElementById('comp_player_a_search').value = ''; Shiny.setInputValue('comp_player_a_search', ''); document.getElementById('comp_player_a_search').focus();",
                      "✕"
                    ),
                    tags$datalist(
                      id = "ipl_comp_datalist_a",
                      lapply(all_search_suggestions, function(p) tags$option(value = p))
                    )
                  ),
                  # Quick Pick Player A Chips
                  div(
                    class = "quick-chips-wrap",
                    style = "margin-top: 8px;",
                    span(style = "font-size: 0.76rem; font-weight: 700; color: #94A3B8; margin-right: 4px; align-self: center;", "⚡ Quick Pick:"),
                    lapply(
                      c("Virat Kohli", "Shubman Gill", "Jasprit Bumrah", "Sai Sudharsan", "Suryakumar Yadav", "Rashid Khan"),
                      function(star) {
                        tags$button(
                          type = "button",
                          class = "quick-chip-btn",
                          onclick = sprintf("document.getElementById('comp_player_a_search').value = '%s'; Shiny.setInputValue('comp_player_a_search', '%s');", star, star),
                          star
                        )
                      }
                    )
                  )
                ),
                # Hidden element for test backwards-compatibility
                tags$div(style = "display: none;", selectizeInput("comp_player_a", label = NULL, choices = all_players, selected = "Sai Sudharsan"))
              ),
              div(
                style = "flex: 1; min-width: 280px;",
                tags$label("Player B (Type to Search)", style = "font-weight: 700; color: #FDE047; margin-bottom: 6px; display: block;"),
                div(
                  class = "player-search-bar-wrap",
                  div(
                    class = "search-input-group",
                    span(class = "search-icon", "⚡"),
                    tags$input(
                      id = "comp_player_b_search",
                      type = "text",
                      class = "player-search-control",
                      placeholder = "Type Player B (e.g. Bumrah, Noor, Head)...",
                      value = "Suryakumar Yadav",
                      list = "ipl_comp_datalist_b",
                      autocomplete = "off",
                      oninput = "Shiny.setInputValue('comp_player_b_search', this.value);"
                    ),
                    tags$button(
                      type = "button",
                      class = "search-clear-btn",
                      title = "Clear Player B",
                      onclick = "document.getElementById('comp_player_b_search').value = ''; Shiny.setInputValue('comp_player_b_search', ''); document.getElementById('comp_player_b_search').focus();",
                      "✕"
                    ),
                    tags$datalist(
                      id = "ipl_comp_datalist_b",
                      lapply(all_search_suggestions, function(p) tags$option(value = p))
                    )
                  ),
                  # Dynamic Competing Rivals for Player B
                  uiOutput("comp_player_b_suggestions_ui")
                ),
                # Hidden element for test backwards-compatibility
                tags$div(style = "display: none;", selectizeInput("comp_player_b", label = NULL, choices = all_players, selected = "Suryakumar Yadav"))
              ),
              div(
                style = "min-width: 220px;",
                tags$label("Comparison Mode", style = "font-weight: 700; color: #CBD5E1; margin-bottom: 6px; display: block;"),
                radioGroupButtons(
                  inputId = "comp_mode",
                  label = NULL,
                  choices = c("Batting Duel" = "batting", "Bowling Duel" = "bowling"),
                  selected = "batting",
                  justified = TRUE,
                  status = "primary"
                )
              )
            ),
            
            # Validation Warning / Advisory
            uiOutput("comp_validation_ui"),
            
            # Comparison Studio Content
            uiOutput("comp_content_ui")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 3. SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Synchronize Navigation Buttons with hidden TabsetPanel
  observeEvent(input$nav_menu, {
    updateTabsetPanel(session, "main_tabs", selected = input$nav_menu)
  })
  
  # Reactive Player Resolver for Player Analytics (Supports instant typing & programmatic input)
  get_active_player <- reactive({
    query <- if (!is.null(input$player_search) && trimws(input$player_search) != "") {
      input$player_search
    } else if (!is.null(input$player_select) && trimws(input$player_select) != "") {
      input$player_select
    } else {
      "Sai Sudharsan"
    }
    raw_q <- trimws(as.character(query))
    resolved <- resolve_player(raw_q)
    list(raw = raw_q, resolved = resolved)
  })
  
  # Reactive Player Resolver for Comparison Studio
  get_comp_players <- reactive({
    qa <- if (!is.null(input$comp_player_a_search) && trimws(input$comp_player_a_search) != "") {
      input$comp_player_a_search
    } else if (!is.null(input$comp_player_a) && trimws(input$comp_player_a) != "") {
      input$comp_player_a
    } else {
      "Sai Sudharsan"
    }
    
    qb <- if (!is.null(input$comp_player_b_search) && trimws(input$comp_player_b_search) != "") {
      input$comp_player_b_search
    } else if (!is.null(input$comp_player_b) && trimws(input$comp_player_b) != "") {
      input$comp_player_b
    } else {
      "Suryakumar Yadav"
    }
    
    raw_a <- trimws(as.character(qa))
    raw_b <- trimws(as.character(qb))
    
    list(
      raw_a = raw_a,
      resolved_a = resolve_player(raw_a),
      raw_b = raw_b,
      resolved_b = resolve_player(raw_b)
    )
  })
  
  # Dynamic Header Title and Subtitle based on active page
  output$header_title_ui <- renderUI({
    switch(
      input$nav_menu,
      "overview" = tagList(
        tags$h1("IPL 2025 Season Overview"),
        tags$p("Comprehensive tournament benchmarks, franchise standings and award contenders")
      ),
      "player" = {
        act <- get_active_player()
        p_name <- if (!is.null(act$resolved)) {
          get_player_cricbuzz_meta(act$resolved)$full_name
        } else {
          act$raw
        }
        tagList(
          tags$h1(paste("Player Analytics —", p_name)),
          tags$p("Deep-dive ball-by-ball performance metrics, Cricbuzz profile and match breakdowns")
        )
      },
      "team" = tagList(
        tags$h1(paste("Franchise Intelligence —", req(input$team_select))),
        tags$p("Team scoring rates, bowling efficiency, and roster leaderboards")
      ),
      "comparison" = tagList(
        tags$h1("Player Comparison Studio"),
        tags$p("Head-to-head performance radar, metric duels, and Cricbuzz intelligence")
      )
    )
  })
  
  # ============================================================================
  # PAGE 1: OVERVIEW SERVER OUTPUTS
  # ============================================================================
  
  # Overview Chart A: Runs Distribution by Innings & Phase (Lighter Colors & High Contrast)
  output$plot_overview_runs_dist <- renderPlotly({
    phase_data <- ipl %>%
      filter(innings %in% c(1, 2)) %>%
      group_by(innings, over_phase) %>%
      summarise(runs = sum(runs_of_bat + extras, na.rm = TRUE), .groups = "drop") %>%
      mutate(
        innings_label = paste("Innings", innings),
        over_phase = factor(over_phase, levels = c("Powerplay (1-6)", "Middle (7-15)", "Death (16-20)"))
      )
    
    p <- plot_ly(
      data = phase_data,
      x = ~over_phase,
      y = ~runs,
      color = ~innings_label,
      colors = c("#38BDF8", "#FDE047"), # Lighter Sky Cyan and Lighter Sun Gold
      type = "bar",
      marker = list(line = list(color = "rgba(255, 255, 255, 0.25)", width = 1))
    ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Match Phase"),
        yaxis = list(title = "Total Runs Scored")
      )
    
    light_plotly_layout(p, title = "")
  })
  
  # Overview Chart B: Franchise Total Runs Horizontal Bar (With Lighter Franchise Colors)
  output$plot_overview_team_scoring <- renderPlotly({
    team_data <- team_summary %>%
      arrange(runs_scored) %>%
      mutate(team = factor(team, levels = team))
    
    p <- plot_ly(
      data = team_data,
      x = ~runs_scored,
      y = ~team,
      color = ~team,
      colors = team_colors,
      type = "bar",
      orientation = "h",
      marker = list(
        line = list(color = "rgba(255, 255, 255, 0.25)", width = 1)
      ),
      hovertemplate = "<b>%{y}</b><br>Total Runs: %{x}<br>Matches: %{customdata[0]}<br>Run Rate: %{customdata[1]}<extra></extra>",
      customdata = cbind(team_data$matches, team_data$run_rate)
    ) %>%
      layout(
        xaxis = list(title = "Cumulative Runs Scored"),
        yaxis = list(title = "")
      )
    
    light_plotly_layout(p, title = "", showlegend = FALSE)
  })
  
  # Overview Table C: Top 5 Batsmen (With Franchise Logos & Protected Borders)
  output$table_top_batsmen <- renderDT({
    top5_bat <- batting_summary %>%
      head(5) %>%
      transmute(
        Rank = row_number(),
        Player = striker,
        Franchise = sapply(teams, team_logo_html, size = 20),
        Runs = runs,
        Balls = balls,
        `SR` = format(strike_rate, nsmall = 1),
        `Avg` = ifelse(is.na(batting_avg), paste0(runs, "*"), format(batting_avg, nsmall = 1)),
        Boundaries = paste0(fours, "x4 · ", sixes, "x6")
      )
    
    datatable(
      top5_bat,
      escape = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  }, server = FALSE)
  
  # Overview Table D: Top 5 Bowlers (With Franchise Logos & Protected Borders)
  output$table_top_bowlers <- renderDT({
    top5_bowl <- bowling_summary %>%
      head(5) %>%
      transmute(
        Rank = row_number(),
        Bowler = bowler,
        Franchise = sapply(teams, team_logo_html, size = 20),
        Wickets = wickets,
        Overs = overs_display,
        Runs = runs_conceded,
        Economy = format(economy, nsmall = 2),
        `Avg` = ifelse(is.na(bowling_avg), "—", format(bowling_avg, nsmall = 1)),
        `Dot %` = paste0(dot_pct, "%")
      )
    
    datatable(
      top5_bowl,
      escape = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  }, server = FALSE)
  
  # Overview Table E: Team Rankings Standings (With Team Logos)
  output$table_team_rankings <- renderDT({
    team_table <- team_summary %>%
      transmute(
        Rank = row_number(),
        Franchise = sapply(team, team_logo_html, size = 24),
        Matches = matches,
        `Runs Scored` = runs_scored,
        `Balls Faced` = balls_faced,
        `Run Rate` = format(run_rate, nsmall = 2),
        `Wickets Taken` = wickets_taken,
        `Runs Conceded` = runs_conceded
      )
    
    datatable(
      team_table,
      escape = FALSE,
      options = list(
        pageLength = 10,
        dom = "tp",
        ordering = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  }, server = FALSE)
  
  # ============================================================================
  # PAGE 2: PLAYER ANALYTICS SERVER OUTPUTS
  # ============================================================================
  
  output$player_analytics_ui <- renderUI({
    req(input$player_category)
    act <- get_active_player()
    cat_type <- input$player_category
    
    # Check if player was not part of IPL 2025
    if (is.null(act$resolved)) {
      return(
        div(
          class = "player-not-found-banner",
          div(class = "not-found-icon-wrap", "⚠️"),
          div(
            tags$h3(style = "color: #EF4444; margin: 0 0 6px 0; font-size: 1.25rem; font-weight: 800;", "Player Not Available in IPL 2025"),
            tags$p(
              style = "color: #CBD5E1; margin: 0 0 10px 0; font-size: 0.95rem; line-height: 1.5;",
              tags$strong(paste0("'", act$raw, "'")), " was not available or did not play in the IPL 2025 season."
            ),
            div(
              class = "not-found-badge",
              "❌ No delivery records found for this player in the official 2025 tournament dataset."
            ),
            div(
              style = "margin-top: 14px; color: #94A3B8; font-size: 0.84rem;",
              "💡 Tip: Try searching for active 2025 stars like ",
              tags$span(style = "color: #38BDF8; font-weight: 600;", "Virat Kohli, Sai Sudharsan, Suryakumar Yadav, Jasprit Bumrah, Travis Head, Heinrich Klaasen, Nicholas Pooran, or Prasidh.")
            )
          )
        )
      )
    }
    
    selected_p <- act$resolved
    meta <- get_player_cricbuzz_meta(selected_p)
    p_team_code <- tolower(trimws(strsplit(meta$franchise, ",")[[1]][1]))
    p_team_logo <- get_team_logo_path(p_team_code)
    
    # Cricbuzz Player Profile Card
    cricbuzz_card <- div(
      class = "cricbuzz-profile-card",
      div(
        class = "cricbuzz-header",
        if (!is.null(p_team_logo)) tags$img(src = p_team_logo, class = "cricbuzz-team-logo", alt = meta$franchise) else NULL,
        div(
          class = "cricbuzz-title-group",
          div(class = "cricbuzz-player-name", meta$full_name),
          div(class = "cricbuzz-franchise-sub", paste0("IPL 2025 Franchise: ", meta$franchise))
        )
      ),
      div(
        class = "cricbuzz-specs-grid",
        div(
          class = "cricbuzz-spec-item",
          div(class = "spec-label", "Role"),
          div(class = "spec-val role", meta$role)
        ),
        div(
          class = "cricbuzz-spec-item",
          div(class = "spec-label", "Batting Style"),
          div(class = "spec-val", meta$bat_style)
        ),
        div(
          class = "cricbuzz-spec-item",
          div(class = "spec-label", "Bowling Style"),
          div(class = "spec-val", meta$bowl_style)
        ),
        div(
          class = "cricbuzz-spec-item",
          div(class = "spec-label", "IPL 2025 Squad"),
          div(class = "spec-val", meta$franchise)
        )
      )
    )
    
    if (cat_type == "batting") {
      p_bat <- batting_summary %>% filter(striker == selected_p)
      
      if (nrow(p_bat) == 0 || p_bat$balls == 0) {
        return(
          tagList(
            cricbuzz_card,
            div(
              class = "empty-state",
              div(class = "empty-state-icon", "🏏"),
              tags$h3(paste("No Batting Records for", meta$full_name)),
              tags$p("This player has not faced any legal deliveries in the 2025 season. Switch to Bowling Analysis above if they are a specialist bowler.")
            )
          )
        )
      }
      
      tagList(
        cricbuzz_card,
        # 8 Batting KPI Cards
        div(
          class = "kpi-grid",
          div(
            class = "kpi-card kpi-primary",
            div(class = "kpi-header", span(class = "kpi-title", "Total Runs"), div(class = "kpi-icon-wrap", "🏏")),
            div(class = "kpi-value", format(p_bat$runs, big.mark = ",")),
            div(
              class = "kpi-subtitle",
              if (!is.null(p_team_logo)) tags$img(src = p_team_logo, class = "team-logo-icon", style = "width: 18px; height: 18px; vertical-align: middle; margin-right: 6px;"),
              paste("Franchise:", p_bat$teams)
            )
          ),
          div(
            class = "kpi-card kpi-secondary",
            div(class = "kpi-header", span(class = "kpi-title", "Balls Faced"), div(class = "kpi-icon-wrap", "⚡")),
            div(class = "kpi-value", format(p_bat$balls, big.mark = ",")),
            div(class = "kpi-subtitle", "Legal deliveries faced")
          ),
          div(
            class = "kpi-card kpi-positive",
            div(class = "kpi-header", span(class = "kpi-title", "Batting Average"), div(class = "kpi-icon-wrap", "📊")),
            div(class = "kpi-value", p_bat$display_avg),
            div(class = "kpi-subtitle", paste(p_bat$dismissals, "dismissals in season"))
          ),
          div(
            class = "kpi-card kpi-purple",
            div(class = "kpi-header", span(class = "kpi-title", "Strike Rate"), div(class = "kpi-icon-wrap", "🚀")),
            div(class = "kpi-value", p_bat$strike_rate),
            div(class = "kpi-subtitle", "Runs per 100 legal balls")
          ),
          div(
            class = "kpi-card kpi-primary",
            div(class = "kpi-header", span(class = "kpi-title", "Fours (4s)"), div(class = "kpi-icon-wrap", "🔥")),
            div(class = "kpi-value", p_bat$fours),
            div(class = "kpi-subtitle", paste(p_bat$fours * 4, "runs from boundaries"))
          ),
          div(
            class = "kpi-card kpi-secondary",
            div(class = "kpi-header", span(class = "kpi-title", "Sixes (6s)"), div(class = "kpi-icon-wrap", "💥")),
            div(class = "kpi-value", p_bat$sixes),
            div(class = "kpi-subtitle", paste(p_bat$sixes * 6, "runs over ropes"))
          ),
          div(
            class = "kpi-card kpi-positive",
            div(class = "kpi-header", span(class = "kpi-title", "Boundary Run %"), div(class = "kpi-icon-wrap", "🎯")),
            div(class = "kpi-value", paste0(p_bat$boundary_pct, "%")),
            div(class = "kpi-subtitle", paste0(p_bat$fours * 4 + p_bat$sixes * 6, " / ", p_bat$runs, " boundary runs"))
          ),
          div(
            class = "kpi-card kpi-purple",
            div(class = "kpi-header", span(class = "kpi-title", "Matches Played"), div(class = "kpi-icon-wrap", "🏟️")),
            div(class = "kpi-value", p_bat$matches),
            div(class = "kpi-subtitle", "Appearances with batting impact")
          )
        ),
        
        # Batting Visualizations Row 1 (Light & Spacious)
        fluidRow(
          column(
            width = 7,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Match-by-Match Runs Scored"),
                  tags$p("Runs scored and balls faced in each match")
                ),
                span(class = "card-action-pill", "Match Trend")
              ),
              withSpinner(plotlyOutput("plot_player_bat_match_trend", height = "320px"), type = 4, color = "#0090DF")
            )
          ),
          column(
            width = 5,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Scoring Breakdown"),
                  tags$p("Proportion of runs by boundaries vs running")
                ),
                span(class = "card-action-pill", "Run Composition")
              ),
              withSpinner(plotlyOutput("plot_player_bat_runs_donut", height = "320px"), type = 4, color = "#0090DF")
            )
          )
        ),
        
        # Batting Visualizations Row 2
        fluidRow(
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Performance by Innings (1st vs 2nd)"),
                  tags$p("Target setting vs chasing runs and strike rate")
                ),
                span(class = "card-action-pill", "Innings Split")
              ),
              withSpinner(plotlyOutput("plot_player_bat_innings", height = "280px"), type = 4, color = "#0090DF")
            )
          ),
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Runs by Match Phase"),
                  tags$p("Scoring contribution in Powerplay, Middle, and Death overs")
                ),
                span(class = "card-action-pill", "Phase Progression")
              ),
              withSpinner(plotlyOutput("plot_player_bat_phase", height = "280px"), type = 4, color = "#0090DF")
            )
          )
        ),
        
        # Match-by-match Batting Log Table
        div(
          class = "analytics-card",
          div(
            class = "analytics-card-header",
            div(
              class = "card-title-group",
              tags$h2("Detailed Match-by-Match Batting Log"),
              tags$p("Full scorecard history across all appearances")
            ),
            span(class = "card-action-pill", "Innings Log")
          ),
          div(
            class = "table-responsive-container",
            DTOutput("table_player_bat_log")
          )
        )
      )
    } else {
      # Bowling Analysis
      p_bowl <- bowling_summary %>% filter(bowler == selected_p)
      
      if (nrow(p_bowl) == 0 || p_bowl$legal_balls == 0) {
        return(
          tagList(
            cricbuzz_card,
            div(
              class = "empty-state",
              div(class = "empty-state-icon", "🎯"),
              tags$h3(paste("No Bowling Records for", meta$full_name)),
              tags$p("This player has not bowled any legal deliveries in the 2025 season. Switch to Batting Analysis above if they are a specialist batter.")
            )
          )
        )
      }
      
      tagList(
        cricbuzz_card,
        # 7 Bowling KPI Cards
        div(
          class = "kpi-grid",
          div(
            class = "kpi-card kpi-purple",
            div(class = "kpi-header", span(class = "kpi-title", "Total Wickets"), div(class = "kpi-icon-wrap", "🎯")),
            div(class = "kpi-value", p_bowl$wickets),
            div(
              class = "kpi-subtitle",
              if (!is.null(get_team_logo_path(p_team_code))) tags$img(src = get_team_logo_path(p_team_code), class = "team-logo-icon", style = "width: 18px; height: 18px; vertical-align: middle; margin-right: 6px;"),
              paste("Franchise:", p_bowl$teams)
            )
          ),
          div(
            class = "kpi-card kpi-primary",
            div(class = "kpi-header", span(class = "kpi-title", "Runs Conceded"), div(class = "kpi-icon-wrap", "🏏")),
            div(class = "kpi-value", format(p_bowl$runs_conceded, big.mark = ",")),
            div(class = "kpi-subtitle", "Excludes byes & leg-byes")
          ),
          div(
            class = "kpi-card kpi-secondary",
            div(class = "kpi-header", span(class = "kpi-title", "Overs Bowled"), div(class = "kpi-icon-wrap", "⚡")),
            div(class = "kpi-value", p_bowl$overs_display),
            div(class = "kpi-subtitle", paste(p_bowl$legal_balls, "legal balls bowled"))
          ),
          div(
            class = "kpi-card kpi-positive",
            div(class = "kpi-header", span(class = "kpi-title", "Economy Rate"), div(class = "kpi-icon-wrap", "📉")),
            div(class = "kpi-value", format(p_bowl$economy, nsmall = 2)),
            div(class = "kpi-subtitle", "Runs per legal 6-ball over")
          ),
          div(
            class = "kpi-card kpi-primary",
            div(class = "kpi-header", span(class = "kpi-title", "Bowling Average"), div(class = "kpi-icon-wrap", "📊")),
            div(class = "kpi-value", ifelse(is.na(p_bowl$bowling_avg), "—", format(p_bowl$bowling_avg, nsmall = 1))),
            div(class = "kpi-subtitle", "Runs conceded per wicket")
          ),
          div(
            class = "kpi-card kpi-secondary",
            div(class = "kpi-header", span(class = "kpi-title", "Bowling Strike Rate"), div(class = "kpi-icon-wrap", "🚀")),
            div(class = "kpi-value", ifelse(is.na(p_bowl$bowling_sr), "—", format(p_bowl$bowling_sr, nsmall = 1))),
            div(class = "kpi-subtitle", "Legal balls per wicket")
          ),
          div(
            class = "kpi-card kpi-positive",
            div(class = "kpi-header", span(class = "kpi-title", "Dot-Ball %"), div(class = "kpi-icon-wrap", "🛡️")),
            div(class = "kpi-value", paste0(p_bowl$dot_pct, "%")),
            div(class = "kpi-subtitle", paste(p_bowl$dot_balls, "dot balls bowled"))
          )
        ),
        
        # Bowling Visualizations Row 1 (Light & Spacious)
        fluidRow(
          column(
            width = 7,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Match-by-Match Wickets & Runs"),
                  tags$p("Wickets taken and runs conceded in each appearance")
                ),
                span(class = "card-action-pill", "Spell Trend")
              ),
              withSpinner(plotlyOutput("plot_player_bowl_match_trend", height = "320px"), type = 4, color = "#0090DF")
            )
          ),
          column(
            width = 5,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Economy Rate Progression"),
                  tags$p("Economy rate per match against tournament benchmark (8.5)")
                ),
                span(class = "card-action-pill", "Economy")
              ),
              withSpinner(plotlyOutput("plot_player_bowl_economy", height = "320px"), type = 4, color = "#0090DF")
            )
          )
        ),
        
        # Bowling Visualizations Row 2
        fluidRow(
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Bowling by Innings (1st vs 2nd)"),
                  tags$p("Wickets and economy defending vs containing targets")
                ),
                span(class = "card-action-pill", "Innings Split")
              ),
              withSpinner(plotlyOutput("plot_player_bowl_innings", height = "280px"), type = 4, color = "#0090DF")
            )
          ),
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Wickets by Match Phase"),
                  tags$p("Breakthrough distribution across Powerplay, Middle, and Death overs")
                ),
                span(class = "card-action-pill", "Phase Impact")
              ),
              withSpinner(plotlyOutput("plot_player_bowl_phase", height = "280px"), type = 4, color = "#0090DF")
            )
          )
        ),
        
        # Match-by-match Bowling Log Table
        div(
          class = "analytics-card",
          div(
            class = "analytics-card-header",
            div(
              class = "card-title-group",
              tags$h2("Detailed Match-by-Match Bowling Spells"),
              tags$p("Full spell records across all matches bowled")
            ),
            span(class = "card-action-pill", "Bowling Log")
          ),
          div(
            class = "table-responsive-container",
            DTOutput("table_player_bowl_log")
          )
        )
      )
    }
  })
  
  # Player Batting Charts (Luminous & Eye-Catching Lighter Elements)
  output$plot_player_bat_match_trend <- renderPlotly({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    p_data <- ipl %>%
      filter(striker == selected_p) %>%
      group_by(match_id, match_no, date, bowling_team) %>%
      summarise(
        runs = sum(runs_of_bat, na.rm = TRUE),
        balls = sum(wide == 0, na.rm = TRUE),
        fours = sum(runs_of_bat == 4, na.rm = TRUE),
        sixes = sum(runs_of_bat == 6, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(match_id) %>%
      mutate(match_label = paste0("M", match_no, " vs ", bowling_team))
    
    p <- plot_ly(data = p_data) %>%
      add_bars(
        x = ~match_label,
        y = ~runs,
        name = "Runs Scored",
        marker = list(color = "#38BDF8", line = list(color = "#7DD3FC", width = 1))
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines+markers",
        x = ~match_label,
        y = ~balls,
        name = "Balls Faced",
        line = list(color = "#FDE047", width = 2.5),
        marker = list(color = "#FDE047", size = 8)
      ) %>%
      layout(
        xaxis = list(title = "Match (Opponent)"),
        yaxis = list(title = "Count")
      )
    
    light_plotly_layout(p, title = "")
  })
  
  output$plot_player_bat_runs_donut <- renderPlotly({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    p_data <- ipl %>% filter(striker == selected_p)
    
    fours_runs <- sum(p_data$runs_of_bat == 4, na.rm = TRUE) * 4
    sixes_runs <- sum(p_data$runs_of_bat == 6, na.rm = TRUE) * 6
    running_runs <- sum(p_data$runs_of_bat[p_data$runs_of_bat %in% c(1, 2, 3)], na.rm = TRUE)
    
    donut_df <- data.frame(
      category = c("Fours (4s)", "Sixes (6s)", "Running (1s, 2s, 3s)"),
      runs = c(fours_runs, sixes_runs, running_runs)
    ) %>%
      filter(runs > 0)
    
    p <- plot_ly(
      donut_df,
      labels = ~category,
      values = ~runs,
      type = "pie",
      hole = 0.55,
      marker = list(
        colors = c("#38BDF8", "#FDE047", "#4ADE80"),
        line = list(color = "#141E33", width = 2)
      ),
      textinfo = "label+percent",
      textfont = list(color = "#FFFFFF", size = 11, family = "Inter, sans-serif")
    ) %>%
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#CBD5E1", family = "Inter, sans-serif"),
        showlegend = TRUE,
        legend = list(
          font = list(color = "#F8FAFC", size = 11),
          bgcolor = "rgba(16, 27, 46, 0.85)",
          bordercolor = "rgba(255, 255, 255, 0.12)"
        )
      )
    
    light_plotly_layout(p, title = "")
  })
  
  output$plot_player_bat_innings <- renderPlotly({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    inn_data <- ipl %>%
      filter(striker == selected_p, innings %in% c(1, 2)) %>%
      group_by(innings) %>%
      summarise(
        runs = sum(runs_of_bat, na.rm = TRUE),
        balls = sum(wide == 0, na.rm = TRUE),
        strike_rate = ifelse(balls > 0, round((runs / balls) * 100, 1), 0),
        .groups = "drop"
      ) %>%
      mutate(innings_label = paste("Innings", innings))
    
    p <- plot_ly(
      data = inn_data,
      x = ~innings_label,
      y = ~runs,
      type = "bar",
      name = "Runs",
      marker = list(color = "#38BDF8", line = list(color = "#7DD3FC", width = 1)),
      text = ~paste("SR:", strike_rate),
      textposition = "auto",
      textfont = list(color = "#FFFFFF", size = 11)
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Runs Scored")
      )
    
    light_plotly_layout(p, title = "", showlegend = FALSE)
  })
  
  output$plot_player_bat_phase <- renderPlotly({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    phase_data <- ipl %>%
      filter(striker == selected_p) %>%
      group_by(over_phase) %>%
      summarise(
        runs = sum(runs_of_bat, na.rm = TRUE),
        balls = sum(wide == 0, na.rm = TRUE),
        strike_rate = ifelse(balls > 0, round((runs / balls) * 100, 1), 0),
        .groups = "drop"
      ) %>%
      mutate(over_phase = factor(over_phase, levels = c("Powerplay (1-6)", "Middle (7-15)", "Death (16-20)")))
    
    p <- plot_ly(
      data = phase_data,
      x = ~over_phase,
      y = ~runs,
      type = "bar",
      marker = list(color = "#67E8F9", line = list(color = "#A5F3FC", width = 1)),
      text = ~paste("SR:", strike_rate),
      textposition = "auto",
      textfont = list(color = "#FFFFFF", size = 11)
    ) %>%
      layout(
        xaxis = list(title = "Match Phase"),
        yaxis = list(title = "Runs Scored")
      )
    
    light_plotly_layout(p, title = "", showlegend = FALSE)
  })
  
  output$table_player_bat_log <- renderDT({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    match_log <- ipl %>%
      filter(striker == selected_p) %>%
      group_by(match_id, match_no, date, venue, batting_team, bowling_team) %>%
      summarise(
        Runs = sum(runs_of_bat, na.rm = TRUE),
        Balls = sum(wide == 0, na.rm = TRUE),
        `4s` = sum(runs_of_bat == 4, na.rm = TRUE),
        `6s` = sum(runs_of_bat == 6, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        `Strike Rate` = ifelse(Balls > 0, round((Runs / Balls) * 100, 1), 0)
      ) %>%
      arrange(desc(match_id)) %>%
      transmute(
        `Match #` = match_no,
        Date = date,
        Opponent = sapply(bowling_team, team_logo_html, size = 18),
        Runs,
        Balls,
        `4s`,
        `6s`,
        `Strike Rate`
      )
    
    datatable(
      match_log,
      escape = FALSE,
      options = list(
        pageLength = 8,
        dom = "tp",
        ordering = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  }, server = FALSE)
  
  # Player Bowling Charts (Luminous & Eye-Catching Lighter Elements)
  output$plot_player_bowl_match_trend <- renderPlotly({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    b_data <- ipl %>%
      filter(bowler == selected_p) %>%
      group_by(match_id, match_no, date, batting_team) %>%
      summarise(
        wickets = sum(is_bowler_wicket, na.rm = TRUE),
        runs = sum(bowler_runs, na.rm = TRUE),
        legal_balls = sum(is_legal, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(match_id) %>%
      mutate(match_label = paste0("M", match_no, " vs ", batting_team))
    
    p <- plot_ly(data = b_data) %>%
      add_bars(
        x = ~match_label,
        y = ~wickets,
        name = "Wickets",
        marker = list(color = "#4ADE80", line = list(color = "#86EFAC", width = 1))
      ) %>%
      add_trace(
        type = "scatter",
        mode = "lines+markers",
        x = ~match_label,
        y = ~runs,
        name = "Runs Conceded",
        yaxis = "y2",
        line = list(color = "#FB7185", width = 2.5),
        marker = list(color = "#FB7185", size = 7)
      ) %>%
      layout(
        xaxis = list(title = "Match (Opponent)"),
        yaxis = list(title = "Wickets Taken", side = "left"),
        yaxis2 = list(title = "Runs Conceded", overlaying = "y", side = "right", showgrid = FALSE, tickfont = list(color = "#FB7185"), titlefont = list(color = "#FB7185"))
      )
    
    light_plotly_layout(p, title = "")
  })
  
  output$plot_player_bowl_economy <- renderPlotly({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    econ_data <- ipl %>%
      filter(bowler == selected_p) %>%
      group_by(match_id, match_no, batting_team) %>%
      summarise(
        runs = sum(bowler_runs, na.rm = TRUE),
        legal_balls = sum(is_legal, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(legal_balls > 0) %>%
      mutate(
        economy = round(runs / (legal_balls / 6), 2),
        match_label = paste0("M", match_no, " vs ", batting_team)
      )
    
    p <- plot_ly(data = econ_data) %>%
      add_trace(
        type = "scatter",
        mode = "lines+markers",
        x = ~match_label,
        y = ~economy,
        name = "Economy Rate",
        line = list(color = "#38BDF8", width = 2.5),
        marker = list(color = "#38BDF8", size = 7)
      ) %>%
      add_lines(
        x = ~match_label,
        y = 8.5,
        name = "Benchmark (8.5)",
        line = list(color = "#FDE047", dash = "dash", width = 2)
      ) %>%
      layout(
        xaxis = list(title = "Match"),
        yaxis = list(title = "Economy Rate (Runs/Over)")
      )
    
    light_plotly_layout(p, title = "")
  })
  
  output$plot_player_bowl_innings <- renderPlotly({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    inn_data <- ipl %>%
      filter(bowler == selected_p, innings %in% c(1, 2)) %>%
      group_by(innings) %>%
      summarise(
        wickets = sum(is_bowler_wicket, na.rm = TRUE),
        runs = sum(bowler_runs, na.rm = TRUE),
        legal_balls = sum(is_legal, na.rm = TRUE),
        economy = ifelse(legal_balls > 0, round(runs / (legal_balls / 6), 2), NA),
        .groups = "drop"
      ) %>%
      mutate(innings_label = paste("Innings", innings))
    
    p <- plot_ly(
      data = inn_data,
      x = ~innings_label,
      y = ~wickets,
      type = "bar",
      marker = list(color = "#38BDF8", line = list(color = "#7DD3FC", width = 1)),
      text = ~paste("Econ:", economy),
      textposition = "auto",
      textfont = list(color = "#FFFFFF", size = 11)
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Wickets Taken")
      )
    
    light_plotly_layout(p, title = "", showlegend = FALSE)
  })
  
  output$plot_player_bowl_phase <- renderPlotly({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    phase_data <- ipl %>%
      filter(bowler == selected_p) %>%
      group_by(over_phase) %>%
      summarise(
        wickets = sum(is_bowler_wicket, na.rm = TRUE),
        runs = sum(bowler_runs, na.rm = TRUE),
        legal_balls = sum(is_legal, na.rm = TRUE),
        economy = ifelse(legal_balls > 0, round(runs / (legal_balls / 6), 2), NA),
        .groups = "drop"
      ) %>%
      mutate(over_phase = factor(over_phase, levels = c("Powerplay (1-6)", "Middle (7-15)", "Death (16-20)")))
    
    p <- plot_ly(
      data = phase_data,
      x = ~over_phase,
      y = ~wickets,
      type = "bar",
      marker = list(color = "#4ADE80", line = list(color = "#86EFAC", width = 1)),
      text = ~paste("Econ:", economy),
      textposition = "auto",
      textfont = list(color = "#FFFFFF", size = 11)
    ) %>%
      layout(
        xaxis = list(title = "Match Phase"),
        yaxis = list(title = "Wickets Taken")
      )
    
    light_plotly_layout(p, title = "", showlegend = FALSE)
  })
  
  output$table_player_bowl_log <- renderDT({
    req(get_active_player()$resolved)
    selected_p <- get_active_player()$resolved
    bowl_log <- ipl %>%
      filter(bowler == selected_p) %>%
      group_by(match_id, match_no, date, venue, bowling_team, batting_team) %>%
      summarise(
        legal_balls = sum(is_legal, na.rm = TRUE),
        Wickets = sum(is_bowler_wicket, na.rm = TRUE),
        Runs = sum(bowler_runs, na.rm = TRUE),
        Dots = sum(is_dot, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        Overs = paste0(legal_balls %/% 6, ".", legal_balls %% 6),
        Economy = ifelse(legal_balls > 0, round(Runs / (legal_balls / 6), 2), 0)
      ) %>%
      arrange(desc(match_id)) %>%
      transmute(
        `Match #` = match_no,
        Date = date,
        Opponent = sapply(batting_team, team_logo_html, size = 18),
        Overs,
        Runs,
        Wickets,
        Dots,
        Economy
      )
    
    datatable(
      bowl_log,
      escape = FALSE,
      options = list(
        pageLength = 8,
        dom = "tp",
        ordering = TRUE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  }, server = FALSE)
  
  # ============================================================================
  # PAGE 3: TEAM ANALYTICS SERVER OUTPUTS (With Team Logo Header)
  # ============================================================================
  
  output$team_kpi_cards_ui <- renderUI({
    req(input$team_select)
    tm <- input$team_select
    t_stat <- team_summary %>% filter(team == tm)
    tm_lower <- tolower(tm)
    
    tagList(
      # Franchise Brand Header Banner (Using Authentic PNG Logo from www)
      div(
        class = "team-header-banner",
        if (!is.null(get_team_logo_path(tm))) tags$img(src = get_team_logo_path(tm), class = "team-header-logo", alt = tm),
        div(
          div(style = "font-size: 1.3rem; font-weight: 800; color: #FFFFFF;", paste(tm, "Franchise Intelligence")),
          div(style = "font-size: 0.82rem; color: #94A3B8;", "Season 2025 Performance • Official Squad Analytics & Phase Progression")
        )
      ),
      # Team KPI Cards
      div(
        class = "kpi-grid",
        div(
          class = "kpi-card kpi-primary",
          div(class = "kpi-header", span(class = "kpi-title", "Total Runs Scored"), div(class = "kpi-icon-wrap", "🏏")),
          div(class = "kpi-value", format(t_stat$runs_scored, big.mark = ",")),
          div(class = "kpi-subtitle", paste("Scored in", t_stat$matches_bat, "batting innings"))
        ),
        div(
          class = "kpi-card kpi-positive",
          div(class = "kpi-header", span(class = "kpi-title", "Total Wickets Taken"), div(class = "kpi-icon-wrap", "🎯")),
          div(class = "kpi-value", t_stat$wickets_taken),
          div(class = "kpi-subtitle", "Taken by bowling attack")
        ),
        div(
          class = "kpi-card kpi-secondary",
          div(class = "kpi-header", span(class = "kpi-title", "Matches Played"), div(class = "kpi-icon-wrap", "🏟️")),
          div(class = "kpi-value", t_stat$matches),
          div(class = "kpi-subtitle", "2025 season fixtures")
        ),
        div(
          class = "kpi-card kpi-purple",
          div(class = "kpi-header", span(class = "kpi-title", "Average Run Rate"), div(class = "kpi-icon-wrap", "⚡")),
          div(class = "kpi-value", format(t_stat$run_rate, nsmall = 2)),
          div(class = "kpi-subtitle", "Runs per 6 legal deliveries")
        )
      )
    )
  })
  
  output$plot_team_match_trend <- renderPlotly({
    req(input$team_select)
    tm <- input$team_select
    
    match_data <- ipl %>%
      filter(batting_team == tm) %>%
      group_by(match_id, match_no, bowling_team) %>%
      summarise(runs = sum(runs_of_bat + extras, na.rm = TRUE), .groups = "drop") %>%
      arrange(match_id) %>%
      mutate(match_label = paste0("M", match_no, " vs ", bowling_team))
    
    p <- plot_ly(data = match_data) %>%
      add_bars(
        x = ~match_label,
        y = ~runs,
        marker = list(color = "#38BDF8", line = list(color = "#7DD3FC", width = 1)),
        name = "Runs"
      ) %>%
      add_lines(
        x = ~match_label,
        y = mean(match_data$runs),
        line = list(color = "#FDE047", dash = "dash", width = 2),
        name = "Team Average"
      ) %>%
      layout(
        xaxis = list(title = "Match (Opponent)"),
        yaxis = list(title = "Runs Scored")
      )
    
    light_plotly_layout(p, title = "")
  })
  
  output$plot_team_innings_comp <- renderPlotly({
    req(input$team_select)
    tm <- input$team_select
    
    inn_runs <- ipl %>%
      filter(batting_team == tm, innings %in% c(1, 2)) %>%
      group_by(innings) %>%
      summarise(runs = sum(runs_of_bat + extras, na.rm = TRUE), .groups = "drop") %>%
      mutate(metric = "Runs Scored", innings_label = paste("Innings", innings))
    
    p <- plot_ly(
      data = inn_runs,
      x = ~innings_label,
      y = ~runs,
      type = "bar",
      name = "Runs Scored",
      marker = list(color = "#38BDF8", line = list(color = "#7DD3FC", width = 1))
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Total Runs")
      )
    
    light_plotly_layout(p, title = "", showlegend = FALSE)
  })
  
  output$plot_team_bat_phase <- renderPlotly({
    req(input$team_select)
    tm <- input$team_select
    
    p_data <- ipl %>%
      filter(batting_team == tm) %>%
      group_by(over_phase) %>%
      summarise(
        runs = sum(runs_of_bat + extras, na.rm = TRUE),
        balls = sum(wide == 0, na.rm = TRUE),
        run_rate = ifelse(balls > 0, round(runs / (balls / 6), 2), 0),
        .groups = "drop"
      ) %>%
      mutate(over_phase = factor(over_phase, levels = c("Powerplay (1-6)", "Middle (7-15)", "Death (16-20)")))
    
    p <- plot_ly(
      data = p_data,
      x = ~over_phase,
      y = ~runs,
      type = "bar",
      marker = list(color = "#67E8F9", line = list(color = "#A5F3FC", width = 1)),
      text = ~paste("RR:", run_rate),
      textposition = "auto",
      textfont = list(color = "#FFFFFF", size = 11)
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Runs Scored")
      )
    
    light_plotly_layout(p, title = "", showlegend = FALSE)
  })
  
  output$plot_team_bowl_phase <- renderPlotly({
    req(input$team_select)
    tm <- input$team_select
    
    p_data <- ipl %>%
      filter(bowling_team == tm) %>%
      group_by(over_phase) %>%
      summarise(
        wickets = sum(is_any_dismissal, na.rm = TRUE),
        runs = sum(runs_of_bat + extras, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(over_phase = factor(over_phase, levels = c("Powerplay (1-6)", "Middle (7-15)", "Death (16-20)")))
    
    p <- plot_ly(
      data = p_data,
      x = ~over_phase,
      y = ~wickets,
      type = "bar",
      marker = list(color = "#4ADE80", line = list(color = "#86EFAC", width = 1)),
      text = ~paste("Wkts:", wickets),
      textposition = "auto",
      textfont = list(color = "#FFFFFF", size = 11)
    ) %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Wickets Taken")
      )
    
    light_plotly_layout(p, title = "", showlegend = FALSE)
  })
  
  output$table_team_top_bat <- renderDT({
    req(input$team_select)
    tm <- input$team_select
    
    t_bat <- ipl %>%
      filter(batting_team == tm) %>%
      group_by(striker) %>%
      summarise(
        runs = sum(runs_of_bat, na.rm = TRUE),
        balls = sum(wide == 0, na.rm = TRUE),
        fours = sum(runs_of_bat == 4, na.rm = TRUE),
        sixes = sum(runs_of_bat == 6, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        strike_rate = ifelse(balls > 0, round((runs / balls) * 100, 1), 0)
      ) %>%
      arrange(desc(runs)) %>%
      head(3) %>%
      transmute(
        Rank = row_number(),
        Batsman = striker,
        Franchise = sapply(tm, team_logo_html, size = 18),
        Runs = runs,
        Balls = balls,
        `Strike Rate` = strike_rate,
        `4s` = fours,
        `6s` = sixes
      )
    
    datatable(
      t_bat,
      escape = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  }, server = FALSE)
  
  output$table_team_top_bowl <- renderDT({
    req(input$team_select)
    tm <- input$team_select
    
    t_bowl <- ipl %>%
      filter(bowling_team == tm) %>%
      group_by(bowler) %>%
      summarise(
        wickets = sum(is_bowler_wicket, na.rm = TRUE),
        runs = sum(bowler_runs, na.rm = TRUE),
        legal_balls = sum(is_legal, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        overs = paste0(legal_balls %/% 6, ".", legal_balls %% 6),
        economy = ifelse(legal_balls > 0, round(runs / (legal_balls / 6), 2), 0)
      ) %>%
      arrange(desc(wickets), economy) %>%
      head(3) %>%
      transmute(
        Rank = row_number(),
        Bowler = bowler,
        Franchise = sapply(tm, team_logo_html, size = 18),
        Wickets = wickets,
        Overs = overs,
        Runs = runs,
        Economy = economy
      )
    
    datatable(
      t_bowl,
      escape = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      rownames = FALSE,
      class = "compact stripe hover"
    )
  }, server = FALSE)
  
  # ============================================================================
  # PAGE 4: PLAYER COMPARISON STUDIO SERVER OUTPUTS (With Team Logos & Dynamic Intelligence)
  # ============================================================================
  
  # Dynamic Competing Rivals for Player B based on Player A's role
  output$comp_player_b_suggestions_ui <- renderUI({
    comp <- get_comp_players()
    p_a <- comp$resolved_a
    
    if (is.null(p_a)) {
      suggested_rivals <- c("Shubman Gill", "Suryakumar Yadav", "Virat Kohli", "Jasprit Bumrah", "Travis Head")
      p_name <- "IPL Stars"
    } else {
      meta_a <- get_player_cricbuzz_meta(p_a)
      p_name <- meta_a$full_name
      role_a <- meta_a$role
      
      if (grepl("Bowler", role_a, ignore.case = TRUE)) {
        candidates <- bowling_summary %>% 
          filter(bowler != p_a) %>% 
          pull(bowler)
        suggested_rivals <- head(unique(sapply(candidates, function(c) {
          if (c %in% names(cricbuzz_db)) cricbuzz_db[[c]]$full_name else c
        })), 5)
      } else if (grepl("Allrounder", role_a, ignore.case = TRUE)) {
        allrounders <- c("Hardik Pandya", "Andre Russell", "Sunil Narine", "Axar Patel", "Sam Curran", "Marcus Stoinis", "Glenn Maxwell", "Ravindra Jadeja")
        suggested_rivals <- head(setdiff(allrounders, p_name), 5)
      } else {
        candidates <- batting_summary %>% 
          filter(striker != p_a) %>% 
          pull(striker)
        suggested_rivals <- head(unique(sapply(candidates, function(c) {
          if (c %in% names(cricbuzz_db)) cricbuzz_db[[c]]$full_name else c
        })), 5)
      }
    }
    
    div(
      class = "suggested-rivals-wrap",
      div(
        class = "suggested-rivals-label",
        span("⚡ Competing Rivals for"),
        tags$b(style = "color: #38BDF8;", p_name),
        span(":")
      ),
      div(
        class = "rival-chips-row",
        lapply(suggested_rivals, function(rival) {
          tags$button(
            type = "button",
            class = "suggested-rival-btn",
            onclick = sprintf("document.getElementById('comp_player_b_search').value = '%s'; Shiny.setInputValue('comp_player_b_search', '%s');", rival, rival),
            rival
          )
        })
      )
    )
  })

  output$comp_validation_ui <- renderUI({
    comp <- get_comp_players()
    
    # Check if Player A is not in IPL 2025
    if (is.null(comp$resolved_a)) {
      return(
        div(
          class = "player-not-found-banner",
          style = "margin-bottom: 20px;",
          div(class = "not-found-icon-wrap", "⚠️"),
          div(
            tags$h4(style = "color: #EF4444; margin: 0 0 4px 0; font-weight: 700;", "Player A Not Available in IPL 2025"),
            tags$p(
              style = "color: #CBD5E1; margin: 0; font-size: 0.9rem;",
              tags$strong(paste0("'", comp$raw_a, "'")), " was not available or did not play in the IPL 2025 season. Please enter or select an active 2025 player."
            )
          )
        )
      )
    }
    
    # Check if Player B is not in IPL 2025
    if (is.null(comp$resolved_b)) {
      return(
        div(
          class = "player-not-found-banner",
          style = "margin-bottom: 20px;",
          div(class = "not-found-icon-wrap", "⚠️"),
          div(
            tags$h4(style = "color: #EF4444; margin: 0 0 4px 0; font-weight: 700;", "Player B Not Available in IPL 2025"),
            tags$p(
              style = "color: #CBD5E1; margin: 0; font-size: 0.9rem;",
              tags$strong(paste0("'", comp$raw_b, "'")), " was not available or did not play in the IPL 2025 season. Please enter or select an active 2025 player."
            )
          )
        )
      )
    }
    
    # Check if identical player selected
    if (comp$resolved_a == comp$resolved_b) {
      return(
        div(
          class = "alert alert-warning",
          style = "background: rgba(255, 184, 0, 0.15); border: 1px solid #FFB800; color: #FFB800; border-radius: 8px; padding: 12px 18px; margin-bottom: 20px;",
          tags$b("Notice: "), "You have selected the same player for both Player A and Player B. Please select two different players to view a head-to-head comparison."
        )
      )
    }
    
    NULL
  })
  
  output$comp_content_ui <- renderUI({
    req(input$comp_mode)
    comp <- get_comp_players()
    
    if (is.null(comp$resolved_a) || is.null(comp$resolved_b)) {
      return(
        div(
          class = "empty-state",
          div(class = "empty-state-icon", "⚔️"),
          tags$h3("Comparison Studio Awaiting Valid 2025 Players"),
          tags$p("Select or search for two active IPL 2025 players above to view radar models and head-to-head metrics.")
        )
      )
    }
    
    p_a <- comp$resolved_a
    p_b <- comp$resolved_b
    meta_a <- get_player_cricbuzz_meta(p_a)
    meta_b <- get_player_cricbuzz_meta(p_b)
    mode <- input$comp_mode
    
    # Accurate Team Logos and Franchises
    first_team_a <- tolower(trimws(strsplit(meta_a$franchise, ",")[[1]][1]))
    first_team_b <- tolower(trimws(strsplit(meta_b$franchise, ",")[[1]][1]))
    logo_a <- get_team_logo_path(first_team_a)
    logo_b <- get_team_logo_path(first_team_b)
    
    # Complete Data Retrieval for both Batting and Bowling
    bat_stat_a <- batting_summary %>% filter(striker == p_a)
    bat_stat_b <- batting_summary %>% filter(striker == p_b)
    bowl_stat_a <- bowling_summary %>% filter(bowler == p_a)
    bowl_stat_b <- bowling_summary %>% filter(bowler == p_b)
    
    runs_a <- if (nrow(bat_stat_a) > 0) bat_stat_a$runs else 0
    balls_a <- if (nrow(bat_stat_a) > 0) bat_stat_a$balls else 0
    sr_a <- if (nrow(bat_stat_a) > 0) bat_stat_a$strike_rate else 0
    avg_a <- if (nrow(bat_stat_a) > 0 && !is.na(bat_stat_a$batting_avg)) bat_stat_a$batting_avg else 0
    fours_a <- if (nrow(bat_stat_a) > 0) bat_stat_a$fours else 0
    sixes_a <- if (nrow(bat_stat_a) > 0) bat_stat_a$sixes else 0
    bpct_a <- if (nrow(bat_stat_a) > 0) bat_stat_a$boundary_pct else 0
    matches_bat_a <- if (nrow(bat_stat_a) > 0) bat_stat_a$matches else 0
    
    runs_b <- if (nrow(bat_stat_b) > 0) bat_stat_b$runs else 0
    balls_b <- if (nrow(bat_stat_b) > 0) bat_stat_b$balls else 0
    sr_b <- if (nrow(bat_stat_b) > 0) bat_stat_b$strike_rate else 0
    avg_b <- if (nrow(bat_stat_b) > 0 && !is.na(bat_stat_b$batting_avg)) bat_stat_b$batting_avg else 0
    fours_b <- if (nrow(bat_stat_b) > 0) bat_stat_b$fours else 0
    sixes_b <- if (nrow(bat_stat_b) > 0) bat_stat_b$sixes else 0
    bpct_b <- if (nrow(bat_stat_b) > 0) bat_stat_b$boundary_pct else 0
    matches_bat_b <- if (nrow(bat_stat_b) > 0) bat_stat_b$matches else 0
    
    wkts_a <- if (nrow(bowl_stat_a) > 0) bowl_stat_a$wickets else 0
    runs_conceded_a <- if (nrow(bowl_stat_a) > 0) bowl_stat_a$runs_conceded else 0
    legal_balls_a <- if (nrow(bowl_stat_a) > 0) bowl_stat_a$legal_balls else 0
    overs_a <- if (nrow(bowl_stat_a) > 0) bowl_stat_a$overs_display else "0.0"
    econ_a <- if (nrow(bowl_stat_a) > 0 && !is.na(bowl_stat_a$economy)) bowl_stat_a$economy else NA
    bowl_avg_a <- if (nrow(bowl_stat_a) > 0 && !is.na(bowl_stat_a$bowling_avg)) bowl_stat_a$bowling_avg else NA
    bowl_sr_a <- if (nrow(bowl_stat_a) > 0 && !is.na(bowl_stat_a$bowling_sr)) bowl_stat_a$bowling_sr else NA
    dot_a <- if (nrow(bowl_stat_a) > 0) bowl_stat_a$dot_pct else 0
    matches_bowl_a <- if (nrow(bowl_stat_a) > 0) bowl_stat_a$matches else 0
    
    wkts_b <- if (nrow(bowl_stat_b) > 0) bowl_stat_b$wickets else 0
    runs_conceded_b <- if (nrow(bowl_stat_b) > 0) bowl_stat_b$runs_conceded else 0
    legal_balls_b <- if (nrow(bowl_stat_b) > 0) bowl_stat_b$legal_balls else 0
    overs_b <- if (nrow(bowl_stat_b) > 0) bowl_stat_b$overs_display else "0.0"
    econ_b <- if (nrow(bowl_stat_b) > 0 && !is.na(bowl_stat_b$economy)) bowl_stat_b$economy else NA
    bowl_avg_b <- if (nrow(bowl_stat_b) > 0 && !is.na(bowl_stat_b$bowling_avg)) bowl_stat_b$bowling_avg else NA
    bowl_sr_b <- if (nrow(bowl_stat_b) > 0 && !is.na(bowl_stat_b$bowling_sr)) bowl_stat_b$bowling_sr else NA
    dot_b <- if (nrow(bowl_stat_b) > 0) bowl_stat_b$dot_pct else 0
    matches_bowl_b <- if (nrow(bowl_stat_b) > 0) bowl_stat_b$matches else 0
    
    both_batters <- (legal_balls_a == 0 && legal_balls_b == 0)
    both_bowlers <- (balls_a == 0 && balls_b == 0)
    
    if (mode == "batting") {
      if (both_bowlers) {
        wins_a <- 0
        wins_b <- 0
      } else if (balls_a > 0 && balls_b == 0) {
        wins_a <- 5
        wins_b <- 0
      } else if (balls_b > 0 && balls_a == 0) {
        wins_a <- 0
        wins_b <- 5
      } else {
        wins_a <- sum(c(runs_a > runs_b, sr_a > sr_b, avg_a > avg_b, (fours_a + sixes_a) > (fours_b + sixes_b), bpct_a > bpct_b))
        wins_b <- sum(c(runs_b > runs_a, sr_b > sr_a, avg_b > avg_a, (fours_b + sixes_b) > (fours_a + sixes_a), bpct_b > bpct_a))
      }
      
      tagList(
        div(
          class = "comparison-grid",
          # Player A Card
          div(
            class = paste("player-card", ifelse(wins_a > wins_b, "winner", "")),
            if (wins_a > wins_b) span(class = "leader-pill", "🏆 DUEL LEADER"),
            div(
              class = "player-card-header-row",
              if (!is.null(logo_a)) tags$img(src = logo_a, class = "player-card-team-logo", alt = meta_a$franchise) else NULL,
              div(
                tags$h3(style = "color: #38BDF8; margin: 0;", meta_a$full_name),
                tags$p(style = "color: #94A3B8; font-size: 0.85rem; margin: 2px 0 0 0;", paste("Franchise:", meta_a$franchise)),
                div(
                  style = "display: flex; gap: 6px; flex-wrap: wrap; margin-top: 6px;",
                  span(style = "background: rgba(0, 144, 223, 0.2); border: 1px solid rgba(0, 144, 223, 0.4); color: #38BDF8; font-size: 0.72rem; padding: 2px 8px; border-radius: 4px; font-weight: 600;", meta_a$role),
                  span(style = "background: rgba(255, 255, 255, 0.08); color: #CBD5E1; font-size: 0.72rem; padding: 2px 8px; border-radius: 4px;", meta_a$bat_style)
                ),
                if (both_bowlers || balls_a == 0) span(class = "specialist-badge", "Specialist Bowler • Primary Bowling Focus") else NULL
              )
            ),
            div(class = "metric-row", span(class = "metric-label", "Total Runs"), span(class = paste("metric-val", ifelse(runs_a >= runs_b && balls_a > 0, "lead", "")), runs_a)),
            div(class = "metric-row", span(class = "metric-label", "Balls Faced"), span(class = paste("metric-val", ifelse(balls_a >= balls_b && balls_a > 0, "lead", "")), balls_a)),
            div(class = "metric-row", span(class = "metric-label", "Batting Average"), span(class = paste("metric-val", ifelse(avg_a >= avg_b && avg_a > 0, "lead", "")), ifelse(avg_a > 0, avg_a, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Strike Rate"), span(class = paste("metric-val", ifelse(sr_a >= sr_b && balls_a > 0, "lead", "")), ifelse(balls_a > 0, sr_a, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Boundaries (4s / 6s)"), span(class = paste("metric-val", ifelse(fours_a + sixes_a >= fours_b + sixes_b && balls_a > 0, "lead", "")), paste0(fours_a, " / ", sixes_a))),
            div(class = "metric-row", span(class = "metric-label", "Boundary %"), span(class = paste("metric-val", ifelse(bpct_a >= bpct_b && balls_a > 0, "lead", "")), paste0(bpct_a, "%")))
          ),
          
          # Central VS Badge
          div(class = "vs-badge-container", div(class = "vs-badge", "VS")),
          
          # Player B Card
          div(
            class = paste("player-card", ifelse(wins_b > wins_a, "winner", "")),
            if (wins_b > wins_a) span(class = "leader-pill", "🏆 DUEL LEADER"),
            div(
              class = "player-card-header-row",
              if (!is.null(logo_b)) tags$img(src = logo_b, class = "player-card-team-logo", alt = meta_b$franchise) else NULL,
              div(
                tags$h3(style = "color: #FDE047; margin: 0;", meta_b$full_name),
                tags$p(style = "color: #94A3B8; font-size: 0.85rem; margin: 2px 0 0 0;", paste("Franchise:", meta_b$franchise)),
                div(
                  style = "display: flex; gap: 6px; flex-wrap: wrap; margin-top: 6px;",
                  span(style = "background: rgba(253, 224, 71, 0.2); border: 1px solid rgba(253, 224, 71, 0.4); color: #FDE047; font-size: 0.72rem; padding: 2px 8px; border-radius: 4px; font-weight: 600;", meta_b$role),
                  span(style = "background: rgba(255, 255, 255, 0.08); color: #CBD5E1; font-size: 0.72rem; padding: 2px 8px; border-radius: 4px;", meta_b$bat_style)
                ),
                if (both_bowlers || balls_b == 0) span(class = "specialist-badge", "Specialist Bowler • Primary Bowling Focus") else NULL
              )
            ),
            div(class = "metric-row", span(class = "metric-label", "Total Runs"), span(class = paste("metric-val", ifelse(runs_b >= runs_a && balls_b > 0, "lead", "")), runs_b)),
            div(class = "metric-row", span(class = "metric-label", "Balls Faced"), span(class = paste("metric-val", ifelse(balls_b >= balls_a && balls_b > 0, "lead", "")), balls_b)),
            div(class = "metric-row", span(class = "metric-label", "Batting Average"), span(class = paste("metric-val", ifelse(avg_b >= avg_a && avg_b > 0, "lead", "")), ifelse(avg_b > 0, avg_b, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Strike Rate"), span(class = paste("metric-val", ifelse(sr_b >= sr_a && balls_b > 0, "lead", "")), ifelse(balls_b > 0, sr_b, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Boundaries (4s / 6s)"), span(class = paste("metric-val", ifelse(fours_b + sixes_b >= fours_a + sixes_a && balls_b > 0, "lead", "")), paste0(fours_b, " / ", sixes_b))),
            div(class = "metric-row", span(class = "metric-label", "Boundary %"), span(class = paste("metric-val", ifelse(bpct_b >= bpct_a && balls_b > 0, "lead", "")), paste0(bpct_b, "%")))
          )
        ),
        
        # Radar & Metrics Table Row
        fluidRow(
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2(ifelse(both_bowlers, "Primary Impact Radar: Bowling Profile (0–100 Scale)", "Normalized Batting Radar (0–100 Scale)")),
                  tags$p(ifelse(both_bowlers, "Scaled against IPL 2025 bowling benchmarks (inverted economy/avg)", "Scaled against IPL 2025 season benchmarks"))
                ),
                span(class = "card-action-pill", "Radar Model")
              ),
              withSpinner(plotlyOutput("plot_comp_radar", height = "360px"), type = 4, color = "#0090DF")
            )
          ),
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Head-to-Head Metric Scorecard"),
                  tags$p("Granular comparison showing metric leader")
                ),
                span(class = "card-action-pill", "Scorecard")
              ),
              div(
                class = "table-responsive-container",
                DTOutput("table_comp_scorecard")
              )
            )
          )
        )
      )
    } else {
      # Bowling Duel Mode
      if (both_batters) {
        # Both are specialist batters - neither bowled in IPL 2025
        wins_a <- 0
        wins_b <- 0
      } else if (legal_balls_a > 0 && legal_balls_b == 0) {
        wins_a <- 5
        wins_b <- 0
      } else if (legal_balls_b > 0 && legal_balls_a == 0) {
        wins_a <- 0
        wins_b <- 5
      } else {
        wins_a <- sum(c(
          wkts_a > wkts_b,
          (!is.na(econ_a) && !is.na(econ_b) && econ_a < econ_b) || (!is.na(econ_a) && is.na(econ_b)),
          (!is.na(bowl_avg_a) && !is.na(bowl_avg_b) && bowl_avg_a < bowl_avg_b) || (!is.na(bowl_avg_a) && is.na(bowl_avg_b)),
          (!is.na(bowl_sr_a) && !is.na(bowl_sr_b) && bowl_sr_a < bowl_sr_b) || (!is.na(bowl_sr_a) && is.na(bowl_sr_b)),
          dot_a > dot_b
        ))
        wins_b <- sum(c(
          wkts_b > wkts_a,
          (!is.na(econ_b) && !is.na(econ_a) && econ_b < econ_a) || (!is.na(econ_b) && is.na(econ_a)),
          (!is.na(bowl_avg_b) && !is.na(bowl_avg_a) && bowl_avg_b < bowl_avg_a) || (!is.na(bowl_avg_b) && is.na(bowl_avg_a)),
          (!is.na(bowl_sr_b) && !is.na(bowl_sr_a) && bowl_sr_b < bowl_sr_a) || (!is.na(bowl_sr_b) && is.na(bowl_sr_a)),
          dot_b > dot_a
        ))
      }
      
      tagList(
        div(
          class = "comparison-grid",
          # Player A Card
          div(
            class = paste("player-card", ifelse(wins_a > wins_b, "winner", "")),
            if (wins_a > wins_b) span(class = "leader-pill", "🏆 DUEL LEADER"),
            div(
              class = "player-card-header-row",
              if (!is.null(logo_a)) tags$img(src = logo_a, class = "player-card-team-logo", alt = meta_a$franchise) else NULL,
              div(
                tags$h3(style = "color: #38BDF8; margin: 0;", meta_a$full_name),
                tags$p(style = "color: #94A3B8; font-size: 0.85rem; margin: 2px 0 0 0;", paste("Franchise:", meta_a$franchise)),
                div(
                  style = "display: flex; gap: 6px; flex-wrap: wrap; margin-top: 6px;",
                  span(style = "background: rgba(0, 144, 223, 0.2); border: 1px solid rgba(0, 144, 223, 0.4); color: #38BDF8; font-size: 0.72rem; padding: 2px 8px; border-radius: 4px; font-weight: 600;", meta_a$role),
                  span(style = "background: rgba(255, 255, 255, 0.08); color: #CBD5E1; font-size: 0.72rem; padding: 2px 8px; border-radius: 4px;", meta_a$bowl_style)
                ),
                if (both_batters || legal_balls_a == 0) span(class = "specialist-badge", "Specialist Batter • 0 Overs Bowled in 2025") else NULL
              )
            ),
            div(class = "metric-row", span(class = "metric-label", "Total Wickets"), span(class = paste("metric-val", ifelse(wkts_a >= wkts_b && legal_balls_a > 0, "lead", "")), wkts_a)),
            div(class = "metric-row", span(class = "metric-label", "Legal Overs Bowled"), span(class = paste("metric-val", ifelse(legal_balls_a >= legal_balls_b && legal_balls_a > 0, "lead", "")), overs_a)),
            div(class = "metric-row", span(class = "metric-label", "Economy Rate"), span(class = paste("metric-val", ifelse(!is.na(econ_a) && (is.na(econ_b) || econ_a <= econ_b) && legal_balls_a > 0, "lead", "")), ifelse(!is.na(econ_a), econ_a, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Bowling Average"), span(class = paste("metric-val", ifelse(!is.na(bowl_avg_a) && (is.na(bowl_avg_b) || bowl_avg_a <= bowl_avg_b) && legal_balls_a > 0, "lead", "")), ifelse(!is.na(bowl_avg_a), bowl_avg_a, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Bowling Strike Rate"), span(class = paste("metric-val", ifelse(!is.na(bowl_sr_a) && (is.na(bowl_sr_b) || bowl_sr_a <= bowl_sr_b) && legal_balls_a > 0, "lead", "")), ifelse(!is.na(bowl_sr_a), bowl_sr_a, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Dot-Ball %"), span(class = paste("metric-val", ifelse(dot_a >= dot_b && legal_balls_a > 0, "lead", "")), paste0(dot_a, "%")))
          ),
          
          # Central VS Badge
          div(class = "vs-badge-container", div(class = "vs-badge", "VS")),
          
          # Player B Card
          div(
            class = paste("player-card", ifelse(wins_b > wins_a, "winner", "")),
            if (wins_b > wins_a) span(class = "leader-pill", "🏆 DUEL LEADER"),
            div(
              class = "player-card-header-row",
              if (!is.null(logo_b)) tags$img(src = logo_b, class = "player-card-team-logo", alt = meta_b$franchise) else NULL,
              div(
                tags$h3(style = "color: #FDE047; margin: 0;", meta_b$full_name),
                tags$p(style = "color: #94A3B8; font-size: 0.85rem; margin: 2px 0 0 0;", paste("Franchise:", meta_b$franchise)),
                div(
                  style = "display: flex; gap: 6px; flex-wrap: wrap; margin-top: 6px;",
                  span(style = "background: rgba(253, 224, 71, 0.2); border: 1px solid rgba(253, 224, 71, 0.4); color: #FDE047; font-size: 0.72rem; padding: 2px 8px; border-radius: 4px; font-weight: 600;", meta_b$role),
                  span(style = "background: rgba(255, 255, 255, 0.08); color: #CBD5E1; font-size: 0.72rem; padding: 2px 8px; border-radius: 4px;", meta_b$bowl_style)
                ),
                if (both_batters || legal_balls_b == 0) span(class = "specialist-badge", "Specialist Batter • 0 Overs Bowled in 2025") else NULL
              )
            ),
            div(class = "metric-row", span(class = "metric-label", "Total Wickets"), span(class = paste("metric-val", ifelse(wkts_b >= wkts_a && legal_balls_b > 0, "lead", "")), wkts_b)),
            div(class = "metric-row", span(class = "metric-label", "Legal Overs Bowled"), span(class = paste("metric-val", ifelse(legal_balls_b >= legal_balls_a && legal_balls_b > 0, "lead", "")), overs_b)),
            div(class = "metric-row", span(class = "metric-label", "Economy Rate"), span(class = paste("metric-val", ifelse(!is.na(econ_b) && (is.na(econ_a) || econ_b <= econ_a) && legal_balls_b > 0, "lead", "")), ifelse(!is.na(econ_b), econ_b, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Bowling Average"), span(class = paste("metric-val", ifelse(!is.na(bowl_avg_b) && (is.na(bowl_avg_a) || bowl_avg_b <= bowl_avg_a) && legal_balls_b > 0, "lead", "")), ifelse(!is.na(bowl_avg_b), bowl_avg_b, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Bowling Strike Rate"), span(class = paste("metric-val", ifelse(!is.na(bowl_sr_b) && (is.na(bowl_sr_a) || bowl_sr_b <= bowl_sr_a) && legal_balls_b > 0, "lead", "")), ifelse(!is.na(bowl_sr_b), bowl_sr_b, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Dot-Ball %"), span(class = paste("metric-val", ifelse(dot_b >= dot_a && legal_balls_b > 0, "lead", "")), paste0(dot_b, "%")))
          )
        ),
        
        # Radar & Metrics Table Row
        fluidRow(
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2(ifelse(both_batters, "Primary Impact Radar: Batting Profile (0–100 Scale)", "Normalized Bowling Radar (0–100 Scale)")),
                  tags$p(ifelse(both_batters, "Scaled against IPL 2025 batting benchmarks for specialist batters", "Scaled against IPL 2025 season benchmarks (inverted for economy/avg)"))
                ),
                span(class = "card-action-pill", "Radar Model")
              ),
              withSpinner(plotlyOutput("plot_comp_radar", height = "360px"), type = 4, color = "#0090DF")
            )
          ),
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Head-to-Head Metric Scorecard"),
                  tags$p("Granular comparison showing metric leader")
                ),
                span(class = "card-action-pill", "Scorecard")
              ),
              div(
                class = "table-responsive-container",
                DTOutput("table_comp_scorecard")
              )
            )
          )
        )
      )
    }
  })
  
  # Radar Chart Rendering (Light, Spacious, Attractive Polar Layout with Specialist Adaptations)
  output$plot_comp_radar <- renderPlotly({
    req(input$comp_mode)
    comp <- get_comp_players()
    req(comp$resolved_a, comp$resolved_b)
    p_a <- comp$resolved_a
    p_b <- comp$resolved_b
    mode <- input$comp_mode
    
    stat_bat_a <- batting_summary %>% filter(striker == p_a)
    stat_bat_b <- batting_summary %>% filter(striker == p_b)
    stat_bowl_a <- bowling_summary %>% filter(bowler == p_a)
    stat_bowl_b <- bowling_summary %>% filter(bowler == p_b)
    
    runs_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$runs else 0
    runs_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$runs else 0
    sr_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$strike_rate else 0
    sr_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$strike_rate else 0
    avg_a <- if (nrow(stat_bat_a) > 0 && !is.na(stat_bat_a$batting_avg)) stat_bat_a$batting_avg else 0
    avg_b <- if (nrow(stat_bat_b) > 0 && !is.na(stat_bat_b$batting_avg)) stat_bat_b$batting_avg else 0
    bnd_a <- if (nrow(stat_bat_a) > 0) (stat_bat_a$fours + stat_bat_a$sixes) else 0
    bnd_b <- if (nrow(stat_bat_b) > 0) (stat_bat_b$fours + stat_bat_b$sixes) else 0
    bpct_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$boundary_pct else 0
    bpct_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$boundary_pct else 0
    balls_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$balls else 0
    balls_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$balls else 0
    matches_bat_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$matches else 0
    matches_bat_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$matches else 0
    
    wkts_a <- if (nrow(stat_bowl_a) > 0) stat_bowl_a$wickets else 0
    wkts_b <- if (nrow(stat_bowl_b) > 0) stat_bowl_b$wickets else 0
    econ_a <- if (nrow(stat_bowl_a) > 0 && !is.na(stat_bowl_a$economy)) stat_bowl_a$economy else 12.0
    econ_b <- if (nrow(stat_bowl_b) > 0 && !is.na(stat_bowl_b$economy)) stat_bowl_b$economy else 12.0
    bowl_avg_a <- if (nrow(stat_bowl_a) > 0 && !is.na(stat_bowl_a$bowling_avg)) stat_bowl_a$bowling_avg else 45.0
    bowl_avg_b <- if (nrow(stat_bowl_b) > 0 && !is.na(stat_bowl_b$bowling_avg)) stat_bowl_b$bowling_avg else 45.0
    bowl_sr_a <- if (nrow(stat_bowl_a) > 0 && !is.na(stat_bowl_a$bowling_sr)) stat_bowl_a$bowling_sr else 30.0
    bowl_sr_b <- if (nrow(stat_bowl_b) > 0 && !is.na(stat_bowl_b$bowling_sr)) stat_bowl_b$bowling_sr else 30.0
    dot_a <- if (nrow(stat_bowl_a) > 0) stat_bowl_a$dot_pct else 0
    dot_b <- if (nrow(stat_bowl_b) > 0) stat_bowl_b$dot_pct else 0
    overs_a <- if (nrow(stat_bowl_a) > 0) stat_bowl_a$overs else 0
    overs_b <- if (nrow(stat_bowl_b) > 0) stat_bowl_b$overs else 0
    
    both_batters <- (overs_a == 0 && overs_b == 0)
    both_bowlers <- (balls_a == 0 && balls_b == 0)
    
    if (mode == "batting") {
      if (both_bowlers) {
        # Both are bowlers in batting duel -> plot primary bowling radar
        cats <- c("Wickets", "Economy", "Bowling Avg", "Strike Rate", "Dot Ball %", "Overs")
        max_wkts <- max(bowling_summary$wickets)
        max_overs <- max(bowling_summary$overs)
        norm_a <- c(
          normalize_metric(wkts_a, 0, max_wkts),
          normalize_metric(econ_a, 6.0, 12.0, invert = TRUE),
          normalize_metric(bowl_avg_a, 15.0, 45.0, invert = TRUE),
          normalize_metric(bowl_sr_a, 10.0, 30.0, invert = TRUE),
          normalize_metric(dot_a, 15.0, 50.0),
          normalize_metric(overs_a, 0, max_overs)
        )
        norm_b <- c(
          normalize_metric(wkts_b, 0, max_wkts),
          normalize_metric(econ_b, 6.0, 12.0, invert = TRUE),
          normalize_metric(bowl_avg_b, 15.0, 45.0, invert = TRUE),
          normalize_metric(bowl_sr_b, 10.0, 30.0, invert = TRUE),
          normalize_metric(dot_b, 15.0, 50.0),
          normalize_metric(overs_b, 0, max_overs)
        )
        hover_a <- paste0(cats, "<br><b>", p_a, " (Bowling Profile): </b>", c(wkts_a, econ_a, bowl_avg_a, bowl_sr_a, paste0(dot_a, "%"), overs_a), "<br>Score: ", norm_a, "/100")
        hover_b <- paste0(cats, "<br><b>", p_b, " (Bowling Profile): </b>", c(wkts_b, econ_b, bowl_avg_b, bowl_sr_b, paste0(dot_b, "%"), overs_b), "<br>Score: ", norm_b, "/100")
      } else {
        cats <- c("Runs", "Strike Rate", "Batting Avg", "Boundaries", "Boundary %", "Balls Faced")
        max_runs <- max(batting_summary$runs)
        max_balls <- max(batting_summary$balls)
        max_bnd <- max(batting_summary$fours + batting_summary$sixes)
        norm_a <- c(
          normalize_metric(runs_a, 0, max_runs),
          normalize_metric(sr_a, 80, 200),
          normalize_metric(avg_a, 10, 65),
          normalize_metric(bnd_a, 0, max_bnd),
          normalize_metric(bpct_a, 0, 90),
          normalize_metric(balls_a, 0, max_balls)
        )
        norm_b <- c(
          normalize_metric(runs_b, 0, max_runs),
          normalize_metric(sr_b, 80, 200),
          normalize_metric(avg_b, 10, 65),
          normalize_metric(bnd_b, 0, max_bnd),
          normalize_metric(bpct_b, 0, 90),
          normalize_metric(balls_b, 0, max_balls)
        )
        hover_a <- paste0(cats, "<br><b>", p_a, ": </b>", c(runs_a, paste0(sr_a, " SR"), avg_a, bnd_a, paste0(bpct_a, "%"), balls_a), "<br>Score: ", norm_a, "/100")
        hover_b <- paste0(cats, "<br><b>", p_b, ": </b>", c(runs_b, paste0(sr_b, " SR"), avg_b, bnd_b, paste0(bpct_b, "%"), balls_b), "<br>Score: ", norm_b, "/100")
      }
    } else {
      # Bowling mode
      if (both_batters) {
        # Both are specialist batters in bowling duel -> plot primary tournament batting radar!
        cats <- c("Total Runs", "Strike Rate", "Batting Avg", "Boundaries", "Boundary %", "Matches")
        max_runs <- max(batting_summary$runs)
        max_bnd <- max(batting_summary$fours + batting_summary$sixes)
        max_matches <- max(batting_summary$matches)
        norm_a <- c(
          normalize_metric(runs_a, 0, max_runs),
          normalize_metric(sr_a, 80, 200),
          normalize_metric(avg_a, 10, 65),
          normalize_metric(bnd_a, 0, max_bnd),
          normalize_metric(bpct_a, 0, 90),
          normalize_metric(matches_bat_a, 0, max_matches)
        )
        norm_b <- c(
          normalize_metric(runs_b, 0, max_runs),
          normalize_metric(sr_b, 80, 200),
          normalize_metric(avg_b, 10, 65),
          normalize_metric(bnd_b, 0, max_bnd),
          normalize_metric(bpct_b, 0, 90),
          normalize_metric(matches_bat_b, 0, max_matches)
        )
        hover_a <- paste0(cats, "<br><b>", p_a, " (Batting Profile): </b>", c(runs_a, paste0(sr_a, " SR"), avg_a, bnd_a, paste0(bpct_a, "%"), matches_bat_a), "<br>Score: ", norm_a, "/100")
        hover_b <- paste0(cats, "<br><b>", p_b, " (Batting Profile): </b>", c(runs_b, paste0(sr_b, " SR"), avg_b, bnd_b, paste0(bpct_b, "%"), matches_bat_b), "<br>Score: ", norm_b, "/100")
      } else {
        cats <- c("Wickets", "Economy", "Bowling Avg", "Strike Rate", "Dot Ball %", "Overs")
        max_wkts <- max(bowling_summary$wickets)
        max_overs <- max(bowling_summary$overs)
        norm_a <- c(
          normalize_metric(wkts_a, 0, max_wkts),
          normalize_metric(econ_a, 6.0, 12.0, invert = TRUE),
          normalize_metric(bowl_avg_a, 15.0, 45.0, invert = TRUE),
          normalize_metric(bowl_sr_a, 10.0, 30.0, invert = TRUE),
          normalize_metric(dot_a, 15.0, 50.0),
          normalize_metric(overs_a, 0, max_overs)
        )
        norm_b <- c(
          normalize_metric(wkts_b, 0, max_wkts),
          normalize_metric(econ_b, 6.0, 12.0, invert = TRUE),
          normalize_metric(bowl_avg_b, 15.0, 45.0, invert = TRUE),
          normalize_metric(bowl_sr_b, 10.0, 30.0, invert = TRUE),
          normalize_metric(dot_b, 15.0, 50.0),
          normalize_metric(overs_b, 0, max_overs)
        )
        hover_a <- paste0(cats, "<br><b>", p_a, ": </b>", c(wkts_a, econ_a, bowl_avg_a, bowl_sr_a, paste0(dot_a, "%"), overs_a), "<br>Score: ", norm_a, "/100")
        hover_b <- paste0(cats, "<br><b>", p_b, ": </b>", c(wkts_b, econ_b, bowl_avg_b, bowl_sr_b, paste0(dot_b, "%"), overs_b), "<br>Score: ", norm_b, "/100")
      }
    }
    
    p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself") %>%
      add_trace(
        r = c(norm_a, norm_a[1]),
        theta = c(cats, cats[1]),
        name = p_a,
        mode = "lines+markers",
        text = c(hover_a, hover_a[1]),
        hoverinfo = "text",
        line = list(color = "#38BDF8", width = 3),
        marker = list(color = "#38BDF8", size = 8),
        fillcolor = "rgba(56, 189, 248, 0.28)"
      ) %>%
      add_trace(
        r = c(norm_b, norm_b[1]),
        theta = c(cats, cats[1]),
        name = p_b,
        mode = "lines+markers",
        text = c(hover_b, hover_b[1]),
        hoverinfo = "text",
        line = list(color = "#FDE047", width = 3),
        marker = list(color = "#FDE047", size = 8),
        fillcolor = "rgba(253, 224, 71, 0.28)"
      ) %>%
      layout(
        polar = list(
          bgcolor = "rgba(0,0,0,0)",
          radialaxis = list(
            visible = TRUE,
            range = c(0, 100),
            color = "#94A3B8",
            gridcolor = "rgba(255, 255, 255, 0.12)",
            tickfont = list(size = 9, color = "#94A3B8")
          ),
          angularaxis = list(
            color = "#F8FAFC",
            gridcolor = "rgba(255, 255, 255, 0.12)",
            tickfont = list(size = 11, color = "#F8FAFC", family = "Inter, sans-serif")
          )
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#CBD5E1", family = "Inter, sans-serif"),
        showlegend = TRUE,
        legend = list(
          orientation = "h", 
          x = 0.2, 
          y = -0.15,
          font = list(color = "#F8FAFC", size = 11),
          bgcolor = "rgba(16, 27, 46, 0.85)",
          bordercolor = "rgba(255, 255, 255, 0.12)"
        )
      ) %>%
      config(displayModeBar = FALSE, responsive = TRUE)
    
    p
  })
  
  # Comparison Scorecard Table
  output$table_comp_scorecard <- renderDT({
    req(input$comp_mode)
    comp <- get_comp_players()
    req(comp$resolved_a, comp$resolved_b)
    p_a <- comp$resolved_a
    p_b <- comp$resolved_b
    mode <- input$comp_mode
    
    meta_a <- get_player_cricbuzz_meta(p_a)
    meta_b <- get_player_cricbuzz_meta(p_b)
    name_a <- meta_a$full_name
    name_b <- meta_b$full_name
    
    stat_bat_a <- batting_summary %>% filter(striker == p_a)
    stat_bat_b <- batting_summary %>% filter(striker == p_b)
    stat_bowl_a <- bowling_summary %>% filter(bowler == p_a)
    stat_bowl_b <- bowling_summary %>% filter(bowler == p_b)
    
    runs_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$runs else 0
    runs_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$runs else 0
    balls_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$balls else 0
    balls_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$balls else 0
    sr_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$strike_rate else 0
    sr_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$strike_rate else 0
    avg_a <- if (nrow(stat_bat_a) > 0 && !is.na(stat_bat_a$batting_avg)) stat_bat_a$batting_avg else 0
    avg_b <- if (nrow(stat_bat_b) > 0 && !is.na(stat_bat_b$batting_avg)) stat_bat_b$batting_avg else 0
    fours_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$fours else 0
    fours_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$fours else 0
    sixes_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$sixes else 0
    sixes_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$sixes else 0
    bpct_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$boundary_pct else 0
    bpct_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$boundary_pct else 0
    matches_bat_a <- if (nrow(stat_bat_a) > 0) stat_bat_a$matches else 0
    matches_bat_b <- if (nrow(stat_bat_b) > 0) stat_bat_b$matches else 0
    
    wkts_a <- if (nrow(stat_bowl_a) > 0) stat_bowl_a$wickets else 0
    wkts_b <- if (nrow(stat_bowl_b) > 0) stat_bowl_b$wickets else 0
    runs_conceded_a <- if (nrow(stat_bowl_a) > 0) stat_bowl_a$runs_conceded else 0
    runs_conceded_b <- if (nrow(stat_bowl_b) > 0) stat_bowl_b$runs_conceded else 0
    legal_balls_a <- if (nrow(stat_bowl_a) > 0) stat_bowl_a$legal_balls else 0
    legal_balls_b <- if (nrow(stat_bowl_b) > 0) stat_bowl_b$legal_balls else 0
    overs_a <- paste0(legal_balls_a %/% 6, ".", legal_balls_a %% 6)
    overs_b <- paste0(legal_balls_b %/% 6, ".", legal_balls_b %% 6)
    econ_a <- if (nrow(stat_bowl_a) > 0 && !is.na(stat_bowl_a$economy)) stat_bowl_a$economy else NA
    econ_b <- if (nrow(stat_bowl_b) > 0 && !is.na(stat_bowl_b$economy)) stat_bowl_b$economy else NA
    bowl_avg_a <- if (nrow(stat_bowl_a) > 0 && !is.na(stat_bowl_a$bowling_avg)) stat_bowl_a$bowling_avg else NA
    bowl_avg_b <- if (nrow(stat_bowl_b) > 0 && !is.na(stat_bowl_b$bowling_avg)) stat_bowl_b$bowling_avg else NA
    bowl_sr_a <- if (nrow(stat_bowl_a) > 0 && !is.na(stat_bowl_a$bowling_sr)) stat_bowl_a$bowling_sr else NA
    bowl_sr_b <- if (nrow(stat_bowl_b) > 0 && !is.na(stat_bowl_b$bowling_sr)) stat_bowl_b$bowling_sr else NA
    dot_a <- if (nrow(stat_bowl_a) > 0) stat_bowl_a$dot_pct else 0
    dot_b <- if (nrow(stat_bowl_b) > 0) stat_bowl_b$dot_pct else 0
    
    if (mode == "batting") {
      score_df <- tibble(
        Metric = c("Total Runs", "Balls Faced", "Batting Average", "Strike Rate", 
                   "Fours (4s)", "Sixes (6s)", "Boundary Run %", "Matches Played"),
        Player_A = as.character(c(runs_a, balls_a, ifelse(avg_a > 0, avg_a, "—"), ifelse(balls_a > 0, sr_a, "—"), fours_a, sixes_a, paste0(bpct_a, "%"), matches_bat_a)),
        Player_B = as.character(c(runs_b, balls_b, ifelse(avg_b > 0, avg_b, "—"), ifelse(balls_b > 0, sr_b, "—"), fours_b, sixes_b, paste0(bpct_b, "%"), matches_bat_b)),
        Leader = c(
          ifelse(runs_a > runs_b, paste("👈", name_a), ifelse(runs_b > runs_a, paste(name_b, "👉"), "Tie")),
          ifelse(balls_a > balls_b, paste("👈", name_a), ifelse(balls_b > balls_a, paste(name_b, "👉"), "Tie")),
          ifelse(avg_a > avg_b, paste("👈", name_a), ifelse(avg_b > avg_a, paste(name_b, "👉"), "Tie")),
          ifelse(sr_a > sr_b && balls_a > 0 && balls_b > 0, paste("👈", name_a), ifelse(sr_b > sr_a && balls_a > 0 && balls_b > 0, paste(name_b, "👉"), ifelse(balls_a > 0, paste("👈", name_a), ifelse(balls_b > 0, paste(name_b, "👉"), "Tie")))),
          ifelse(fours_a > fours_b, paste("👈", name_a), ifelse(fours_b > fours_a, paste(name_b, "👉"), "Tie")),
          ifelse(sixes_a > sixes_b, paste("👈", name_a), ifelse(sixes_b > sixes_a, paste(name_b, "👉"), "Tie")),
          ifelse(bpct_a > bpct_b, paste("👈", name_a), ifelse(bpct_b > bpct_a, paste(name_b, "👉"), "Tie")),
          ifelse(matches_bat_a > matches_bat_b, paste("👈", name_a), ifelse(matches_bat_b > matches_bat_a, paste(name_b, "👉"), "Tie"))
        )
      )
    } else {
      # Bowling Duel Scorecard
      calc_bowl_lead <- function(type, val_a, val_b) {
        if (legal_balls_a == 0 && legal_balls_b == 0) return("Tie")
        if (legal_balls_a > 0 && legal_balls_b == 0) return(paste("👈", name_a))
        if (legal_balls_b > 0 && legal_balls_a == 0) return(paste(name_b, "👉"))
        
        if (type == "higher") {
          if (val_a > val_b) paste("👈", name_a) else if (val_b > val_a) paste(name_b, "👉") else "Tie"
        } else {
          # lower is better
          if (is.na(val_a) && is.na(val_b)) "Tie"
          else if (!is.na(val_a) && is.na(val_b)) paste("👈", name_a)
          else if (!is.na(val_b) && is.na(val_a)) paste(name_b, "👉")
          else if (val_a < val_b) paste("👈", name_a)
          else if (val_b < val_a) paste(name_b, "👉")
          else "Tie"
        }
      }
      
      score_df <- tibble(
        Metric = c("Total Wickets", "Legal Overs Bowled", "Runs Conceded", 
                   "Economy Rate", "Bowling Average", "Bowling Strike Rate", "Dot-Ball %"),
        Player_A = as.character(c(wkts_a, overs_a, runs_conceded_a, 
                                  ifelse(!is.na(econ_a), econ_a, "—"), 
                                  ifelse(!is.na(bowl_avg_a), bowl_avg_a, "—"), 
                                  ifelse(!is.na(bowl_sr_a), bowl_sr_a, "—"), 
                                  paste0(dot_a, "%"))),
        Player_B = as.character(c(wkts_b, overs_b, runs_conceded_b, 
                                  ifelse(!is.na(econ_b), econ_b, "—"), 
                                  ifelse(!is.na(bowl_avg_b), bowl_avg_b, "—"), 
                                  ifelse(!is.na(bowl_sr_b), bowl_sr_b, "—"), 
                                  paste0(dot_b, "%"))),
        Leader = c(
          calc_bowl_lead("higher", wkts_a, wkts_b),
          calc_bowl_lead("higher", legal_balls_a, legal_balls_b),
          calc_bowl_lead("lower", runs_conceded_a, runs_conceded_b),
          calc_bowl_lead("lower", econ_a, econ_b),
          calc_bowl_lead("lower", bowl_avg_a, bowl_avg_b),
          calc_bowl_lead("lower", bowl_sr_a, bowl_sr_b),
          calc_bowl_lead("higher", dot_a, dot_b)
        )
      )
    }
    
    datatable(
      score_df,
      colnames = c("Metric", name_a, name_b, "Comparison Leader"),
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(width = "34%", targets = 0, className = "dt-left"),
          list(width = "22%", targets = 1, className = "dt-center"),
          list(width = "22%", targets = 2, className = "dt-center"),
          list(width = "22%", targets = 3, className = "dt-center")
        )
      ),
      rownames = FALSE,
      class = "compact stripe hover scorecard-unified-table"
    )
  }, server = FALSE)
}

# ------------------------------------------------------------------------------
# 4. RUN APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

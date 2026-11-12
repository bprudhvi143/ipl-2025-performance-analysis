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
            # KPI Cards Row
            div(
              class = "kpi-grid",
              div(
                class = "kpi-card kpi-primary",
                div(class = "kpi-header", span(class = "kpi-title", "Total Matches"), div(class = "kpi-icon-wrap", "🏟️")),
                div(class = "kpi-value", format(total_matches, big.mark = ",")),
                div(class = "kpi-subtitle", "Complete 2025 tournament")
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
            # Player Selection & Category Filter Bar
            div(
              class = "filter-panel",
              div(
                style = "flex: 1; min-width: 260px;",
                tags$label("Select Player"),
                selectizeInput(
                  inputId = "player_select",
                  label = NULL,
                  choices = all_players,
                  selected = "Sai Sudharsan",
                  options = list(
                    placeholder = "Type player name to search...",
                    maxOptions = 250,
                    openOnFocus = TRUE,
                    selectOnTab = TRUE
                  )
                )
              ),
              div(
                style = "min-width: 240px;",
                tags$label("Performance Category"),
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
          # PAGE 4: PLAYER COMPARISON STUDIO (Fast Autocomplete & Team Logos)
          # ====================================================================
          tabPanel(
            "comparison",
            # Selectors Bar
            div(
              class = "filter-panel",
              div(
                style = "flex: 1; min-width: 240px;",
                tags$label("Player A (Type to Search)"),
                selectizeInput(
                  inputId = "comp_player_a",
                  label = NULL,
                  choices = all_players,
                  selected = "Sai Sudharsan",
                  options = list(
                    placeholder = "Search Player A (e.g. Kohli, Rohit, Gill)...",
                    maxOptions = 250,
                    openOnFocus = TRUE,
                    selectOnTab = TRUE
                  )
                )
              ),
              div(
                style = "flex: 1; min-width: 240px;",
                tags$label("Player B (Type to Search)"),
                selectizeInput(
                  inputId = "comp_player_b",
                  label = NULL,
                  choices = all_players,
                  selected = "Suryakumar Yadav",
                  options = list(
                    placeholder = "Search Player B (e.g. Bumrah, Noor, Head)...",
                    maxOptions = 250,
                    openOnFocus = TRUE,
                    selectOnTab = TRUE
                  )
                )
              ),
              div(
                style = "min-width: 220px;",
                tags$label("Comparison Mode"),
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
  
  # Initialize Server-Side Selectize for Instant, Responsive Search
  updateSelectizeInput(session, "comp_player_a", choices = all_players, selected = "Sai Sudharsan", server = TRUE)
  updateSelectizeInput(session, "comp_player_b", choices = all_players, selected = "Suryakumar Yadav", server = TRUE)
  updateSelectizeInput(session, "player_select", choices = all_players, selected = "Sai Sudharsan", server = TRUE)
  
  # Dynamic Header Title and Subtitle based on active page
  output$header_title_ui <- renderUI({
    switch(
      input$nav_menu,
      "overview" = tagList(
        tags$h1("IPL 2025 Season Overview"),
        tags$p("Comprehensive tournament benchmarks, franchise standings and award contenders")
      ),
      "player" = tagList(
        tags$h1(paste("Player Analytics —", req(input$player_select))),
        tags$p("Deep-dive ball-by-ball performance metrics, match contributions and breakdowns")
      ),
      "team" = tagList(
        tags$h1(paste("Franchise Intelligence —", req(input$team_select))),
        tags$p("Team scoring rates, bowling efficiency, and roster leaderboards")
      ),
      "comparison" = tagList(
        tags$h1("Player Comparison Studio"),
        tags$p("Head-to-head performance radar, metric duels, and percentile intelligence")
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
  })
  
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
  })
  
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
  })
  
  # ============================================================================
  # PAGE 2: PLAYER ANALYTICS SERVER OUTPUTS
  # ============================================================================
  
  output$player_analytics_ui <- renderUI({
    req(input$player_select, input$player_category)
    
    selected_p <- input$player_select
    cat_type <- input$player_category
    
    if (cat_type == "batting") {
      p_bat <- batting_summary %>% filter(striker == selected_p)
      
      if (nrow(p_bat) == 0 || p_bat$balls == 0) {
        return(
          div(
            class = "empty-state",
            div(class = "empty-state-icon", "🏏"),
            tags$h3(paste("No Batting Records for", selected_p)),
            tags$p("This player has not faced any legal deliveries in the 2025 season. Switch to Bowling Analysis above if they are a specialist bowler.")
          )
        )
      }
      
      p_team_code <- tolower(trimws(strsplit(p_bat$teams, ",")[[1]][1]))
      
      tagList(
        # 8 Batting KPI Cards
        div(
          class = "kpi-grid",
          div(
            class = "kpi-card kpi-primary",
            div(class = "kpi-header", span(class = "kpi-title", "Total Runs"), div(class = "kpi-icon-wrap", "🏏")),
            div(class = "kpi-value", format(p_bat$runs, big.mark = ",")),
            div(
              class = "kpi-subtitle",
              if (!is.null(get_team_logo_path(p_team_code))) tags$img(src = get_team_logo_path(p_team_code), class = "team-logo-icon", style = "width: 18px; height: 18px; vertical-align: middle; margin-right: 6px;"),
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
          div(
            class = "empty-state",
            div(class = "empty-state-icon", "🎯"),
            tags$h3(paste("No Bowling Records for", selected_p)),
            tags$p("This player has not bowled any legal deliveries in the 2025 season. Switch to Batting Analysis above if they are a specialist batter.")
          )
        )
      }
      
      p_team_code <- tolower(trimws(strsplit(p_bowl$teams, ",")[[1]][1]))
      
      tagList(
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
    req(input$player_select)
    p_data <- ipl %>%
      filter(striker == input$player_select) %>%
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
    req(input$player_select)
    p_data <- ipl %>% filter(striker == input$player_select)
    
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
    req(input$player_select)
    inn_data <- ipl %>%
      filter(striker == input$player_select, innings %in% c(1, 2)) %>%
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
    req(input$player_select)
    phase_data <- ipl %>%
      filter(striker == input$player_select) %>%
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
    req(input$player_select)
    match_log <- ipl %>%
      filter(striker == input$player_select) %>%
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
  })
  
  # Player Bowling Charts (Luminous & Eye-Catching Lighter Elements)
  output$plot_player_bowl_match_trend <- renderPlotly({
    req(input$player_select)
    b_data <- ipl %>%
      filter(bowler == input$player_select) %>%
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
    req(input$player_select)
    econ_data <- ipl %>%
      filter(bowler == input$player_select) %>%
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
    req(input$player_select)
    inn_data <- ipl %>%
      filter(bowler == input$player_select, innings %in% c(1, 2)) %>%
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
    req(input$player_select)
    phase_data <- ipl %>%
      filter(bowler == input$player_select) %>%
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
    req(input$player_select)
    bowl_log <- ipl %>%
      filter(bowler == input$player_select) %>%
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
  })
  
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
  })
  
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
  })
  
  # ============================================================================
  # PAGE 4: PLAYER COMPARISON STUDIO SERVER OUTPUTS (With Team Logos)
  # ============================================================================
  
  output$comp_validation_ui <- renderUI({
    req(input$comp_player_a, input$comp_player_b)
    if (input$comp_player_a == input$comp_player_b) {
      div(
        class = "alert alert-warning",
        style = "background: rgba(255, 184, 0, 0.15); border: 1px solid #FFB800; color: #FFB800; border-radius: 8px; padding: 12px 18px; margin-bottom: 20px;",
        tags$b("Notice: "), "You have selected the same player for both Player A and Player B. Please select two different players to view a head-to-head comparison."
      )
    }
  })
  
  output$comp_content_ui <- renderUI({
    req(input$comp_player_a, input$comp_player_b, input$comp_mode)
    
    p_a <- input$comp_player_a
    p_b <- input$comp_player_b
    mode <- input$comp_mode
    
    if (mode == "batting") {
      stat_a <- batting_summary %>% filter(striker == p_a)
      stat_b <- batting_summary %>% filter(striker == p_b)
      
      runs_a <- if (nrow(stat_a) > 0) stat_a$runs else 0
      balls_a <- if (nrow(stat_a) > 0) stat_a$balls else 0
      sr_a <- if (nrow(stat_a) > 0) stat_a$strike_rate else 0
      avg_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$batting_avg)) stat_a$batting_avg else 0
      fours_a <- if (nrow(stat_a) > 0) stat_a$fours else 0
      sixes_a <- if (nrow(stat_a) > 0) stat_a$sixes else 0
      bpct_a <- if (nrow(stat_a) > 0) stat_a$boundary_pct else 0
      matches_a <- if (nrow(stat_a) > 0) stat_a$matches else 0
      team_a <- if (nrow(stat_a) > 0) stat_a$teams else "—"
      
      runs_b <- if (nrow(stat_b) > 0) stat_b$runs else 0
      balls_b <- if (nrow(stat_b) > 0) stat_b$balls else 0
      sr_b <- if (nrow(stat_b) > 0) stat_b$strike_rate else 0
      avg_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$batting_avg)) stat_b$batting_avg else 0
      fours_b <- if (nrow(stat_b) > 0) stat_b$fours else 0
      sixes_b <- if (nrow(stat_b) > 0) stat_b$sixes else 0
      bpct_b <- if (nrow(stat_b) > 0) stat_b$boundary_pct else 0
      matches_b <- if (nrow(stat_b) > 0) stat_b$matches else 0
      team_b <- if (nrow(stat_b) > 0) stat_b$teams else "—"
      
      first_team_a <- tolower(trimws(strsplit(team_a, ",")[[1]][1]))
      first_team_b <- tolower(trimws(strsplit(team_b, ",")[[1]][1]))
      
      wins_a <- sum(c(runs_a > runs_b, sr_a > sr_b, avg_a > avg_b, (fours_a + sixes_a) > (fours_b + sixes_b), bpct_a > bpct_b))
      wins_b <- sum(c(runs_b > runs_a, sr_b > sr_a, avg_b > avg_a, (fours_b + sixes_b) > (fours_a + sixes_a), bpct_b > bpct_a))
      
      tagList(
        div(
          class = "comparison-grid",
          # Player A Card with Franchise Crest
          div(
            class = paste("player-card", ifelse(wins_a > wins_b, "winner", "")),
            if (wins_a > wins_b) span(class = "leader-pill", "🏆 DUEL LEADER"),
            div(
              class = "player-card-header-row",
              if (!is.null(get_team_logo_path(team_a))) tags$img(src = get_team_logo_path(team_a), class = "player-card-team-logo", alt = team_a) else NULL,
              div(
                tags$h3(style = "color: #38BDF8; margin: 0;", p_a),
                tags$p(style = "color: #94A3B8; font-size: 0.85rem; margin: 2px 0 0 0;", paste("Franchise:", team_a))
              )
            ),
            div(class = "metric-row", span(class = "metric-label", "Total Runs"), span(class = paste("metric-val", ifelse(runs_a >= runs_b, "lead", "")), runs_a)),
            div(class = "metric-row", span(class = "metric-label", "Strike Rate"), span(class = paste("metric-val", ifelse(sr_a >= sr_b, "lead", "")), sr_a)),
            div(class = "metric-row", span(class = "metric-label", "Batting Average"), span(class = paste("metric-val", ifelse(avg_a >= avg_b, "lead", "")), ifelse(avg_a > 0, avg_a, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Boundaries (4s / 6s)"), span(class = paste("metric-val", ifelse(fours_a + sixes_a >= fours_b + sixes_b, "lead", "")), paste0(fours_a, " / ", sixes_a))),
            div(class = "metric-row", span(class = "metric-label", "Boundary %"), span(class = paste("metric-val", ifelse(bpct_a >= bpct_b, "lead", "")), paste0(bpct_a, "%")))
          ),
          
          # Central VS Badge
          div(class = "vs-badge-container", div(class = "vs-badge", "VS")),
          
          # Player B Card with Franchise Crest
          div(
            class = paste("player-card", ifelse(wins_b > wins_a, "winner", "")),
            if (wins_b > wins_a) span(class = "leader-pill", "🏆 DUEL LEADER"),
            div(
              class = "player-card-header-row",
              if (!is.null(get_team_logo_path(team_b))) tags$img(src = get_team_logo_path(team_b), class = "player-card-team-logo", alt = team_b) else NULL,
              div(
                tags$h3(style = "color: #FDE047; margin: 0;", p_b),
                tags$p(style = "color: #94A3B8; font-size: 0.85rem; margin: 2px 0 0 0;", paste("Franchise:", team_b))
              )
            ),
            div(class = "metric-row", span(class = "metric-label", "Total Runs"), span(class = paste("metric-val", ifelse(runs_b >= runs_a, "lead", "")), runs_b)),
            div(class = "metric-row", span(class = "metric-label", "Strike Rate"), span(class = paste("metric-val", ifelse(sr_b >= sr_a, "lead", "")), sr_b)),
            div(class = "metric-row", span(class = "metric-label", "Batting Average"), span(class = paste("metric-val", ifelse(avg_b >= avg_a, "lead", "")), ifelse(avg_b > 0, avg_b, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Boundaries (4s / 6s)"), span(class = paste("metric-val", ifelse(fours_b + sixes_b >= fours_a + sixes_a, "lead", "")), paste0(fours_b, " / ", sixes_b))),
            div(class = "metric-row", span(class = "metric-label", "Boundary %"), span(class = paste("metric-val", ifelse(bpct_b >= bpct_a, "lead", "")), paste0(bpct_b, "%")))
          )
        ),
        
        # Radar & Metrics Table Row (Light Card Background)
        fluidRow(
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Normalized Batting Radar (0–100 Scale)"),
                  tags$p("Scaled against IPL 2025 season benchmarks")
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
      stat_a <- bowling_summary %>% filter(bowler == p_a)
      stat_b <- bowling_summary %>% filter(bowler == p_b)
      
      wkts_a <- if (nrow(stat_a) > 0) stat_a$wickets else 0
      balls_a <- if (nrow(stat_a) > 0) stat_a$legal_balls else 0
      overs_a <- if (nrow(stat_a) > 0) stat_a$overs_display else "0.0"
      runs_a <- if (nrow(stat_a) > 0) stat_a$runs_conceded else 0
      econ_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$economy)) stat_a$economy else NA
      avg_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$bowling_avg)) stat_a$bowling_avg else NA
      sr_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$bowling_sr)) stat_a$bowling_sr else NA
      dot_a <- if (nrow(stat_a) > 0) stat_a$dot_pct else 0
      team_a <- if (nrow(stat_a) > 0) stat_a$teams else "—"
      
      wkts_b <- if (nrow(stat_b) > 0) stat_b$wickets else 0
      balls_b <- if (nrow(stat_b) > 0) stat_b$legal_balls else 0
      overs_b <- if (nrow(stat_b) > 0) stat_b$overs_display else "0.0"
      runs_b <- if (nrow(stat_b) > 0) stat_b$runs_conceded else 0
      econ_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$economy)) stat_b$economy else NA
      avg_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$bowling_avg)) stat_b$bowling_avg else NA
      sr_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$bowling_sr)) stat_b$bowling_sr else NA
      dot_b <- if (nrow(stat_b) > 0) stat_b$dot_pct else 0
      team_b <- if (nrow(stat_b) > 0) stat_b$teams else "—"
      
      first_team_a <- tolower(trimws(strsplit(team_a, ",")[[1]][1]))
      first_team_b <- tolower(trimws(strsplit(team_b, ",")[[1]][1]))
      
      w_a <- sum(c(
        wkts_a > wkts_b,
        (!is.na(econ_a) && !is.na(econ_b) && econ_a < econ_b) || (!is.na(econ_a) && is.na(econ_b)),
        (!is.na(avg_a) && !is.na(avg_b) && avg_a < avg_b) || (!is.na(avg_a) && is.na(avg_b)),
        (!is.na(sr_a) && !is.na(sr_b) && sr_a < sr_b) || (!is.na(sr_a) && is.na(sr_b)),
        dot_a > dot_b
      ))
      w_b <- sum(c(
        wkts_b > wkts_a,
        (!is.na(econ_b) && !is.na(econ_a) && econ_b < econ_a) || (!is.na(econ_b) && is.na(econ_a)),
        (!is.na(avg_b) && !is.na(avg_a) && avg_b < avg_a) || (!is.na(avg_b) && is.na(avg_a)),
        (!is.na(sr_b) && !is.na(sr_a) && sr_b < sr_a) || (!is.na(sr_b) && is.na(sr_a)),
        dot_b > dot_a
      ))
      
      tagList(
        div(
          class = "comparison-grid",
          # Player A Card
          div(
            class = paste("player-card", ifelse(w_a > w_b, "winner", "")),
            if (w_a > w_b) span(class = "leader-pill", "🏆 DUEL LEADER"),
            div(
              class = "player-card-header-row",
              if (!is.null(get_team_logo_path(team_a))) tags$img(src = get_team_logo_path(team_a), class = "player-card-team-logo", alt = team_a) else NULL,
              div(
                tags$h3(style = "color: #38BDF8; margin: 0;", p_a),
                tags$p(style = "color: #94A3B8; font-size: 0.85rem; margin: 2px 0 0 0;", paste("Franchise:", team_a))
              )
            ),
            div(class = "metric-row", span(class = "metric-label", "Credited Wickets"), span(class = paste("metric-val", ifelse(wkts_a >= wkts_b, "lead", "")), wkts_a)),
            div(class = "metric-row", span(class = "metric-label", "Economy Rate"), span(class = paste("metric-val", ifelse(!is.na(econ_a) && (is.na(econ_b) || econ_a <= econ_b), "lead", "")), ifelse(!is.na(econ_a), econ_a, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Bowling Average"), span(class = paste("metric-val", ifelse(!is.na(avg_a) && (is.na(avg_b) || avg_a <= avg_b), "lead", "")), ifelse(!is.na(avg_a), avg_a, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Strike Rate"), span(class = paste("metric-val", ifelse(!is.na(sr_a) && (is.na(sr_b) || sr_a <= sr_b), "lead", "")), ifelse(!is.na(sr_a), sr_a, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Dot-Ball %"), span(class = paste("metric-val", ifelse(dot_a >= dot_b, "lead", "")), paste0(dot_a, "%")))
          ),
          
          # Central VS Badge
          div(class = "vs-badge-container", div(class = "vs-badge", "VS")),
          
          # Player B Card
          div(
            class = paste("player-card", ifelse(w_b > w_a, "winner", "")),
            if (w_b > w_a) span(class = "leader-pill", "🏆 DUEL LEADER"),
            div(
              class = "player-card-header-row",
              if (!is.null(get_team_logo_path(team_b))) tags$img(src = get_team_logo_path(team_b), class = "player-card-team-logo", alt = team_b) else NULL,
              div(
                tags$h3(style = "color: #FDE047; margin: 0;", p_b),
                tags$p(style = "color: #94A3B8; font-size: 0.85rem; margin: 2px 0 0 0;", paste("Franchise:", team_b))
              )
            ),
            div(class = "metric-row", span(class = "metric-label", "Credited Wickets"), span(class = paste("metric-val", ifelse(wkts_b >= wkts_a, "lead", "")), wkts_b)),
            div(class = "metric-row", span(class = "metric-label", "Economy Rate"), span(class = paste("metric-val", ifelse(!is.na(econ_b) && (is.na(econ_a) || econ_b <= econ_a), "lead", "")), ifelse(!is.na(econ_b), econ_b, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Bowling Average"), span(class = paste("metric-val", ifelse(!is.na(avg_b) && (is.na(avg_a) || avg_b <= avg_a), "lead", "")), ifelse(!is.na(avg_b), avg_b, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Strike Rate"), span(class = paste("metric-val", ifelse(!is.na(sr_b) && (is.na(sr_a) || sr_b <= sr_a), "lead", "")), ifelse(!is.na(sr_b), sr_b, "—"))),
            div(class = "metric-row", span(class = "metric-label", "Dot-Ball %"), span(class = paste("metric-val", ifelse(dot_b >= dot_a, "lead", "")), paste0(dot_b, "%")))
          )
        ),
        
        # Radar & Metrics Table Row (Light Card Background)
        fluidRow(
          column(
            width = 6,
            div(
              class = "analytics-card",
              div(
                class = "analytics-card-header",
                div(
                  class = "card-title-group",
                  tags$h2("Normalized Bowling Radar (0–100 Scale)"),
                  tags$p("Scaled against IPL 2025 season benchmarks (inverted for economy/avg)")
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
  
  # Radar Chart Rendering (Light, Spacious, Attractive Polar Layout)
  output$plot_comp_radar <- renderPlotly({
    req(input$comp_player_a, input$comp_player_b, input$comp_mode)
    p_a <- input$comp_player_a
    p_b <- input$comp_player_b
    mode <- input$comp_mode
    
    if (mode == "batting") {
      stat_a <- batting_summary %>% filter(striker == p_a)
      stat_b <- batting_summary %>% filter(striker == p_b)
      
      cats <- c("Runs", "Strike Rate", "Batting Avg", "Boundaries", "Boundary %", "Balls Faced")
      
      runs_a <- if (nrow(stat_a) > 0) stat_a$runs else 0
      sr_a <- if (nrow(stat_a) > 0) stat_a$strike_rate else 0
      avg_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$batting_avg)) stat_a$batting_avg else 0
      bnd_a <- if (nrow(stat_a) > 0) (stat_a$fours + stat_a$sixes) else 0
      bpct_a <- if (nrow(stat_a) > 0) stat_a$boundary_pct else 0
      balls_a <- if (nrow(stat_a) > 0) stat_a$balls else 0
      
      runs_b <- if (nrow(stat_b) > 0) stat_b$runs else 0
      sr_b <- if (nrow(stat_b) > 0) stat_b$strike_rate else 0
      avg_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$batting_avg)) stat_b$batting_avg else 0
      bnd_b <- if (nrow(stat_b) > 0) (stat_b$fours + stat_b$sixes) else 0
      bpct_b <- if (nrow(stat_b) > 0) stat_b$boundary_pct else 0
      balls_b <- if (nrow(stat_b) > 0) stat_b$balls else 0
      
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
      
    } else {
      stat_a <- bowling_summary %>% filter(bowler == p_a)
      stat_b <- bowling_summary %>% filter(bowler == p_b)
      
      cats <- c("Wickets", "Economy", "Bowling Avg", "Strike Rate", "Dot Ball %", "Overs")
      
      wkts_a <- if (nrow(stat_a) > 0) stat_a$wickets else 0
      econ_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$economy)) stat_a$economy else 12.0
      avg_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$bowling_avg)) stat_a$bowling_avg else 45.0
      sr_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$bowling_sr)) stat_a$bowling_sr else 30.0
      dot_a <- if (nrow(stat_a) > 0) stat_a$dot_pct else 0
      overs_a <- if (nrow(stat_a) > 0) stat_a$overs else 0
      
      wkts_b <- if (nrow(stat_b) > 0) stat_b$wickets else 0
      econ_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$economy)) stat_b$economy else 12.0
      avg_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$bowling_avg)) stat_b$bowling_avg else 45.0
      sr_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$bowling_sr)) stat_b$bowling_sr else 30.0
      dot_b <- if (nrow(stat_b) > 0) stat_b$dot_pct else 0
      overs_b <- if (nrow(stat_b) > 0) stat_b$overs else 0
      
      max_wkts <- max(bowling_summary$wickets)
      max_overs <- max(bowling_summary$overs)
      
      norm_a <- c(
        normalize_metric(wkts_a, 0, max_wkts),
        normalize_metric(econ_a, 6.0, 12.0, invert = TRUE),
        normalize_metric(avg_a, 15.0, 45.0, invert = TRUE),
        normalize_metric(sr_a, 10.0, 30.0, invert = TRUE),
        normalize_metric(dot_a, 15.0, 50.0),
        normalize_metric(overs_a, 0, max_overs)
      )
      
      norm_b <- c(
        normalize_metric(wkts_b, 0, max_wkts),
        normalize_metric(econ_b, 6.0, 12.0, invert = TRUE),
        normalize_metric(avg_b, 15.0, 45.0, invert = TRUE),
        normalize_metric(sr_b, 10.0, 30.0, invert = TRUE),
        normalize_metric(dot_b, 15.0, 50.0),
        normalize_metric(overs_b, 0, max_overs)
      )
      
      hover_a <- paste0(cats, "<br><b>", p_a, ": </b>", c(wkts_a, econ_a, avg_a, sr_a, paste0(dot_a, "%"), overs_a), "<br>Score: ", norm_a, "/100")
      hover_b <- paste0(cats, "<br><b>", p_b, ": </b>", c(wkts_b, econ_b, avg_b, sr_b, paste0(dot_b, "%"), overs_b), "<br>Score: ", norm_b, "/100")
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
    req(input$comp_player_a, input$comp_player_b, input$comp_mode)
    p_a <- input$comp_player_a
    p_b <- input$comp_player_b
    mode <- input$comp_mode
    
    if (mode == "batting") {
      stat_a <- batting_summary %>% filter(striker == p_a)
      stat_b <- batting_summary %>% filter(striker == p_b)
      
      runs_a <- if (nrow(stat_a) > 0) stat_a$runs else 0
      runs_b <- if (nrow(stat_b) > 0) stat_b$runs else 0
      balls_a <- if (nrow(stat_a) > 0) stat_a$balls else 0
      balls_b <- if (nrow(stat_b) > 0) stat_b$balls else 0
      sr_a <- if (nrow(stat_a) > 0) stat_a$strike_rate else 0
      sr_b <- if (nrow(stat_b) > 0) stat_b$strike_rate else 0
      avg_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$batting_avg)) stat_a$batting_avg else 0
      avg_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$batting_avg)) stat_b$batting_avg else 0
      fours_a <- if (nrow(stat_a) > 0) stat_a$fours else 0
      fours_b <- if (nrow(stat_b) > 0) stat_b$fours else 0
      sixes_a <- if (nrow(stat_a) > 0) stat_a$sixes else 0
      sixes_b <- if (nrow(stat_b) > 0) stat_b$sixes else 0
      bpct_a <- if (nrow(stat_a) > 0) stat_a$boundary_pct else 0
      bpct_b <- if (nrow(stat_b) > 0) stat_b$boundary_pct else 0
      matches_a <- if (nrow(stat_a) > 0) stat_a$matches else 0
      matches_b <- if (nrow(stat_b) > 0) stat_b$matches else 0
      
      score_df <- tibble(
        Metric = c("Total Runs", "Balls Faced", "Batting Average", "Strike Rate", "Fours (4s)", "Sixes (6s)", "Boundary Run %", "Matches Played"),
        `Player A` = as.character(c(runs_a, balls_a, ifelse(avg_a > 0, avg_a, "—"), sr_a, fours_a, sixes_a, paste0(bpct_a, "%"), matches_a)),
        Leader = c(
          ifelse(runs_a > runs_b, paste("👈", p_a), ifelse(runs_b > runs_a, paste(p_b, "👉"), "Tie")),
          ifelse(balls_a > balls_b, paste("👈", p_a), ifelse(balls_b > balls_a, paste(p_b, "👉"), "Tie")),
          ifelse(avg_a > avg_b, paste("👈", p_a), ifelse(avg_b > avg_a, paste(p_b, "👉"), "Tie")),
          ifelse(sr_a > sr_b, paste("👈", p_a), ifelse(sr_b > sr_a, paste(p_b, "👉"), "Tie")),
          ifelse(fours_a > fours_b, paste("👈", p_a), ifelse(fours_b > fours_a, paste(p_b, "👉"), "Tie")),
          ifelse(sixes_a > sixes_b, paste("👈", p_a), ifelse(sixes_b > sixes_a, paste(p_b, "👉"), "Tie")),
          ifelse(bpct_a > bpct_b, paste("👈", p_a), ifelse(bpct_b > bpct_a, paste(p_b, "👉"), "Tie")),
          ifelse(matches_a > matches_b, paste("👈", p_a), ifelse(matches_b > matches_a, paste(p_b, "👉"), "Tie"))
        ),
        `Player B` = as.character(c(runs_b, balls_b, ifelse(avg_b > 0, avg_b, "—"), sr_b, fours_b, sixes_b, paste0(bpct_b, "%"), matches_b))
      )
    } else {
      stat_a <- bowling_summary %>% filter(bowler == p_a)
      stat_b <- bowling_summary %>% filter(bowler == p_b)
      
      wkts_a <- if (nrow(stat_a) > 0) stat_a$wickets else 0
      wkts_b <- if (nrow(stat_b) > 0) stat_b$wickets else 0
      runs_a <- if (nrow(stat_a) > 0) stat_a$runs_conceded else 0
      runs_b <- if (nrow(stat_b) > 0) stat_b$runs_conceded else 0
      balls_a <- if (nrow(stat_a) > 0) stat_a$legal_balls else 0
      balls_b <- if (nrow(stat_b) > 0) stat_b$legal_balls else 0
      econ_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$economy)) stat_a$economy else NA
      econ_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$economy)) stat_b$economy else NA
      avg_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$bowling_avg)) stat_a$bowling_avg else NA
      avg_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$bowling_avg)) stat_b$bowling_avg else NA
      sr_a <- if (nrow(stat_a) > 0 && !is.na(stat_a$bowling_sr)) stat_a$bowling_sr else NA
      sr_b <- if (nrow(stat_b) > 0 && !is.na(stat_b$bowling_sr)) stat_b$bowling_sr else NA
      dot_a <- if (nrow(stat_a) > 0) stat_a$dot_pct else 0
      dot_b <- if (nrow(stat_b) > 0) stat_b$dot_pct else 0
      
      score_df <- tibble(
        Metric = c("Total Wickets", "Runs Conceded", "Legal Balls Bowled", "Economy Rate", "Bowling Average", "Bowling Strike Rate", "Dot-Ball %"),
        `Player A` = as.character(c(wkts_a, runs_a, balls_a, ifelse(!is.na(econ_a), econ_a, "—"), ifelse(!is.na(avg_a), avg_a, "—"), ifelse(!is.na(sr_a), sr_a, "—"), paste0(dot_a, "%"))),
        Leader = c(
          ifelse(wkts_a > wkts_b, paste("👈", p_a), ifelse(wkts_b > wkts_a, paste(p_b, "👉"), "Tie")),
          ifelse(runs_a < runs_b, paste("👈", p_a), ifelse(runs_b < runs_a, paste(p_b, "👉"), "Tie")),
          ifelse(balls_a > balls_b, paste("👈", p_a), ifelse(balls_b > balls_a, paste(p_b, "👉"), "Tie")),
          ifelse(!is.na(econ_a) && (is.na(econ_b) || econ_a < econ_b), paste("👈", p_a), ifelse(!is.na(econ_b) && (is.na(econ_a) || econ_b < econ_a), paste(p_b, "👉"), "Tie")),
          ifelse(!is.na(avg_a) && (is.na(avg_b) || avg_a < avg_b), paste("👈", p_a), ifelse(!is.na(avg_b) && (is.na(avg_a) || avg_b < avg_a), paste(p_b, "👉"), "Tie")),
          ifelse(!is.na(sr_a) && (is.na(sr_b) || sr_a < sr_b), paste("👈", p_a), ifelse(!is.na(sr_b) && (is.na(sr_a) || sr_b < sr_a), paste(p_b, "👉"), "Tie")),
          ifelse(dot_a > dot_b, paste("👈", p_a), ifelse(dot_b > dot_a, paste(p_b, "👉"), "Tie"))
        ),
        `Player B` = as.character(c(wkts_b, runs_b, balls_b, ifelse(!is.na(econ_b), econ_b, "—"), ifelse(!is.na(avg_b), avg_b, "—"), ifelse(!is.na(sr_b), sr_b, "—"), paste0(dot_b, "%")))
      )
    }
    
    colnames(score_df)[2] <- p_a
    colnames(score_df)[4] <- p_b
    
    datatable(
      score_df,
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
  })
}

# ------------------------------------------------------------------------------
# 4. RUN APPLICATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)

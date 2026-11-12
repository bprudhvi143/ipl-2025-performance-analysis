# 🏏 IPL 2025 Performance Analysis — Cricket Intelligence Platform

[![R 4.6+](https://img.shields.io/badge/R-4.6%2B-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Bootstrap_5-4479A1?logo=rstudio&logoColor=white)](https://shiny.posit.co/)
[![Plotly](https://img.shields.io/badge/Plotly-Interactive_Charts-3F4F75?logo=plotly&logoColor=white)](https://plotly.com/r/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)

An enterprise-grade, interactive sports analytics and cricket intelligence platform built in **R Shiny** to analyze the complete ball-by-ball dataset of the **IPL 2025 Season** (17,246 deliveries across 74 matches). 

The platform features a modern dark-themed interface, statistically rigorous cricket performance modeling, interactive Plotly visualizations, searchable DataTables, and a head-to-head **Player Comparison Studio** with normalized radar intelligence.

🔗 **Live Application:** [🏏 IPL 2025 Analytics Dashboard](https://vizwithprudhvi.shinyapps.io/datasets/)  
📦 **GitHub Repository:** [bprudhvi143/ipl-2025-performance-analysis](https://github.com/bprudhvi143/ipl-2025-performance-analysis)

---

## 📌 Key Platform Features

### 1. 📊 IPL 2025 Season Overview
- **Dynamic KPI Cards:** Instant summary of total matches (74), legal deliveries (17,246), cumulative runs scored (26,493), and total dismissals (871 fallen, 830 credited to bowlers).
- **Runs Distribution by Phase & Innings:** Over-phase breakdown comparing run generation across Powerplay (Overs 1–6), Middle (Overs 7–15), and Death overs (Overs 16–20) for 1st vs 2nd innings.
- **Franchise Total Runs Comparison:** Horizontal bar rankings of cumulative scoring output across all 10 franchises.
- **Orange Cap Contenders (Top 5 Batsmen):** Real-time leaderboard detailing runs, strike rate, batting average, fours, and sixes.
- **Purple Cap Contenders (Top 5 Bowlers):** Real-time leaderboard detailing wickets, legal overs bowled, economy rate, and dot-ball percentage.
- **Franchise Standings:** Season table with matches, runs scored, balls faced, scoring run rate, and wickets taken.

### 2. 👤 Unified Player Analytics
- **Searchable Player Selection:** Access all 168 batters and 129 bowlers through instant autocomplete.
- **Batting Analysis Module:**
  - 8 core performance indicators: Total Runs, Balls Faced, Batting Average (with not-out handling), Strike Rate, Fours, Sixes, Boundary Run %, and Matches Played.
  - Match-by-match scoring progression with balls faced.
  - Run composition breakdown donut chart (4s, 6s, and running between the wickets).
  - Target setting vs. chasing innings splits.
  - Phase-by-phase scoring analysis (Powerplay, Middle, Death).
  - Searchable match-by-match innings log table.
- **Bowling Analysis Module:**
  - 7 core bowling indicators: Credited Wickets, Runs Conceded, Legal Overs Bowled, Economy Rate, Bowling Average, Bowling Strike Rate, and Dot-Ball %.
  - Match-by-match spell trend tracking wickets and runs conceded.
  - Economy rate progression mapped against the tournament baseline (8.5 RPO).
  - Target defending vs containing innings splits.
  - Searchable spell scorecard log.

### 3. 🛡️ Franchise & Team Analytics
- **Franchise Selection & Season Filtering:** Inspect detailed franchise metrics for any of the 10 teams.
- **Team KPI Cards:** Runs scored, wickets taken, matches played, and net scoring run rate.
- **Match-by-Match Scoring Trend:** Per-match team totals compared with team season average.
- **Innings Splits:** Runs and wickets comparative breakdown between setting and chasing targets.
- **Phase Distribution:** Batting scoring rates and bowling breakthroughs across Powerplay, Middle, and Death overs.
- **Franchise Top 3 Batsmen & Bowlers:** Top performers within the selected squad.

### 4. ⚔️ Player Comparison Studio (Head-to-Head Duel)
- **Interactive Matchup:** Compare any two players side-by-side in either **Batting Duel** or **Bowling Duel** mode.
- **Dynamic Leader Highlighting:** Automatic detection of metric leaders with visual leader badges.
- **Normalized Multi-Axis Radar Model:** 
  - Standardizes raw performance metrics onto a 0–100 scale based on IPL 2025 season benchmarks.
  - Properly inverts defensive bowling statistics (Economy Rate, Bowling Average, and Bowling Strike Rate) so lower, superior figures yield higher normalized scores.
  - Interactive tooltips display both normalized scores and actual raw statistics simultaneously.
- **Comprehensive Scorecard Table:** Granular head-to-head comparison with directional leader indicators.
- **Safe Edge-Case Handling:** Clean fallbacks and warnings for identical player selections or players with insufficient records.

---

## 📐 Statistical Rules & Precision Modeling

The platform adheres strictly to official cricket laws and analytical standards:

| Dimension | Metric | Exact Calculation Rule |
| :--- | :--- | :--- |
| **Batting** | **Total Runs** | `sum(runs_of_bat)` |
| **Batting** | **Balls Faced** | `sum(wide == 0)` (Wides excluded; No-balls count as balls faced) |
| **Batting** | **Batting Average** | `total_runs / dismissals` (Counts all dismissals where player was out, including non-striker runouts; non-dismissals formatted with `*`) |
| **Batting** | **Strike Rate** | `(total_runs / balls_faced) * 100` |
| **Batting** | **Boundary Run %** | `((fours * 4 + sixes * 6) / total_runs) * 100` |
| **Bowling** | **Legal Deliveries** | `sum(wide == 0 & noballs == 0)` |
| **Bowling** | **Bowler Wickets** | Bowler-credited wickets only: `bowled`, `caught`, `hit wicket`, `lbw`, `stumped` (Excludes runouts, retired hurt, and retired out) |
| **Bowling** | **Runs Conceded** | `runs_of_bat + extras(wide & noballs)` (Byes, leg-byes, and penalty runs excluded) |
| **Bowling** | **Economy Rate** | `runs_conceded / (legal_balls / 6)` |
| **Bowling** | **Bowling Average** | `runs_conceded / wickets` |
| **Bowling** | **Bowling Strike Rate** | `legal_balls / wickets` |
| **Bowling** | **Dot-Ball %** | `(dot_balls / legal_balls) * 100` where a dot ball is a legal delivery with 0 runs conceded |
| **Team** | **Scoring Run Rate** | `team_runs_scored / (legal_balls_faced / 6)` |

---

## 🛠️ Technology Stack

- **Core Engine:** R (v4.6+)
- **Application Framework:** [Shiny](https://shiny.posit.co/)
- **Theme & UI Architecture:** [bslib](https://rstudio.github.io/bslib/) (Bootstrap 5, dark theme palette)
- **Data Manipulation:** [tidyverse](https://www.tidyverse.org/) (`dplyr`, `tidyr`, `readr`, `tibble`, `purrr`)
- **Interactive Visualizations:** [Plotly for R](https://plotly.com/r/)
- **Interactive Data Tables:** [DT](https://rstudio.github.io/DT/)
- **Controls & UI Components:** `shinyWidgets`, `shinycssloaders`, `bsicons`, `scales`
- **Custom Styling:** Pure CSS3 (`www/styles.css`) with responsive CSS Grid and Flexbox

---

## 🎨 Official IPL Brand Design System & Color Palette

Designed using the official IPL logo color identity:

| UI Component | Hex Code | Visual Sample & Description |
| :--- | :--- | :--- |
| **IPL Electric Swoosh Blue** | `#0090DF` | Dynamic curved stroke from the official IPL logo |
| **IPL Royal Navy Blue** | `#1D428A` | Powerful batsman silhouette blue |
| **IPL Trophy Gold** | `#FFB800` | Golden champions highlight and secondary accent |
| **Main Canvas Background** | `#0A101D` | Deep Midnight IPL Stadium Navy |
| **Sidebar Navigation** | `#0E1726` | Deep Slate Navy |
| **Card Surface** | `#152238` | Midnight Blue analytical card |
| **Elevated Card Surface** | `#1C2E4C` | Interactive hover & active container |
| **Borders & Dividers** | `#253B5C` | Crisp navy border containment |
| **Primary Typography** | `#FFFFFF` | Inter / High-contrast white |
| **Official Logo** | `www/ipl_logo.png` | Official IPL Batsman icon integrated in sidebar & topbar |

---

## 📊 Dataset Information

- **Local Path:** `data/ipl_2025_deliveries.csv`
- **Total Deliveries:** 17,246 records
- **Matches Covered:** 74 matches (Group stage and Playoffs)
- **Franchises:** CSK, DC, GT, KKR, LSG, MI, PBKS, RCB, RR, SRH
- **Variables:** `match_id`, `season`, `phase`, `match_no`, `date`, `venue`, `batting_team`, `bowling_team`, `innings`, `over`, `striker`, `bowler`, `runs_of_bat`, `extras`, `wide`, `legbyes`, `byes`, `noballs`, `wicket_type`, `player_dismissed`, `fielder`.

---

## 📁 Project Directory Structure

```text
IPL_2025_Performance_Analysis/
│
├── .git/
├── .gitignore
├── app.R                                   # Main R Shiny dashboard application
├── ipl-2025-performance-analysis.Rproj     # RStudio project configuration
├── README.md                               # Project documentation & reference
│
├── data/
│   └── ipl_2025_deliveries.csv             # Official IPL 2025 ball-by-ball dataset
│
├── plots/
│   ├── ipl2025_analysis.png                # Strike rate distribution
│   ├── ipl2025_bat_perf.png                # Top batsmen chart
│   └── ipl2025_bowl_perf.png               # Top bowlers chart
│
├── scripts/
│   └── ipl_player_performance_analyzer.R   # Baseline exploratory analysis script
│
├── tests/
│   └── validation_tests.R                  # Automated statistical & reactive test suite
│
└── www/
    ├── ipl_logo.png                        # Official IPL batsman icon
    ├── logo.png                            # Application logo asset
    ├── logo.svg                            # Vector badge fallback
    └── styles.css                          # Official IPL dark theme stylesheet
```

---

## 📸 Sample Visualizations

<div align="center">

### Strike Rate Distribution
<img width="500" alt="ipl2025_analysis" src="https://github.com/user-attachments/assets/fe6a142f-0afe-4034-bd06-57f8f9361876" />

### Top Batsmen by Runs
<img width="500" alt="ipl2025_bat_perf" src="https://github.com/user-attachments/assets/5aa9ef58-4080-4893-800b-83290d4af768" />

### Top Bowlers by Wickets
<img width="500" alt="ipl2025_bowl_perf" src="https://github.com/user-attachments/assets/35f5bd3a-9319-4069-a956-5d92c6251fff" />

</div>

---

## ▶️ How to Run Locally

### 1. Prerequisites
Ensure you have **R 4.2+** installed on your system.

### 2. Clone the Repository
```bash
git clone https://github.com/bprudhvi143/ipl-2025-performance-analysis.git
cd ipl-2025-performance-analysis
git checkout ui-redesign
```

### 3. Install Dependencies
Open R or RStudio and install the required packages:
```r
install.packages(c(
  "shiny",
  "tidyverse",
  "bslib",
  "plotly",
  "DT",
  "bsicons",
  "shinyWidgets",
  "shinycssloaders",
  "scales"
))
```

### 4. Run the Validation Test Suite
Verify that all statistical calculations, edge cases, and server pipelines pass:
```bash
Rscript tests/validation_tests.R
```

### 5. Launch the Dashboard
Run the application from the project root:
```r
shiny::runApp()
```
Or via the terminal:
```bash
Rscript -e "shiny::runApp('.')"
```

The application will start and open in your default browser at `http://127.0.0.1:port/`.

---

## 🚀 Manual Deployment to shinyapps.io

To deploy the updated application to your existing live instance:

1. In R, ensure `rsconnect` is installed and authorized:
   ```r
   install.packages("rsconnect")
   ```
2. Deploy the application:
   ```r
   rsconnect::deployApp(
     appDir = ".",
     appName = "datasets",
     account = "vizwithprudhvi",
     forceUpdate = TRUE
   )
   ```

---

## 👥 Author & Acknowledgements
- **Author:** Prudhvi
- **Live Demo:** [IPL 2025 Analytics Dashboard](https://vizwithprudhvi.shinyapps.io/datasets/)
- **Data Source:** IPL 2025 Complete Ball-by-Ball Dataset

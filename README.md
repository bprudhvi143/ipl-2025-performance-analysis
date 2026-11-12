IPL 2025 Performance Analysis – R Shiny Dashboard

🔗 **Live Dashboard:** [🏏 IPL 2025 Analytics Dashboard](https://vizwithprudhvi.shinyapps.io/datasets/)

📌 Overview
An interactive R Shiny dashboard built to analyze IPL 2025 ball-by-ball data.
The project focuses on Exploratory Data Analysis (EDA), data cleaning,
statistical analysis, and data visualization to generate meaningful insights
into team and player performances.

📊 Dataset
- **Local File:** `data/ipl_2025_deliveries.csv`
- **Kaggle Sources:**
  - [IPL 2025 Ball-by-Ball Dataset (Kaggle Search)](https://www.kaggle.com/datasets?search=ipl+2025+deliveries)
  - [IPL 2025 Complete Match & Ball-by-Ball Dataset](https://www.kaggle.com/datasets/7714511)
  - [IPL Ball-By-Ball Dataset (2020–2025)](https://www.kaggle.com/datasets/7108074)
  - [IPL Ball By Ball (2008–2025)](https://www.kaggle.com/datasets/2616463)
- **Format:** CSV
- **Records:** 17,246 deliveries (2025 season)

🛠️ Tools & Technologies
- R
- Shiny
- ggplot2
- dplyr
- Statistical Analysis


🔍 Key Features
- Team-wise performance analysis
- Batting and bowling insights
- Player-wise statistics (runs, wickets, strike rate)
- Interactive filters and dynamic visualizations


📈 Concepts Applied
- Exploratory Data Analysis (EDA)
- Data Cleaning
- Data Visualization
- Statistical Analysis
- Dashboard Development


📸 Sample Visualizations

Strike Rate Distribution

<img width="419" height="214" alt="ipl2025_analysis" src="https://github.com/user-attachments/assets/fe6a142f-0afe-4034-bd06-57f8f9361876" />


Top 10 Batsmen by Runs

<img width="419" height="214" alt="ipl2025_bat_perf" src="https://github.com/user-attachments/assets/5aa9ef58-4080-4893-800b-83290d4af768" />


Top 10 Bowlers by Wickets

<img width="419" height="214" alt="ipl2025_bowl_perf" src="https://github.com/user-attachments/assets/35f5bd3a-9319-4069-a956-5d92c6251fff" />


---
📁 Project Structure
```text
.
├── app.R                                   # Shiny dashboard application
├── data/
│   └── ipl_2025_deliveries.csv             # Ball-by-ball IPL 2025 dataset
├── plots/
│   ├── ipl2025_analysis.png                # Strike rate distribution plot
│   ├── ipl2025_bat_perf.png                # Top 10 batsmen plot
│   └── ipl2025_bowl_perf.png               # Top 10 bowlers plot
├── scripts/
│   └── ipl_player_performance_analyzer.R   # Exploratory data analysis script
├── ipl-2025-performance-analysis.Rproj     # RStudio project file
└── README.md
```

---
▶️ How to Run
```r
# Install required packages
install.packages(c("shiny", "tidyverse"))

# Run the app
shiny::runApp()
```


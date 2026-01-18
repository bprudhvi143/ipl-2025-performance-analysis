IPL 2025 Performance Analysis – R Shiny Dashboard

📌 Overview
An interactive R Shiny dashboard built to analyze IPL 2025 ball-by-ball data.
The project focuses on Exploratory Data Analysis (EDA), data cleaning,
statistical analysis, and data visualization to generate meaningful insights
into team and player performances.

📊 Dataset
- **Source:** Kaggle (IPL 2025 Ball-by-Ball Data)
- **Format:** CSV
- **Records:** 90,000+

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
![Strike Rate Distribution]<img width="419" height="214" alt="ipl2025_analysis" src="https://github.com/user-attachments/assets/fe6a142f-0afe-4034-bd06-57f8f9361876" />


Top 10 Batsmen by Runs<img width="419" height="214" alt="ipl2025_bat_perf" src="https://github.com/user-attachments/assets/5aa9ef58-4080-4893-800b-83290d4af768" />

![Top Batsmen]

Top 10 Bowlers by Wickets
![Top Bowlers]<img width="419" height="214" alt="ipl2025_bowl_perf" src="https://github.com/user-attachments/assets/35f5bd3a-9319-4069-a956-5d92c6251fff" />


---
▶️ How to Run
```r
# Install required packages
install.packages(c("shiny", "ggplot2", "dplyr"))

# Run the app
shiny::runApp()


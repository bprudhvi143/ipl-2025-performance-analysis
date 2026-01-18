
library(tidyverse)

ipl <- read.csv("ipl_2025_deliveries.csv")


str(ipl)
head(ipl)


#Batting Performance

batting_summary <- ipl %>%
  group_by(striker) %>%
  summarise(
    total_runs = sum(runs_of_bat, na.rm = TRUE),
    balls_faced = n() - sum(wide, na.rm = TRUE),  # wides not counted as faced
    dismissals = sum(!is.na(player_dismissed) & player_dismissed == striker),
    .groups = 'drop'
  ) %>%
  mutate(
    strike_rate = round((total_runs / balls_faced) * 100, 2),
    batting_avg = ifelse(dismissals > 0, round(total_runs / dismissals, 2), NA)
  ) %>%
  arrange(desc(total_runs))

head(batting_summary, 10)


#Bowling Performance

bowling_summary <- ipl %>%
  group_by(bowler) %>%
  summarise(
    runs_conceded = sum(runs_of_bat + extras, na.rm = TRUE),
    balls_bowled = n() - sum(wide, na.rm = TRUE) - sum(noballs, na.rm = TRUE), 
    wickets = sum(!is.na(player_dismissed) & wicket_type != "" & wicket_type != "run out", na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    overs = balls_bowled / 6,
    economy = round(runs_conceded / overs, 2)
  ) %>%
  arrange(desc(wickets))

head(bowling_summary, 10)


#Visualizations
# Top 10 Run Scorers
top_batsmen <- batting_summary %>% head(10)

ggplot(top_batsmen, aes(x=reorder(striker, total_runs), y=total_runs, fill=total_runs)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Top 10 Batsmen by Runs", x="Player", y="Total Runs")

# Top 10 Wicket Takers
top_bowlers <- bowling_summary %>% head(10)

ggplot(top_bowlers, aes(x=reorder(bowler, wickets), y=wickets, fill=wickets)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title="Top 10 Bowlers by Wickets", x="Player", y="Wickets Taken")

# Strike Rate Distribution
ggplot(batting_summary, aes(x=strike_rate)) +
  geom_histogram(bins=30, fill="blue", color="white", alpha=0.7) +
  labs(title="Distribution of Strike Rates", x="Strike Rate", y="Number of Players")

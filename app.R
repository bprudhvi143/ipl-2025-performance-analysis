# -------------------------------------------
# IPL 2025 Shiny Dashboard (Extended with Team Analysis)
# -------------------------------------------
library(shiny)
library(tidyverse)

# Load dataset
ipl <- read.csv("ipl_2025_deliveries.csv")

# Precompute batting & bowling summaries
batting_summary <- ipl %>%
  group_by(striker) %>%
  summarise(total_runs = sum(runs_of_bat, na.rm = TRUE),
            balls = n() - sum(wide, na.rm = TRUE),
            .groups="drop")

bowling_summary <- ipl %>%
  group_by(bowler) %>%
  summarise(wickets = sum(!is.na(player_dismissed) & wicket_type != "" & wicket_type != "run out"),
            runs_conceded = sum(runs_of_bat + extras, na.rm = TRUE),
            .groups="drop")

# -------------------------------------------
# UI
# -------------------------------------------
ui <- fluidPage(
  titlePanel("🏏 IPL 2025 Analytics Dashboard"),
  tabsetPanel(
    
    # Batting Tab
    tabPanel("Batting Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("batsman", "Choose a Batsman:", 
                             choices = unique(ipl$striker))
               ),
               mainPanel(
                 tableOutput("battingTable"),
                 plotOutput("battingPlot")
               )
             )
    ),
    
    # Bowling Tab
    tabPanel("Bowling Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("bowler", "Choose a Bowler:", 
                             choices = unique(ipl$bowler))
               ),
               mainPanel(
                 tableOutput("bowlingTable"),
                 plotOutput("bowlingPlot")
               )
             )
    ),
    
    # Team Tab
    tabPanel("Team Analysis",
             sidebarLayout(
               sidebarPanel(
                 selectInput("team", "Choose a Team:", choices = unique(ipl$batting_team)),
                 selectInput("season", "Choose a Season:", choices = unique(ipl$season))
               ),
               mainPanel(
                 h4("Team Summary"),
                 tableOutput("teamSummary"),
                 h4("Top 3 Batsmen"),
                 tableOutput("topBatsmen"),
                 h4("Top 3 Bowlers"),
                 tableOutput("topBowlers")
               )
             )
    )
  )
)

# -------------------------------------------
# SERVER
# -------------------------------------------
server <- function(input, output) {
  
  # Batting performance
  output$battingTable <- renderTable({
    batting_summary %>% filter(striker == input$batsman)
  })
  
  output$battingPlot <- renderPlot({
    player_data <- ipl %>%
      filter(striker == input$batsman) %>%
      group_by(match_id) %>%
      summarise(runs = sum(runs_of_bat, na.rm = TRUE), .groups="drop")
    
    ggplot(player_data, aes(x=match_id, y=runs)) +
      geom_line(color="blue") + geom_point(color="red") +
      labs(title=paste("Runs per Match -", input$batsman),
           x="Match ID", y="Runs")
  })
  
  # Bowling performance
  output$bowlingTable <- renderTable({
    bowling_summary %>% filter(bowler == input$bowler)
  })
  
  output$bowlingPlot <- renderPlot({
    bowler_data <- ipl %>%
      filter(bowler == input$bowler) %>%
      group_by(match_id) %>%
      summarise(wickets = sum(!is.na(player_dismissed) & wicket_type != "" & wicket_type != "run out"),
                .groups="drop")
    
    ggplot(bowler_data, aes(x=match_id, y=wickets)) +
      geom_line(color="green") + geom_point(color="orange") +
      labs(title=paste("Wickets per Match -", input$bowler),
           x="Match ID", y="Wickets")
  })
  
  # Team performance
  output$teamSummary <- renderTable({
    team_data <- ipl %>%
      filter(batting_team == input$team & season == input$season)
    
    bowling_data <- ipl %>%
      filter(bowling_team == input$team & season == input$season)
    
    tibble(
      Team = input$team,
      Season = input$season,
      Total_Runs = sum(team_data$runs_of_bat, na.rm = TRUE) + sum(team_data$extras, na.rm = TRUE),
      Total_Wickets = sum(!is.na(bowling_data$player_dismissed) & bowling_data$wicket_type != "" & bowling_data$wicket_type != "run out")
    )
  })
  
  output$topBatsmen <- renderTable({
    ipl %>%
      filter(batting_team == input$team & season == input$season) %>%
      group_by(striker) %>%
      summarise(runs = sum(runs_of_bat, na.rm = TRUE), .groups="drop") %>%
      arrange(desc(runs)) %>%
      head(3)
  })
  
  output$topBowlers <- renderTable({
    ipl %>%
      filter(bowling_team == input$team & season == input$season) %>%
      group_by(bowler) %>%
      summarise(wickets = sum(!is.na(player_dismissed) & wicket_type != "" & wicket_type != "run out"),
                .groups="drop") %>%
      arrange(desc(wickets)) %>%
      head(3)
  })
}

# Run app
shinyApp(ui = ui, server = server)

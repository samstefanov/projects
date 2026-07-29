#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("Velocity and release point relationship"),
  navbarPage("Tabs",
    tabPanel("Relase point vs velocity",

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
            selectInput("teamName", "Select Team",
                        choices = unique(oct_pitcher_data_clean$team), 
                        selected = unique(oct_pitcher_data_clean$team)),
            selectInput("playerName",
                        "Player Name",
                        choices = NULL),
            checkboxGroupInput("pitch_type", "Pitch Types", choices = unique(oct_pitcher_data_clean$pitchtype), selected = unique(oct_pitcher_data_clean$pitchtype))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot"),
           plotOutput("scatterPlot1"),
           tableOutput("vtable")
        )),
    div(
      style = "
            position: fixed;
            bottom: 0;
            left: 0;
            width: 100%;
            color: black;
            font-size: 15px;
            font-weight: bold;
            text-align: left;
            padding: 15px;
            z-index: 1000;
           ",
      "Some variables have missing data, so not all data points will be printed"
    )
    ),
    tabPanel("Spin Rate vs velocity",         
              #Sidebar with a slider input for number of bins 
            sidebarLayout(
               sidebarPanel(
                 selectInput("teamName1", "Select Team",
                             choices = unique(oct_pitcher_data_clean$team), 
                             selected = unique(oct_pitcher_data_clean$team)),
                 selectInput("playerName1",
                             "Player Name",
                             choices = NULL),
                 checkboxGroupInput("pitch_type1", "Pitch Types", choices = unique(oct_pitcher_data_clean$pitchtype), selected = unique(oct_pitcher_data_clean$pitchtype))
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("scatterPlot2"),
                 plotOutput("scatterPlot3"),
                 plotOutput("scatterPlot4"),
                 tableOutput("spintable")
               )),
            div(
              style = "
            position: fixed;
            bottom: 0;
            left: 0;
            width: 100%;
            color: black;
            font-size: 15px;
            font-weight: bold;
            text-align: left;
            padding: 15px;
            z-index: 1000;
           ",
              "Some variables have missing data, so not all data points will be printed"
            )
),
    tabPanel("Hits",
         
         # Sidebar with a slider input for number of bins 
         sidebarLayout(
           sidebarPanel(
             selectInput("teamName2", "Select Team",
                         choices = unique(oct_pitcher_data_hits$team_hitter), 
                         selected = unique(oct_pitcher_data_hits$team_hitter)),
             selectInput("playerName2",
                         "Player Name",
                         choices = NULL),
             checkboxGroupInput("pitch_type2", "Pitch Types", choices = unique(oct_pitcher_data_hits$pitchtype), selected = unique(oct_pitcher_data_hits$pitchtype)),
             checkboxGroupInput("hit_type", "Hit Types", choices = unique(oct_pitcher_data_hits$bb_type), select = unique(oct_pitcher_data_hits$bb_type))
           ),
           
           # Show a plot of the generated distribution
           mainPanel(
             plotOutput("scatterPlot5"),
             plotOutput("scatterPlot6"),
             plotOutput("scatterPlot7"),
             tableOutput("hitstable")
           )),
         div(
           style = "
            position: fixed;
            bottom: 0;
            left: 0;
            width: 100%;
            color: black;
            font-size: 15px;
            font-weight: bold;
            text-align: left;
            padding: 15px;
            z-index: 1000;
           ",
           "Some variables have missing data, so not all data points will be printed",
           tags$br(),
           "(in particular for bat speed)."
         )
  ),
  tabPanel("Graphplot",
           sidebarPanel(
             selectInput("teamName3", "Select Team", 
                         choices = unique(oct_pitcher_data_clean$team),
                         selected = unique(oct_pitcher_data_clean$team)),
             selectInput("pitcherstat", "Select stat", choices = c("walk_rate", "swinging_strike_rate", "strike_rate"), selected = "walk rate")),
           mainPanel(
             plotOutput("pitcher_stats_plot"),
           )
  ),
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$teamName, {
    p_choices = oct_pitcher_data_clean %>%
        filter(team == input$teamName) %>%
        pull(player_name) %>%
        unique()
    updateSelectInput(session, "playerName", choices = p_choices)})
  
  observeEvent(input$playerName, {
    p_choices_a = oct_pitcher_data_clean %>%
      filter(player_name == input$playerName) %>%
      pull(pitchtype) %>%
      unique()
    updateCheckboxGroupInput(
      session,
      "pitch_type",
      choices = p_choices_a,
      selected = p_choices_a
    )
  })
  
  observeEvent(input$teamName1, {
    p_choices1 = oct_pitcher_data_clean %>%
      filter(team == input$teamName1) %>%
      pull(player_name) %>%
      unique()
    updateSelectInput(session, "playerName1", choices = p_choices1)})
  
  observeEvent(input$playerName1, {
    p_choices_b = oct_pitcher_data_clean %>%
      filter(player_name == input$playerName1) %>%
      pull(pitchtype) %>%
      unique()
    updateCheckboxGroupInput(
      session,
      "pitch_type1",
      choices = p_choices_b,
      selected = p_choices_b
    )
  })
  
  observeEvent(input$teamName2, {
    p_choices2 = oct_pitcher_data_hits %>%
      filter(team_hitter == input$teamName2) %>%
      pull(player_name_hitter) %>%
      unique()
    updateSelectInput(session, "playerName2", choices = p_choices2)})
  
  observeEvent(input$playerName2, {
    p_choices_c = oct_pitcher_data_hits %>%
      filter(player_name_hitter == input$playerName2) %>%
      pull(pitchtype) %>%
      unique()
    updateCheckboxGroupInput(
      session,
      "pitch_type2",
      choices = p_choices_c,
      selected = p_choices_c
    )
  })
  
  observeEvent(input$playerName2, {
    p_choices_k = oct_pitcher_data_hits %>%
      filter(player_name_hitter == input$playerName2) %>%
      pull(bb_type) %>%
      unique()
    updateCheckboxGroupInput(
      session,
      "hit_type",
      choices = p_choices_k,
      selected = p_choices_k
    )
  })
  
  pitcher_stats = pitcher_table$ball
  
  Pitcher_Stats = reactive({
    req(input$teamName3, input$pitcherstat)
    
    pitcher_table %>%
      filter(team == input$teamName3)
  })
  
  xFilter = reactive({
    req(input$playerName)
    req(input$pitch_type)
    oct_pitcher_data_clean %>% filter(player_name == input$playerName,
                                      pitchtype %in% input$pitch_type)
  })
  zFilter = reactive({
    req(input$playerName1)
    req(input$pitch_type1)
    oct_pitcher_data_clean %>% filter(player_name == input$playerName1,
                                      pitchtype %in% input$pitch_type1)
  })
  hitFilter = reactive({
    req(input$playerName2)
    req(input$pitch_type2)
    req(input$hit_type)
    oct_pitcher_data_hits %>% filter(player_name_hitter == input$playerName2,
                                     pitchtype %in% input$pitch_type2,
                                     bb_type %in% input$hit_type)
  })

    output$scatterPlot <- renderPlot({
        ggplot(data = xFilter(), aes(x = release_pos_x, y = release_speed, color = description_adj, shape = pitchtype)) + geom_point(size = 5) +
        scale_color_manual(values = c("hit" = "blue", "out" = "purple", "error_or_fielders_choice" = "violet", "called_strike" = "red", "swinging_strike" = "darkred", "ball" = "green", "foul" = "orange", "blocked_ball" = "magenta", "swinging_strike_blocked" = "darkred", "hit_by_pitch" = "cyan", "foul_bunt" = "orange", "foul_tip" = "orange", "missed_bunt" = "darkred", "bunt_foul_tip" = "orange"),
                           breaks = c("hit", "out", "error_or_fielders_choice", "called_strike", "swinging_strike", "ball", "foul", "blocked_ball", "hit_by_pitch")) +
        scale_shape_manual(values = c("Fastball" = 15, "Slider" = 16, "Sweeper" = 17, "Changeup" = 18, "Sinker" = 8, "Splitter" = 25, "Curveball" = 7, "Cutter" = 9, "Knuckle-Curve" = 10, "Forkball" = 11, "Slurve" = 12, "Slow Curve" = 13)) +
        geom_vline(xintercept = -1.63) +
        geom_hline(yintercept = 91.5)
    })
    
    output$scatterPlot1 <- renderPlot({
      ggplot(data = xFilter(), aes(x = release_pos_z, y = release_speed, color = description_adj, shape = pitchtype)) + geom_point(size = 5) +
        scale_color_manual(values = c("hit" = "blue", "out" = "purple", "error_or_fielders_choice" = "violet", "called_strike" = "red", "swinging_strike" = "darkred", "ball" = "green", "foul" = "orange", "blocked_ball" = "magenta", "swinging_strike_blocked" = "darkred", "hit_by_pitch" = "cyan", "foul_bunt" = "orange", "foul_tip" = "orange", "missed_bunt" = "darkred", "bunt_foul_tip" = "orange"),
                           breaks = c("hit", "out", "error_or_fielders_choice", "called_strike", "swinging_strike", "ball", "foul", "blocked_ball", "hit_by_pitch")) +
        scale_shape_manual(values = c("Fastball" = 15, "Slider" = 16, "Sweeper" = 17, "Changeup" = 18, "Sinker" = 8, "Splitter" = 25, "Curveball" = 7, "Cutter" = 9, "Knuckle-Curve" = 10, "Forkball" = 11, "Slurve" = 12, "Slow Curve" = 13)) +
        geom_vline(xintercept = 5.7) +
        geom_hline(yintercept = 91.5)
    })
    
    output$scatterPlot2 <- renderPlot({
      ggplot(data = zFilter(), aes(x = release_spin_rate, y = release_speed, color = description_adj, shape = pitchtype)) + geom_point(size = 5) +
        scale_color_manual(values = c("hit" = "blue", "out" = "purple", "error_or_fielders_choice" = "violet", "called_strike" = "red", "swinging_strike" = "darkred", "ball" = "green", "foul" = "orange", "blocked_ball" = "magenta", "swinging_strike_blocked" = "darkred", "hit_by_pitch" = "cyan", "foul_bunt" = "orange", "foul_tip" = "orange", "missed_bunt" = "darkred", "bunt_foul_tip" = "orange"),
                           breaks = c("hit", "out", "error_or_fielders_choice", "called_strike", "swinging_strike", "ball", "foul", "blocked_ball", "hit_by_pitch")) +
        scale_shape_manual(values = c("Fastball" = 15, "Slider" = 16, "Sweeper" = 17, "Changeup" = 18, "Sinker" = 8, "Splitter" = 25, "Curveball" = 7, "Cutter" = 9, "Knuckle-Curve" = 10, "Forkball" = 11, "Slurve" = 12, "Slow Curve" = 13)) +
        geom_vline(xintercept = 2349.5) +
        geom_hline(yintercept = 91.5)
    })
    
    output$scatterPlot3 <- renderPlot({
      ggplot(data = zFilter(), aes(x = api_break_x_arm, y = release_speed, color = description_adj, shape = pitchtype)) + geom_point(size = 5) +
        scale_color_manual(values = c("hit" = "blue", "out" = "purple", "error_or_fielders_choice" = "violet", "called_strike" = "red", "swinging_strike" = "darkred", "ball" = "green", "foul" = "orange", "blocked_ball" = "magenta", "swinging_strike_blocked" = "darkred", "hit_by_pitch" = "cyan", "foul_bunt" = "orange", "foul_tip" = "orange", "missed_bunt" = "darkred", "bunt_foul_tip" = "orange"),
                           breaks = c("hit", "out", "error_or_fielders_choice", "called_strike", "swinging_strike", "ball", "foul", "blocked_ball", "hit_by_pitch")) +
        scale_shape_manual(values = c("Fastball" = 15, "Slider" = 16, "Sweeper" = 17, "Changeup" = 18, "Sinker" = 8, "Splitter" = 25, "Curveball" = 7, "Cutter" = 9, "Knuckle-Curve" = 10, "Forkball" = 11, "Slurve" = 12, "Slow Curve" = 13)) +
        geom_vline(xintercept = 0.49) +
        geom_hline(yintercept = 91.5)
    })
    
    output$scatterPlot4 <- renderPlot({
      ggplot(data = zFilter(), aes(x = api_break_z_with_gravity, y = release_speed, color = description_adj, shape = pitchtype)) + geom_point(size = 5) +
        scale_color_manual(values = c("hit" = "blue", "out" = "purple", "error_or_fielders_choice" = "violet", "called_strike" = "red", "swinging_strike" = "darkred", "ball" = "green", "foul" = "orange", "blocked_ball" = "magenta", "swinging_strike_blocked" = "darkred", "hit_by_pitch" = "cyan", "foul_bunt" = "orange", "foul_tip" = "orange", "missed_bunt" = "darkred", "bunt_foul_tip" = "orange"),
                           breaks = c("hit", "out", "error_or_fielders_choice", "called_strike", "swinging_strike", "ball", "foul", "blocked_ball", "hit_by_pitch")) +
        scale_shape_manual(values = c("Fastball" = 15, "Slider" = 16, "Sweeper" = 17, "Changeup" = 18, "Sinker" = 8, "Splitter" = 25, "Curveball" = 7, "Cutter" = 9, "Knuckle-Curve" = 10, "Forkball" = 11, "Slurve" = 12, "Slow Curve" = 13)) +
        geom_vline(xintercept = 2.16) +
        geom_hline(yintercept = 91.5)
    })
    
    output$scatterPlot5 <- renderPlot({
      ggplot(data = hitFilter(), aes(x = launch_speed, y = hit_distance_sc, color = events, shape = bb_type)) + geom_point(size = 5) +
        scale_color_manual(values = c("single" = "green", "double" = "cyan", "triple" = "purple", "home_run" = "blue", "field_out" = "red", "field_error" = "yellow", "sac_bunt" = "gray", "sac_fly" = "magenta", "grounded_into_double_play" = "darkred"),
                           breaks = c("single", "double", "triple", "home_run", "field_out", "field_error", "sac_bunt", "sac_fly", "grounded_into_double_play"),
                           labels = c("single", "double", "triple", "home run", "out", "reached on error or fielders choice", "sac bunt", "sac fly", "double play out")) +
        scale_shape_manual(values = c("line_drive" = 15, "fly_ball" = 16, "ground_ball" = 17, "popup" = 18)) +
        geom_vline(xintercept = 91.6) +
        geom_hline(yintercept = 160)
    })
    
    output$scatterPlot6 <- renderPlot({
      ggplot(data = hitFilter(), aes(x = launch_angle, y = hit_distance_sc, color = events, shape = bb_type)) + geom_point(size = 5) +
        scale_color_manual(values = c("single" = "green", "double" = "cyan", "triple" = "purple", "home_run" = "blue", "field_out" = "red", "field_error" = "yellow", "sac_bunt" = "gray", "sac_fly" = "magenta", "grounded_into_double_play" = "darkred"),
                           breaks = c("single", "double", "triple", "home_run", "field_out", "field_error", "sac_bunt", "sac_fly", "grounded_into_double_play"),
                           labels = c("single", "double", "triple", "home run", "out", "reached on error or fielders choice", "sac bunt", "sac fly", "double play out")) +
        scale_shape_manual(values = c("line_drive" = 15, "fly_ball" = 16, "ground_ball" = 17, "popup" = 18)) +
        geom_vline(xintercept = 15) +
        geom_hline(yintercept = 160)
    })
    
    output$scatterPlot7 <- renderPlot({
      ggplot(data = hitFilter(), aes(x = bat_speed, y = hit_distance_sc, color = events, shape = bb_type)) + geom_point(size = 5) +
        scale_color_manual(values = c("single" = "green", "double" = "cyan", "triple" = "purple", "home_run" = "blue", "field_out" = "red", "field_error" = "yellow", "sac_bunt" = "gray", "sac_fly" = "magenta", "grounded_into_double_play" = "darkred"),
                           breaks = c("single", "double", "triple", "home_run", "field_out", "field_error", "sac_bunt", "sac_fly", "grounded_into_double_play"),
                           labels = c("single", "double", "triple", "home run", "out", "reached on error or fielders choice", "sac bunt", "sac fly", "double play out")) +
        scale_shape_manual(values = c("line_drive" = 15, "fly_ball" = 16, "ground_ball" = 17, "popup" = 18)) +
        geom_vline(xintercept = 71.9) +
        geom_hline(yintercept = 160)
    })
    
    output$pitcher_stats_plot <- renderPlot({
      col = switch(
        input$pitcherstat,
        walk_rate = "walk_rate",
        swinging_strike_rate = "swinging_strike_rate",
        strike_rate = "strike_rate"
      )
      ggplot(Pitcher_Stats(), aes(x = surname, y = .data[[col]], fill = pitchtype)) +
        geom_bar(stat = "identity")
    })
    
    xpolished = reactive({
      xFilter() %>%
        group_by(pitchtype) %>%
        summarize(average_velocity = mean(release_speed, na.rm = TRUE),
                  average_horizontal_release_point = mean(release_pos_x, na.rm = TRUE),
                  average_vertical_release_point = mean(release_pos_z, na.rm = TRUE),
                  pitches_thrown = n(),
                  balls = sum(description == "ball"),
                  strikes = sum(description == "called_strike") + sum(description == "swinging_strike") + sum(description == "swinging_strike_blocked") + sum(description == "missed_bunt"),
                  hits = sum(description_adj == "hit"),
                  reached_error_or_fc = sum(description_adj == "error_or_fielders_choice"),
                  field_outs = sum(description_adj == "out"),
                  fouls = sum(description == "foul") + sum(description == "foul_tip") + sum(description == "bunt_foul_tip") + sum(description == "foul_bunt"),
                  hit_by_pitches = sum(description == "hit_by_pitch"),
                  blocked_balls = sum(description == "blocked_ball"))
    })
    
    spinPolished = reactive({
      zFilter() %>%
        group_by(pitchtype) %>%
        summarize(average_velocity = mean(release_speed, na.rm = TRUE),
                  average_spin_rate = mean(release_spin_rate, na.rm = TRUE),
                  horizontal_break = mean(api_break_x_arm, na.rm = TRUE),
                  vertical_break = mean(api_break_z_with_gravity, na.rm = TRUE),
                  pitches_thrown = n(),
                  balls = sum(description == "ball"),
                  strikes = sum(description == "called_strike") + sum(description == "swinging_strike") + sum(description == "swinging_strike_blocked") + sum(description == "missed_bunt"),
                  hits = sum(description_adj == "hit"),
                  reached_error_or_fc = sum(description_adj == "error_or_fielders_choice"),
                  field_outs = sum(description_adj == "out"),
                  fouls = sum(description == "foul") + sum(description == "foul_tip") + sum(description == "bunt_foul_tip") + sum(description == "foul_bunt"),
                  hit_by_pitches = sum(description == "hit_by_pitch"),
                  blocked_balls = sum(description == "blocked_ball"))
    })
    
    hitPolished = reactive({
      hitFilter() %>%
        group_by(pitchtype) %>%
        summarize(average_hit_distance = mean(hit_distance_sc, na.rm = TRUE),
                  average_launch_speed = mean(launch_speed, na.rm = TRUE),
                  average_launch_angle = mean(launch_angle, na.rm = TRUE),
                  average_bat_speed = mean(bat_speed, na.rm = TRUE),
                  pitches_seen = n(),
                  singles = sum(events == "single"),
                  doubles = sum(events == "double"),
                  triples = sum(events == "triple"),
                  home_runs = sum(events == "home_run"),
                  outs = sum(events == "field_out") + sum(events == "force_out") + sum(events == "fielders_choice_out"),
                  double_play_outs = sum(events == "grounded_into_double_play") + sum(events == "double_play"),
                  reached_on_error_or_fielders_choice = sum(events == "field_error") + sum(events == "fielders_choice"),
                  sac_flies = sum(events == "sac_fly"),
                  sac_bunts = sum(events == "sac_bunt"))
    })
    
    output$vtable = renderTable({
      xpolished()
    })
    
    output$spintable = renderTable({
      spinPolished()
    })
    
    output$hitstable = renderTable({
      hitPolished()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
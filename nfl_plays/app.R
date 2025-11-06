###############################################################################
# Building an An App to Explore Plays from the 2018-2019 NFL Season
# By: Cass Crews
#Date: 11/5/25
###############################################################################

#Loading packages
library(shiny)
library(shinyalert)
library(tidyverse)
library(shinyWidgets)
library(bslib)
library(shinydashboard)
library(DT)
library(gt)
library(janitor)
library(gtExtras)
library(shinycssloaders)
library(nflplotR)
library(plotly)
library(lubridate)

#Reading in main dataset
play_data<-read_csv("2018_2019_rp_plays.csv") |>
  mutate(wp=round(wp,4),wpa=round(wpa,4)) |>
  #Creating turnover indicator based on fumbles and interceptions
  mutate(turnover=if_else(fumble_lost==1 | interception==1,"yes","no")) |>
  #Creating winning indicator
  mutate(winning=if_else(score_differential>=0,"yes","no")) |>
  #Converting quarter to factor
  mutate(qtr=factor(qtr,levels=1:5,labels=c(1:4,"Overtime"))) |>
  #Removing 2-pt conversions
  drop_na(down) |>
  #Converting down to factor
  mutate(down=factor(down))

#Reading in dataset for win probability plot
play_data2<-read_csv("2018_2019_all_plays.csv") |> 
  #Creating game week and matchup label variables
  mutate(days_in_season=as.numeric(difftime(game_date,ymd("2018-09-04"),units="days"))) |>
  mutate(week=floor(days_in_season/7)+1) |>
  mutate(game_label=paste0(away_team," at ",home_team))

#Sourcing helper script
source("helpers.R")

# Defining UI
ui <- fluidPage(
  
    #Adding theme and custom colors
    theme=bs_theme(bootswatch="darkly",
                   "card-bg" = "#ffffff",
                   "card-border-color" = "#ffffff",
                   "card-color" = "#2f2f2f",
                   "accordion-bg" = "#2f2f2f",
                   "input-bg" = "#ffffff",
                   "navbar-bg" = "#ffffff",
                   "navbar-fg" = "#2f2f2f",
                   "dropdown-bg" = "darkgray",
                   "nav-tabs-link-active-bg" = "#004d26",
                   "nav-tabs-link-active-color" = "#ffffff",
                   "nav-tabs-link-hover-bg" = "#006d34",
                   "nav-link-color" = "#006d34",
                   "nav-link-hover-color" = "#006d34",
                   "form-check-input-border-color" = "darkgray",
                   "form-check-input-bg" = "darkgray"),
    
    #Adjusting tabs to allow them to "stretch" when necessary
    tags$style(HTML("
    .tab-pane, .tab-content, .card-body {
      height: auto;
      overflow: visible;
    }
  ")),
    

    # Application title
    titlePanel("Exploration of Plays from the 2018-2019 NFL Season"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
          h2("Subset the Data by Offensive Play Type and Team"),
          
          #select run, pass, or both
          pickerInput(
            inputId="run_pass",
            label="Runs, Passes, or Both?",
            choices=c("Run"="run","Pass"="pass"),
            selected=c("run","pass"),
            multiple=TRUE,
            options=pickerOptions(
              selectedTextFormat = "count > 1",
              countSelectedText="Both Types Selected")
          ),
          
          #select teams
          pickerInput(
            inputId="teams",
            label="Which Teams' Offensive Plays?",
            choices=nfl_teams,
            selected=nfl_teams,
            multiple=TRUE,
            options=pickerOptions(
              actionsBox=TRUE,
              selectedTextFormat = "count > 31",
              countSelectedText="All Teams Selected")
          ),
          
          br(),
          
          h2("Select Numeric Variables to Subset By"),
          
          #select a numeric variable to subset by
          pickerInput(
            inputId = "num_var1",
            label = "Select a Numeric Variable:",
            choices = c("None"="",num_vars),
            selected = character(0),
            multiple = FALSE,
            options = list(
              title = "Select a variable..."
            )
          ),
          
          #Adding slider if user selects numeric variable
          uiOutput("variable1_selected"),
          
          #Adding additional numeric variable option
          conditionalPanel("input.num_var1",
             pickerInput(
               inputId = "num_var2",
               label = "Select another Numeric Variable:",
               choices = NULL,
               selected = character(0),
               multiple = FALSE,
               options = list(
                 title = "Select a variable..."
               )
            )
          ),
          
          #Adding slider if user selects another numeric variable
          uiOutput("variable2_selected"),
          
          br(),
          
          h2("Done Subsetting? Submit Your Changes!"),
          
          #Button to subset the data
          actionButton("subset_data","Subset the Data")
        ),

        # Main panel
        mainPanel(
          #Adding top-level tabs
          fluidRow(
            tabBox(
              id="tabs",
              width=12,
              tabPanel(title=h5("About the Dashboard"),
                       "This dashboard allows users to explore the characteristics of individual offensive plays from the 2018-2019 NFL season. The play data were created by Carnegie Mellon Sports Analytics Club founders Max Horowitz, 
                       Ron Yurko, and Sam Ventura, who made the data available via ",
                       HTML('<a href="https://www.kaggle.com/datasets/maxhorowitz/nflplaybyplay2009to2016/data?select=NFL+Play+by+Play+2009-2018+%28v5%29.csv">Kaggle</a>.'), "The model-estimated variables included in this dashboard, such as win probability, were produced by the dataset creators and documented ",
                       HTML('<a href="https://arxiv.org/pdf/1802.00998">here</a>.'),
                       br(),
                       br(),
                       "The sidebar on the left can be used to subset the plays explored throughout the dashboard. Users can subset by play type and offensive team, and can also subset by values of key numeric variables.",
                       br(),
                       br(),
                       "The ",tags$b("Download the Data")," tab offers the user the ability to preview the play dataset they have created using the subsetting tools and also download the dataset. 
                       Unlike the rest of the dashboard this tab uses programming-friendly variable names instead of reader-friendly labels. To aid in interpretation of these variables, 
                       there is a drop-down box containing variable definitions.",
                       br(),
                       br(),
                       "The ",tags$b("Explore the Data")," tab allows the user to summarize and visualize the play data in a variety of ways. This section is broken into three subsections:",
                       tags$ul(
                         tags$li(tags$b("Categorical Variables")," offers the user the ability to summarize the distributions of categorical variables, such as play type and quarter, via contingency tables and bar plots. 
                                 The user can focus on one categorical variable via one-way contingency tables and standard bar plots, or two categorical variables via two-way contingency tables and filled bar plots. 
                                 While the bar plot that is automatically produced aggregates data across all teams, the user can choose to generate the same bar plot for each team's offensive plays."),
                         tags$li(tags$b("Numeric Variables")," offers the user the ability to explore summary statistics, kernel density plots, and scatterplots for pairs of user-selected numeric variables. 
                                 The user also has the option to segment the numeric variables by a categorical grouping variable."),
                         tags$li(tags$b("A Numeric Side Quest: Win Probability")," offers the user the ability to visualize win probability dynamics across the plays of a user-selected game. 
                                 To preserve the continuity of this plot, any user-specified data subsetting is not applied and special teams plays are included.")
                       ),
                       br(),
                       br(),
                       br(),
                       tags$i("Note: The play data only extend through Week 15 of the 2018-2019 NFL season as the dataset creators stopped updating the data at that point."),
                       br(),
                       br(),
                       tags$img(src="nfl_logo.png",width="10%",height="10%",style="display: block; margin-left: auto; margin-right: auto;")),
              
                      
              
              tabPanel(title=h5("Download the Data"),
                       card(card_header(h5("Preview the Table Before Downloading")),
                            card_body(
                              #Generating preview data table
                              dataTableOutput(outputId="data_table")
                            )
                       ),
                        #Adding download button
                         downloadButton("download_button","Download the Data"),
                       br(), br(),
                       #Adding variable definition dropdown
                       card(card_header(
                              accordion(
                                id="dict_accordion",
                                accordion_panel(
                                  title=h5("Variable Definitions"),
                                  h6("Data are structured to be consistent with 
                                     R data naming conventions. The variable definitions are provided below."),
                                  uiOutput("dict_text"),
                                  value="dict"
                                ),open=FALSE
                              )
                            )
                      )
              ),
              tabPanel(title=h5("Explore the Data"),
                       #Creating sub-tabs by variable type/topic
                       tabBox(
                         id="explore_tabs",
                         width=12,
                         tabPanel(title=h5("Categorical Variables"),
                                  card(card_header(h5("Select Your Variables")),
                                                   card_body(
                                                     h6("The Categorical Variables exploration section focuses on the distribution of a 
                                                        primary categorical variable, such as the type of play, across levels of a 
                                                        secondary grouping variable, such as the quarter the play occurred in. 
                                                        The user is not required to select a grouping variable."),
                                                     #Adding panel of categorical variable selectors
                                                     layout_columns(
                                                       #Adding first categorical variable selector
                                                       pickerInput(
                                                         inputId = "cat_var1",
                                                         label = "Select a Primary Categorical Variable:",
                                                         choices = primary_cat_vars,
                                                         selected = "play_type",
                                                         multiple = FALSE
                                                       ),
                                                        #Adding second selector for optional categorical variable
                                                         pickerInput(
                                                           inputId = "cat_var2",
                                                           label = "Select a Grouping Variable:",
                                                           choices = c("Not Currently Selected"="",secondary_cat_vars),
                                                           selected = character(0),
                                                           multiple = FALSE,
                                                           options = list(
                                                             title = "Select a variable..."
                                                           )
                                                         ),
                                                         col_widths=c(6,6)
                                                       ),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     br()
                                                    )
                                       ),
                                  #Generating numeric summaries (contingency tables)
                                  card(card_header(h5("Numerical Summaries: Contingency Tables")),
                                       card_body(withSpinner(gt_output("cont_tbl"))
                                                 )
                                       ),
                                  #Generating bar plots based on selections
                                  card(card_header(h5("Graphical Summaries: Bar Plots")),
                                       card_body(
                                         #Generating plots across all teams
                                         withSpinner(plotOutput("overall_plot")
                                         ),
                                         #Check box to open faceted plots by team
                                         checkboxInput(inputId="teams_check",
                                                       label=tags$b("Want Graphs by Team?"),
                                                       width="400px"),
                                         #Faceted bar plots by team based on selections
                                         conditionalPanel("input.teams_check",
                                                          withSpinner(plotOutput("team_plots"))
                                         )
                                         
                                       )
                                  )
                                  ),
                         tabPanel(title=h5("Numeric Variables"),
                                  card(card_header(h5("Select Your Variables")),
                                       card_body(
                                         h6("The Numeric Variables exploration section focuses on the individual 
                                         and joint empirical distributions of two user-selected numeric variables.
                                            The user can also select a grouping variable to evaluate the empirical 
                                            distributions conditional on the value of this variable."),
                                         #Creating panel of variable selectors
                                         layout_columns(
                                           #Selector for first numeric variable
                                           pickerInput(
                                             inputId = "num_var_select1",
                                             label = "Select a Numeric Variable:",
                                             choices = num_vars,
                                             selected = "wpa",
                                             multiple = FALSE
                                           ),
                                           #Selector for second numeric variable
                                           pickerInput(
                                             inputId = "num_var_select2",
                                             label = "Select another Numeric Variable:",
                                             choices = num_vars,
                                             selected = "yards_gained",
                                             multiple = FALSE
                                           ),
                                           #Selector for optional grouping variable
                                           pickerInput(
                                             inputId = "group_var",
                                             label = "Select a Grouping Variable:",
                                             choices = c("None"="",grouping_vars),
                                             selected = character(0),
                                             multiple = FALSE,
                                             options = list(
                                               title = "Select a variable..."
                                             )
                                           ),
                                           col_widths=c(4,4,4)
                                         ),
                                         br(),
                                         br(),
                                         br(),
                                         br()
                                       )
                                  ),
                                  #Generating summary statistics by selections
                                  card(card_header(h5("Numerical Summaries: Summary Statistics")),
                                       card_body(withSpinner(gt_output("num_summs"))
                                       )
                                  ),
                                  #Generating density plots for each variable and scatterplot
                                  card(card_header(h5("Graphical Summaries: Kernel Density Plots and Scatterplots")),
                                       card_body(
                                           withSpinner(plotOutput("density1")),
                                           withSpinner(plotOutput("density2")),
                                           withSpinner(plotOutput("scatterplot"))
                                         )
                                  )
                                  ),
                         tabPanel(title=h5("A Numeric Side Quest: Win Probability"),
                                  card(card_header(h5("Pick a Game")),
                                       card_body(
                                         h6("The data presented in this subsection are not impacted by data subsetting. 
                                          Instead, this subsection displays win probability across the plays a selected game, 
                                          with interactive components allowing the user to hover over a point on
                                          the plot to see a description of the corresponding play. Just select a game to start exploring!"),
                                         #Creating a panel for game week and matchup selection
                                         layout_columns(
                                           pickerInput(
                                             inputId = "week",
                                             label = "Select a Game Week:",
                                             choices = sort(unique(play_data2$week)),
                                             selected = 1,
                                             multiple = FALSE,
                                             options = list(
                                               container = "body"
                                             )
                                           ),
                                           pickerInput(
                                             inputId = "matchup",
                                             label = "Select a Game:",
                                             choices = NULL,
                                             selected = NULL,
                                             multiple = FALSE,
                                             options = list(
                                               title = "Select a game...",
                                               container = "body"
                                             )
                                           ),
                                           col_widths=c(6,6)
                                         ),
                                         #Generating win probability time series plot
                                         withSpinner(plotlyOutput("probplot"))
                                       ))
                                )
                       ))
              
            )            
          )

        )
    )
)

# Defining server logic 
server <- function(input, output,session) {
  
  
  ##############################################################################
  #Sidebar Server Logic
  ##############################################################################
  
  #Generating UI to create subsetting numeric variable slider based on user variable selection
  output$variable1_selected<-renderUI({
    #Requiring that the user selected a variable
    req(input$num_var1)
    
    #Capturing associated variable label from helper script
    label<-names(num_vars)[num_vars==input$num_var1]
    
    #Creating numeric subset slider that defaults to full variable range
    num_slider<-sliderInput(
      inputId="num_subset1",
      label=paste0("Select a Range for ", label),
      min=min(play_data[[input$num_var1]],na.rm=TRUE),
      max=max(play_data[[input$num_var1]],na.rm=TRUE),
      value=c(min(play_data[[input$num_var1]],na.rm=TRUE),max(play_data[[input$num_var1]],na.rm=TRUE))
    )
  })
  
  #Updating second numeric variable selector that excludes initial selection as an option
  observeEvent(input$num_var1,ignoreInit=TRUE,{
    #Capturing non-selected variables for options in second numeric variable selector
    non_selected_vars<-num_vars[num_vars!=input$num_var1]
    
    #Updating selector
    updatePickerInput(
      session,inputId = "num_var2",
      choices = c("None"="",non_selected_vars),
      selected = character(0)
    )
  })
  
  #Generating UI to create subsetting numeric variable slider based on second user variable selection
  output$variable2_selected<-renderUI({
    #Requiring that the user selected a variable
    req(input$num_var2)
    
    #Capturing corresponding label for second selected numeric variable
    non_selected_vars<-num_vars[num_vars!=input$num_var1]
    label2<-names(non_selected_vars)[non_selected_vars==input$num_var2]
    
    #Generating second subset slider
    num_slider<-sliderInput(
      inputId="num_subset2",
      label=paste0("Select a Range for ", label2),
      min=min(play_data[[input$num_var2]],na.rm=TRUE),
      max=max(play_data[[input$num_var2]],na.rm=TRUE),
      value=c(min(play_data[[input$num_var2]],na.rm=TRUE),max(play_data[[input$num_var2]],na.rm=TRUE))
    )
  })
  
  ##############################################################################
  #Constructing the subset dataset and allowing download
  ##############################################################################
  
  #Initially using entire dataset
  data_subset<-reactiveValues(data=play_data)
  
  #Applying user-selected subsets once subset button is clicked
  observeEvent(input$subset_data,{
    
    #Subsetting based on play type and team selection
    temp_subset<-play_data |>
      filter(play_type %in% c(input$run_pass),posteam %in% c(input$teams))
    
    #Subsetting based on first numeric variable
    if (isTruthy(input$num_var1)) {
      temp_subset<-temp_subset |>
        filter(!!sym(input$num_var1)>=input$num_subset1[1],!!sym(input$num_var1)<=input$num_subset1[2])
    }
    
    #Subsetting based on second numeric variable
    if (isTruthy(input$num_var2)) {
      temp_subset<-temp_subset |>
        filter(!!sym(input$num_var2)>=input$num_subset2[1],!!sym(input$num_var2)<=input$num_subset2[2])
    }
    
    #Generating notification if subset results in empty dataset
    if (nrow(temp_subset)==0) {
      showNotification("Your subset does not contain any data. Please adjust your selections.",type="warning",duration=10)
    }
    
    #Only applying subset if resulting dataset is not empty
    req(nrow(temp_subset)> 0)
    
    data_subset$data<-temp_subset
  })
  
  #Constructing preview data table
  output$data_table<-renderDataTable(data_subset$data)
  
  #Constructing data download button to generate csv
  output$download_button<-downloadHandler(
    filename=function() {
      "plays.csv"
    },
    content=function(file) {
      write_csv(data_subset$data,file)
    }
  )
  
  #Generating content for the variable definition drop-down
  output$dict_text<-renderUI({
    HTML(paste0(all_vars,": ",names(all_vars),"<br>"))
  })
  

  ##############################################################################
  #Constructing categorical variable summaries
  ##############################################################################


  #Updating widgets to ensure validity##########################################
  
  #Updating cat_var2 options based on cat_var1 selection
  observeEvent(list(input$cat_var1,input$subset_data),{
    #Removing the first selected categorical variable from the list of options
    choices<-c("Not Selected"="",secondary_cat_vars[-which(secondary_cat_vars==input$cat_var1)])
    
    #Also removing play type as an option is the user has subset play type
    if (length(unique(data_subset$data$play_type))< 2 & input$cat_var1!="play_type") {
      choices<-choices[-which(choices=="play_type")]
    }
    #Updating the widget depending on whether play type is still an option
    if (length(unique(data_subset$data$play_type))< 2 & input$cat_var2=="play_type") {
      updatePickerInput(session,
                        "cat_var2",
                        choices=choices,
                        selected=NULL)
    } else {
      updatePickerInput(session,
                        "cat_var2",
                        choices=choices,
                        selected=input$cat_var2)
    }
  })
  
  #Updating cat_var1 options based on cat_var2 selection
  observeEvent(list(input$cat_var2,input$subset_data),{
    #Capturing initial options
    choices<-primary_cat_vars
    
    #Removing second categorical variable selection from the options
    if (input$cat_var2 %in% primary_cat_vars) {
      choices<-choices[-which(choices==input$cat_var2)]
    }
    
    #Also removing play type if play type has been subset and updating widget
    if (length(unique(data_subset$data$play_type))< 2) {
      choices<-choices[-which(choices=="play_type")]
      
      updatePickerInput(session,
                        "cat_var1",
                        choices=choices,
                        selected=NULL)
    } else {
      updatePickerInput(session,
                        "cat_var1",
                        choices=choices,
                        selected=input$cat_var1)
    }
  })

  #Numeric Summaries############################################################
  
  #Constructing either a 1-way or 2-way contingency table based on number of
  #categorical variables selected
  output$cont_tbl<-render_gt({
    #Two-way table case
    if (isTruthy(input$cat_var2)) {
      #Generating cat_var1 breakdown by cat_var2
      data_subset$data |>
        tabyl(!!sym(input$cat_var2),!!sym(input$cat_var1),show_na=FALSE) |>
        #Adding row percentages in addition to counts
        adorn_percentages("row") |>
        adorn_pct_formatting(digits=1) |>
        adorn_ns("front") |>
        #Converting to gt table
        gt() |>
        #Adding individual column labels and broad label for all columns
        cols_label(!!sym(input$cat_var2):=names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2]) |>
        tab_spanner(label=names(primary_cat_vars)[primary_cat_vars==input$cat_var1],columns=2:3) |>
        #Using Pro Football Focus theme
        gt_theme_pff() |>
        #Table formatting
        tab_options(
          heading.title.font.size = px(20),
          table.font.size=px(16),
          data_row.padding=px(8),
          heading.padding=px(12),
          column_labels.padding=px(8)
        ) |>
        #Using main font for the app
        opt_table_font(google_font(name = "Helvetica Neue")) |>
        #Adding title and subtitle
        tab_header(title=paste0("Two-Way Table: ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1],
                                " by ",names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2]),
                   subtitle="Percentages Indicate the Share of a Group's (Row's) Total Plays")
      #One-way table case
    } else {
      #Generating cat_var1 breakdown
      data_subset$data |>
        tabyl(!!sym(input$cat_var1),show_na=FALSE) |>
        #Adding percentages
        adorn_pct_formatting(digits=1) |>
        #Converting to gt table
        gt() |>
        #Adding column labels
        cols_label(!!sym(input$cat_var1):=names(primary_cat_vars)[primary_cat_vars==input$cat_var1],
                   n="Number of Plays",percent="Percent of Plays") |>
        #Using Pro Football Focus theme
        gt_theme_pff() |>
        #Table formatting
        tab_options(
          heading.title.font.size = px(20),
          table.font.size=px(16),
          data_row.padding=px(8),
          heading.padding=px(12),
          column_labels.padding=px(8)
        ) |>
        #Using main font for the app
        opt_table_font(google_font(name = "Helvetica Neue")) |>
        #Adding title
        tab_header(title=paste0("One-Way Table: Breakdown of Plays by ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1]))
    }
  })
  
  #Graphical Summaries##########################################################
  
  #Constructing basic infrastructure for overall and faceted (by team) bar plots
  base_plot<-reactive({
    #Constructing bar plot when a second categorical variable is selected
    if (isTruthy(input$cat_var2)) {
      g<-ggplot(data=data_subset$data,aes(x=!!sym(input$cat_var2),fill=!!sym(input$cat_var1)))+geom_bar(position="fill",alpha=0.8)+
        theme_light(base_size = 22, base_family = "Helvetica Neue")+scale_fill_manual(values=c("navy","darkred"))+
        #Adding custom theming
        theme(
          plot.title.position = "plot",
          plot.title = element_text(face = "bold",color = "#ffffff",size=28),
          axis.title.y = element_text(face="bold",color = "#ffffff",size = 22),
          axis.title.x = element_text(face="bold",color = "#ffffff",size = 22),
          plot.background = element_rect(fill = "#2f2f2f"),
          panel.background = element_rect(fill = "#2f2f2f"),
          legend.text = element_text(color="#ffffff",face="bold"),
          legend.background= element_rect(fill = NA, color = NA),
          axis.text = element_text(color = "#ffffff", size = 22,face="bold")
        )+labs(title=paste0(names(primary_cat_vars)[primary_cat_vars==input$cat_var1]," by ",
                            names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2]),
               x=names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2],y="Share of Plays",fill=NULL)
      #Constructing bar plot when a second categorical variable is not selected
    } else {
      g<-ggplot(data=data_subset$data,aes(x=!!sym(input$cat_var1),fill=!!sym(input$cat_var1)))+geom_bar(alpha=0.8)+
        theme_light(base_size = 14, base_family = "Helvetica Neue")+scale_fill_manual(values=c("navy","darkred"))+
        #Adding custom theming
        theme(
          plot.title.position = "plot",
          plot.title = element_text(face = "bold",color = "#ffffff",size=28),
          axis.title.y = element_text(face="bold",color = "#ffffff",size = 22),
          axis.title.x=element_blank(),
          plot.background = element_rect(fill = "#2f2f2f"),
          panel.background = element_rect(fill = "#2f2f2f"),
          legend.position = "none",
          axis.text = element_text(color = "#ffffff", size = 22,face="bold")
        )+labs(title=paste0("Breakdown of Plays by ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1]),y="Number of Plays",fill=NULL)
    }
    
    g
  })
  
  #Constructing overall bar plot (not broken down by team)
  output$overall_plot<-renderPlot({
    base_plot()
  })
  
  #Constructing faceted bar plots by team when user requests them
  output$team_plots<-renderPlot(height=function() {
    rows<-ceiling(length(unique(data_subset$data$posteam))/2)
    300*rows
  },{
    #Confirming user checked box for plots by team
    req(input$teams_check)
    if (isTruthy(input$cat_var2)) {
      base_plot()+facet_wrap(vars(posteam),ncol=2,axes="all",axis.labels="all")+
        theme(
          #Add designed team names based on match abbreviations
          strip.text = nflplotR::element_nfl_wordmark(size = 1)
        )+labs(title=paste0("Team Level: ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1],
                            " by ",names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2]))
    } else {
      base_plot()+facet_wrap(vars(posteam),ncol=2,axes="all",axis.labels="all")+
        theme(
          #Add designed team names based on matched abbreviations
          strip.text = nflplotR::element_nfl_wordmark(size = 1)
        )+labs(title=paste0("Team-Level Breakdown of Plays by ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1]))
    }
  })
  
  ##############################################################################
  #Constructing numeric variable summaries
  ##############################################################################
  
  #Updating widgets to ensure validity##########################################
  
  #Updating num_var_select2 options based on num_var_select1 selection
  observeEvent(input$num_var_select1,{
    #Capturing initial list of numeric variable options
    choices<-num_vars
    
    #Removing first selected variable as an option
    choices<-choices[-which(choices==input$num_var_select1)]

    #Updating selector widget
    updatePickerInput(session,
                      "num_var_select2",
                      choices=choices,
                      selected=input$num_var_select2)
  })

  #Updating num_var_select1 options based on num_var_select2 selection  
  observeEvent(input$num_var_select2,{
    #Capturing initial list of numeric variables
    choices<-num_vars
    
    #Removing second selected variable as an option
    choices<-choices[-which(choices==input$num_var_select2)]
    
    #Updating selector widget
    updatePickerInput(session,
                      "num_var_select1",
                      choices=choices,
                      selected=input$num_var_select1)
  })
  
  #Updating group_var options based on play_type subset
  observeEvent(input$subset_data,{
    #Initially constructing grouping variable options, allowing for no selection
    choices<-c("None"="",grouping_vars)
    
    #Removing play type as an option if the user has subset it
    if (length(unique(data_subset$data$play_type))< 2) {
      choices<-choices[-which(choices=="play_type")]
    }
    
    #Updating selector widget depending on whether play type is still an option and currently selected
    if (length(unique(data_subset$data$play_type))< 2 & input$group_var=="play_type") {
      updatePickerInput(session,
                        "group_var",
                        choices=choices,
                        selected=character(0))
    } else {
      updatePickerInput(session,
                        "group_var",
                        choices=choices,
                        selected=input$group_var)
    }

  })
  
  #Numeric Summaries############################################################
  
  #Generating summary statistics dependent on whether grouping variable selected
  output$num_summs<-render_gt({
    #Constructing table when grouping variable selected
    if (isTruthy(input$group_var)) {
      data_subset$data |>
        #Grouping by group variable selection
        group_by(!!sym(input$group_var)) |>
        #Capturing mean, median, SD, IQR, min, and max
        summarize(across(c(!!sym(input$num_var_select1),!!sym(input$num_var_select2)),list(Mean= ~round(mean(.x, na.rm = TRUE),4),
                                                                        Median= ~round(median(.x, na.rm = TRUE),4),
                                                                        SD= ~round(sd(.x, na.rm = TRUE),4),
                                                                        IQR= ~round(IQR(.x, na.rm = TRUE),4),
                                                                        Min= ~round(min(.x, na.rm = TRUE),4),
                                                                        Max= ~round(max(.x, na.rm = TRUE),4)),.names="{.fn}__{.col}")) |>
        pivot_longer(2:13,names_to=c(".value","variable"),names_sep="__") |>
        arrange(variable,!!sym(input$group_var)) |>
        #Adding variable labels
        mutate(`Numeric Variable`=names(num_vars)[match(variable,num_vars)]) |>
        select(!variable) |>
        select(!!sym(input$group_var),`Numeric Variable`,everything()) |>
        #Assigning grouping variable label for easier interpretability
        rename(!!sym(names(grouping_vars)[grouping_vars==input$group_var]):=!!sym(input$group_var)) |>
        #Converting to gt table
        gt() |>
        #Using PFF theme for table
        gt_theme_pff() |>
        #Applying custom format options
        tab_options(
          heading.title.font.size = px(20),
          table.font.size=px(16),
          data_row.padding=px(8),
          heading.padding=px(12),
          column_labels.padding=px(8)
        ) |>
        #Using font for main app
        opt_table_font(google_font(name = "Helvetica Neue")) |>
        #Assigning a title
        tab_header(title=paste0("Summary Statistics for ",names(num_vars)[num_vars==input$num_var_select1]," and ",
                                names(num_vars)[num_vars==input$num_var_select2]," by ",names(group_vars)[grouping_vars==input$group_var]))
      #Constructing table when no grouping variable selected
    } else {
      data_subset$data |>
        #Capturing mean, median, SD, IQR, min, and max
        summarize(across(c(!!sym(input$num_var_select1),!!sym(input$num_var_select2)),list(Mean= ~round(mean(.x, na.rm = TRUE),4),
                                                                                           Median= ~round(median(.x, na.rm = TRUE),4),
                                                                                           SD= ~round(sd(.x, na.rm = TRUE),4),
                                                                                           IQR= ~round(IQR(.x, na.rm = TRUE),4),
                                                                                           Min= ~round(min(.x, na.rm = TRUE),4),
                                                                                           Max= ~round(max(.x, na.rm = TRUE),4)),.names="{.fn}__{.col}")) |>
        pivot_longer(everything(),names_to=c(".value","variable"),names_sep="__") |>
        #Adding variable labels
        mutate(`Numeric Variable`=names(num_vars)[match(variable,num_vars)]) |>
        select(!variable) |>
        select(`Numeric Variable`,everything()) |>
        #Converting to gt table
        gt() |>
        #Applying PFF theme
        gt_theme_pff() |>
        #Applying custom format options
        tab_options(
          heading.title.font.size = px(20),
          table.font.size=px(16),
          data_row.padding=px(8),
          heading.padding=px(12),
          column_labels.padding=px(8)
        ) |>
        #Using font for main app
        opt_table_font(google_font(name = "Helvetica Neue")) |>
        #Assigning a title
        tab_header(title=paste0("Summary Statistics for ",names(num_vars)[num_vars==input$num_var_select1]," and ",names(num_vars)[num_vars==input$num_var_select2]))
    }
  })
  
  #Graphical Summaries##########################################################
  
  #Generating density plot for first numeric variable using functions in helpers script
  output$density1<-renderPlot({
    #Generating densities across levels of selected grouping variable
    if (isTruthy(input$group_var)) {
      grouped_density(data_subset$data,input$num_var_select1,input$group_var)
      #Generating overall density plot when no grouping variable selected
    } else {
      no_group_density(data_subset$data,input$num_var_select1)
    }
  })
  
  #Generating density plot for second numeric variable using functions in helpers script
  output$density2<-renderPlot({
    #Generating densities across levels of selected grouping variable
    if (isTruthy(input$group_var)) {
      grouped_density(data_subset$data,input$num_var_select2,input$group_var)
      #Generating overall density plot when no grouping variable selected
    } else {
      no_group_density(data_subset$data,input$num_var_select2)
    }
  })
  
  #Constructing scatterplot dependent on whether grouping variable selected
  output$scatterplot<-renderPlot({
    #Constructing grouped scatterplot when grouping variable selected
    if (isTruthy(input$group_var)) {
      #Specifying color palette and subsetting depending on number of groups
      colors<-c("navy","darkred","darkgreen","darkorange","darkgrey")
      colors_subset<-colors[1:length(unique(data_subset$data[[input$group_var]]))]
      
      #Constructing scatterplot with point colors varying by group
      g<-ggplot(data=data_subset$data,aes(x=!!sym(input$num_var_select1),y=!!sym(input$num_var_select2),color=!!sym(input$group_var)))+
        geom_point(alpha=0.3)+
        theme_light(base_size = 14, base_family = "Helvetica Neue")+scale_color_manual(values=colors_subset)+
        #Applying custom theming
        theme(
          plot.title.position = "plot",
          plot.title = element_text(face = "bold",color = "#ffffff",size=22),
          axis.title.y = element_text(face="bold",color = "#ffffff",size = 16),
          axis.title.x = element_text(face="bold",color = "#ffffff",size = 16),
          plot.background = element_rect(fill = "#2f2f2f"),
          panel.background = element_rect(fill = "#2f2f2f"),
          legend.text = element_text(color="#ffffff",face="bold"),
          legend.background= element_rect(fill = NA, color = NA),
          axis.text = element_text(color = "#ffffff", size = 16,face="bold")
        )+labs(title=paste0(names(num_vars)[num_vars==input$num_var_select1]," vs. ",names(num_vars)[num_vars==input$num_var_select2]," by ",
                            names(grouping_vars)[grouping_vars==input$group_var]),x=names(num_vars)[num_vars==input$num_var_select1],y=names(num_vars)[num_vars==input$num_var_select2],color=NULL)
      
      g
      
      #Constructing scatterplot when no grouping variable selected
      } else {
        #Constructing the basic scatterplot
        g<-ggplot(data=data_subset$data,aes(x=!!sym(input$num_var_select1),y=!!sym(input$num_var_select2)))+
          geom_point(alpha=0.4,color="navy")+
          theme_light(base_size = 14, base_family = "Helvetica Neue")+
          #Adding custom theming
          theme(
            plot.title.position = "plot",
            plot.title = element_text(face = "bold",color = "#ffffff",size=22),
            axis.title.y = element_text(face="bold",color = "#ffffff",size = 16),
            axis.title.x = element_text(face="bold",color = "#ffffff",size = 16),
            plot.background = element_rect(fill = "#2f2f2f"),
            panel.background = element_rect(fill = "#2f2f2f"),
            legend.text = element_text(color="#ffffff",face="bold"),
            legend.background= element_rect(fill = NA, color = NA),
            axis.text = element_text(color = "#ffffff", size = 16,face="bold")
          )+labs(title=paste0(names(num_vars)[num_vars==input$num_var_select1]," vs. ",names(num_vars)[num_vars==input$num_var_select2]),
                 x=names(num_vars)[num_vars==input$num_var_select1],y=names(num_vars)[num_vars==input$num_var_select2],color=NULL)
        
        g
    }
  })
  

  #Win Probability Plot#########################################################
  
  #Updating game selector widget based on user-selected week
  observeEvent(input$week,{
    games<-play_data2 |>
      filter(week==input$week) |>
      distinct(game_label) |>
      pull(game_label)
    
    updatePickerInput(
      session,inputId = "matchup",
      choices = games,
      selected = NULL
    )
  })
  
  #Constructing interactive win probability plot
  output$probplot<-renderPlotly({
    #Requiring matchup selection before constructing
    req(input$matchup)
    
    #Constructing dataset for plot
    play_data_1game<-play_data2 |>
      #Filtering to correct week and game
      filter(week==input$week,game_label==input$matchup) |>
      #Dropping end-of-quarter plays with missing win probability
      drop_na(wp) |>
      #Generating play number
      mutate(dummy=1) |>
      mutate(play_num=cumsum(dummy)) |>
      #Generating tooltip information
      mutate(tooltip=paste0("Quarter: ", qtr, 
                            "<br>Game Clock: ", time,
                            "<br>Play: ", desc, 
                            "<br>Score: ", away_team, " ", total_away_score, " ", home_team, " ", total_home_score,
                            "<br>", home_team, " Win Probability: ", round(home_wp,3),
                            "<br>",away_team," Win Probability: ", round(away_wp,3)))
    
    #Extracting home team for plot title
    home<-play_data_1game |>
      distinct(home_team) |>
      pull(home_team)
    
    #Extracting away team for plot title
    away<-play_data_1game |>
      distinct(away_team) |>
      pull(away_team)
    
    #Generating static time series plot
    g<-ggplot(data=play_data_1game,aes(x=play_num,y=home_wp,color=home_team))+geom_line(linewidth=1.5)+scale_color_nfl(type = "primary") + 
      coord_cartesian(ylim=c(0,1))+
      geom_point(aes(text = tooltip), alpha = 0, size = 0.01, show.legend = FALSE)+
      theme_light(base_size = 14, base_family = "Helvetica Neue")+
      #Applying custom theming
      theme(
        plot.title.position = "plot",
        plot.title = element_text(face = "bold",color = "#ffffff",size=18),
        axis.title.y = element_text(face="bold",color = "#ffffff",size = 16),
        axis.title.x = element_text(face="bold",color = "#ffffff",size = 16),
        plot.background = element_rect(fill = "#2f2f2f"),
        panel.background = element_rect(fill = "#2f2f2f"),
        legend.position = "none",
        axis.text = element_text(color = "#ffffff", size = 16,face="bold")
      )+
      labs(title=paste0("Win Probability for the ",names(nfl_teams)[nfl_teams==home]," in their Week ",input$week,
                        " Game Against the ",names(nfl_teams)[nfl_teams==away]),x="Play Number",y="Win Probability")
    
    #Converting to an interactive plotly plot
    ggplotly(g,tooltip="text")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

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

play_data<-read_csv("2018_2019_rp_plays.csv") |>
  mutate(wp=round(wp,4),wpa=round(wpa,4)) |>
  mutate(turnover=if_else(fumble_lost==1 | interception==1,"yes","no")) |>
  mutate(winning=if_else(score_differential>=0,"yes","no")) |>
  mutate(qtr=factor(qtr,levels=1:5,labels=c(1:4,"Overtime"))) |>
  drop_na(down) |>
  mutate(down=factor(down))

play_data2<-read_csv("2018_2019_all_plays.csv") |> 
  mutate(days_in_season=as.numeric(difftime(game_date,ymd("2018-09-04"),units="days"))) |>
  mutate(week=floor(days_in_season/7)+1) |>
  mutate(game_label=paste0(away_team," at ",home_team))

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    #Adding theme
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

    # Sidebar with a slider input for number of bins 
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
          
          #select a numeric variable
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
          
          uiOutput("variable1_selected"),
          
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
          
          
          uiOutput("variable2_selected"),
          
          br(),
          
          h2("Done Subsetting? Submit Your Changes!"),
          
          actionButton("subset_data","Subset the Data")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            tabBox(
              id="tabs",
              width=12,
              tabPanel(title=h5("About the Dashboard"),
                       "Some text about the dashboard"),
              tabPanel(title=h5("Download the Data"),
                       card(card_header(h5("Preview the Table Before Downloading")),
                            card_body(
                              dataTableOutput(outputId="data_table")
                            )
                       ),
                         downloadButton("download_button","Download the Data"),
                       br(), br(),
                       card(card_header(
                              accordion(
                                id="dict_accordion",
                                accordion_panel(
                                  title=h5("Variable Definitions"),
                                  h6("Data are structured to be consistent with R data naming conventions. The variable definitions are provided below."),
                                  uiOutput("dict_text"),
                                  value="dict"
                                ),open=FALSE
                              )
                            )
                      )
              ),
              tabPanel(title=h5("Explore the Data"),
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
                                                     layout_columns(
                                                       pickerInput(
                                                         inputId = "cat_var1",
                                                         label = "Select a Primary Categorical Variable:",
                                                         choices = primary_cat_vars,
                                                         selected = "play_type",
                                                         multiple = FALSE
                                                       ),
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
                                  card(card_header(h5("Numerical Summaries")),
                                       card_body(withSpinner(gt_output("cont_tbl"))
                                                 )
                                       ),
                                  card(card_header(h5("Graphical Summaries")),
                                       card_body(
                                         withSpinner(plotOutput("overall_plot")
                                         ),
                                         checkboxInput(inputId="teams_check",
                                                       label=tags$b("Want Graphs by Team?"),
                                                       width="400px"),
                                         conditionalPanel("input.teams_check",
                                                          withSpinner(plotOutput("team_plots"))
                                         )
                                         
                                       )
                                  )
                                  ),
                         tabPanel(title=h5("Numeric Variables"),
                                  card(card_header(h5("Select Your Variables")),
                                       card_body(
                                         h6("The Numeric Variables exploration section focuses on the individual and joint empirical distributions of two user-selected numeric variables.
                                            The user can also select a grouping variable to evaluate the empirical distributions conditional on the value of this variable."),
                                         layout_columns(
                                           pickerInput(
                                             inputId = "num_var_select1",
                                             label = "Select a Numeric Variable:",
                                             choices = num_vars,
                                             selected = "wpa",
                                             multiple = FALSE
                                           ),
                                           pickerInput(
                                             inputId = "num_var_select2",
                                             label = "Select another Numeric Variable:",
                                             choices = num_vars,
                                             selected = "yards_gained",
                                             multiple = FALSE
                                           ),
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
                                  card(card_header(h5("Numerical Summaries")),
                                       card_body(withSpinner(gt_output("num_summs"))
                                       )
                                  ),
                                  card(card_header(h5("Graphical Summaries")),
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
                                         h6("The data presented in this subsection are not impacted by data subsetting. Instead, this subsection displays
                                            win probability across the plays a selected game, with interactive components allowing the user to hover over a point on
                                            the plot to see a description of the corresponding play. Just select a game to start exploring!"),
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

  output$variable1_selected<-renderUI({
    req(input$num_var1)
    
    label<-names(num_vars)[num_vars==input$num_var1]
    
    non_selected_vars<-num_vars[num_vars!=input$num_var1]
    
    num_slider<-sliderInput(
      inputId="num_subset1",
      label=paste0("Select a Range for ", label),
      min=min(play_data[[input$num_var1]],na.rm=TRUE),
      max=max(play_data[[input$num_var1]],na.rm=TRUE),
      value=c(min(play_data[[input$num_var1]],na.rm=TRUE),max(play_data[[input$num_var1]],na.rm=TRUE))
    )
  })
  
  output$dict_text<-renderUI({
    HTML(paste0(all_vars,": ",names(all_vars),"<br>"))
  })
  
  observeEvent(input$num_var1,ignoreInit=TRUE,{
    non_selected_vars<-num_vars[num_vars!=input$num_var1]
    
    updatePickerInput(
      session,inputId = "num_var2",
      choices = c("None"="",non_selected_vars),
      selected = character(0)
    )
  })
  

  output$variable2_selected<-renderUI({
    req(input$num_var2)
    
    non_selected_vars<-num_vars[num_vars!=input$num_var1]
    
    label2<-names(non_selected_vars)[non_selected_vars==input$num_var2]
    
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
  
  
  observeEvent(input$subset_data,{
    req(length(input$teams)> 0)
    
    temp_subset<-play_data |>
      filter(play_type %in% c(input$run_pass),posteam %in% c(input$teams))
    
    if (isTruthy(input$num_var1)) {
      temp_subset<-temp_subset |>
        filter(!!sym(input$num_var1)>=input$num_subset1[1],!!sym(input$num_var1)<=input$num_subset1[2])
    }
    
    if (isTruthy(input$num_var2)) {
      temp_subset<-temp_subset |>
        filter(!!sym(input$num_var2)>=input$num_subset2[1],!!sym(input$num_var2)<=input$num_subset2[2])
    }
    
    if (nrow(temp_subset)==0) {
      showNotification("Your subset does not contain any data. Please adjust your selections.",type="warning",duration=10)
    }
    
    req(nrow(temp_subset)> 0)
    
    data_subset$data<-temp_subset
  })
  
  output$data_table<-renderDataTable(data_subset$data)
  
  
  output$download_button<-downloadHandler(
    filename=function() {
      "plays.csv"
    },
    content=function(file) {
      write_csv(data_subset$data,file)
    }
  )

################################################################################
#Constructing categorical variable summaries
################################################################################


  #Updating widgets to ensure validity##########################################
  
  #Updating cat_var2 options based on cat_var1 selection
  observeEvent(list(input$cat_var1,input$subset_data),{
    choices<-c("Not Currently Selected"="",secondary_cat_vars[-which(secondary_cat_vars==input$cat_var1)])
    if (length(unique(data_subset$data$play_type))< 2 & input$cat_var1!="play_type") {
      choices<-choices[-which(secondary_cat_vars=="play_type")]
    }
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
    choices<-primary_cat_vars
    
    if (input$cat_var2 %in% primary_cat_vars) {
      choices<-choices[-which(primary_cat_vars==input$cat_var2)]
    }
    if (length(unique(data_subset$data$play_type))< 2) {
      choices<-choices[-which(primary_cat_vars=="play_type")]
      
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
  
  output$cont_tbl<-render_gt({
    if (isTruthy(input$cat_var2)) {
      #Generating cat_var1 breakdown by cat_var2
      data_subset$data |>
        tabyl(!!sym(input$cat_var2),!!sym(input$cat_var1),show_na=FALSE) |>
        adorn_percentages("row") |>
        adorn_pct_formatting(digits=1) |>
        adorn_ns("front") |>
        gt() |>
        cols_label(!!sym(input$cat_var2):=names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2]) |>
        tab_spanner(label=names(primary_cat_vars)[primary_cat_vars==input$cat_var1],columns=2:3) |>
        gt_theme_pff() |>
        tab_options(
          heading.title.font.size = px(20),
          table.font.size=px(16),
          data_row.padding=px(8),
          heading.padding=px(12),
          column_labels.padding=px(8)
        ) |>
        opt_table_font(google_font(name = "Helvetica Neue")) |>
        tab_header(title=paste0("Two-Way Table: ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1]," by ",names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2]),
                   subtitle="Percentages Indicate the Share of a Group's (Row's) Total Plays")
    } else {
      #Generating cat_var1 breakdown
      data_subset$data |>
        tabyl(!!sym(input$cat_var1),show_na=FALSE) |>
        adorn_pct_formatting(digits=1) |>
        gt() |>
        cols_label(!!sym(input$cat_var1):=names(primary_cat_vars)[primary_cat_vars==input$cat_var1],
                   n="Number of Plays",percent="Percent of Plays") |>
        gt_theme_pff() |>
        tab_options(
          heading.title.font.size = px(20),
          table.font.size=px(16),
          data_row.padding=px(8),
          heading.padding=px(12),
          column_labels.padding=px(8)
        ) |>
        opt_table_font(google_font(name = "Helvetica Neue")) |>
        tab_header(title=paste0("One-Way Table: Breakdown of Plays by ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1]))
    }
  })
  
  #Graphical Summaries##########################################################
  base_plot<-reactive({
    if (isTruthy(input$cat_var2)) {
      g<-ggplot(data=data_subset$data,aes(x=!!sym(input$cat_var2),fill=!!sym(input$cat_var1)))+geom_bar(position="fill",alpha=0.8)+
        theme_light(base_size = 22, base_family = "Helvetica Neue")+scale_fill_manual(values=c("navy","darkred"))+
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
        )+labs(title=paste0(names(primary_cat_vars)[primary_cat_vars==input$cat_var1]," by ",names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2]),x=names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2],y="Share of Plays",fill=NULL)
    } else {
      g<-ggplot(data=data_subset$data,aes(x=!!sym(input$cat_var1),fill=!!sym(input$cat_var1)))+geom_bar(alpha=0.8)+
        theme_light(base_size = 14, base_family = "Helvetica Neue")+scale_fill_manual(values=c("navy","darkred"))+
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
  
  output$overall_plot<-renderPlot({
    base_plot()
  })
  
  output$team_plots<-renderPlot(height=function() {
    rows<-ceiling(length(unique(data_subset$data$posteam))/2)
    300*rows
  },{
    req(input$teams_check)
    if (isTruthy(input$cat_var2)) {
      base_plot()+facet_wrap(vars(posteam),ncol=2,axes="all",axis.labels="all")+
        theme(
          #Add designed team names based on match abbreviations
          strip.text = nflplotR::element_nfl_wordmark(size = 1)
        )+labs(title=paste0("Team Level: ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1]," by ",names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2]))
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
    choices<-num_vars
    
    choices<-choices[-which(num_vars==input$num_var_select1)]

    updatePickerInput(session,
                      "num_var_select2",
                      choices=choices,
                      selected=input$num_var_select2)
  })

  #Updating num_var_select1 options based on num_var_select2 selection  
  observeEvent(input$num_var_select2,{
    choices<-num_vars
    
    choices<-choices[-which(num_vars==input$num_var_select2)]
    
    updatePickerInput(session,
                      "num_var_select1",
                      choices=choices,
                      selected=input$num_var_select1)
  })
  
  #Updating group_var options based on play_type subset
  observeEvent(input$subset_data,{
    choices<-c("None"="",grouping_vars)
    if (length(unique(data_subset$data$play_type))< 2) {
      choices<-choices[-which(grouping_vars=="play_type")]
    }
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
  
  output$num_summs<-render_gt({
    if (isTruthy(input$group_var)) {
      data_subset$data |>
        group_by(!!sym(input$group_var)) |>
        summarize(across(c(!!sym(input$num_var_select1),!!sym(input$num_var_select2)),list(Mean= ~round(mean(.x, na.rm = TRUE),4),
                                                                        Median= ~round(median(.x, na.rm = TRUE),4),
                                                                        SD= ~round(sd(.x, na.rm = TRUE),4),
                                                                        IQR= ~round(IQR(.x, na.rm = TRUE),4),
                                                                        Min= ~round(min(.x, na.rm = TRUE),4),
                                                                        Max= ~round(max(.x, na.rm = TRUE),4)),.names="{.fn}__{.col}")) |>
        pivot_longer(2:13,names_to=c(".value","variable"),names_sep="__") |>
        arrange(variable,!!sym(input$group_var)) |>
        mutate(`Numeric Variable`=names(num_vars)[match(variable,num_vars)]) |>
        select(!variable) |>
        select(!!sym(input$group_var),`Numeric Variable`,everything()) |>
        rename(!!names(grouping_vars)[grouping_vars==input$group_var]:=!!sym(input$group_var)) |>
        gt() |>
        gt_theme_pff() |>
        tab_options(
          heading.title.font.size = px(20),
          table.font.size=px(16),
          data_row.padding=px(8),
          heading.padding=px(12),
          column_labels.padding=px(8)
        ) |>
        opt_table_font(google_font(name = "Helvetica Neue")) |>
        tab_header(title=paste0("Summary Statistics for ",names(num_vars)[num_vars==input$num_var_select1]," and ",names(num_vars)[num_vars==input$num_var_select2]," by ",names(group_vars)[grouping_vars==input$group_var]))
    } else {
      data_subset$data |>
        summarize(across(c(!!sym(input$num_var_select1),!!sym(input$num_var_select2)),list(Mean= ~round(mean(.x, na.rm = TRUE),4),
                                                                                           Median= ~round(median(.x, na.rm = TRUE),4),
                                                                                           SD= ~round(sd(.x, na.rm = TRUE),4),
                                                                                           IQR= ~round(IQR(.x, na.rm = TRUE),4),
                                                                                           Min= ~round(min(.x, na.rm = TRUE),4),
                                                                                           Max= ~round(max(.x, na.rm = TRUE),4)),.names="{.fn}__{.col}")) |>
        pivot_longer(everything(),names_to=c(".value","variable"),names_sep="__") |>
        mutate(`Numeric Variable`=names(num_vars)[match(variable,num_vars)]) |>
        select(!variable) |>
        select(`Numeric Variable`,everything()) |>
        gt() |>
        gt_theme_pff() |>
        tab_options(
          heading.title.font.size = px(20),
          table.font.size=px(16),
          data_row.padding=px(8),
          heading.padding=px(12),
          column_labels.padding=px(8)
        ) |>
        opt_table_font(google_font(name = "Helvetica Neue")) |>
        tab_header(title=paste0("Summary Statistics for ",names(num_vars)[num_vars==input$num_var_select1]," and ",names(num_vars)[num_vars==input$num_var_select2]))
    }
  })
  
  #Graphical Summaries##########################################################
  
  output$density1<-renderPlot({
    if (isTruthy(input$group_var)) {
      grouped_density(data_subset$data,input$num_var_select1,input$group_var)
    } else {
      no_group_density(data_subset$data,input$num_var_select1)
    }
  })
  
  output$density2<-renderPlot({
    if (isTruthy(input$group_var)) {
      grouped_density(data_subset$data,input$num_var_select2,input$group_var)
    } else {
      no_group_density(data_subset$data,input$num_var_select2)
    }
  })
  
  output$scatterplot<-renderPlot({
    if (isTruthy(input$group_var)) {
      colors<-c("navy","darkred","darkgreen","darkorange","darkgrey")
      colors_subset<-colors[1:length(unique(data_subset$data[[input$group_var]]))]
      g<-ggplot(data=data_subset$data,aes(x=!!sym(input$num_var_select1),y=!!sym(input$num_var_select2),color=!!sym(input$group_var)))+
        geom_point(alpha=0.3)+
        theme_light(base_size = 14, base_family = "Helvetica Neue")+scale_color_manual(values=colors_subset)+
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
        )+labs(title=paste0(names(num_vars)[num_vars==input$num_var_select1]," vs. ",names(num_vars)[num_vars==input$num_var_select2]," by ",names(grouping_vars)[grouping_vars==input$group_var]),x=names(num_vars)[num_vars==input$num_var_select1],y=names(num_vars)[num_vars==input$num_var_select2],color=NULL)
      
      g
      } else {
        g<-ggplot(data=data_subset$data,aes(x=!!sym(input$num_var_select1),y=!!sym(input$num_var_select2)))+
          geom_point(alpha=0.4,color="navy")+
          theme_light(base_size = 14, base_family = "Helvetica Neue")+
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
          )+labs(title=paste0(names(num_vars)[num_vars==input$num_var_select1]," vs. ",names(num_vars)[num_vars==input$num_var_select2]),x=names(num_vars)[num_vars==input$num_var_select1],y=names(num_vars)[num_vars==input$num_var_select2],color=NULL)
        
        g
    }
  })
  

  #Win Probability Plot#########################################################
  
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
  
  output$probplot<-renderPlotly({
    req(input$matchup)
    
    play_data_1game<-play_data2 |>
      filter(week==input$week,game_label==input$matchup) |>
      drop_na(wp) |>
      mutate(dummy=1) |>
      mutate(play_num=cumsum(dummy)) |>
      select(home_team,away_team,home_wp,away_wp,play_num,qtr,time,desc) |>
      mutate(tooltip=paste0("Quarter: ", qtr, 
                            "<br>Game Clock: ", time,
                            "<br>Play: ", desc, 
                            "<br>", home_team, " Win Probability: ", round(home_wp,3),
                            "<br>",away_team," Win Probability: ", round(away_wp,3)))
    
    home<-play_data_1game |>
      distinct(home_team) |>
      pull(home_team)
    
    away<-play_data_1game |>
      distinct(away_team) |>
      pull(away_team)
    
    g<-ggplot(data=play_data_1game,aes(x=play_num,y=home_wp,color=home_team))+geom_line(linewidth=1.5,show.legend = FALSE)+scale_color_nfl(type = "primary") + 
      coord_cartesian(ylim=c(0,1))+
      geom_point(aes(text = tooltip), alpha = 0, size = 0.01, show.legend = FALSE)+
      theme_light(base_size = 14, base_family = "Helvetica Neue")+
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
      labs(title=paste0("Win Probability for the ",names(nfl_teams)[nfl_teams==home]," in their Week ",input$week," Game Against the ",names(nfl_teams)[nfl_teams==away]),x="Play Number",y="Win Probability")
    
    ggplotly(g,tooltip="text")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

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

play_data<-read_csv("2018_2019_rp_plays.csv") |>
  mutate(wp=round(wp,4),wpa=round(wpa,4)) |>
  mutate(turnover=if_else(fumble_lost==1 | interception==1,"yes","no")) |>
  mutate(winning=if_else(score_differential>=0,"yes","no")) |>
  mutate(qtr=factor(qtr,levels=1:5,labels=c(1:4,"Overtime")))

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    #Adding theme
    theme=bs_theme(bootswatch="darkly",
                   "card-bg" = "#ffffff",
                   "card-border-color" = "#ffffff",
                   "card-color" = "#2f2f2f",
                   "accordion-bg" = "#ffffff",
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
              actionsBox=TRUE,
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
            choices = num_vars,
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
                                                         selected = play_type,
                                                         multiple = FALSE
                                                       ),
                                                         pickerInput(
                                                           inputId = "cat_var2",
                                                           label = "Select a Grouping Variable:",
                                                           choices = secondary_cat_vars,
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

                       ))
              
            )            
          )

        )
    )
)

# Define server logic required to draw a histogram
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
      choices = non_selected_vars,
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
    data_subset$data<-play_data |>
      filter(play_type %in% c(input$run_pass),posteam %in% c(input$teams))
    
    if (isTruthy(input$num_var1)) {
      data_subset$data<-data_subset$data |>
        filter(!!sym(input$num_var1)>=input$num_subset1[1],!!sym(input$num_var1)<=input$num_subset1[2])
    }
    
    if (isTruthy(input$num_var2)) {
      data_subset$data<-data_subset$data |>
        filter(!!sym(input$num_var2)>=input$num_subset2[1],!!sym(input$num_var2)<=input$num_subset2[2])
    }    
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

#Numeric Summaries##############################################################
  
  #Updating cat_var2 options based on cat_var1 selection
  observeEvent(list(input$cat_var1,input$subset_data),{
    choices<-secondary_cat_vars[-which(secondary_cat_vars==input$cat_var1)]
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
    200*rows
  },{
    req(input$teams_check)
    if (isTruthy(input$cat_var2)) {
      base_plot()+facet_wrap(vars(posteam),ncol=2,axes="all",axis.labels="all")+
        theme(
          # make wordmarks of team abbreviations
          strip.text = nflplotR::element_nfl_wordmark(size = 1),
          # load image from url in caption
          plot.caption = ggpath::element_path(hjust = 1, size = 0.4)
        )+labs(title=paste0("Team Level: ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1]," by ",names(secondary_cat_vars)[secondary_cat_vars==input$cat_var2]))
    } else {
      base_plot()+facet_wrap(vars(posteam),ncol=2,axes="all",axis.labels="all")+
        theme(
          # make wordmarks of team abbreviations
          strip.text = nflplotR::element_nfl_wordmark(size = 1),
          # load image from url in caption
          plot.caption = ggpath::element_path(hjust = 1, size = 0.4)
        )+labs(title=paste0("Team-Level Breakdown of Plays by ",names(primary_cat_vars)[primary_cat_vars==input$cat_var1]))
    }
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

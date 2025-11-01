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

play_data<-read_csv("2018_2019_rp_plays.csv") |>
  mutate(wp=round(wp,4),wpa=round(wpa,4))

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
    #Adding theme
    theme=bs_theme(bootswatch="slate"),

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
             num_var2<-pickerInput(
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
                       "Some functionality to explore the data")
              
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
      selected = character(0),
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

}



# Run the application 
shinyApp(ui = ui, server = server)

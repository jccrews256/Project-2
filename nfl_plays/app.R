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

play_data<-read_csv("2018_2019_rp_plays.csv") |>
  mutate(wp=round(wp,4),wpa=round(wpa,4))

source("helpers.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

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
            choices=c("Run","Pass"),
            selected=c("Run","Pass"),
            multiple=TRUE
          ),
          
          #select a team
          pickerInput(
            inputId="team",
            label="Which Teams' Offensive Plays?",
            choices=nfl_teams,
            selected=nfl_teams,
            multiple=TRUE
          ),
          
          br(),
          
          h2("Select Numeric Variables to Subset By"),
          
          #select a numeric variable
          pickerInput(
            inputId = "num_var1",
            label = "Select a Numeric Variable:",
            choices = num_vars,
            selected = NULL,
            multiple = FALSE,
            options = list(
              title = "Select a variable..."
            )
          ),
          
          uiOutput("variable1_selected"),
          
          
          uiOutput("variable2_selected"),
          
          h2("Done Subsetting? Submit Your Changes!"),
          
          actionButton("subset_data","Subset the Data")
        ),

        # Show a plot of the generated distribution
        mainPanel(

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
    
    num_var2<-pickerInput(
      inputId = "num_var2",
      label = "Select another Numeric Variable:",
      choices = non_selected_vars,
      selected = NULL,
      multiple = FALSE,
      options = list(
        title = "Select a variable..."
      )
    )
    
    tagList(
      num_slider,
      num_var2
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

}

# Run the application 
shinyApp(ui = ui, server = server)

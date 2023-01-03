
#

library(tidyverse)
library(leaflet)
library(here)
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Homelessness in B.C."),
  
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Basic Overview", 
               tabName = "overview"
               ),
      menuItem("More Details",
               menuSubItem("Sub1", tabName = "sub1"),
               menuSubItem("Sub2",tabName = "sub2" )
               ),
      menuItem("About",
               tabName = "about"),
      menuItem(selectInput(inputId = "homeless_type",
                           label = "Homeless Type",
                           choices = c("Respondents","Sheltered", "Unsheltered")))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "overview",
              fluidRow(valueBoxOutput("total_homeless_identified"),
                       valueBoxOutput("percent_sheltered_card"),
                       valueBoxOutput("percent_unsheltered_card")
                       ),
              fluidRow(
                column(width = 12,
                       leafletOutput("map_bc_communities")
                       )
                ),
              fluidRow(
                column(width = 3,
                       plotOutput("gender_distn")
                       ),
                column(width = 3,
                       plotOutput("age_distn")
                       ),
                column(width = 6,
                       plotOutput("racial_identity_distn")
                       )
                ),
              
              fluidRow(plotOutput("source_of_income_distn"))
              
      ),
      
      tabItem(tabName = "sub1",
              fluidRow(box(plotOutput("age_when_first_homeless")),
                       box(plotOutput("homeless_period"))
                       ),
              fluidRow(tabBox(title = "Where Stayed Night of Count",
                              id = "place_of_stay_tab",
                              height = "300px",
                              tabPanel("Sheltered",tableOutput("place_of_stay_sheltered")),
                              tabPanel("Unsheltered",tableOutput("place_of_stay_unsheltered"))
                              )
                       ),
              fluidRow(
                box(box(tableOutput("housing_loss_top10_table")))
              )
              
      ),
      
      tabItem(tabName = "sub2",
              fluidRow(box(plotOutput("health_condition_distn")),
                       box(plotOutput("num_health_conditions_distn"))
                       )
              ),
      
      tabItem(tabName = "about",
              div(includeMarkdown("about.md")))
      
    )
    
  )
  
)



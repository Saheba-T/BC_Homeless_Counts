
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
      menuItem("Provincial Findings",
               tabName = "all_communities"),
      menuItem("Summary By Community",
               tabName = "by_communities"),
      menuItem("About",
               tabName = "about")
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "all_communities",
              fluidRow(valueBoxOutput("total_homeless_identified"),
                       valueBoxOutput("percent_sheltered_card"),
                       valueBoxOutput("percent_unsheltered_card")
              ),
              fluidRow(
                
                tabBox(title = "Demographics",
                       id = "demographics_tab",
                       height = "375px",
                       tabPanel("Gender",plotOutput("gender_distn")),
                       tabPanel("Age",plotOutput("age_distn")),
                       tabPanel("Race",plotOutput("racial_identity_distn"))
                ),
                tabBox(title = "Health Conditions",
                       id = "health_info_tab",
                       height = "375px",
                       tabPanel("Health Concerns",plotOutput("health_condition_distn")),
                       tabPanel("Number of Health Concerns", plotOutput("num_health_conditions_distn"))
                )
                
              ),
              fluidRow(
                box(plotOutput("source_of_income_distn")),
                box(uiOutput("homeless_type_selector")),
                tabBox(title = "Where Stayed Night of Count",
                       id = "place_of_stay_tab",
                       height = "300px",
                       tabPanel("Sheltered",tableOutput("place_of_stay_sheltered")),
                       tabPanel("Unsheltered",tableOutput("place_of_stay_unsheltered"))
                )
                
                
                
              ),
              fluidRow(
                box(tableOutput("housing_loss_top10_table")),
                tabBox(title = "History",
                       tabPanel("Age when First Homeless",plotOutput("age_when_first_homeless")),
                       tabPanel("Time Period as Homeless",plotOutput("homeless_period"))
                )
              )
              
      ),
      
      tabItem(tabName = "by_communities",
              fluidRow(box(width = 12,leafletOutput("map_bc_communities"))),
              fluidRow(box(width = 12,tableOutput("time_in_community")))
              
      ),
      
      tabItem(tabName = "about",
              div(includeMarkdown("about.md")))
      
    )
    
  )
  
)



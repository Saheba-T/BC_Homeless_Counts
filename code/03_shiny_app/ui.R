
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
      menuItem("General Overview", 
               tabName = "overview"),
      menuItem("Further Findings",
               menuSubItem("Sub1", tabName = "sub1"),
               menuSubItem("Sub2",tabName = "sub2" )),
      menuItem("About",
               tabName = "about")
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
                
                tabBox(title = "Demographics",
                       id = "demographics_tab",
                       height = "375px",
                       tabPanel("Gender",plotOutput("gender_distn")),
                       tabPanel("Age",plotOutput("age_distn")),
                       tabPanel("Race",plotOutput("racial_identity_distn"))
                )
                
                
              ),
              fluidRow(
                box(plotOutput("source_of_income_distn")),
                box(uiOutput("homeless_type_selector"))
              ),
              fluidRow(
                box(tableOutput("housing_loss_top10_table")),
                box(leafletOutput("map_bc_communities"))
                
              )
              
      ),
      
      tabItem(tabName = "sub1",
              fluidRow(box(plotOutput("age_when_first_homeless")),
                       box(plotOutput("homeless_period"))),
              fluidRow(tabBox(title = "Where Stayed Night of Count",
                              id = "place_of_stay_tab",
                              height = "300px",
                              tabPanel("Sheltered",tableOutput("place_of_stay_sheltered")),
                              tabPanel("Unsheltered",tableOutput("place_of_stay_unsheltered"))
              ))
              
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



# This file defines the UI of the shiny app


ui <- dashboardPage(
  dashboardHeader(title = "B.C. Homeless Counts", titleWidth = 245),
  
  dashboardSidebar(width = 245,
    sidebarMenu(
      menuItem(selectInput(inputId = "homeless_type",
                           label = "Homeless Type",
                           choices = c("Respondents","Sheltered", "Unsheltered"))
      ),
      menuItem("Basic Demographic", 
               tabName = "overview"
               ),
      menuItem("More Details",
               menuSubItem("History of Homelessness", tabName = "sub1"),
               menuSubItem("Health-Related Information",tabName = "sub2" )
               ),
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
                column(width = 12,
                       h4("Map of Participating Communities"),
                       leafletOutput("map_bc_communities")
                       )
                ),
              fluidRow(
                column(width = 3,
                       h4("Age"),
                       plotOutput("age_distn")
                       ),
                column(width = 3,
                       h4("Gender"),
                       plotOutput("gender_distn")
                ),
                column(width = 6,
                       h4("Racial Identity"),
                       plotOutput("racial_identity_distn")
                       )
                ),
              fluidRow(h4("Source of Income"),
                       plotOutput("source_of_income_distn"))
              
      ),
      
      tabItem(tabName = "sub1",
              fluidRow(h4("Loss of Housing (More than 1 response possible)"),
                       plotOutput("housing_loss")),
              fluidRow(box(plotOutput("age_when_first_homeless")),
                       box(plotOutput("homeless_period"))
                       ),
              fluidRow(tabBox(title = "Where Stayed Night of Count",
                              id = "place_of_stay_tab",
                              height = "300px",
                              tabPanel("Sheltered",tableOutput("place_of_stay_sheltered")),
                              tabPanel("Unsheltered",tableOutput("place_of_stay_unsheltered"))
                              )
                       )
              
      ),
      
      tabItem(tabName = "sub2",
              fluidRow(
                column(width = 7,
                       h4("Health Condition"),
                       plotOutput("health_condition_distn")
                       ),
                column(width = 5,
                       h4("Total Number of Health Conditions"),
                       plotOutput("num_health_conditions_distn")
                       )),
              fluidRow(h4("Services Accessed in Previous 12 Months"),
                       plotOutput("services_accessed")
                       )
              ),
      
      tabItem(tabName = "about",
              div(includeMarkdown("about.md")))
      
    )
    
  )
  
)



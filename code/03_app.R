
# Author: Saheba Tegally

# Note: Always set working directory to project directory before running the script


library(tidyverse)
library(here)
library(webr)
library(shiny)
library(shinydashboard)


 

ui <- dashboardPage(
  dashboardHeader(title = "Homelessness in B.C."),
  
  dashboardSidebar(
                   sidebarMenu(
                     menuItem("All Communities",
                              tabName = "all_communities"),
                     menuItem("By Communities",
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
                tabBox(title = "Where Stayed Night of Count",
                       id = "place_of_stay_tab",
                       height = "250px",
                       tabPanel("Sheltered",tableOutput("place_of_stay_sheltered")),
                       tabPanel("Unsheltered",tableOutput("place_of_stay_unsheltered"))
                       ),
                box(plotOutput("health_condition_distn"))
                
                ),
              fluidRow(
                tabBox(title = "Demographics",
                       id = "demographics_tab",
                       height = "350px",
                       tabPanel("Gender",plotOutput("gender_distn")),
                       tabPanel("Age",plotOutput("age_distn")),
                       tabPanel("Race",plotOutput("racial_identity_distn"))
                ),
                box(uiOutput("homeless_type_selector")),
                box(tableOutput("housing_loss_top10_table")),
                box(plotOutput("source_of_income_distn"))
                )
        
              ),
  
      tabItem(tabName = "by_communities"),
      
      tabItem(tabName = "about",
              div(includeMarkdown("about.md")))
      
      )
    
  )
  
)






server <- function(input, output) {
  
  # Provincial findings tab ---------------------------------------------------
  
  output$percent_sheltered_card <- renderValueBox({
    
    df_homeless_distn <- 
      read_csv(paste0(here(),"/data/clean_data/table1_c.csv")) %>%
      group_by(Homeless_type) %>%
      summarise(total = sum(Total_homeless))
    
    percent_sheltered <- round(df_homeless_distn %>%
                                 filter(Homeless_type == "Sheltered") %>%
                                 select(total)*100/sum(df_homeless_distn[,"total"]),
                               0)
      
    
    valueBox(value = percent_sheltered,
             subtitle = "Percentage Sheltered",
             icon = icon("percent"),
             color = "blue"
            )
  })
  
  
  output$percent_unsheltered_card <- renderValueBox({
    
    df_homeless_distn <- 
      read_csv(paste0(here(),"/data/clean_data/table1_c.csv")) %>%
      group_by(Homeless_type) %>%
      summarise(total = sum(Total_homeless))
    
    percent_unsheltered <- round(df_homeless_distn %>%
                                 filter(Homeless_type == "Unsheltered") %>%
                                 select(total)*100/sum(df_homeless_distn[,"total"]),
                               0)
    
    valueBox(value = percent_unsheltered,
             subtitle = "Percentage Unsheltered",
             icon = icon("percent"),
             color = "purple"
             )
  })
  
  
  output$total_homeless_identified <- renderValueBox({
    
    df_homeless_distn <- read_csv(paste0(here(),"/data/clean_data/table1_c.csv"))
    total_homeless <- sum(df_homeless_distn[,"Total_homeless"])
      
    
    valueBox(value = total_homeless,
             subtitle = "Total Homeless Identified",
             icon = icon("hashtag"),
             color = "red")
  })
  
  
  output$homeless_type_selector <- renderUI({
    
    selectInput(inputId = "homeless_type",
                label = "Homeless Type",
                choices = c("Sheltered", "Unsheltered", "Respondents"))
    
  })
  
  
  output$gender_distn <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.2_c.csv"))
    
    df %>% 
      slice_head( n = nrow(df)-3)%>%
      select("Gender_identity",contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),names_to = "type", values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      arrange(desc("Gender_identity")) %>%
      ggplot(aes(x = "", y = percentage, fill = Gender_identity)) +
      geom_bar(stat = "identity", width = 1, col = "white") +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Dark2") +
      geom_text(aes(label = paste(percentage,"%", sep = "")), 
                position = position_stack(vjust = 0.5), 
                col = "white",
                fontface = "bold") +
      geom_text(aes(x = 1.8, label = Gender_identity),
                position = position_stack(vjust = .5),
                fontface = "bold",
                size = 4) +
      theme(panel.background = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank(),
            line = element_blank(),
            legend.position = "none") 
    
  })
  
  
  output$housing_loss_top10_table <- renderTable({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.10_c.csv"))
    
    df %>%
      slice_head(n = (nrow(df)-3)) %>%
      select("Housing_loss_reason",contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),names_to = "type", values_to = "Percentage") %>%
      mutate_at(.vars = vars("type"), .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      arrange(desc(Percentage)) %>%
      slice_max(order_by = Percentage, n = 10) %>%
      mutate(Rank = row_number()) %>%
      select(Rank,Housing_loss_reason,Percentage) 
    
  })
  
  
  output$age_distn <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.4_c.csv"))
    
    df %>% 
      slice_head(n = nrow(df)-3) %>%
      select("Age_groups",contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),names_to = "type", values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      ggplot(aes(x="",y=percentage, fill = Age_groups)) +
      geom_bar(stat = "identity",width = 1, color = "white") +
      coord_polar("y", start = 0) +
      scale_fill_brewer(palette = "Dark2") +
      geom_text(aes(label = paste(percentage,"%", sep = "")),
                position = position_stack(vjust = 0.5), 
                col = "white",
                fontface = "bold") +
      geom_text(aes(x = 1.8, label = Age_groups),
                position = position_stack(vjust = .5),
                fontface = "bold",
                size = 4) +
      theme(legend.position = "none",
            panel.background = element_blank(),
            axis.text = element_blank(),
            line = element_blank(),
            axis.title = element_blank()) 
    
  })
  
  
  output$racial_identity_distn <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.6_c.csv"))
    
    df %>% 
      slice_head(n = nrow(df)-3) %>%
      select("Racial_identity",contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),names_to = "type", values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      ggplot(aes(x = reorder(Racial_identity,percentage), y = percentage)) +
      geom_bar(stat = "identity", fill = "darkcyan") +
      geom_text(aes(label = paste(percentage,"%", sep = "")), 
                nudge_y = 2, 
                col = "black", 
                fontface = "bold") +
      theme(line = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(face = "bold", size = 11),
            axis.text.x = element_blank(),
            legend.position = "none") +
      coord_flip()
      
    
  })
  
  
  output$health_condition_distn <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.11_c.csv"))
    
    df %>% 
      slice_head(n = nrow(df)-3) %>%
      select("Health_condition",contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),names_to = "type", values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      ggplot(aes(x = reorder(Health_condition,percentage), y = percentage)) +
      geom_bar(stat = "identity",width = 0.5,fill = "darkcyan") +
      geom_text(aes(label = paste(percentage,"%", sep = "")), 
                position = position_stack(vjust = 0.5), 
                col = "white", 
                fontface = "bold") +
      scale_x_discrete(labels = c("Addiction","Learning disability", "Medical Condition",
                                  "Mental Health Issue", "Physical disability")) +
      theme(line = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(face = "bold", size = 11),
            axis.text.x = element_blank(),
            legend.position = "none",
            aspect.ratio = 1/3) +
      coord_flip()
    
  })
  
  
  output$source_of_income_distn <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.15_c.csv"))
    
    df %>% 
      slice_head(n = nrow(df)-3) %>%
      select("Sources_of_income",contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),names_to = "type", values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      ggplot(aes(x = reorder(Sources_of_income,percentage), y = percentage)) +
      geom_bar(stat = "identity", fill = "darkcyan") +
      geom_text(aes(label = paste(percentage,"%", sep = "")), 
                position = position_stack(vjust = 0.5), 
                col = "white", 
                fontface = "bold") +
      theme(line = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(face = "bold", size = 11),
            axis.text.x = element_blank(),
            legend.position = "none") +
      coord_flip()
    
  })
  
  
  output$place_of_stay_sheltered <- renderTable({
      
      df <- read_csv(paste0(here(),"/data/clean_data/table1_c.csv"))
      
      df %>%
        filter(Homeless_type == "Sheltered") %>%
        select(-Homeless_type) %>%
        mutate_at(.vars = vars(contains("Percent")),
                  .funs = list(~paste0(.,"%"))) %>%
        rename("Place_of Stay" = "Sheltered_and_unsheltered",
               "Total" = "Total_homeless",
               "Percentage" = "Percent_homeless")
    
  })
  
  
  output$place_of_stay_unsheltered <- renderTable({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table1_c.csv"))
    
    df %>%
      filter(Homeless_type == "Unsheltered") %>%
      mutate_at(.vars = vars(contains("Percent")),
                .funs = list(~paste0(.,"%"))) %>%
      select(-Homeless_type) %>%
      rename("Place of Stay" = "Sheltered_and_unsheltered",
             "Total" = "Total_homeless",
             "Percentage" = "Percent_homeless")
    
  })
  
    
    
 
  
  # By communities tab --------------------------------------------------------
  
  
  
  
  # About page Tab ------------------------------------------------------------
  
  
}



shinyApp(ui, server)



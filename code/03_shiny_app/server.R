

library(tidyverse)
library(leaflet)
library(htmltools)
library(here)
library(shiny)
library(shinydashboard)



server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------
  # Provincial Findings tab 
  #-----------------------------------------------------------------------------
  
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
  
  
  output$map_bc_communities <- renderLeaflet({
    
    df <- read_csv(paste0(here(), "/data/clean_data/table3.1_c.csv"))
    df$labels <- paste("<p>","Community Name: ", df$BC_community,"</p>",
                       "<p>","Number of homeless identified: ",df$Total_Respondents_2021,"</p>",
                       "<p>","Change in homeless population from 2018: ",df$Percent_change,"%","</p>")
    
    leaflet() %>%
      setView(lng = -127.6476, lat = 53.7267, zoom = 4.7) %>%
      addProviderTiles(provider = providers$CartoDB.Voyager) %>%
      addCircleMarkers(lng = df$lon, 
                       lat = df$lat,
                       color = "red",
                       radius = 5,
                       weight = 2,
                       label = lapply(df$labels, HTML))
    
    
    
  })
  
  
  output$gender_distn <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.2_c.csv"))
    df %>% 
      slice_head(n = nrow(df)-3)%>%
      select("Gender_identity",contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),
                   names_to = "type", 
                   values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), 
                .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      arrange(desc("Gender_identity")) %>%
      ggplot(aes(x = reorder(Gender_identity,-percentage), 
                 y = percentage, 
                 fill = Gender_identity)) +
      geom_bar(stat = "identity", width = 1, col = "white") +
      scale_fill_brewer(palette = "Dark2") +
      geom_text(aes(label = paste(percentage,"%", sep = "")), 
                nudge_y = 2, 
                col = "black",
                fontface = "bold") +
      theme(panel.background = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            axis.text.y = element_blank(),
            axis.title = element_blank(),
            line = element_blank(),
            legend.position = "none") 
    
  })
  
  
  output$age_distn <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.4_c.csv"))
    
    df %>% 
      slice_head(n = nrow(df)-3) %>%
      select("Age_groups",contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),
                   names_to = "type", 
                   values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), 
                .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      ggplot(aes(x = reorder(Age_groups,-percentage),
                 y = percentage, 
                 fill = Age_groups)) +
      geom_bar(stat = "identity",
               width = 1, 
               color = "white") +
      scale_fill_brewer(palette = "Dark2") +
      geom_text(aes(label = paste(percentage,"%", sep = "")),
                nudge_y = 2, 
                col = "black",
                fontface = "bold") +
      theme(legend.position = "none",
            panel.background = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            axis.text.y = element_blank(),
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
            legend.position = "none",
            aspect.ratio = 1/2) +
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
  
 
  #-----------------------------------------------------------------------------
  # More Details - Sub1 tab 
  #-----------------------------------------------------------------------------
  
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
  
  
  output$age_when_first_homeless <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.18_c.csv"))
    
    df %>%
      select(Age,contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),names_to = "type",values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == c("Sheltered","Unsheltered")) %>% 
      ggplot(aes(Age, percentage, fill = type)) +
      geom_col(position = position_dodge()) +
      geom_text(aes(label = paste0(percentage,"%")),
                position = position_dodge(0.9),
                color = "white",
                fontface = "bold",
                size = 4.5,
                vjust = 1.5) +
      theme(panel.background = element_blank(),
            axis.title = element_blank(),
            line = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            axis.text.y = element_blank(),
            legend.position = "right",
            aspect.ratio = 1.1/1)
    
    
  })
  
  
  output$homeless_period <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.17_c.csv"))
    
    df %>%
      select(Time_period,contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"), names_to = "type",values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == c("Sheltered", "Unsheltered")) %>%
      ggplot(aes(Time_period, percentage, fill = type)) +
      geom_col(position = position_dodge()) +
      geom_text(aes(label = paste0(percentage,"%")),
                position = position_dodge(0.65),
                color = "black",
                fontface = "bold",
                size = 4,
                hjust = -0.05) +
      theme(panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(face = "bold", size = 11),
            axis.text.x = element_blank(),
            line = element_blank(),
            legend.position = "bottom") +
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
  
  
  #-----------------------------------------------------------------------------
  # More Details - Sub2 tab 
  #-----------------------------------------------------------------------------
  
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
            aspect.ratio = 1/2) +
      coord_flip()
    
  })
  
  
  output$num_health_conditions_distn <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.12_c.csv"))
    
    df %>%
      select("Number_of_conditions",contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),
                   names_to = "type", 
                   values_to = "percentage") %>%
      mutate_at(.vars = vars("type"), 
                .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      ggplot(aes(x = factor(Number_of_conditions), y = percentage)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(percentage,"%", sep = "")), 
                position = position_stack(vjust = 0.5), 
                col = "white", 
                fontface = "bold") +
      scale_x_discrete(labels = c("None","One","Two","Three","Four","Five")) +
      theme(line = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            axis.text.y = element_blank(),
            legend.position = "none",
            aspect.ratio = 1/3) 
    
  }) 
  
  
  output$services_accessed <- renderPlot({
    
    df <- read_csv(paste0(here(),"/data/clean_data/table2.16_c.csv"))
    
    df %>%
      select(Services_Accessed, contains("Percent")) %>%
      pivot_longer(cols = contains("Percent"),
                   names_to = "type",
                   values_to = "percentage") %>%
      mutate_at(.vars = vars("type"),
                .funs = list(~ str_to_title(gsub("Percent_","",.)))) %>%
      filter(type == input$homeless_type) %>%
      ggplot(aes(x = reorder(Services_Accessed, percentage), y = percentage)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(percentage,"%", sep = "")), 
                position = position_stack(vjust = 0.5), 
                col = "white", 
                fontface = "bold") +
      theme(panel.background = element_blank(),
            line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(face = "bold",size = 11),
            axis.title = element_blank()) +
      coord_flip()
    
  })
  
}


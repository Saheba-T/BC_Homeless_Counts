# This file defines the Server of the shiny app


server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------
  # Provincial Findings tab 
  #-----------------------------------------------------------------------------
  
  output$percent_sheltered_card <- renderValueBox({
    
    df_homeless_distn <- my_data[[1]] %>%
      group_by(Homeless_type) %>%
      summarise(total = sum(Total_homeless))
    
    percent_sheltered <- round(df_homeless_distn %>%
                                 filter(Homeless_type == "Sheltered") %>%
                                 select(total)*100/sum(df_homeless_distn[,"total"]),
                               0)
    
    valueBox(value = percent_sheltered,
             subtitle = "Percentage Sheltered",
             icon = icon("percent"),
             color = "orange"
             )
    
  })
  
  
  output$percent_unsheltered_card <- renderValueBox({
    
    df_homeless_distn <- my_data[[1]] %>%
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
    
    df_homeless_distn <- my_data[[1]]
    total_homeless <- sum(df_homeless_distn[,"Total_homeless"])
    
    valueBox(value = total_homeless,
             subtitle = "Total Homeless Identified",
             icon = icon("hashtag"),
             color = "red")
  })
  
  
  output$map_bc_communities <- renderLeaflet({
    
    df <- my_data[[12]]
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
  
  
  # Prepare data for gender table
  df_gender <- reactive({
    
    prep_data(my_data[[9]],"Gender_identity", input$homeless_type) 
    
  })
  
  # plot side-by-side bar plot for gender distribution
  output$gender_distn <- renderPlot({

      df_gender() %>%
      arrange(desc("Gender_identity")) %>%
      ggplot(aes(x = reorder(Gender_identity,-Percentage), 
                 y = Percentage, 
                 fill = Colour)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.8) +
      scale_fill_identity() +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                vjust = -0.5,
                col = "black",
                fontface = "bold") +
      theme(panel.background = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank(),
            legend.position = "bottom"
            ) 
    
  })
  
  # Prepare data for age table
  df_age <- reactive({
    
    prep_data(my_data[[10]],"Age_groups", input$homeless_type)
    
  })
  
  # plot side-by-side bar plot for age distribution
  output$age_distn <- renderPlot({
    
      df_age() %>%
      ggplot(aes(x = reorder(Age_groups,-Percentage),
                 y = Percentage, 
                 fill = Colour)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      scale_fill_identity() +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                vjust = -0.5,
                col = "black",
                fontface = "bold") +
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            axis.text.x = element_text(face = "bold", size = 11),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank()
            ) 
    
  })

  # Prepare data for racial identity table
  df_race <- reactive({
    
    prep_data(my_data[[11]],"Racial_identity", input$homeless_type) %>%
      mutate(Racial_identity = as.factor(Racial_identity))
  })
  
  
  # plot back-to-back bar chart for racial identity distribution
  output$racial_identity_distn <- renderPlot({
    
    df_race() %>%
      ggplot(aes(x = reorder(Racial_identity,Percentage),
                 y = Percentage, 
                 fill = Colour)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      scale_fill_identity() +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                hjust = -0.5,
                col = "black",
                fontface = "bold") +
      coord_flip() +
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(face = "bold", size = 11),
            axis.ticks.x = element_blank(),
            axis.title = element_blank()
      ) 
    
    
  })

  
  # Prepare data for sources of income table
  df_income <- reactive({
    
    prep_data(my_data[[5]], "Sources_of_income", input$homeless_type) %>%
      mutate(Sources_of_income = as.factor(Sources_of_income))
    
  })
  
  output$source_of_income_distn <- renderPlot({
    
    df_income() %>%
      ggplot(aes(x = reorder(Sources_of_income,Percentage),
                 y = Percentage, 
                 fill = Colour)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      scale_fill_identity() +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                hjust = -0.5,
                col = "black",
                fontface = "bold") +
      coord_flip() +
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            axis.text.y = element_text(face = "bold", size = 11),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_blank()
      ) 
    
  })
  
  #-----------------------------------------------------------------------------
  # More Details - History of Homelessness tab
  #-----------------------------------------------------------------------------
  #2
  # Prepare data for housing loss reasons table
  df_housing_loss <- reactive({
    
    prep_data(my_data[[2]], "Housing_loss_reason", input$homeless_type) %>%
      mutate(Housing_loss_reason = as.factor(Housing_loss_reason))
    
  })
  
  output$housing_loss <- renderPlot({
    
    df_housing_loss() %>%
      ggplot(aes(x = reorder(Housing_loss_reason,Percentage),
                 y = Percentage, 
                 fill = Colour)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      scale_fill_identity() +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                hjust = -0.5,
                col = "black",
                fontface = "bold") +
      coord_flip() +
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            axis.text.y = element_text(face = "bold", size = 11),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_blank())
    
  })
  
  df_homeless_age <- reactive({
    
    prep_data(my_data[[8]], "Age", input$homeless_type) %>%
      mutate(Age = as.factor(Age))
    
  })
  
  output$age_when_first_homeless <- renderPlot({
    
    df_homeless_age() %>%
      ggplot(aes(Age, Percentage, fill = Colour)) +
      geom_col(position = position_dodge()) +
      scale_fill_identity() +
      geom_text(aes(label = paste0(Percentage,"%")),
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
  
  df_homeless_period <- reactive({
    
    prep_data(my_data[[7]], "Time_period", input$homeless_type) %>%
      mutate(Time_period = as.factor(Time_period))
    
  })
  
  output$homeless_period <- renderPlot({
    
    df_homeless_period() %>%
      ggplot(aes(Time_period, Percentage, fill = Colour)) +
      geom_col(position = position_dodge()) +
      scale_fill_identity() +
      geom_text(aes(label = paste0(Percentage,"%")),
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
    #1
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
    #1
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
  # More Details - Health-Related Info tab 
  #-----------------------------------------------------------------------------
  
  # Prepare data for health condition table
  df_health_condition <- reactive({
    
    prep_data(my_data[[3]],"Health_condition",input$homeless_type) 
    
  })
  
  output$health_condition_distn <- renderPlot({
    
    df_health_condition() %>% 
      ggplot(aes(x = reorder(Health_condition,Percentage), 
                 y = Percentage,
                 fill = Colour)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                hjust = -0.5,
                col = "black",
                fontface = "bold") +
      scale_fill_identity() +
      coord_flip() +
      theme(line = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text.y = element_text(face = "bold", size = 12),
            axis.text.x = element_blank(),
            legend.position = "bottom")
      
    
  })
  
  # Prepare data for number of health condition(s) table
  df_num_health_conditions <- reactive({
    
    prep_data(my_data[[4]],"Number_of_conditions", input$homeless_type) 
  })
  
  output$num_health_conditions_distn <- renderPlot({
    
    df_num_health_conditions() %>%
      ggplot(aes(x = factor(Number_of_conditions), 
                 y = Percentage,
                 fill = Colour)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                vjust = 1.2,
                col = "white", 
                fontface = "bold") +
      scale_fill_identity() +
      scale_x_discrete(labels = c("None","One","Two","Three","Four","Five")) +
      theme(line = element_blank(),
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(face = "bold", size = 12),
            axis.text.y = element_blank(),
            legend.position = "bottom") 
    
  }) 
  
  # Prepare data for services accessed table
  df_services_accessed <- reactive({
    
    prep_data(my_data[[6]],"Services_Accessed", input$homeless_type) %>%
      mutate(Services_Accessed = as.factor(Services_Accessed))
    
  })
  
  output$services_accessed <- renderPlot({
    
    df_services_accessed() %>%
      ggplot(aes(x = reorder(Services_Accessed,Percentage),
                 y = Percentage, 
                 fill = Colour)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      scale_fill_identity() +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                hjust = -0.5,
                col = "black",
                fontface = "bold") +
      coord_flip() +
      theme(legend.position = "bottom",
            panel.background = element_blank(),
            axis.text.y = element_text(face = "bold", size = 11),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title = element_blank()
      ) 
    
  })
  
}


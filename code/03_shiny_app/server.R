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
  
  # Change groups to filter on
  # based on whether there is a comparison to 2018
  groups <- reactive({
    
    if (input$comparison_to_2018){
      c("Respondents","2018")
    }else{
      c("Sheltered","Unsheltered")
    }
  })
  
  # Prepare data for gender table
  df_gender <- reactive({
    
    prep_data(my_data[[9]],"Gender_identity", groups()) %>%
      filter(!Gender_identity %in% c("Respondents","Total","Don't Know/No Answer"))
  })
  
  # plot side-by-side bar plot for gender distribution
  output$gender_distn <- renderPlot({

      df_gender() %>%
      arrange(desc("Gender_identity")) %>%
      ggplot(aes(x = reorder(Gender_identity,-Percentage), 
                 y = Percentage, 
                 fill = Type)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      scale_fill_manual(values = c("#023047","#219ebc")) +
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
    
    prep_data(my_data[[10]],"Age_groups", groups()) %>%
      filter(!Age_groups %in% c("Respondents","Total","Don't Know/No Answer"))
  })
  
  # plot side-by-side bar plot for age distribution
  output$age_distn <- renderPlot({
    
      df_age() %>%
      ggplot(aes(x = reorder(Age_groups,-Percentage),
                 y = Percentage, 
                 fill = Type)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      scale_fill_manual(values = c("#023047","#219ebc")) +
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
    
    prep_data(my_data[[11]],"Racial_identity", c("Sheltered","Unsheltered")) %>%
      filter(!Racial_identity %in% c("Respondents","Total","Don't Know/No Answer")) %>%
      mutate(Racial_identity = as.factor(Racial_identity))
  })
  
  
  # plot back-to-back bar chart for racial identity distribution
  output$racial_identity_distn <- renderPlot({
    
    plt.group1 <-
      df_race() %>%
      filter(str_to_title(Type) == "Unsheltered") %>%
      ggplot(aes(x = Racial_identity,y = Percentage)) +
      geom_bar(stat = "identity", fill = "#219ebc", alpha = 0.75) +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                nudge_y = -2.7,
                col = "black", 
                fontface = "bold") +
      scale_y_reverse() +
      coord_flip() +
      theme(legend.position = 'none',
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(), 
            axis.text = element_blank(),
            plot.title = element_text(size = 11.5),
            plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm")) 
    
    
    plt.group2 <-
      df_race() %>%
      filter(str_to_title(Type) == "Sheltered") %>%
      ggplot(aes(x = Racial_identity,y = Percentage)) +
      geom_bar(stat = "identity", fill = "#023047", alpha = 0.75) +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                nudge_y = 3,
                col = "black", 
                fontface = "bold") +
      coord_flip() +
      theme(legend.position = 'none',
            panel.background = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_blank(), 
            axis.text.y = element_text(face = "bold", 
                                       size = 10, 
                                       vjust = 0.5, 
                                       hjust = 0.9),
            axis.ticks = element_blank(), 
            plot.title = element_text(size = 11.5),
            plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm")) 
    
    
    grid.arrange(plt.group1, plt.group2,
                 widths=c(0.4, 0.6), ncol=2
    )
    
  })

  
  # Prepare data for sources of income table
  df_income <- reactive({
    
    prep_data(my_data[[5]], "Sources_of_income", groups()) %>%
      filter(!Sources_of_income %in% c("Respondents","Total","Don't Know/No Answer")) %>%
      mutate(Sources_of_income = as.factor(Sources_of_income)) %>%
      pivot_wider(names_from = "Type", values_from = "Percentage") %>%
      mutate(mydiff = .data[[groups()[1]]] - .data[[groups()[2]]]) %>% 
      mutate(Sources_of_income = fct_reorder(Sources_of_income, abs(mydiff)))
    
  })
  
  output$source_of_income_distn <- renderPlot({
    
    df_income() %>%
      ggplot() +
      geom_segment(aes(x = Sources_of_income,
                       xend = Sources_of_income,
                       y = .data[[groups()[1]]],
                       yend = .data[[groups()[2]]]),
                   color = "darkgrey")+
      geom_point(aes(x = Sources_of_income, 
                     y = .data[[groups()[1]]]),
                 size = 4,
                 color = "#219ebc") +
      geom_point(aes(x = Sources_of_income, 
                     y = .data[[groups()[2]]]), 
                 size = 4,
                 color = "#023047") +
      coord_flip() +
      theme(legend.position = "top") 
    
  })
  
  #-----------------------------------------------------------------------------
  # More Details - Sub1 tab 
  #-----------------------------------------------------------------------------
  #2
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
    #8
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
    #7
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
  # More Details - Sub2 tab 
  #-----------------------------------------------------------------------------
  
  # Prepare data for health condition table
  df_health_condition <- reactive({
    
    prep_data(my_data[[3]],"Health_condition", groups()) %>%
      filter(!Health_condition %in% c("Respondents","Total","Don't Know/No Answer"))
  })
  
  output$health_condition_distn <- renderPlot({
    
    df_health_condition() %>% 
      ggplot(aes(x = reorder(Health_condition,Percentage), 
                 y = Percentage,
                 fill = Type)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                vjust = 0.5,
                hjust = 1.1,
                col = "white", 
                fontface = "bold") +
      scale_fill_manual(values = c("#023047","#219ebc")) +
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
    
    prep_data(my_data[[4]],"Number_of_conditions", groups()) %>%
      filter(!Number_of_conditions %in% c("Respondents","Total","Don't Know/No Answer"))
  })
  
  output$num_health_conditions_distn <- renderPlot({
    
    df_num_health_conditions() %>%
      ggplot(aes(x = factor(Number_of_conditions), 
                 y = Percentage,
                 fill = Type)) +
      geom_bar(stat = "identity",
               position = position_dodge(),
               alpha = 0.75) +
      geom_text(aes(label = paste(Percentage,"%", sep = "")), 
                position = position_dodge(0.9),
                vjust = 1.2,
                col = "white", 
                fontface = "bold") +
      scale_fill_manual(values = c("#023047","#219ebc")) +
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
    
    prep_data(my_data[[6]],"Services_Accessed", c("Sheltered","Unsheltered")) %>%
      filter(!Services_Accessed %in% c("Respondents","Total","Don't Know/No Answer")) %>%
      pivot_wider(names_from = "Type", values_from = "Percentage") %>%
      mutate(mydiff = Sheltered - Unsheltered) %>% 
      mutate(Services_Accessed = fct_reorder(Services_Accessed, abs(mydiff)))
    
  })
  
  output$services_accessed <- renderPlot({
    
    df_services_accessed() %>%
      ggplot() +
      geom_segment(aes(x = Services_Accessed,
                       xend = Services_Accessed,
                       y = Sheltered,
                       yend = Unsheltered),
                   color = "darkgrey")+
      geom_point(aes(x = Services_Accessed, y = Sheltered),
                 size = 4,
                 color = "#219ebc") +
      geom_point(aes(x = Services_Accessed, y = Unsheltered), 
                 size = 4,
                 color = "#023047") +
      coord_flip() +
      theme(legend.position = "top") 
    
  })
  
}


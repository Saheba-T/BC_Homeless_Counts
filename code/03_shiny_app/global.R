# This file contains all the global objects used by the UI and Server

# Load all required libraries
library(tidyverse)
library(leaflet)
library(htmltools)
library(here)
library(shiny)
library(shinydashboard)
library(gridExtra)
library(grid)

#-------------------------------------------------------------------------------
# Load all csv files from clean_data folder into a list of data frames
my_files <- list.files(path = paste0(here(),"/data/clean_data"),
                       full.names = TRUE)

my_data <- lapply(my_files,read.csv)


#-------------------------------------------------------------------------------
# Functions

# Function to prepare data for visualization
prep_data <- function(df, subject_var, homeless_type){
  df %>%
    select(subject_var, contains("Percent") | contains("2018")) %>%
    pivot_longer(cols = -subject_var,
                 names_to = "Type",
                 values_to = "Percentage") %>%
    mutate_at(.vars = vars("Type"),
              .funs = list(~ str_to_title(gsub(".*_","",.)))
              ) %>%
    mutate(Colour  = case_when(Type == "Sheltered" ~ "#219ebc",
                               Type == "Unsheltered" ~ "#5F9EA0",
                               Type == "2018" ~ "black",
                               TRUE ~ "#4682B4")) %>%
    filter(Type == homeless_type) %>%
    filter(!.data[[subject_var]] %in% c("Respondents","Total","Don't Know/No Answer")) %>%
    mutate(subject_var = factor(subject_var, subject_var))
}


  
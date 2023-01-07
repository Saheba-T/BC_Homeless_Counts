# this file contains all the global objects used by the UI and Server

# load all required libraries
library(tidyverse)
library(leaflet)
library(htmltools)
library(here)
library(shiny)
library(shinydashboard)


# load all csv files from clean_data folder into a list of data frames
my_files <- list.files(path = paste0(here(),"/data/clean_data"),
                       full.names = TRUE)

my_data <- lapply(my_files,read.csv)



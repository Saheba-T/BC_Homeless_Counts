
# Author: Saheba Tegally



# load required libraries
library(tidyverse)
library(here)


#--------------
## Import raw data 

# names of data tables pertaining to all communities
names_ls1 <- paste0("table2.",1:23)
# names of data tables gathered across communities
names_ls2 <- paste0("table3.",1:9)
# combine the two lists of names into a single list
table_names <- append(names_ls1,names_ls2)

# read in from csv files and store raw data into data frames
for (i in table_names){
  filename <- paste0("data//raw_data//",i,".csv")
  assign(i,read_csv(filename))
}


#---------------
## Data Cleaning

# clean table2.2 - Gender
table2.2_c <- table2.2[-c(1,2),-2] %>%
  rename( "Gender_identity" = "X",
          "Total_sheltered" = "X.2",
          "Percent_sheltered" = "Sheltered", 
          "Total_unsheltered" = "X.3",
          "Percent_unsheltered" = "Unsheltered",
          "Total_respondents" = "X.4",
          "Percent_respondents" = "Total",
          "year_2018" = "X2018") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.))))

table2.2_c[,"year_2018"] <- as.numeric(gsub("%","",table2.2_c[,"year_2018"]))


# clean table2.4 - Age
table2.4_c <- table2.4[-c(1,2),] %>%
  rename("Age_groups" = "X",
         "Total_sheltered" = "X.1",
         "Percent_sheltered" = "Sheltered",
         "Total_unsheltered" = "X.2",
         "Percent_unsheltered" = "Unsheltered",
         "Total_respondents" = "X.3",
         "Percent_respondents" = "Total",
         "year_2018" = "X2018") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) 

table2.4_c[,"year_2018"] <- as.numeric(gsub("%","",table2.4_c[,"year_2018"]))


# clean table2.6 - Racial identity
table2.6_c <- table2.6[-c(1,8,11),] %>%
  rename( "Applicable_reason(s)" = "Racial.Identity..more.than",
          "Total_sheltered" = "X",
          "Percent_sheltered" = "Sheltered",
          "Total_unsheltered" = "X.1",
          "Percent_unsheltered" = "Unsheltered",
          "Total_respondents"= "X.2",
          "Percent_respondents"= "Total") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) 

table2.6_c[7,1] <- "Black - Caribbean and Latin America"
table2.6_c[9,1] <- "Black - Canadian/American"


# clean table2.10 - Reasons for Loss of Housing
table2.10_c <- table2.10[-c(1,7,14,17),] %>%
  rename( "Applicable_reason(s)" = "Loss.of.Housing...more.than.1",
          "Total_sheltered" = "X",
          "Percent_sheltered" = "Sheltered",
          "Total_unsheltered" = "X.1",
          "Percent_unsheltered" = "Unsheltered",
          "Total_respondents"= "X.2",
          "Percent_respondents"= "Total") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) 

# Modify values of some columns
table2.10_c[6,1] <- "Complaint (e.g. pets/noise/damage)"
table2.10_c[12,1] <- "Experienced abuse by parent/guardian"
table2.10_c[14,1] <- "Death or Departure of family member"
table2.10_c[12, c("Total_respondents", "Percent_respondents")] <- c(84,2)


# clean table 2.11 - Health Conditions
table2.11_c <- table2.11[-c(1,6),] %>%
  rename("Health_condition(s)" = "Health.Conditions..more",
         "Total_sheltered" = "X",
         "Percent_sheltered" = "Sheltered",
         "Total_unsheltered" = "X.1",
         "Percent_unsheltered" = "Unsheltered",
         "Total_respondents" = "X.2",
         "Percent_respondents" = "Total",
         "year_2018" = "X2018") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) 

table2.11_c[5,1] <- "Learning disability or cognitive impairment"
table2.11_c[,"year_2018"] <- as.numeric(gsub("%","",table2.11_c[,"year_2018"]))

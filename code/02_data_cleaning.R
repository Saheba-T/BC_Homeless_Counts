
# Author: Saheba Tegally



# load required libraries
library(tidyverse)
library(here)


#------------------
## Import raw data 

# get path of current working directory
prj_dir <- here()

# names of data tables pertaining to all communities
names_ls1 <- paste0("table2.",1:23)

# names of data tables gathered across communities
names_ls2 <- paste0("table3.",1:9)

# combine the two lists of names into a single list
table_names <- append(names_ls1,names_ls2)

# read in from csv files and store raw data into data frames
for (i in table_names){
  filename <- paste0(prj_dir,"/data/raw_data/",i,".csv")
  assign(i,read_csv(filename))
}


#---------------
## Data Cleaning

# clean table2.1 - Sheltered and Unsheltered distribution
table2.1_c <- table2.1[-c(1,2,3,12),c(1,7,8)] %>%
  rename("Sheltered_and_unsheltered" = "X",
         "Total_homeless" = "X.5",
         "Percent_homeless" = "X.6") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate(Homeless_type = "Sheltered")

# clean table2.22 - Unsheltered (where they stayed night of the count)
table2.22_c <- table2.22[-c(1,2),c(1,3,4)] %>%
  rename("Sheltered_and_unsheltered" = "X",
         "Total_homeless" = "X.2",
         "Percent_homeless" = "Unsheltered") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate(Homeless_type = "Unsheltered")
  
# join table2.1_c and table2.22_c by stacking the rows.
table1_c <- rbind(table2.1_c, table2.22_c) 
table1_c <- table1_c[-c(1,8,9,15,17,19,21),] 
table1_c[c(6,12,13),"Sheltered_and_unsheltered"] <- c("No Fixed Address","Parent/Guardian's place","Not listed")
  

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
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate_at(.vars = vars(contains("2018")),
            .funs = list(~ as.numeric(gsub("%","",.))))

table2.2_c[3,"Gender_identity"] <- "Other"


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
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate_at(.vars = vars(contains("2018")),
            .funs = list(~ as.numeric(gsub("%","",.)))) 

table2.4_c[1:3,"Age_groups"] <- c("Youth (Under 25)", "Adult (25-54)", "Senior (55+)")


# clean table2.6 - Racial identity
table2.6_c <- table2.6[-c(1,8,11),] %>%
  rename( "Racial_identity" = "Racial.Identity..more.than",
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
  rename( "Housing_loss_reason" = "Loss.of.Housing...more.than.1",
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
table2.10_c[c(6,12,14),1] <- c("Complaint (e.g. pets/noise/damage)",
                               "Experienced abuse by parent/guardian",
                               "Death or Departure of family member")

table2.10_c[12, c("Total_respondents", "Percent_respondents")] <- c(84,2)


# clean table 2.11 - Health Conditions
table2.11_c <- table2.11[-c(1,6),] %>%
  rename("Health_condition" = "Health.Conditions..more",
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
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate_at(.vars = vars(contains("2018")),
            .funs = list(~ as.numeric(gsub("%","",.)))) 

table2.11_c[5,1] <- "Learning disability or cognitive impairment"


# clean table 2.15 - Sources of income
table2.15_c <- table2.15[-c(1,12,14,16),] %>%
  rename("Sources_of_income" = "Sources.of.Income..more",
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
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate_at(.vars = vars(contains("2018")),
            .funs = list(~ as.numeric(gsub("%","",.)))) 

table2.15_c[11,1] <- "Disability benefit (e.g. PWD, PPMB)"
table2.15_c[11,"year_2018"] <- 29
table2.15_c[12,1] <- "Old Age Security (OAS)/Guaranteed Income Supplement"


#-------------------
## Save cleaned data frames as csv in clean_data folder
clean_table_names <- c("table1_c","table2.2_c","table2.4_c","table2.6_c",
                       "table2.10_c","table2.11_c", "table2.15_c")

for (t in clean_table_names){
  df <- get(t)
  filename <- paste0("data/clean_data/",t,".csv")
  if (!file.exists(filename)){
    write_csv(df, filename)
  }
  
}


# Author: Saheba Tegally



# load required libraries
library(tidyverse)
library(here)
library(mapquestr)


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

table2.10_c[12, "Total_respondents"] <- 84
table2.10_c[12,"Percent_respondents"] <- 2


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


# clean table 2.12 - Total # of Health concerns
table2.12_c <- table2.12[-c(1,8,9,10),] %>%
  rename("Number_of_conditions" = "Number.of.Health",
         "Total_sheltered" = "X",
         "Percent_sheltered" = "Sheltered",
         "Total_unsheltered" = "X.1",
         "Percent_unsheltered" = "Unsheltered",
         "Total_respondents" = "X.2",
         "Percent_respondents" = "Total") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate("Number_of_conditions" = 0:5)


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


# clean table 2.18 - Age when first homeless
table2.18_c <- table2.18[-c(1,2,6,7,8),] %>%
  rename("Age" = "X",
         "Total_sheltered" = "X.1",
         "Percent_sheltered" = "Sheltered",
         "Total_unsheltered" = "X.2",
         "Percent_unsheltered" = "Unsheltered",
         "Total_respondents" = "X.3",
         "Percent_respondents" = "Total",
         "Percent_2018" = "X2018") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate_at(.vars = vars("Age"),
            .funs = list(~ gsub("Years","",.)))


# clean table 2.17 - length of time without owning a place
table2.17_c <- table2.17[-c(1,3,9,10,11),] %>%
  rename("Time_period" = "Length.of.Time.Without",
         "Total_sheltered" = "X",
         "Percent_sheltered" = "Sheltered",
         "Total_unsheltered" = "X.1",
         "Percent_unsheltered" = "Unsheltered",
         "Total_respondents" = "X.2",
         "Percent_respondents" = "Total",
         "Percent_2018" = "X2018") %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%", "", .)))) %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) 

table2.17_c[2,"Time_period"] <- "Under 7 days"
table2.17_c[2,"Total_respondents"] <- 152
table2.17_c[2,"Percent_respondents"] <- 3
table2.17_c[2,"Percent_2018"] <- 3
  

# clean table 3.1 - sheltered and unsheltered homeless
table3.1_c <- table3.1[-c(1,2,28,29),-7] %>%
  rename("BC_community" = "X",
         "Total_Sheltered" = "X.1",
         "Percent_Sheltered" = "Sheltered",
         "Total_Unsheltered" = "X.2",
         "Percent_Unsheltered" = "Unsheltered",
         "Total_Respondents_2021" = "X.3",
         "Total_Respondents_2018" = "X.4",
         "Percent_change" = "X2018") %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%","",.)))) %>%
  mutate(address = paste(gsub("/.+","",BC_community),
                         "British Columbia",
                         "Canada", 
                         sep = ", ")) 

# data frame of the addresses 
addresses <- table3.1_c$address

# get coordinates of the different Canadian cities
geocodes <- geocode_mapquest(addresses = addresses, 
                             key = my_key) %>% 
  as.data.frame()

# join geocodes to table_3.1c 
table3.1_c <- left_join(table3.1_c, geocodes, by = "address")


# clean table3.9 - length of time in community
table3.9_c <- table3.9[-c(1:11,37,38),] %>%
  separate(X.2, 
           into = c("Total_1to5","Percent_1to5")) %>%
  separate(X.3, 
         into = c("Total_5to10","Percent_5to10")) %>%
  separate(X.4, 
           into = c("Total_10plus","Percent_10plus")) %>%
  separate(X.5, 
           into = c("Total_Always","Percent_Always")) %>%
  separate(X.6, 
           into = c("Total_Respondents","Percent_Respondents")) %>%
  rename("BC_community" = "X",
         "Total_Under1" = "X.1",
         "Percent_Under1" = "Under.1",
         "Do_Not_Know" = "X.7",
         "Total_Homeless_Identified" = "X.8") %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%","",.)))) %>%
  select(- Percent_Respondents)


# clean table2.16 - Services accessed in the last 12 months
table2.16_c <- table2.16[-c(1,15,16,17),] %>%
  rename("Services_Accessed" = "Services.Accessed..more.than.1",
         "Total_Sheltered" = "X",
         "Percent_Sheltered" = "Sheltered",
         "Total_Unsheltered" = "Unsheltered",
         "Percent_Unsheltered" = "X.1",
         "Total_Respondents" = "X.2",
         "Percent_Respondents" = "Total") %>%
  mutate_at(.vars = vars(starts_with("Total")),
            .funs = list(~ as.numeric(gsub(",","",.)))) %>%
  mutate_at(.vars = vars(starts_with("Percent")),
            .funs = list(~ as.numeric(gsub("%","",.))))


#-------------------
## Save cleaned data frames as csv in clean_data folder
clean_table_names <- c("table1_c","table2.2_c","table2.4_c","table2.6_c",
                       "table2.10_c","table2.11_c","table2.12_c","table2.15_c",
                       "table2.17_c","table2.18_c","table3.1_c","table3.9_c",
                       "table2.16_c")

for (t in clean_table_names){
  filename <- paste0("data/clean_data/",t,".csv")
  if (!file.exists(filename)){
    df <- get(t)
    write_csv(df, filename)
  }
  
}


# Author: Saheba Tegally



# load required libraries
library(remotes)
library(rJava)
library(tabulizer)
library(tidyverse)
library(here)


# get the current working directory 
prj_dir <- here()

# load pdf file
pdf_file <- paste0(prj_dir,"/data/2020-21-BC-Homeless-Counts.pdf")


#-----------------
## Data Extraction

# names of data tables pertaining to all communities
names_ls1 <- paste0("table2.",1:23)

# names of data tables gathered across communities
names_ls2 <- paste0("table3.",1:9)

# combine list names into a single list
table_names <- append(names_ls1,names_ls2)

# list of page numbers each table is on
page_numbers <- c(15,16,16,17,17,18,18,19,19,21,21,22,22,23,24,25,25,26,26,26,27,27,29,30,31,32,33,34,36,37)

# extract the tables from the pdf file 
for (i in 1:length(table_names)){
  assign(table_names[i],
         extract_areas(pdf_file, page = page_numbers[i], output = "data.frame") %>% as.data.frame())
}












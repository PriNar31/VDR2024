
ID <- "e4JUnnk/sx2"
sumPID_info(ID)


dir.create("data")
dir.create("data_output")
dir.create("fig_output")

# Install & load dplyr
install.packages("dplyr", dependencies = TRUE)                   
library("dplyr")

# Install & load ggplot2
install.packages("ggplot2", dependencies = TRUE)               
library("ggplot2")


# Step 1: Install and load required packages

install.packages("data.table")

if (!requireNamespace("data.table", quietly = TRUE)) {
  install.packages("data.table")
}

install.packages("tidyverse")

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(readr)


dir.create
setwd
getwd()


# Define the directory path where your files are located
directory_path <- "C:/Users/goupr74p/OneDrive - University of Otago/Desktop/VDR data anlaysis PNApril2024/VDR raw files"

# Set the working directory to the specified directory path
setwd(directory_path)

#Load all 17 datasets into R environment
# For example, if datasets are CSV files, you can use read_csv() function
VDR2005 <- read_csv("MOH-DataServices_merged_VDR05.csv")
VDR2006 <- read_csv("MOH-DataServices_merged_VDR06.csv")
VDR2007 <- read_csv("MOH-DataServices_merged_VDR07.csv")
VDR2008 <- read_csv("MOH-DataServices_merged_VDR08.csv")
VDR2009 <- read_csv("MOH-DataServices_merged_VDR09.csv")
VDR2010 <- read_csv("MOH-DataServices_merged_VDR10.csv")
VDR2011 <- read_csv("MOH-DataServices_merged_VDR11.csv")
VDR2012 <- read_csv("MOH-DataServices_merged_VDR12.csv")
VDR2013 <- read_csv("MOH-DataServices_merged_VDR13.csv")
VDR2014 <- read_csv("MOH-DataServices_merged_VDR14.csv")
VDR2015 <- read_csv("MOH-DataServices_merged_VDR15.csv")
VDR2016 <- read_csv("MOH-DataServices_merged_VDR16.csv")
VDR2017 <- read_csv("MOH-DataServices_merged_VDR17.csv")
VDR2018 <- read_csv("MOH-DataServices_merged_VDR18.csv")
VDR2019 <- read_csv("MOH-DataServices_merged_VDR19.csv")
VDR2020 <- read_csv("MOH-DataServices_merged_VDR20.csv")
VDR2021 <- read_csv("MOH-DataServices_merged_VDR21.csv")

column_names <- colnames(merged_dataset)
print(column_names)

# Assuming VDR2005, VDR2006, ..., VDR2021 are your datasets
# Combine all datasets into one large dataset
merged_dataset <- bind_rows(VDR2005, VDR2006, VDR2007, VDR2008, VDR2009, VDR2010, VDR2011, VDR2012, VDR2013, VDR2014, VDR2015, VDR2016, VDR2017, VDR2018, VDR2019, VDR2020, VDR2021)
# Now merged_dataset contains all the rows from VDR2005 to VDR2021

# Step 3: Understand data structure (optional)
# str(merged_dataset)
# summary(merged_dataset)
str(merged_dataset)
summary(merged_dataset)
head(merged_dataset)





## remove duplicates
distinct(merged_dataset)

##new name
VDRapril2024 <- distinct(merged_dataset)

nrow(distinct(VDRapril2024))
length(unique(VDRapril2024$MASTER_ENCRYPTED_HCU_ID))
nrow(distinct(VDRapril2024,MASTER_ENCRYPTED_HCU_ID))

patientID <- "ZjnvCu8JDZM"

patientDF <- VDRapril2024 %>% filter(MASTER_ENCRYPTED_HCU_ID == patientID)

patientDF

print(patientDF, n = 150)

patientgender <- max(patientDF$GENDER, na.rm = TRUE)

year_min <- min(patientDF$VDRyear, na.rm = TRUE)
year_max <- max(patientDF$VDRyear, na.rm = TRUE)
year_diff <- year_max-year_min

age_min <- min(patientDF$age, na.rm = TRUE)
age_max <- max(patientDF$age, na.rm = TRUE)
age_diff <- age_max-age_min


prioritiseID <- unique(patientDF$ethnic_name4, na.rm = TRUE)
ethniccodes <- unique(patientDF$Ethnic_Group, na.rm = TRUE)
ethnic_4 <- 4 %in% c(ethniccodes)
ethnic_36 <- 36 %in% c(ethniccodes)
ethnic_43 <- 43 %in% c(ethniccodes)
ethnic_3 <- 3 %in% c(ethniccodes)

# Assuming your dataframe is called patientDF and the column containing age is named "age"

# Define breaks for age groups (e.g., 0-9, 10-19, 20-29, ...)
breaks <- seq(0, min(patientDF$age) + 10, by = 10) - 1

# Create age groups using cut()
patientDF$age_group <- cut(patientDF$age, breaks = breaks, labels = paste0(breaks[-length(breaks)] + 1, "-", breaks[-1]))

agegroup <- max((as.character(patientDF$age_group)), na.rm = TRUE)

# View the first few rows to check if the age groups are assigned correctly
head(patientDF)

finalDF <- data.frame (patientID = patientID, 
                      patientgender = patientgender,
                      year_min = year_min,
                      year_max = year_max,
                      year_diff = year_diff,
                      age_min = age_min,
                      age_max = age_max,
                      age_diff = age_diff,
                      agegroup = agegroup,
                      ethnic_3 = ethnic_3,
                      ethnic_4 = ethnic_4,
                      ethnic_36 = ethnic_36,
                      ethnic_43 = ethnic_43)



sumPID_info <- function(patientID){

  patientDF <- VDRapril2024 %>% filter(MASTER_ENCRYPTED_HCU_ID == patientID)
  
  patientDF
  
  patientgender <- max(patientDF$GENDER, na.rm = TRUE)
  
  year_min <- min(patientDF$VDRyear, na.rm = TRUE)
  year_max <- max(patientDF$VDRyear, na.rm = TRUE)
  year_diff <- year_max-year_min
  
  age_min <- min(patientDF$age, na.rm = TRUE)
  age_max <- max(patientDF$age, na.rm = TRUE)
  age_diff <- age_max-age_min
  
  
  breaks <- seq(0, min(patientDF$age) + 10, by = 10) - 1
  patientDF$age_group <- cut(patientDF$age, breaks = breaks, labels = paste0(breaks[-length(breaks)] + 1, "-", breaks[-1]))
  agegroup <- max((as.character(patientDF$age_group)), na.rm = TRUE)
  
prioritiseID <- unique(patientDF$ethnic_name4, na.rm = TRUE)
  ethniccodes <- unique(patientDF$Ethnic_Group, na.rm = TRUE)
  ethnic_4 <- 4 %in% c(ethniccodes)
  ethnic_36 <- 36 %in% c(ethniccodes)
  ethnic_43 <- 43 %in% c(ethniccodes)
  ethnic_3 <- 3 %in% c(ethniccodes)
  
  
  finalDF <- data.frame (patientID = patientID, 
                         patientgender = patientgender,
                         year_min = year_min,
                         year_max = year_max,
                         year_diff = year_diff,
                         age_min = age_min,
                         age_max = age_max,
                         age_diff = age_diff,
                         agegroup = agegroup,
                         ethnic_3 = ethnic_3,
                         ethnic_4 = ethnic_4,
                         ethnic_36 = ethnic_36,
                         ethnic_43 = ethnic_43)
  
  return(finalDF)
}

# loop the entire PERSON_ID
avail_ID <- unique(VDRapril2024$MASTER_ENCRYPTED_HCU_ID)

DF <- vector()
for(i in 1:length(avail_ID)){
  ID <- avail_ID[i]
  print(i)
  intDF <- sumPID_info(ID)
  DF <- rbind(DF, intDF)
}

fwrite(DF, "VDRapril2024.csv")
warnings()

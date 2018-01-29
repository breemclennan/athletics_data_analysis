# R Script: 00_DataLoad.R
# Description: Import CSV files and convert to one large CSV, and a feather file for future usage
# Author: Bree McLennan
# Date: 03/01/2018
#
# ======================================================================================================================== #

# Setup

# Load library
library(data.table)
library(feather)

# Get a List of all files named with a key word, use regex pattern to identify only the athletics graded results csv files
filenames <- list.files("D:/Data Science/Athletics Data/Project Files/athletics_data_analysis/Data/Raw", pattern = "*Graded_Results.csv", full.names = TRUE)

# Load and bind all data sets
raw.data <- rbindlist(lapply(filenames,fread))

# Load data sets. Bind all data files into a list of data frames
list.DFs <- lapply(filenames,fread)

write.csv(raw.data, "D:/Data Science/Athletics Data/Project Files/athletics_data_analysis/Data/Raw/raw.AllInterclubResults.csv", row.names = FALSE)  

# Save Feather file from csv
write_feather(raw.data, "D:/Data Science/Athletics Data/Project Files/athletics_data_analysis/Data/Raw/raw.AllInterclubResults.feather")

# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #

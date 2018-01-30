# R Script: 00_DataLoad.R
# Description: Import CSV files and convert to one large CSV, and a feather file for future usage
# Author: Bree McLennan
# Date: 03/01/2018
#
# ======================================================================================================================== #

# Setup

# Load library
library(rprojroot)
library(data.table)
library(feather)

# Define a function that computes file paths relative to where root .git folder is located
F <- is_git_root$make_fix_file() 
# Example usage: F("Data/Raw") 


# Get a List of all files named with a key word, use regex pattern to identify only the athletics graded results csv files
filenames <- list.files(F("Data/Raw") , pattern = "*Graded_Results.csv", full.names = TRUE)

# Load and bind all data sets
raw.data <- rbindlist(lapply(filenames,fread))

# Load data sets. Bind all data files into a list of data frames
list.DFs <- lapply(filenames,fread)

write.csv(raw.data, F("Data/Raw/raw.AllInterclubResults.csv") , row.names = FALSE) 

# Save Feather file from csv
write_feather(raw.data, F("Data/Raw/raw.AllInterclubResults.feather"))

# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #

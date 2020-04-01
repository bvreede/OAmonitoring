## load packages and functions
library(tidyverse)
library(stringr)
library(readxl)
library(jsonlite)
library(httr)
library(magrittr)
library(lubridate)
library(docstring)
library(testthat)

# source scripts with functions and paths
source("config/config.R")
source("R/clean_data.R")
source("R/classification.R")
source("R/reporting.R")

# Generate output folders
dir.create("data/clean")
dir.create("output/")

# STEP ONE: OPEN, CLEAN, AND COMBINE THE DATASETS
allfiles <- read_excel(path_allfiles)
df <- open_everything(allfiles)

# STEP TWO: APPLY CLASSIFICATION
# NB: all data collected here is automatically saved in data/clean

# get data from VSNU, DOAJ, UPW
vsnudf <- get_vsnu(path_vsnu)
doajdf <- doaj_pipeline(df)
upwdf <- upw_pipeline(df)

# perform the classification
df <- classify_oa(df)

# STEP THREE: REPORT
# generate a full report
full_report(df)
hoop_report(df)

# generate individual reports for manually made categories
individual_reports(path_report)

# STEP FOUR: REPORT NECESSARY MANUAL CHECKS
check_all(df)
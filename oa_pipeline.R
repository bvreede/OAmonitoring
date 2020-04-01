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

# generate individual reports for manually made categories
reporting <- open_reporting_file(path_report)
individual_reports(reporting)

# STEP FOUR: REPORT MANUAL CHECKS




checkthese <- NULL
checkthese <- infocheck(df,checkthese)

for(cat in levels(as.factor(df$org_unit))){
  df_temp <- df %>% filter(org_unit==cat)
  checkthese <- infocheck(df_temp,checkthese)
}

checkthese %>% deduplicate() %>% write_csv("output/checkthese.csv")


full_report(df) %>% write_csv("output/full_report.csv")

# custom
df_custom <- classify_oa_custom(df)
full_report(df_custom) %>% write_csv("output/full_report_custom.csv")









## Add manual checks
pure_manual <- mutate(pure_manual,manual=substring(handmatig,1,1)) %>%
  select(manual,pure_id) %>%
  filter(manual=="A"|manual=="B"|manual=="C"|manual=="D")


# Make a field that indicates whether information is available.
# Information is available when there is a DOI, or when there is a confirmed VSNU ISSN.
# Or when OA_STATUS is green, this is likely from Pure info; also counts as available info
all_pubs <- mutate(all_pubs,
                   information = OA_label!="CLOSED"|is.na(electronic_version)|!is.na(doi_resolver))

## load packages and functions
library(tidyverse)
library(stringr)
library(readxl)
library(jsonlite)
library(httr)
library(magrittr)
library(lubridate)

library(dplyr)
library(docstring)
library(testthat)


# source scripts with functions and paths
source("config/config.R")
source("R/clean_data.R")

# Generate output folders
dir.create("data/clean")



allfiles <- read_excel("config/config_pub_files.xlsx")


# STEP ONE: CLEAN THE DATASETS AND COMBINE THEM

alldata <- list()

for(col in allfiles){
  # extract file name and extension
  fn <- col[allfiles$File_info=="Filename"]
  fn_ext <- str_split(fn,"\\.")[[1]]
  
  # test if the column contains NAs; in this case the file will not be read
  if(sum(is.na(col))>0){
    warning("The information for file ", fn, " is not filled out. This file cannot be processed.\n")
    next
  } 
  # skip filenames without extensions
  # NB this automatically skips the first column with row names
  if(length(fn_ext) < 2){
    if(!fn == allfiles[1,1]){ # this behavior is acceptable with the header.
      warning("The filename ", fn, " does not have an extension. This file cannot be processed.\n")
    }
    next
  }

  # open the file, clean columns, and save to the alldata list
  alldata[[fn]] <- open_clean(fn)
}

# remove excess variables, bind to dataframe
df <- bind_rows(alldata)
rm(allfiles,alldata,fn, fn_ext, col)



# STEP TWO: APPLY CLASSIFICATION
# NB: all data collected here is automatically saved in data/clean

source("R/classification.R")
# get data from VSNU, DOAJ, UPW
vsnudf <- get_vsnu(path_vsnu)
taverne <- read_csv(path_taverne)
doajdf <- doaj_pipeline(df)
upwdf <- upw_pipeline(df)

# perform the classification
df <- classify_oa(df)



# STEP THREE: REPORT



checkthese <- NULL
checkthese <- infocheck(df,checkthese)

for(cat in levels(as.factor(df$org_unit))){
  df_temp <- df %>% filter(org_unit==cat)
  checkthese <- infocheck(df_temp,checkthese)
}

checkthese %>% deduplicate() %>% write_csv("output/checkthese.csv")


full_report(df) %>% write_csv("output/full_report.csv")


oacols <- c("gray88","gold1","chartreuse3","orange3")

p <- ggplot(df_r, aes(x = org_unit, fill=OA_label))

p + geom_bar(stat="prop")







## Add manual checks
pure_manual <- mutate(pure_manual,manual=substring(handmatig,1,1)) %>%
  select(manual,pure_id) %>%
  filter(manual=="A"|manual=="B"|manual=="C"|manual=="D")


# Make a field that indicates whether information is available.
# Information is available when there is a DOI, or when there is a confirmed VSNU ISSN.
# Or when OA_STATUS is green, this is likely from Pure info; also counts as available info
all_pubs <- mutate(all_pubs,
                   information = OA_label!="CLOSED"|is.na(electronic_version)|!is.na(doi_resolver))

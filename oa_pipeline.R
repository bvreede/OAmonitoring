## load packages and functions
library(tidyverse)
library(stringr)
library(readxl)
library(jsonlite)
library(httr)
library(magrittr)
library(here)
library(lubridate)

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
    if(!fn == allfiles[1,1]){ # this is acceptable with the header.
      warning("The filename ", fn, " does not have an extension. This file cannot be processed.\n")
    }
    next
  }
  
  df <- open_clean(fn)
  
  # save to the alldata list
  alldata[[fn]] <- df
}

# remove excess variables, bind to dataframe
rm(df, fn, fn_ext, col)
df <- bind_rows(alldata)
rm(alldata)


# STEP TWO: APPLY CLASSIFICATION

source("R/classification.R")
# get data from VSNU, DOAJ, UPW
vsnu_doi_cleaned <- get_vsnu(path_vsnu)
doajdf <- doaj_pipeline(df)
upwdf <- upw_pipeline(df)

# perform the classification
df <- classify_oa(df)

# 





## Add manual checks
pure_manual <- mutate(pure_manual,manual=substring(handmatig,1,1)) %>%
  select(manual,pure_id) %>%
  filter(manual=="A"|manual=="B"|manual=="C"|manual=="D")








## Step 4: Unpaywall
api_csv <- "csv" #indicate here whether you want to load existing data or use the UPW api

# generate a database with unpaywall data using their REST API
# use all DOIs as input
alldois <- union(pure_uu$doi[!is.na(pure_uu$doi)], # all pure_uu dois without NA
                 pure_umcu$doi[!is.na(pure_umcu$doi)]) # all UMCU dois without NA

# mine unpaywall API for each DOI
outlist <- list()


# ensure unpaywall data is saved as factor
unpaywall$evidence %<>% as.factor
unpaywall$free_fulltext_url %<>% as.factor
unpaywall$license %<>% as.factor
unpaywall$oa_color %<>% as.factor

# merge pure with unpaywall
## CONSIDER JOIN IN UPDATE ##
uu_merge <- left_join(pure_uu,unpaywall,by="doi")
umcu_merge <- left_join(pure_umcu,unpaywall,by="doi")








# Make a field that indicates whether information is available.
# Information is available when there is a DOI, or when there is a confirmed VSNU ISSN.
# Or when OA_STATUS is green, this is likely from Pure info; also counts as available info
all_pubs <- mutate(all_pubs,
                   information = OA_label!="CLOSED"|is.na(electronic_version)|!is.na(doi_resolver))

# Label duplicates should be done per unique collection. 
# If possible it should use DOIs, but it might be that titles cover more ground.
# For this purpose:
# make a title field that does not have punctuation and uses lowercase only
all_pubs <- mutate(all_pubs,
                   title_lower = str_to_lower(str_replace_all(title.x, "[[:punct:]]", "")))





## load libraries
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)
library(magrittr)
library(ggplot2)

## source file paths

path_pub <- "data/2018_Monitoring_OA___basislijst_2-aangepast_20190311.xls" 
path_vsnu <- "data/VSNU-DOIs.csv"
path_doaj <- "data/2018-12-31-DOAJ-schoon.xlsx"
path_unpaywall <- "data/unpaywall_2019-03-05.csv"

#### LOAD AND CLEAN DATA ####

## Pub data
pub_data <- read_excel(path_pub)

## Classification data
doaj <- read_excel(path_doaj)
vsnu <- read_csv(path_vsnu)

## Renaming columns so they will not have to be adjusted every time we run the script
colnames(pub_data)[colnames(pub_data) == 'Contributors > Organisations > Organisational unit-0'] <- "org_unit"
colnames(pub_data)[colnames(pub_data) == 'ID-1'] <- "pure_id"
colnames(pub_data)[colnames(pub_data) == 'Title of the contribution in original language-2'] <- "title"
colnames(pub_data)[colnames(pub_data) == 'Journal > ISSN-5'] <- "issn"
colnames(pub_data)[colnames(pub_data) == 'Electronic version(s) of this work > DOI (Digital Object Identifier)-6'] <- "doi"
colnames(pub_data)[colnames(pub_data) == 'Electronic version(s) of this work > Document version-8'] <- "electronic_version"
colnames(pub_data)[colnames(pub_data) == 'Electronic version(s) of this work > Public access to file-9'] <- "public_access"
colnames(pub_data)[colnames(pub_data) == 'Open Access status-7'] <- "OA_status_pure"
colnames(pub_data)[colnames(pub_data) == 'Open Access embargo date-10'] <- "embargo_date"

colnames(doaj)[colnames(doaj) == 'Journal ISSN (print version)'] <- "issn"
colnames(doaj)[colnames(doaj) == 'Journal EISSN (online version)'] <- "eissn"

## Adjust data types

# Set ID as character, so that it will not be treated as a numeral
pub_data$pure_id %<>% as.character

# Set organisational unit, ISSN and OA status as factors because they are fixed variables which we want to analyze.
pub_data$org_unit %<>% as.factor
pub_data$issn %<>% as.factor
pub_data$OA_status_pure %<>% as.factor

## Verify data (uu only)
department_check(pub_data$org_unit)

## Clean data
# clean DOI and ISSN, remove spaces and hyperlinks, change uppercase to lowercase etc.
pub_data$issn <- clean_issn(pub_data$issn)
pub_data$doi <- clean_doi(pub_data$doi)

doaj$issn <- clean_issn(doaj$issn) 
doaj$eissn <- clean_issn(doaj$eissn)

vsnu$DOI <- clean_doi(vsnu$DOI)

#### OA LABELLING ####
## Collect information that can later be used for the classification pipeline.

## Step 1: DOAJ ISSN matching
doaj_issn <- union(doaj$issn[!is.na(doaj$issn)], # all DOAJ ISSN numbers from print, without NAs
                   doaj$eissn[!is.na(doaj$eissn)]) # all DOAJ E-ISSN numbers, without NAs

pub_data$DOAJ_ISSN_match <- pub_data$issn%in%doaj_issn

## Step 2: VSNU DOI matching
vsnu_doi <- vsnu$DOI[!is.na(vsnu$DOI)]

pub_data$VSNU_doi_match <- pub_data$doi%in%vsnu_doi

## Step 3: PURE classification
pub_data <- mutate(pub_data,
                  pure_green = str_detect(electronic_version,"Accepted author manuscript")|
                    !is.na(embargo_date))

## Step 4: Unpaywall
api_csv <- "csv" #indicate here whether you want to load existing data or use the UPW api

# generate a database with unpaywall data using their REST API
# use all DOIs as input
alldois <- pub_data$doi[!is.na(pub_data$doi)] # all pub_data dois without NA

# mine unpaywall API for each DOI
outlist <- list()

for(i in seq_along(alldois)){
  doi <- alldois[i]
  if(api_csv=="api"){ # only mine the api if the user wants to renew unpaywall data
    res <- upw_api(doi)
    outlist[[i]] <- res
  }
}

if(api_csv=="api"){
  unpaywall <- bind_rows(outlist)
  # save unpaywall data
  today <- as.character(Sys.Date())
  upwname <- paste0("data/unpaywall_",today,".csv")
  write_csv(unpaywall,upwname)
} else if(api_csv=="csv"){
  unpaywall <- read_csv(path_unpaywall)
}

# ensure unpaywall data is saved as factor
unpaywall$evidence %<>% as.factor
unpaywall$free_fulltext_url %<>% as.factor
unpaywall$license %<>% as.factor
unpaywall$oa_color %<>% as.factor

# merge pure with unpaywall
## CONSIDER JOIN IN UPDATE ##
uu_merge <- left_join(pub_data,unpaywall,by="doi")
umcu_merge <- left_join(pure_umcu,unpaywall,by="doi")




#### CLASSIFICATION PIPELINE ####
## apply the classification function
uu_merge <- mutate(uu_merge,
                   OA_label=mapply(define_oa,
                                   DOAJ_ISSN_match,
                                   VSNU_doi_match,
                                   oa_color,
                                   OA_status_pure,
                                   pure_green,
                                   manual))
umcu_merge <- mutate(umcu_merge,
                     OA_label=mapply(define_oa,
                                     DOAJ_ISSN_match,
                                     VSNU_doi_match,
                                     oa_color,
                                     OA_status_pure,
                                     pure_green))

uu_merge <- mutate(uu_merge,
                   OA_label_detail=mapply(define_oa_detailed,
                                          DOAJ_ISSN_match,
                                          VSNU_doi_match,
                                          oa_color,
                                          OA_status_pure,
                                          pure_green,
                                          manual))
umcu_merge <- mutate(umcu_merge,
                     OA_label_detail=mapply(define_oa_detailed,
                                            DOAJ_ISSN_match,
                                            VSNU_doi_match,
                                            oa_color,
                                            OA_status_pure,
                                            pure_green))

## turn the results into factors
uu_merge$OA_label %<>% as.factor
umcu_merge$OA_label %<>% as.factor
uu_merge$OA_label_detail %<>% as.factor
umcu_merge$OA_label_detail %<>% as.factor

## load libraries
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)
library(magrittr)
library(ggplot2)

## Functions

# cleaning DOIs and ISSN columns
clean_issn <- function(column){
  column <- str_replace(column,'\\s+','') #remove spaces from ISSN
  column <- str_replace(column,'-','') #remove - from ISSN
  return(column)
}

clean_doi <- function(column){
  column <- str_replace(column,'https://','') #remove https:// from DOI
  column <- str_replace(column,'doi.org/','') #remove doi.org/ from DOI
  column <- str_replace(column,'\\s+','') #remove spaces from DOI
  column <- tolower(column) #Change DOI to lowercase only
  column <- str_replace(column,",.+","") #remove duplicate DOIs separated with a comma
}

# collecting DOI results from Unpaywall using their REST API
upw_api <- function(doi){
  # compile query to send to unpaywall
  api <- "api.unpaywall.org/"
  email <- paste("?email=",email_address,sep="") 
  query <- paste0(api,doi,email)
  result <- GET(query)
  # resolve query results and transform to a line that can be added to a df
  result_txt <- content(result, as="text",encoding="UTF-8")
  result_line <- fromJSON(result_txt,flatten=T)$results
  return(result_line)
}

# The following functions try to classify all publications according to their presence in check lists. In sequence:
## 1. match the journal ISSN with a list from the Directory of Open Access Journals (DOAJ). 
##    If the journal matches, the publication is Gold OA
## 2. match the DOI with a list obtained from VSNU. 
##    If the journal matches, the publication is Hybrid OA
## 3. obtain the OA status from Unpaywall. 
##    If the status is 'gold' or 'bronze', the publication is Hybrid OA
##    If the status is 'green', the publication is Green OA
# NB in the classification pipeline these labels will be applied in sequence
# Thus, e.g. if ISSN matches DOAJ but Unpaywall says 'green', the label will still be Gold OA.

define_oa <- function(doaj,vsnu,upw=NA){
  # DOAJ and VSNU: 
  if(doaj){ # DOAJ means gold
    return("GOLD")
  }
  if(vsnu){ # VSNU deal list means hybrid
    return("HYBRID")
  }
  # UNPAYWALL: resolve to hybrid or green, build in error for different inputs
  if(!is.na(upw)){
    if(upw=="bronze"|upw=="gold"){ # indeed, we choose to label gold only confirmed DOAJ ISSN
      return("HYBRID")
    } else if(upw=="green"){
      return("GREEN")
    } else{ #we only get to this point if there are other labels in unpaywall than bronze/gold/green
      return("UNDERTERMINED - CHECK define_oa FUNCTION")
    }
  }
  return("CLOSED")
}

define_oa_detailed <- function(doaj,vsnu,upw=NA){
  ## DOAJ and VSNU: DOAJ means gold, VSNU deal list means hybrid
  if(doaj){
    return("GOLD")
  }
  if(vsnu){
    return("HYBRID VSNU")
  }
  ## UNPAYWALL: resolve to hybrid or green
  if(!is.na(upw)){
    if(upw=="bronze"){
      return("HYBRID UPW BRONZE") 
    }
    if(upw=="gold"){
      return("HYBRID UPW GOLD")
    }
    if(upw=="green"){
      return("GREEN UPW")
    }
  }
  return("CLOSED")
}


#### LOAD AND CLEAN DATA ####

## Classification data
doaj <- read_excel(path_doaj)
vsnu <- read_csv(path_vsnu)

## Renaming columns so they will not have to be adjusted every time we run the script - should be in config file!
colnames(pub_data)[colnames(pub_data) == id_column] <- "system_id"
colnames(pub_data)[colnames(pub_data) == title_column] <- "title"
colnames(pub_data)[colnames(pub_data) == issn_column] <- "issn"
colnames(pub_data)[colnames(pub_data) == doi_column] <- "doi"

colnames(doaj)[colnames(doaj) == issn_column_doaj] <- "issn"
colnames(doaj)[colnames(doaj) == eissn_column_doaj] <- "eissn"

## Adjust data types

# Set ID as character, so that it will not be treated as a numeral
pub_data$system_id %<>% as.character

# Set ISSN and OA status as factors because they are fixed variables which we want to analyze.
pub_data$issn %<>% as.factor

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

## Step 3: Unpaywall
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
  upwname <- paste0("output/unpaywall_",today,".csv")
  write_csv(unpaywall,upwname)
} else if(api_csv=="csv"){
  unpaywall <- read_csv(path_unpaywall)
}

# ensure unpaywall data is saved as factor
unpaywall$evidence %<>% as.factor
unpaywall$free_fulltext_url %<>% as.factor
unpaywall$license %<>% as.factor
unpaywall$oa_color %<>% as.factor

# merge publication data with unpaywall
## CONSIDER JOIN IN UPDATE ##
pub_data_merge <- left_join(pub_data,unpaywall,by="doi")

#### CLASSIFICATION PIPELINE ####
## apply the classification function
pub_data_merge <- mutate(pub_data_merge,
                   OA_label=mapply(define_oa,
                                   DOAJ_ISSN_match,
                                   VSNU_doi_match,
                                   oa_color))

pub_data_merge <- mutate(pub_data_merge,
                   OA_label_detail=mapply(define_oa_detailed,
                                          DOAJ_ISSN_match,
                                          VSNU_doi_match,
                                          oa_color))

## turn the results into factors
pub_data_merge$OA_label %<>% as.factor
pub_data_merge$OA_label_detail %<>% as.factor

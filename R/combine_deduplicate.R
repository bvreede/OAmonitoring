#### Step two of the Open Access monitoring pipeline
#### Cleaned and indexed files are read and interpreted
#### with this script to write the reports.
#### Input required from the user: a config
#### file with the organisation units that need
#### to be reported on.
####
#### Written by: Barbara Vreede
#### Contact: b.vreede@gmail.com


## load libraries
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)
library(magrittr)
library(ggplot2)


## read data
# read all csv files in the output folder
infiles = list.files(path = "./output", pattern="*_clean.csv")
infiles = paste0("output/",infiles)
myfiles = lapply(infiles, read_csv)
# add source filename to myfiles
for(i in 1:length(infiles)){
  myfiles[[i]] <- myfiles[[i]] %>% mutate(source = infiles[i])
}
# bind all together to one large dataframe
df <- bind_rows(myfiles)

########################################################################
##### NB!! Note that pure_id is system_id in the new input files! ######
########################################################################

report <- function(nrecords_post,nrecords,method){
  cat("records generated:")
  cat(nrecords_post)
  cat("\nfrom:")
  cat(nrecords)
  cat("\nrecords deleted:")
  cat(nrecords-nrecords_post)
  cat("\nduplication method:")
  cat(method)
  cat("\n")
}


deduplicate_doi <- function(df){
  #' @title Deduplicate publication entries based on doi
  #' 
  #' Deduplication function that takes a dataframe with dois
  #' and returns the deduplicated dataframe.
  #' 
  ### tests ###
  # input is a dataframe
  expect_that(df, is_a("data.frame"))
  # column doi exists
  expect_true("doi"%in%names(df))
  # column doi does not contain NA
  expect_that(df$doi )
}



deduplicate <- function(df){
  #' @title Deduplication of publication entries.
  #' 
  #' Deduplication function that uses a single dataframe and applies a variety
  #' of deduplication functions to subsets of the dataframe.
  #' Required to use before reporting on a subset of publications, so that no bias is created
  #' in the results if a publication is entered multiple times by different groups, eg.
  #' @param df The dataframe that needs to be deduplicated.
  #' @return The deduplicated dataframe.
  #' @export
  # first determine whether there is a mix of multiple source files
  sourcefiles <- df$source %>% as.factor() %>% levels()
  nrecords <- nrow(df)
  if(length(sourcefiles) < 2){ # in this case, a single source file has been used, so deduplication can be performed on system ID
    df <- distinct(df,system_id,.keep_all = T)
    report(nrecords_post,nrecords,"System ID")
  }
  else{
    df <- df %>%
      mutate(title_adj = str_to_lower(title),
           title_adj = str_replace_all(title_adj,"[:punct:]+", ""),
           title_adj = str_replace_all(title_adj,"[:blank:]+", ""))
    # separate between entries with and without doi
    # deduplicate entries in the doi dataframe first
    df_nondoi <- df %>% filter(is.na(doi))
    df_doi <- df %>%
      filter(!is.na(doi))
    nrecords <- nrow(df_doi)
    df_doi <- df_doi %>% distinct(doi,.keep_all = T)
    nrecords_post <- nrow(df_doi)
    report(nrecords_post,nrecords,"DOI")
    # deduplicate by system ID
    
    df_nondoi[duplicated(df_nondoi$title_adj),] %>% View()
    nrecords <- nrow(df_nondoi)
    df_nondoi <- df_nondoi %>%
      distinct(title_adj,.keep_all = T)
    nrecords_post <- nrow(df_nondoi)
    report(nrecords_post,nrecords,"title")
    # combine and then duplicate by title
    df <- full_join(df_doi,df_nondoi)
    #nrecords <- nrow(df)
    #df <- df %>%
    #  distinct(title_adj,.keep_all = T)
    #nrecords_post <- nrow(df)
    #report(nrecords_post,nrecords,"both")
  }
  return(df)
}


testdf <- deduplicate(df)

testdf[duplicated(testdf$title_adj),] %>% View()

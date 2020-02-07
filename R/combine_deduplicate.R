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
library(docstring)
library(testthat)


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


report <- function(nrecords_post,nrecords,method){
  cat("Deduplicating by ")
  cat(method)
  cat(" from ")
  cat(nrecords)
  cat(" records, ")
  cat(nrecords-nrecords_post)
  cat(" were deleted.")
  cat(" This df now contains ")
  cat(nrecords_post)
  cat(" records.")
  cat("\n\n")
}


deduplicate_method <- function(df,method){
  #' @title Deduplicate publication entries
  #' 
  #' Deduplication function that takes a dataframe and returns
  #' the deduplicated dataframe based on either the combination of source file and
  #' their internal ID in that source, or DOI.
  #' Reports on the deduplication performed.
  #' @param df The dataframe that needs to be deduplicate
  #' @param method The deduplication method, can be either "doi" or "internal"
  #' @return The deduplicated dataframe
  ### tests ###
  # input is a dataframe
  expect_that(df, is_a("data.frame"))
  # column doi exists
  # expect_true("doi"%in%names(df))
  # column doi does not contain NA
  # expect_that(df$doi )
  ###############
  nrecords <- nrow(df)
  if(method == "internal"){
    df <- distinct(df,system_id,source,.keep_all = T)
    method_print = "internal ID"
  } else if(method == "doi"){
    df <- distinct(df,doi,.keep_all = T)
    method_print = "DOI" 
  }
  nrecords_post <- nrow(df)
  report(nrecords_post,nrecords,method_print)
  return(df)
}


deduplicate <- function(df){
  #' @title Deduplication of publication entries
  #' 
  #' Deduplication function that uses a single dataframe and applies a variety
  #' of deduplication functions to subsets of the dataframe.
  #' Required to use before reporting on a subset of publications, so that no bias is created
  #' in the results if a publication is entered multiple times by different groups, eg.
  #' @param df The dataframe that needs to be deduplicated.
  #' @return The deduplicated dataframe.
  #################################
  # first determine whether there is a mix of multiple source files
  sourcefiles <- df$source %>% as.factor() %>% levels()
  # if a single source file has been used, deduplication can be performed on system ID
  if(length(sourcefiles) < 2){ 
    df <- deduplicate_method(df,method="internal")
  }
  else{
    # separate between entries with and without doi
    df_nondoi <- df %>% filter(is.na(doi))
    df_doi <- df %>% filter(!is.na(doi))
    # deduplicate both individually
    df_doi <- deduplicate_method(df_doi,method="doi")
    df_nondoi <- deduplicate_method(df_nondoi,method="internal")
    # combine
    df <- full_join(df_doi,df_nondoi)
  }
  return(df)
}

df <- deduplicate(df)
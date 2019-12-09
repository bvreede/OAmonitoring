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
  cat("\nduplication method:")
  cat(method)
  cat("\n")
}

deduplicate <- function(df){
  #' Deduplication of publication entries.
  #' 
  #' Deduplication function that uses a single dataframe and deduplicates
  #' based on DOI and then on title.
  #' Required to use before reporting on a subset of publications, so that no bias is created
  #' in the results if a publication is entered multiple times by different groups, eg.
  # first determine whether there is a mix of multiple source files
  sourcefiles <- df$source %>% as.factor() %>% levels()
  nrecords <- nrow(df)
  if(length(sourcefiles) < 2){ # in this case, a single source file has been used, so deduplication can be performed on system ID
    df <- distinct(df,system_id,.keep_all = T)
    report(nrecords_post,nrecords,"System ID")
  }
  else{
    # separate between entries with and without doi
    # deduplicate entries in the doi dataframe first
    df_nondoi <- df %>% filter(is.na(doi))
    df_doi <- df %>%
      filter(!is.na(doi))
    nrecords <- nrow(df_doi)
    df_doi <- df_doi %>% distinct(doi,.keep_all = T)
    nrecords_post <- 34
    report(nrecords_post,nrecords,"DOI")
    # combine with nondoi and deduplicate by title
    df <- full_join(df_doi,df_nondoi) %>%
      mutate(title_adj = str_to_lower(title),
             title_adj = str_replace_all(title_adj,"[:punct:]+", ""),
             title_adj = str_replace_all(title_adj,"[:blank:]+", "")) %>%
      distinct(title_adj,.keep_all = T)
  }
  return(df)
}

testdf <- deduplicate(df)

#### Step two of the Open Access monitoring pipeline
#### Cleaned and indexed files are read and interpreted
#### with this script to write the reports.
#### Input required from the user: a config
#### file with the organisation units that need
#### to be reported on.



#'@title Report on deduplication effort
report <- function(nrecords_post,nrecords,method){
  message <- paste("Deduplicating by",
                   method,
                   "from",
                   nrecords,
                   "records,",
                   nrecords-nrecords_post,
                   "were deleted.\n",
                   "This df now contains",
                   nrecords_post,
                   "records.\n\n")
  cat(message)
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
    # ensure there are only atomic columns in the dataset
    df <- df %>% select_if(is.atomic)
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


infocheck <- function(df,checkthese){
  df <- deduplicate(df)
  f_mis <- sum(df$OA_label_explainer=="NONE")/nrow(df)
  if(f_mis>cutoff_missing){
    checkthese <- rbind(checkthese,filter(df,OA_label_explainer=="NONE"))
  }
  return(checkthese)
}






full_report <- function(df){
  ## Write a general report for the entire dataset
  df_report <- df %>% 
    group_by(org_unit, OA_label) %>% 
    summarise(n_papers = n())
  # deduplicate the dataset and score irrespective of org_unit
  df_all <- df %>% 
    deduplicate() %>% 
    group_by(OA_label) %>% 
    summarise(n_papers = n()) %>% 
    mutate(org_unit = "all")
  # add the all column to the report
  df_report <- bind_rows(df_report,df_all)
  # transform the data
  df_report <- df_report %>% pivot_wider(names_from=OA_label,values_from=n_papers)
  # add percentages
  df_report <- df_report %>% mutate(
    Total_papers = sum(CLOSED,GOLD,GREEN,HYBRID, na.rm=T),
    gold_percent = round(GOLD/Total_papers*100,1),
    hybrid_percent = round(HYBRID/Total_papers*100,1),
    green_percent = round(GREEN/Total_papers*100,1),
    total_OA_percent = round((1 - CLOSED/Total_papers)*100,1)
  )
  return(df_report)
}




#reporting <- read_excel("./config/reports.xlsx")
#reporting <- reporting[2:ncol(reporting)]

#for(r in seq_along(reporting)){
#  name <- colnames(reporting)[r]
#  col <- pull(reporting, name)
#  units <- col[!is.na(col)]
#  df_r <- df %>% filter(org_unit%in%units)
#  name_slug <- str_replace(name," ","_")
#  outfilename <- paste0("./output/report_",name_slug,"_",lubridate::today(),".csv")
#  full_report(df_r) %>% write_csv(outfilename)
#}



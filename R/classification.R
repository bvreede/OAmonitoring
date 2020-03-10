# CLASSIFICATION STEP ONE:
# OBTAINING INFORMATION:
# VSNU DEAL 
# DOAJ REGISTRY
# UNPAYWALL
# 
# 

### VSNU DEAL
get_vsnu <- function(path){
  # get all VSNU DOIs
  vsnu <- read_ext(path, "")
  vsnu <- vsnu %>% mutate(doi = clean_doi(DOI))
  return(vsnu)
}

get_vsnu_df <- function(path){
  # get all VSNU DOIs
  vsnu <- read_ext(path, "")
  return(vsnu)
}

extract_uniques <- function(column){
  #' Uses unique and NA removal to retrieve
  #' a vector of unique entries in a column.
  #' This is useful when mining an API, trying
  #' to minimize the number of calls.
  all_entries <- column %>% unique()
  all_entries <- all_entries[!is.na(all_entries)]
  return(all_entries)
}

doaj_api <- function(issn){
  #' This function uses an issn to mine the
  #' DOAJ API (at doaj.org/api/v1/).
  #' The entry for this ISSN in the DOAJ is returned.
  #'
  Sys.sleep(0.6) # requests for this api are limited at 2 per second, so the request is slowed down.
  api <- "https://doaj.org/api/v1/search/journals/issn:"
  query <- paste0(api,issn)
  result <- GET(query) %>% 
    content(as="text",encoding="UTF-8")
  result_line <- fromJSON(result,flatten=T)$results
  return(result_line)
}


upw_api <- function(doi,email = email_address){
  #' collecting DOI results from Unpaywall using their REST API
  print(doi)
  # compile query to send to unpaywall
  api <- "http://api.unpaywall.org/"
  email <- paste("?email=",email,sep="") 
  query <- paste0(api,doi,email)
  result <- GET(query)
  # resolve query results and transform to a line that can be added to a df
  result_txt <- content(result, as="text",encoding="UTF-8")
  result_line <- fromJSON(result_txt,flatten=T)$results
  return(result_line)
}



api_to_df <- function(df, which_info){
  #' Using an API mining  function, query all unique
  #' entries in a column, and return a data frame
  #' with their results.
  #' 
  #' @param df
  #' @param which_info What api will be mined? Either "doaj" or "upw" (unpaywall).
  #' 
  # extract unique values before mining the api
  if(which_info == "doaj"){
    all_entries <- extract_uniques(df$issn)
  }else if(which_info == "upw"){
    all_entries <- extract_uniques(df$doi)
    
  }
  collect <- list()
  #TODO add time to cat statement
  cat(paste("Mining the", which_info, "api...\n"))
  for(i in seq_along(all_entries)){
    entry <- all_entries[i]
    if(which_info == "doaj"){
      collect[[i]] <- doaj_api(entry)
    } else if(which_info == "upw"){
      collect[[i]] <- upw_api(entry)
    }
  }
  collectdf <- bind_rows(collect)
  return(collectdf)
}


process_doaj <- function(df){
  #' Specifically used to process the data frame
  #' that results from a doaj mining query.
  #' ISSN numbers are in a nested format, 
  #' they need to be un-nested.
  #' For the ease of future processing, the ISSN
  #' columns are renamed.
  df <- df %>%
    # unnest the issn information in the bibjson.identifier column
    unnest(bibjson.identifier, keep_empty = T, names_sep="_") %>%
    rename(issn = bibjson.identifier_id,
           issn_type = bibjson.identifier_type) %>%
    filter(lubridate::year(created_date) <= report_year)
  return(df)
}

save_df <- function(df, which_info){
  # remove list columns so the data can be saved
  df <- df %>% select_if(is.atomic)
  basename <- case_when(
    which_info == "doaj" ~ "doaj_from_issn_",
    which_info == "upw" ~ "upw_from_doi_",
    which_info == "all" ~ "complete_dataframe_",
    TRUE ~ "unknown_info_")
  filename <- paste0("data/clean/", basename, lubridate::today(), ".csv")
  write_csv(df, filename)
}

doaj_pipeline <- function(df){
  if(use_doaj=="api"){
  df <- df %>% 
    api_to_df("doaj") %>%
    process_doaj()
  save_df(df, "doaj")
  } else if(use_doaj=="saved"){
    df <- read_csv(path_doaj)    
  } else{
    warning("Not sure what DOAJ data to use.
Please indicate this in the config.R file, using the option 'api' for use of the DOAJ API,
or 'saved' to use saved data that was previously mined from the DOAJ API.")
    stop
  }
  return(df)
}

upw_pipeline <- function(df){
  if(use_upw=="api"){
  df <- df %>% 
    api_to_df("upw")
  save_df(df, "upw")
  } else if(use_upw=="saved"){
    df <- read_csv(path_upw)
  } else{
    warning("Not sure what Unpaywall data to use.
Please indicate this in the config.R file, using the option 'api' for use of the Unpaywall API,
or 'saved' to use saved data that was previously mined from the Unpaywall API.")
    stop
  }
  return(df)
}



apply_upw <- function(df){
  # extract the relevant column from the unpaywall df
  # and place it in the main df.
  if("oa_color"%in%colnames(df)){
    # it is possible that oa_color already exists. This is the crucial column from Unpaywall,
    # so we ensure here that the column is not duplicated (prompting suffix naming, causing confusion)
    # by removing it from the df.
    df <- select(df, -oa_color)
  }
  df_with_upw <- left_join(df, upwdf, by="doi")
  df <- df %>% 
    mutate(upw = df_with_upw$oa_color)
  return(df)
}

apply_vsnu <- function(df){
  # match with the VSNU document
  vsnu_doi_strip <- vsnudf$doi[!is.na(vsnudf$doi)]
  df <- df %>% mutate(
    vsnu = doi%in%vsnu_doi_strip)
  return(df)
}

apply_doaj <- function(df){
  # match issns with their existence in DOAJ
  doaj_issn_strip <- doajdf$issn[!is.na(doajdf$issn)]
  df <- df %>% mutate(
    doaj = issn%in%doaj_issn_strip)
  return(df)
}

apply_taverne <- function(df){
  # determine whether a paper was made accessible with Taverne
  taverneIDs <- taverne$system_id[!is.na(taverne$system_id)]
  df <- df %>% mutate(
    taverne = system_id%in%taverneIDs)
  return(df)
}

# add matches info to the dataframe
apply_matches <- function(df){
  df <- df %>%
    apply_doaj() %>%
    apply_vsnu() %>%
    apply_upw() %>%
    apply_taverne()
  return(df)
}

# classify based on the information acquired
# All publications are classified according to their presence in check lists. In sequence:
## 1. match the journal ISSN with a list from the Directory of Open Access Journals (DOAJ). 
##    If the journal matches, the publication is Gold OA
## 2. match the DOI with a list obtained from VSNU. 
##    If the journal matches, the publication is Hybrid OA
## 3. obtain the OA status from Unpaywall. 
##    If the status is 'gold' or 'bronze', the publication is Hybrid OA
##    If the status is 'green', the publication is Green OA
# NB in the classification pipeline these labels will be applied in sequence
# Thus, e.g. if ISSN matches DOAJ but Unpaywall says 'green', the label will still be Gold OA.
classify_oa <- function(df){
  df <- df %>%
    apply_matches() %>%
    mutate(
      OA_label = case_when(
        doaj ~ "GOLD",
        vsnu ~ "HYBRID",
        upw == "bronze" ~ "CLOSED",
        upw == "gold" ~ "HYBRID", # indeed, we choose to label gold only confirmed DOAJ ISSN
        upw == "hybrid" ~ "HYBRID",
        taverne ~ "GREEN",
        upw == "green" ~ "GREEN",
        upw == "closed" ~ "CLOSED",
        TRUE ~ "CLOSED"),
      OA_label_explainer = case_when(
        doaj ~ "DOAJ",
        vsnu ~ "VSNU",
        upw == "bronze" ~ "UPW (bronze)",
        upw == "gold" ~ "UPW (gold)", 
        upw == "hybrid" ~ "UPW (hybrid)",
        taverne ~ "TAVERNE",
        upw == "green" ~ "UPW (green)",
        upw == "closed" ~ "UPW (closed)",
        TRUE ~ "NONE")
    )
  save_df(df, "all")
  return(df)
}

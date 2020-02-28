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
  vsnu_doi_cleaned <- clean_doi(vsnu$DOI)
  return(vsnu_doi_cleaned)
}

# match with the VSNU document
vsnu_match <- function(doi,vsnu_doi=vsnu_doi_cleaned){
  return(doi%in%vsnu_doi)
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
  cat(paste("Mining the", which_info, "api..."))
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
           issn_type = bibjson.identifier_type)
  return(df)
}

save_apicollect <- function(df, which_info){
  # remove list columns so the data can be saved
  df <- df %>% select_if(is.atomic)
  basename <- case_when(
    which_info == "doaj" ~ "doaj_from_issn_",
    which_info == "upw" ~ "upw_from_doi_",
    TRUE ~ "unknown_info_")
  filename <- paste0("data/clean/", basename, lubridate::today(), ".csv")
  write_csv(df, filename)
}

doaj_pipeline <- function(df){
  df <- df %>% 
    api_to_df("doaj") %>%
    process_doaj()
  save_apicollect(df, "doaj")
  return(df)
}

upw_pipeline <- function(df){
  df <- df %>% 
    api_to_df("upw")
  save_apicollect(df, "upw")
  return(df)
}

# match issns with their existence in DOAJ
doaj_match <- function(issn, doajissn = doajdf$issn){
  return(issn%in%doajissn)
}

# add matches info to the dataframe
apply_matches <- function(df){
  df <- df %>% 
    mutate(vsnu = vsnu_match(doi),
           doaj = doaj_match(issn)) %>%
    # it is possible that oa_color already exists. This is the crucial column from Unpaywall,
    # so we ensure here that the upw column gets labeled with _upw
    left_join(upwdf, by="doi", suffix = c("","_upw")) %>% 
    rename(oa_color_upw = oa_color) 
  return(df)
}

# classify based on the information acquired
classify_oa <- function(df){
  df <- df %>%
    apply_matches() %>%
    mutate(
      OA_label = case_when(
        doaj ~ "GOLD",
        vsnu ~ "HYBRID",
        oa_color_upw == "bronze" ~ "HYBRID",
        oa_color_upw == "gold" ~ "HYBRID", # indeed, we choose to label gold only confirmed DOAJ ISSN
        oa_color_upw == "green" ~ "GREEN",
        TRUE ~ "CLOSED"),
      OA_label_explainer = case_when(
        doaj ~ "DOAJ",
        vsnu ~ "VSNU",
        oa_color_upw %in% c("bronze","gold","green") ~ "UPW",
        TRUE ~ "NONE")
    )
  return(df)
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

# 
# ## Classification data
# doaj <- read_excel(path_doaj)
# vsnu <- read_csv(path_vsnu)
# 
# ## Renaming columns so they will not have to be adjusted every time we run the script - should be in config file!
# colnames(doaj)[colnames(doaj) == issn_column_doaj] <- "issn"
# colnames(doaj)[colnames(doaj) == eissn_column_doaj] <- "eissn"
# 
# 
# ## Clean data
# # clean DOI and ISSN, remove spaces and hyperlinks, change uppercase to lowercase etc.
# doaj$issn <- clean_issn(doaj$issn) 
# doaj$eissn <- clean_issn(doaj$eissn)
# vsnu$DOI <- clean_doi(vsnu$DOI)
# vsnu_doi <- vsnu$DOI[!is.na(vsnu$DOI)]
# 
# #### OA LABELLING ####
# ## Collect information that can later be used for the classification pipeline.
# doaj_issn <- union(doaj$issn[!is.na(doaj$issn)], # all DOAJ ISSN numbers from print, without NAs
#                    doaj$eissn[!is.na(doaj$eissn)]) # all DOAJ E-ISSN numbers, without NAs

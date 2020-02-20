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

extract_issn <- function(column){
  # get all ISSNs in the dataset
  all_issn <- column %>% unique()
  all_issn <- all_issn[!is.na(all_issn)]
  return(all_issn)
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

make_issn_df <- function(df){
  #' Using the doaj api mine function, all
  #' ISSN numbers in a vector are queried, and a dataframe
  #' of their information in the DOAJ is
  #' returned.
  #' 
  all_issn <- extract_issn(df$issn)
  
  issnlist <- list()
  for(i in seq_along(all_issn)){
    issn <- all_issn[i]
    issnlist[[i]] <- doaj_api(issn)
  }
  issndf <- bind_rows(issnlist)
  return(issndf)
}



upw_api <- function(doi,email = email_address){
  #' collecting DOI results from Unpaywall using their REST API
  # compile query to send to unpaywall
  api <- "api.unpaywall.org/"
  email <- paste("?email=",email,sep="") 
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
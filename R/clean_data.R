# classify OA status
## Step 1: DOAJ ISSN matching
df$DOAJ_ISSN_match <- df$issn%in%doaj_issn

## Step 2: VSNU DOI matching
df$VSNU_doi_match <- df$doi%in%vsnu_doi



## Functions

read_ext <- function(fn){
  # opening a file, with method depending on the extension
  # extract extension and put together filename
  fn_ext <- str_split(fn,"\\.")[[1]]
  ext <- fn_ext[-1]
  fn_path <- paste0("data/",fn)
  
  if(ext == "csv"){ 
    # multiple methods are possible, check which one yields the largest no. of columns
    # this is quite hacky, and generates unnecessary warnings. It does work though...
    df1 <- read_delim(fn_path, delim=";")
    df2 <- read_delim(fn_path, delim=",")
    if(ncol(df1)>ncol(df2)){
      df <- df1
    } else{
      df <- df2
    }
    rm(df1,df2)
  } else if(ext=="tsv"){
    df <- read_delim(fn_path,delim="\t")
  } else if(ext=="xls"|ext=="xlsx"){
    df <- read_excel(fn_path)
  }
  return(df)
}


column_rename <- function(data,col_config){
  # rename column names
  id_column <- col_config[allfiles$File_info=="Internal unique identifier"]
  issn_column <- col_config[allfiles$File_info=="ISSN"]
  doi_column <- col_config[allfiles$File_info=="DOI"]
  org_column <- col_config[allfiles$File_info=="Organization unit"]
  colnames(data)[colnames(data) == id_column] <- "system_id"
  colnames(data)[colnames(data) == issn_column] <- "issn"
  colnames(data)[colnames(data) == doi_column] <- "doi"
  colnames(data)[colnames(data) == org_column] <- "org_unit"
  # return cleaned data
  return(data)
}

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


## Classification data
doaj <- read_excel(path_doaj)
vsnu <- read_csv(path_vsnu)

## Renaming columns so they will not have to be adjusted every time we run the script - should be in config file!
colnames(doaj)[colnames(doaj) == issn_column_doaj] <- "issn"
colnames(doaj)[colnames(doaj) == eissn_column_doaj] <- "eissn"


## Clean data
# clean DOI and ISSN, remove spaces and hyperlinks, change uppercase to lowercase etc.
doaj$issn <- clean_issn(doaj$issn) 
doaj$eissn <- clean_issn(doaj$eissn)
vsnu$DOI <- clean_doi(vsnu$DOI)
vsnu_doi <- vsnu$DOI[!is.na(vsnu$DOI)]

#### OA LABELLING ####
## Collect information that can later be used for the classification pipeline.
doaj_issn <- union(doaj$issn[!is.na(doaj$issn)], # all DOAJ ISSN numbers from print, without NAs
                   doaj$eissn[!is.na(doaj$eissn)]) # all DOAJ E-ISSN numbers, without NAs



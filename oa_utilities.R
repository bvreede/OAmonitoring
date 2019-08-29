## load libraries
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)
library(magrittr)
library(ggplot2)


### Functions for OA monitoring pipeline ###
department_check <- function(orgs){
  # collect all the organisation names
  orgnames <- names(summary(orgs))
  # verify if last year's departments are all present.
  departments <-  c("Dep Aardwetenschappen","Dep Bestuurs- en Organisatiewetenschap","Dep Biologie",
                    "Dep Fysische Geografie","Dep GeoLab","Dep Informatica","Dep Natuurkunde",
                    "Dep of Sustainable Development","Dep Rechtsgeleerdheid","Dep Scheikunde",
                    "Dep Sociale Geografie en Planologie","Dep USE","Dep Wiskunde","Dep Innovatie- Milieu- en Energiewet",
                    "Faculteit Betawetenschappen","Faculteit Diergeneeskunde","Faculteit Geesteswetenschappen",
                    "Faculteit Geowetenschappen","Faculteit REBO","Faculteit Sociale Wetenschappen" )
  for(d in departments){
    if(!is.element(d,orgnames)){
      print(paste("Missing from dataset:",d))
    }
  }
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
  email <- "?email=b.m.i.vreede@uu.nl"
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
## 4. use existing Pure status                                     
##    If the pure status is 'open', the publication is either hybrid or green OA
##    Green OA only if an embargo is noted, or the file contains an AAM (not publisher version)
## 5. apply data from an optional manual check
# NB in the classification pipeline these labels will be applied in sequence
# Thus, e.g. if ISSN matches DOAJ but Unpaywall says 'green', the label will still be Gold OA.

define_oa <- function(doaj,vsnu,upw,pure,green,man=NA){
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
  # PURE: if the status is open
  if(pure=="Open"&!is.na(pure)){
    if(green){
      return("GREEN") # this is true if embargo or AAM in pure
    } else{
      return("HYBRID")
    }
  }
  # MANUAL
  ### last check: is there a manual annotation for this paper?
  if(!is.na(man)){
    if(man=="B"){
      return("HYBRID")
    } else if(man=="C"){
      return("GREEN")
    } 
  }
  return("CLOSED")
}

define_oa_detailed <- function(doaj,vsnu,upw,pure,green,man=NA){
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
  ## PURE: resolve to hybrid or green
  if(!is.na(pure)){
    if(pure=="Open"){
      if(green){
        return("GREEN PURE")
      } else{
        return("HYBRID PURE")
      }
    }
  }
  ## MANUAL: resolve to hybrid or green
  if(!is.na(man)){
    if(man=="B"){
      return("HYBRID MANUAL")
    } else if(man=="C"){
      return("GREEN MANUAL")
    }
  }
  return("CLOSED")
}


deduplicate <- function(df){
  # determine whether there is a mix of UMCU/UU by looking at factor levels in org_unit
  orgs <- df$org_unit
  orgs %<>% as.character
  orgs %<>% as.factor
  if("UMC Utrecht"%in%levels(orgs)&length(levels(orgs))>1){
    ## if a mix between UMC and UU: deduplicate based on DOI, then title
    df1 <- filter(df,is.na(doi))
    df1 <- distinct(df1,title.x,.keep_all=T)
    df2 <- filter(df,!is.na(doi))
    df2 <- distinct(df2,doi,.keep_all=T)
    df <- full_join(df1,df2)
  } else{
    ## if not: deduplicate based on PURE ID
    df <- distinct(df,pure_id,.keep_all=T)
  }
  return(df)
}


infocheck <- function(df,checkthese){
  # checks of the percentage of missing information in a df does not exceed 5%
  info <- df$information
  f_mis <- sum(info==F)/length(info)
  if(f_mis>0.05){
    checkthese <- rbind(checkthese,filter(df,information==F))
  }
  return(checkthese)
}

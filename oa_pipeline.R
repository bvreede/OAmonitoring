## load libraries
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)


## functions
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
  column <- gsub('\\s+','',column) #remove spaces from ISSN
  column <- gsub('-','',column) #remove - from ISSN
  return(column)
}

clean_doi <- function(column){
  column <- gsub('https://','',column) #remove https:// from DOI
  column <- gsub('doi.org/','',column) #remove doi.org/ from DOI
  column <- gsub('\\s+','',column) #remove spaces from DOI
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
  result_line$reported_noncompliant_copies <- NULL # this is a list that will cause conflicts later on
  return(result_line)
}

# The following functions try to classify all publications according to their presence in check lists. In sequence:
## 1. match the journal ISSN with a list from the Directory of Open Access Journals (DOAJ). 
##    If the journal matches, the publication is Gold OA
## 2. match the DOI with a list obtained from VSNU. 
##    If the journal matches, the publication is Hybrid OA
## 3. obtain the OA status from Unpaywall. 
##    If the status is 'gold', the publication could be Gold OA (tbd)         ### OPTIONAL TBD ###
##    If the status is 'bronze', the publication is Hybrid OA
##    If the status is 'green', the publication is Green OA
## 4. (optional) use existing Pure status                                     ### OPTIONAL TBD ###
##    If the pure status is 'open', the publication is Green OA
# NB in the classification pipeline these labels will be applied in sequence
# Thus, e.g. if ISSN matches DOAJ but Unpaywall says 'green', the label will still be Gold OA.
# The optional ("tbd") labels only exist in the second function (define_oa_broad)

define_oa <- function(doaj,vsnu,upw){
  ## DOAJ and VSNU
  if(doaj){
    return("GOLD")
  } else if(vsnu){
    return("HYBRID")
  ## UNPAYWALL
  } else if(is.na(upw)){
    return("CLOSED")
  } else if(upw=="bronze"|upw=="gold"){
    return("HYBRID")
  } else if(upw=="green"){
    return("GREEN")
  } 
}

define_oa_broad <- function(doaj,vsnu,upw,pure){
  if(doaj){
    ## DOAJ and VSNU tests
    return("GOLD")
  } else if(vsnu){
    return("HYBRID")
    ## UNPAYWALL 1: if it is NA, then check PURE or return closed
  } else if(is.na(upw)){
    if(pure=="Open"){
      return("GREEN")}
    else{
      return("CLOSED")
    }
    ## UNPAYWALL 2
  } else if(upw=="bronze"){
    return("HYBRID")
  } else if(upw=="green"){
    return("GREEN")
  } else if(upw=="gold"){
    return("GOLD")
  } 
}

#### LOAD AND CLEAN DATA ####

## UU data - from PURE
pure_uu <- read_excel("data/UU-Monitoring_OA-2018-basislijst-report_3119.xls")
pure_umcu <- read_excel("data/UMC-Monitoring_OA-2018-basislijst-report_3119.xls")

## Classification data
doaj <- read_excel("data/2018-12-31-DOAJ-schoon.xlsx")
vsnu <- read_csv("data/VSNU-DOIs.csv")


## Renaming columns so they will not have to be adjusted every time we run the script
colnames(pure_uu)[colnames(pure_uu) == 'Contributors > Organisations > Organisational unit-0'] <- "org_unit"
colnames(pure_uu)[colnames(pure_uu) == 'ID-1'] <- "pure_id"
colnames(pure_uu)[colnames(pure_uu) == 'Title of the contribution in original language-2'] <- "title"
colnames(pure_uu)[colnames(pure_uu) == 'Journal > ISSN-5'] <- "issn"
colnames(pure_uu)[colnames(pure_uu) == 'Electronic version(s) of this work > DOI (Digital Object Identifier)-7'] <- "doi"
colnames(pure_uu)[colnames(pure_uu) == 'Open Access status-8'] <- "OA_status_pure"

colnames(pure_umcu)[colnames(pure_umcu) == 'Organisations > Organisational unit-0'] <- "org_unit"
colnames(pure_umcu)[colnames(pure_umcu) == 'ID-4'] <- "pure_id"
colnames(pure_umcu)[colnames(pure_umcu) == 'Title of the contribution in original language-6'] <- "title"
colnames(pure_umcu)[colnames(pure_umcu) == 'Journal > ISSN-7'] <- "issn"
colnames(pure_umcu)[colnames(pure_umcu) == 'Electronic version(s) of this work > DOI (Digital Object Identifier)-9'] <- "doi"
colnames(pure_umcu)[colnames(pure_umcu) == 'Open Access status-11'] <- "OA_status_pure"

colnames(doaj)[colnames(doaj) == 'Journal ISSN (print version)'] <- "issn"
colnames(doaj)[colnames(doaj) == 'Journal EISSN (online version)'] <- "eissn"

 
## Adjust data types

# Set ID as character, so that it will not be treated as a numeral
pure_uu$pure_id <- as.character(pure_uu$pure_id)
pure_umcu$pure_id <- as.character(pure_umcu$pure_id)

# Set organisational unit, ISSN and OA status as factors because they are fixed variables which we want to analyze.
pure_uu$org_unit <- as.factor(pure_uu$org_unit)
pure_uu$issn <- as.factor(pure_uu$issn)
pure_uu$OA_status_pure<- as.factor(pure_uu$OA_status_pure)

pure_umcu$org_unit <- as.factor(pure_umcu$org_unit)
pure_umcu$issn <- as.factor(pure_umcu$issn)
pure_umcu$OA_status_pure<- as.factor(pure_umcu$OA_status_pure)

## Verify data (uu only)
department_check(pure_uu$org_unit)

## Clean data
# clean DOI and ISSN, remove spaces and hyperlinks, change uppercase to lowercase etc.
pure_uu$issn <- clean_issn(pure_uu$issn)
pure_uu$doi <- clean_doi(pure_uu$doi)

pure_umcu$issn <- clean_issn(pure_umcu$issn)
pure_umcu$doi <- clean_doi(pure_umcu$doi)

doaj$issn <- clean_issn(doaj$issn) 
doaj$eissn <- clean_issn(doaj$eissn)

vsnu$DOI <- clean_doi(vsnu$DOI)


#### OA LABELLING ####
## Collect information that can later be used for the classification pipeline.

## Step 1: DOAJ ISSN matching
doaj_issn <- union(doaj$issn[!is.na(doaj$issn)], # all DOAJ ISSN numbers from print, without NAs
                   doaj$eissn[!is.na(doaj$eissn)]) # all DOAJ E-ISSN numbers, without NAs

pure_uu$DOAJ_ISSN_match <- pure_uu$issn%in%doaj_issn
pure_umcu$DOAJ_ISSN_match <- pure_umcu$issn%in%doaj_issn

## Step 2: VSNU DOI matching
vsnu_doi <- vsnu$DOI[!is.na(vsnu$DOI)]

pure_uu$VSNU_doi_match <- pure_uu$doi%in%vsnu_doi
pure_umcu$VSNU_doi_match <- pure_umcu$doi%in%vsnu_doi

## Step 3: Unpaywall
# generate a database with unpaywall data using their REST API
# use all DOIs as input
alldois <- union(pure_uu$doi[!is.na(pure_uu$doi)], # all pure_uu dois without NA
                 pure_umcu$doi[!is.na(pure_umcu$doi)]) # all UMCU dois without NA

unpaywall <- NULL
counter <- 1

for(d in alldois[3001:length(alldois)]){
  #print(paste("Unpaywall query: running doi", counter, "of", length(alldois)))
  upw_entry <- upw_api(d)
  # check if the entries match the dataframe in length
  if(!is.null(unpaywall)){
    if(length(upw_entry) == dim(unpaywall)[2]){
    unpaywall <- rbind(unpaywall,upw_entry)
    } else{
    print(paste("Issue with query",counter,": Could not save:",upw_entry))
    } 
  } else{
    unpaywall <- rbind(unpaywall,upw_entry)
  }
  counter <- counter +1
}

## DOIS THAT GIVE PROBLEMS:
# 10.14379/iodp.pr.371.2018 (number 2498)
# 10.14379/iodp.pr.374.2018 (number 2350)
# 10.1002/pds.4705 (number 4951)

# save unpaywall data
today <- as.character(Sys.Date())
upwname <- paste0("data/unpaywall_",today,".csv")
write_csv(unpaywall,upwname)

# ensure unpaywall data is saved as factor
unpaywall$evidence <- as.factor(unpaywall$evidence)
unpaywall$free_fulltext_url <- as.factor(unpaywall$free_fulltext_url)
unpaywall$license <- as.factor(unpaywall$license)
unpaywall$oa_color <- as.factor(unpaywall$oa_color)

# merge pure with unpaywall
uu_merge <- merge(pure_uu,unpaywall,by="doi", all.x=T, all.y=F)
umcu_merge <- merge(pure_umcu,unpaywall,by="doi", all.x=T, all.y=F)



#### CLASSIFICATION PIPELINE ####
## apply the classification functions
uu_merge <- mutate(uu_merge,
                   OA_label_conservative=mapply(define_oa, DOAJ_ISSN_match, VSNU_doi_match, oa_color))
umcu_merge <- mutate(umcu_merge,
                   OA_label_conservative=mapply(define_oa, DOAJ_ISSN_match, VSNU_doi_match, oa_color))
uu_merge <- mutate(uu_merge,
                   OA_label_broad=mapply(define_oa_broad, DOAJ_ISSN_match, VSNU_doi_match, oa_color,OA_status_pure))
umcu_merge <- mutate(umcu_merge,
                   OA_label_broad=mapply(define_oa_broad, DOAJ_ISSN_match, VSNU_doi_match, oa_color,OA_status_pure))

## turn the results into factors
uu_merge$OA_label_conservative <- as.factor(uu_merge$OA_label_conservative)
umcu_merge$OA_label_conservative <- as.factor(umcu_merge$OA_label_conservative)
uu_merge$OA_label_broad <- as.factor(uu_merge$OA_label_broad)
umcu_merge$OA_label_broad <- as.factor(umcu_merge$OA_label_broad)



#### RESULTING TABLES AND FIGURES ####

uu_table <- table(uu_merge$OA_label_broad)
uu_prop <- prop.table(uu_table)

umcu_table <- table(umcu_merge$OA_label_broad)
umcu_prop <- prop.table(umcu_table)








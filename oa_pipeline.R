## load libraries
library(readr)
library(readxl)
library(stringr)
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


#### CLASSIFICATION PIPELINE ####

# The pipeline tries to classify all publications according to their presence in check lists. In sequence:
## 1. match the journal ISSN with a list from the Directory of Open Access Journals (DOAJ). 
##    If the journal matches, the publication is Gold OA
## 2. match the DOI with a list obtained from VSNU. 
##    If the journal matches, the publication is Hybrid OA
## 3. obtain the OA status from Unpaywall. 
##    If the status is publisher, the publication is Hybrid OA. 
##    If the status is repository, the publication is Green OA.

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

alldois <- union(pure_uu$doi[!is.na(pure_uu$doi)], # all pure_uu dois without NA
                 pure_umcu$doi[!is.na(pure_umcu$doi)]) # all UMCU dois without NA

unpaywall <- NULL
counter <- 1

for(d in alldois[101:500]){
  print(paste("Unpaywall query: running doi", counter, "of", length(alldois)))
  counter <- counter +1
  upw_entry <- upw_api(d)
  # check if the entries match in length
  if(!is.null(unpaywall)){
    if(length(upw_entry) == dim(unpaywall)[2]){
    unpaywall <- rbind(unpaywall,upw_entry)
    } else{
    print(paste("Could not save:",upw_entry))
    } 
  } else{
    unpaywall <- rbind(unpaywall,upw_entry)
  }
}


# save unpaywall data
today <- as.character(Sys.Date())
upwname <- paste0("data/unpaywall_",today,".csv")
write_csv(unpaywall,upwname)


# Step 3: get Unpaywall data
#We need to feed the DOI's to http://unpaywall.org/products/simple-query-tool
#Get the DOIs in a list to paste in Unpaywall data search
write(pure_uu$doi, file="dois_uu.txt")
write(pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6`, file="dois_umcu.txt")

# Load the resulting csv files
unpaywall_uu <- read.csv("unpaywall_uu.csv")
unpaywall_uu$best_oa_host <- as.character(unpaywall_uu$best_oa_host)
unpaywall_umcu <- read.csv("unpaywall_umcu.csv")
unpaywall_umcu$best_oa_host <- as.character(unpaywall_umcu$best_oa_host)

# Step 4: Add the Unpaywall results to the pure and scopus files





# edit the column name for doi
names(pure_uu)[names(pure_uu) == "Electronic version(s) of this work > DOI (Digital Object Identifier)-7"] <- "doi"
names(pure_umcu)[names(pure_umcu) == "Electronic version(s) of this work > DOI (Digital Object Identifier)-6"] <- "doi"

# merge with unpaywall
uu_merge <- merge(pure_uu,unpaywall_uu,by="doi", all.x=T, all.y=F)
umcu_merge <- merge(pure_umcu,unpaywall_umcu,by="doi", all.x=T, all.y=F)


#Add column to pure and scopus files with VSNU OA Status, as follows
#if doaj_issn_match=TRUE: GOLD, if not
#if vsnu_doi_match=TRUE: HYBRID, if not
#if Unpaywall_host=publisher: HYBRID, if not
#if Unpaywall_host=repository: GREEN, if not
#CLOSED

# function that takes input from:
# - ISSN match
# - VSNU match
# - unpaywall host
# and returns gold/hybrid/green/notOA

define_oa <- function(refdb){
  ldb <- dim(refdb)[1]
  oalist <- NULL
  for(i in 1:ldb){
  if(is.na(refdb$DOAJ_ISSN_match[i])==FALSE){
    if(refdb$DOAJ_ISSN_match[i]==TRUE){
    oalist <- c(oalist,'GOLD')
    next
    }
  }
  if(is.na(refdb$vsnu_doi_match[i])==FALSE){
    if(refdb$vsnu_doi_match[i]){
      oalist <- c(oalist,'HYBRID')
      next
    }
  }
  if(is.na(refdb$best_oa_host[i])==FALSE){
    if(refdb$best_oa_host[i]=='publisher'){
      oalist <- c(oalist,'HYBRID')
      next
    }
  }
  if(is.na(refdb$best_oa_host[i])==FALSE){
    if(refdb$best_oa_host[i]=='repository'){
      oalist <- c(oalist,'GREEN')
      next
    }
  }
  oalist <- c(oalist,'NOT_OA')
  }
  return(oalist)
}

uu_merge$oa_status <- define_oa(uu_merge)
uu_merge$oa_status <- as.factor(uu_merge$oa_status)

umcu_merge$oa_status <- define_oa(umcu_merge)
umcu_merge$oa_status <- as.factor(umcu_merge$oa_status)

uu_table <- table(uu_merge$oa_status)
uu_prop <- prop.table(uu_table)

umcu_table <- table(umcu_merge$oa_status)
umcu_prop <- prop.table(umcu_table)


#For pure_uu
pure_uu$vsnu_oa_status <- ifelse(
  pure_uu$DOAJ_ISSN_match==TRUE,
  'GOLD',
  ifelse(
    pure_uu$vsnu_doi_match==TRUE,
    'HYBRID',
    ifelse(
      pure_uu$best_oa_host=='publisher',
      'HYBRID', 
      ifelse(
        pure_uu$best_oa_host=='repository',
        'GREEN',
        'NOT_OA'))))
pure_uu$vsnu_oa_status <- as.factor(pure_uu$vsnu_oa_status)

#For pure_umcu
pure_umcu$vsnu_oa_status <- ifelse(pure_umcu$DOAJ_ISSN_match==TRUE,'GOLD',ifelse(pure_umcu$vsnu_doi_match==TRUE,'HYBRID',ifelse(pure_umcu$Unpaywall_host=='publisher','HYBRID', ifelse(pure_umcu$Unpaywall_host=='repository','GREEN','NOT_OA'))))
pure_umcu$vsnu_oa_status <- as.factor(pure_umcu$vsnu_oa_status)


#Get first results 
#Get VSNU status for UU, UMCU respectively

table(pure_uu[c("vsnu_oa_status")])
table(pure_umcu[c("vsnu_oa_status")])
table(scopus_utrecht[c("vsnu_oa_status")])

#Get Unpaywall results for UU, UMCU and Scopus respectively
table(pure_uu[c("Unpaywall_host")])
table(pure_umcu[c("Unpaywall_host")])







## load libraries
library(readr)


## LOAD AND CLEAN DATA ##

# This project takes publication data from a single year at Utrecht University, and determines per article its open access status, using various sources available. It uses peer reviewed journal articles registered in PURE at UU and UMCU as input.

# First load the UU Pure Data
pure_uu <- read_csv2("PURE-UU-2017-20180314.csv")
pure_uu <- as.data.frame(pure_uu)

# Set ID as character, so that it will not be treated as a numeral
pure_uu$`Id-1`<- as.character(pure_uu$`Id-1`)

# Set organisational unit, ISSN and OA status as factors because they are fixed variables which we want to analyze.
pure_uu$`Authors > Organisations > Organisational unit-0`<- as.factor(pure_uu$`Authors > Organisations > Organisational unit-0`)
pure_uu$`Journal > ISSN-5`<- as.factor(pure_uu$`Journal > ISSN-5`)
pure_uu$`Open Access status-8`<- as.factor(pure_uu$`Open Access status-8`)


# We should check if all required organisational units are present in the data. 
## IS THERE A BETTER METHOD THAN SUMMARY?

# Required are:
# Dep Aardwetenschappen
# Dep Bestuurs- en Organisatiewetenschap
# Dep Biologie
# Dep Farmaceutische wetenschappen
# Dep Fysische Geografie
# Dep Informatica
# Dep Innovatie- Milieu- en Energiewet.
# Dep Natuur & Sterrenkunde
# Dep Rechtsgeleerdheid
# Dep Sociale Geografie en Planologie
# Dep USE
# Dep Wiskunde
# Faculteit Diergeneeskunde
# Faculteit Geesteswetenschappen
# Faculteit Sociale Wetenschappen

# As you can see below, Dep Aardwetenschappen, Dep Fysische Geografie, Dep Innovatie- Milieu- en Energiewet. and Dep Sociale Geografie en Planologie are missing from pure_uu, while Faculteit Geowetenschappen is included in pure_uu 
summary(pure_uu$`Authors > Organisations > Organisational unit-0`)


# MISSING FROM PIPELINE BARBARA!
# Before we match data, we will have to clean the DOI and ISSN, remove spaces and hyperlinks, change uppercase to lowercase etc.

#remove spaces from ISSN
pure_uu$`Journal > ISSN-5` <- gsub('\\s+','',pure_uu$`Journal > ISSN-5`)
#remove - from ISSN
pure_uu$`Journal > ISSN-5` <- gsub('-','',pure_uu$`Journal > ISSN-5`)
#remove https:// from DOI
pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7` <- gsub('https://','',pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7`)
#remove doi.org/ from DOI
pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7` <- gsub('doi.org/','',pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7`)
#remove spaces from DOI
pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7` <- gsub('\\s+','',pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7`)
#Change DOI to lowercase only
pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7` <- tolower(pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7`)

# Then load the UMCU Pure Data
pure_umcu <- read_csv2("PURE-UMCU-2017-20180313.csv")
pure_umcu <- as.data.frame(pure_umcu)

# Set ID as character, so that it will not be treated as a numeral
pure_umcu$`Id-0`<- as.character(pure_umcu$`Id-0`)
# Set organisational unit, ISSN and OA status as factors because they are fixed variables which we want to analyze.

pure_umcu$`Journal > ISSN-1`<- as.factor(pure_umcu$`Journal > ISSN-1`)
pure_umcu$`Open Access status-7`<- as.factor(pure_umcu$`Open Access status-7`)

#remove spaces from ISSN
pure_umcu$`Journal > ISSN-1` <- gsub('\\s+','',pure_umcu$`Journal > ISSN-1`)
#remove - from ISSN
pure_umcu$`Journal > ISSN-1` <- gsub('-','',pure_umcu$`Journal > ISSN-1`)
#remove https:// from DOI
pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6` <- gsub('https://','',pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6`)
#remove doi.org/ from DOI
pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6` <- gsub('doi.org/','',pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6`)
#remove spaces from DOI
pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6` <- gsub('\\s+','',pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6`)
#Change DOI to lowercase only
pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6` <- tolower(pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6`)



## CLASSIFICATION PIPELINE ##
#The pipeline tries to classify all publications according to its presence or absence in various check lists. In sequence:
#1. match the journal ISSN with a list from the Directory of Open Access Journals (DOAJ). If the journal matches, the publication is Gold OA
#2. match the DOI with a list obtained from VSNU. If the journal matches, the publication is Hybrid OA
#3. obtain the OA status from Unpaywall. If the status is publisher, the publication is Hybrid OA. If the status is repository, the publication is Green OA.

## Step 1a: Load the DOAJ Data
# Source: On SURF Drive, OA Monitoring
# Date: December 31st 2017
# Content: all journals listed on DOAJ and therefore labeled as Full OA
doaj <- read.csv2("DOAJ-20171231.csv")

# step 1b: Extract ISSN's form DOAJ and add a column to UU_Pure that specifies TRUE/FALSE depending on a DOAJ ISSN matching to this publication
#Extract ISSN's
doaj_issn_print <- doaj$Journal.ISSN..print.version.[is.na(doaj$Journal.ISSN..print.version.)==FALSE]
doaj_issn_online <- doaj$Journal.EISSN..online.version.[is.na(doaj$Journal.EISSN..online.version.)==FALSE]
doaj_issn <- union(doaj_issn_online,doaj_issn_print)
#remove spaces from ISSN
doaj_issn <- gsub('\\s+','',doaj_issn)
#remove - from ISSN
doaj_issn <- gsub('-','',doaj_issn)
#Add a column to Pure and Scopus
pure_uu$DOAJ_ISSN_match <- pure_uu$`Journal > ISSN-5`%in%doaj_issn
pure_umcu$DOAJ_ISSN_match <- pure_umcu$`Journal > ISSN-1`%in%doaj_issn
scopus_utrecht$DOAJ_ISSN_match <- scopus_utrecht$ISSN%in%doaj_issn

# Step 2a: Load VSNU data
# Source: compiled from VSNU OA data on Surfdrive, OA Deals (2016 up until summer 2018)
# Date: September 17th 2018
# Content: Cumulative list of all OA articles published within the Netherlands as part of the VSNU OA deal, including DOI, publisher and publication year.

vsnu <- read.csv2("VSNU_OA_articles_20180917.csv")

# Step 2: match the DOI with the VSNU list 
vsnu_doi <- vsnu$DOI[is.na(vsnu$DOI)==FALSE]
pure_uu$vsnu_doi_match <- pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7`%in%vsnu_doi
pure_umcu$vsnu_doi_match <- pure_umcu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-6`%in%vsnu_doi

# Step 3: get Unpaywall data
#We need to feed the DOI's to http://unpaywall.org/products/simple-query-tool
#Get the DOIs in a list to paste in Unpaywall data search
write(pure_uu$`Electronic version(s) of this work > DOI (Digital Object Identifier)-7`, file="dois_uu.txt")
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







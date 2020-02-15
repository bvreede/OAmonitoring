## load packages and functions
library(tidyverse)
library(stringr)
library(readxl)
library(jsonlite)
library(httr)
library(magrittr)
library(here)

# source scripts with functions and paths
source("config/config.R")
source("R/clean_data.R")


allfiles <- read_excel("config/config_pub_files.xlsx")


# STEP ONE: CLEAN THE DATASETS AND COMBINE THEM

alldata <- list()

for(col in allfiles){
  # extract file name and extension
  fn <- col[allfiles$File_info=="Filename"]
  fn_ext <- str_split(fn,"\\.")[[1]]
  
  # test if the column contains NAs; in this case the file will not be read
  if(sum(is.na(col))>0){
    warning("The information for file ", fn, " is not filled out. This file cannot be processed.\n")
    next
  } 
  # skip filenames without extensions
  # NB this automatically skips the first column with row names
  if(length(fn_ext) < 2){
    if(!fn == allfiles[1,1]){ # this is acceptable with the header.
      warning("The filename ", fn, " does not have an extension. This file cannot be processed.\n")
    }
    next
  }
  
  # open the file and adjust the column names to the config input
  df <- read_ext(fn) %>% column_rename(col)

  # clean DOI and ISSN, remove spaces and hyperlinks, change uppercase to lowercase etc.
  # also add source file column
  df <- df %>% mutate(issn = clean_issn(issn),
                      doi = clean_doi(doi),
                      source = fn)
  
  # save to the alldata list, and remove excess variables
  alldata[[fn]] <- df
}

rm(df, fn, fn_ext, col)

df <- bind_rows(alldata)


# STEP TWO: APPLY CLASSIFICATION

source("R/classification.R")


df <- df %>% mutate(
  vsnu = vsnu_match(doi)
)







#### LOAD AND CLEAN DATA ####





## Step 3: Unpaywall
api_csv <- "csv" #indicate here whether you want to load existing data or use the UPW api

# generate a database with unpaywall data using their REST API
# use all DOIs as input
alldois <- pub_data$doi[!is.na(pub_data$doi)] # all pub_data dois without NA

# mine unpaywall API for each DOI
outlist <- list()

for(i in seq_along(alldois)){
  doi <- alldois[i]
  if(api_csv=="api"){ # only mine the api if the user wants to renew unpaywall data
    res <- upw_api(doi)
    outlist[[i]] <- res
  }
}

if(api_csv=="api"){
  unpaywall <- bind_rows(outlist)
  # save unpaywall data
  today <- as.character(Sys.Date())
  upwname <- paste0("output/unpaywall_",today,".csv")
  write_csv(unpaywall,upwname)
} else if(api_csv=="csv"){
  unpaywall <- read_csv(path_unpaywall)
}

# ensure unpaywall data is saved as factor
unpaywall$evidence %<>% as.factor
unpaywall$free_fulltext_url %<>% as.factor
unpaywall$license %<>% as.factor
unpaywall$oa_color %<>% as.factor

# merge publication data with unpaywall
## CONSIDER JOIN IN UPDATE ##
pub_data_merge <- left_join(pub_data,unpaywall,by="doi")

#### CLASSIFICATION PIPELINE ####
## apply the classification function
pub_data_merge <- mutate(pub_data_merge,
                         OA_label=mapply(define_oa,
                                         DOAJ_ISSN_match,
                                         VSNU_doi_match,
                                         oa_color))

pub_data_merge <- mutate(pub_data_merge,
                         OA_label_detail=mapply(define_oa_detailed,
                                                DOAJ_ISSN_match,
                                                VSNU_doi_match,
                                                oa_color))

## turn the results into factors
pub_data_merge$OA_label %<>% as.factor
pub_data_merge$OA_label_detail %<>% as.factor




==============

## source file paths
#path_pure_uu <- "data/UU-Monitoring_OA-2018-basislijst-report_3119.xls"
path_pure_uu <- "data/2018_Monitoring_OA___basislijst_2-aangepast_20190311.xls"
#path_pure_umcu <- "data/UMC-Monitoring_OA-2018-basislijst-report_3119.xls"
path_pure_umcu <- "data/UMC-Monitoring_OA-extrainfo-2018-12_03_19.xls"
path_vsnu <- "data/VSNU-DOIs.csv"
path_doaj <- "data/2018-12-31-DOAJ-schoon.xlsx"
path_unpaywall <- "data/unpaywall_2019-03-05.csv"
path_hoop <- "data/HOOPgebieden-test.xlsx"
path_handmatig <- "data/handmatige_controle_MenA-gedaan.xlsx"


#### LOAD AND CLEAN DATA ####

## UU data - from PURE
pure_uu <- read_excel(path_pure_uu)
pure_uu_old <- read_excel(path_pure_uu_old)
pure_umcu <- read_excel(path_pure_umcu)
pure_manual <- read_excel(path_handmatig)

## Classification data
doaj <- read_excel(path_doaj)
vsnu <- read_csv(path_vsnu)

## Reporting information
HOOP <- read_excel(path_hoop)


## Renaming columns so they will not have to be adjusted every time we run the script
# NB THESE ARE COL IDS FROM EARLIER VERSION
colnames(pure_uu)[colnames(pure_uu) == 'Contributors > Organisations > Organisational unit-0'] <- "org_unit"
colnames(pure_uu)[colnames(pure_uu) == 'ID-1'] <- "pure_id"
colnames(pure_uu)[colnames(pure_uu) == 'Title of the contribution in original language-2'] <- "title"
colnames(pure_uu)[colnames(pure_uu) == 'Journal > ISSN-5'] <- "issn"
#colnames(pure_uu)[colnames(pure_uu) == 'Electronic version(s) of this work > DOI (Digital Object Identifier)-7'] <- "doi"
#colnames(pure_uu)[colnames(pure_uu) == 'Electronic version(s) of this work > File > File name-6'] <- "electronic_version"
#colnames(pure_uu)[colnames(pure_uu) == 'Open Access status-8'] <- "OA_status_pure"

# THESE ARE FROM ADJUSTED VERSION OF INPUT DATA AND FOR ADDITIONAL COLUMNS
colnames(pure_uu)[colnames(pure_uu) == 'Electronic version(s) of this work > DOI (Digital Object Identifier)-6'] <- "doi"
colnames(pure_uu)[colnames(pure_uu) == 'Electronic version(s) of this work > Document version-8'] <- "electronic_version"
colnames(pure_uu)[colnames(pure_uu) == 'Electronic version(s) of this work > Public access to file-9'] <- "public_access"
colnames(pure_uu)[colnames(pure_uu) == 'Open Access status-7'] <- "OA_status_pure"
colnames(pure_uu)[colnames(pure_uu) == 'Open Access embargo date-10'] <- "embargo_date"

## Add manual checks
pure_manual <- mutate(pure_manual,manual=substring(handmatig,1,1)) %>%
  select(manual,pure_id) %>%
  filter(manual=="A"|manual=="B"|manual=="C"|manual=="D")



# COL IDS FROM EARLIER VERSION
colnames(pure_umcu)[colnames(pure_umcu) == 'Organisations > Organisational unit-0'] <- "org_unit"
colnames(pure_umcu)[colnames(pure_umcu) == 'ID-4'] <- "pure_id"
colnames(pure_umcu)[colnames(pure_umcu) == 'Title of the contribution in original language-6'] <- "title"
colnames(pure_umcu)[colnames(pure_umcu) == 'Journal > ISSN-7'] <- "issn"
colnames(pure_umcu)[colnames(pure_umcu) == 'Electronic version(s) of this work > DOI (Digital Object Identifier)-9'] <- "doi"
#colnames(pure_umcu)[colnames(pure_umcu) == 'Electronic version(s) of this work-10'] <- "electronic_version"
colnames(pure_umcu)[colnames(pure_umcu) == 'Open Access status-11'] <- "OA_status_pure"

# ADJUSTED VERSION PLUS ADDITIONAL COLUMNS
colnames(pure_umcu)[colnames(pure_umcu) == 'Electronic version(s) of this work > Document version-12'] <- "electronic_version"
colnames(pure_umcu)[colnames(pure_umcu) == 'Open Access embargo date-14'] <- "embargo_date"


colnames(doaj)[colnames(doaj) == 'Journal ISSN (print version)'] <- "issn"
colnames(doaj)[colnames(doaj) == 'Journal EISSN (online version)'] <- "eissn"

colnames(HOOP)[colnames(HOOP) == 'Contributors > Organisations > Organisational unit-0'] <- "org_unit"
 
## Adjust data types

# Set ID as character, so that it will not be treated as a numeral
pure_uu$pure_id %<>% as.character
pure_umcu$pure_id %<>% as.character
pure_manual$pure_id %<>% as.character

# Set organisational unit, ISSN and OA status as factors because they are fixed variables which we want to analyze.
pure_uu$org_unit %<>% as.factor
pure_uu$issn %<>% as.factor
pure_uu$OA_status_pure %<>% as.factor

pure_umcu$org_unit %<>% as.factor
pure_umcu$issn %<>% as.factor
pure_umcu$OA_status_pure %<>% as.factor

## Verify data (uu only)
department_check(pure_uu$org_unit)

## Add manual checks
pure_uu <- left_join(pure_uu,pure_manual,by="pure_id")

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

## Step 3: PURE classification
pure_uu <- mutate(pure_uu,
                  pure_green = str_detect(electronic_version,"Accepted author manuscript")|
                    !is.na(embargo_date))
pure_umcu <- mutate(pure_umcu,
                  pure_green = str_detect(electronic_version,"Accepted author manuscript")|
                    !is.na(embargo_date))


## Step 4: Unpaywall
api_csv <- "csv" #indicate here whether you want to load existing data or use the UPW api

# generate a database with unpaywall data using their REST API
# use all DOIs as input
alldois <- union(pure_uu$doi[!is.na(pure_uu$doi)], # all pure_uu dois without NA
                 pure_umcu$doi[!is.na(pure_umcu$doi)]) # all UMCU dois without NA

# mine unpaywall API for each DOI
outlist <- list()

for(i in seq_along(alldois)){
  doi <- alldois[i]
  if(api_csv=="api"){ # only mine the api if the user wants to renew unpaywall data
    res <- upw_api(doi)
    outlist[[i]] <- res
  }
}

if(api_csv=="api"){
  unpaywall <- bind_rows(outlist)
  # save unpaywall data
  today <- as.character(Sys.Date())
  upwname <- paste0("data/unpaywall_",today,".csv")
  write_csv(unpaywall,upwname)
} else if(api_csv=="csv"){
  unpaywall <- read_csv(path_unpaywall)
}

# ensure unpaywall data is saved as factor
unpaywall$evidence %<>% as.factor
unpaywall$free_fulltext_url %<>% as.factor
unpaywall$license %<>% as.factor
unpaywall$oa_color %<>% as.factor

# merge pure with unpaywall
## CONSIDER JOIN IN UPDATE ##
uu_merge <- left_join(pure_uu,unpaywall,by="doi")
umcu_merge <- left_join(pure_umcu,unpaywall,by="doi")




#### CLASSIFICATION PIPELINE ####
## apply the classification function
uu_merge <- mutate(uu_merge,
                   OA_label=mapply(define_oa,
                                   DOAJ_ISSN_match,
                                   VSNU_doi_match,
                                   oa_color,
                                   OA_status_pure,
                                   pure_green,
                                   manual))
umcu_merge <- mutate(umcu_merge,
                     OA_label=mapply(define_oa,
                                     DOAJ_ISSN_match,
                                     VSNU_doi_match,
                                     oa_color,
                                     OA_status_pure,
                                     pure_green))

uu_merge <- mutate(uu_merge,
                   OA_label_detail=mapply(define_oa_detailed,
                                   DOAJ_ISSN_match,
                                   VSNU_doi_match,
                                   oa_color,
                                   OA_status_pure,
                                   pure_green,
                                   manual))
umcu_merge <- mutate(umcu_merge,
                     OA_label_detail=mapply(define_oa_detailed,
                                     DOAJ_ISSN_match,
                                     VSNU_doi_match,
                                     oa_color,
                                     OA_status_pure,
                                     pure_green))

## turn the results into factors
uu_merge$OA_label %<>% as.factor
umcu_merge$OA_label %<>% as.factor
uu_merge$OA_label_detail %<>% as.factor
umcu_merge$OA_label_detail %<>% as.factor


#### RESULTING TABLES AND FIGURES ####

# Merge both documents
all_pubs <- full_join(uu_merge,umcu_merge)
all_pubs$org_unit %<>% as.factor
all_pubs$OA_label_detail %<>% as.factor

# Make a field that indicates whether information is available.
# Information is available when there is a DOI, or when there is a confirmed VSNU ISSN.
# Or when OA_STATUS is green, this is likely from Pure info; also counts as available info
all_pubs <- mutate(all_pubs,
                   information = OA_label!="CLOSED"|is.na(electronic_version)|!is.na(doi_resolver))

# Label duplicates should be done per unique collection. 
# If possible it should use DOIs, but it might be that titles cover more ground.
# For this purpose:
# make a title field that does not have punctuation and uses lowercase only
all_pubs <- mutate(all_pubs,
                   title_lower = str_to_lower(str_replace_all(title.x, "[[:punct:]]", "")))

### Reporting:
# make subselection
# perform deduplication
# report fraction of absent information
## if above 5% of total: give a warning, and put the entries in a df for manual check
# report OA:
## total number of publications in four categories


# initialize a df with publications-to-check
checkthese <- NULL
# initialize lists to collect results
allresults <- list()




### ALL PUBLICATIONS ###
all_pubs_report <- deduplicate(all_pubs)
checkthese <- infocheck(all_pubs_report,checkthese)
allresults[["All publications"]] <- c(table(all_pubs_report$OA_label),table(all_pubs_report$OA_label_detail))


### PER FACULTY ####
all_faculties <- levels(all_pubs$org_unit)
all_faculties <- all_faculties[c(str_which(all_faculties, "Faculteit"),str_which(all_faculties,"UMC Utrecht"))]

for(f in all_faculties){
  subset <- filter(all_pubs, org_unit==f)
  subset_report <- deduplicate(subset)
  checkthese <- infocheck(subset,checkthese)
  allresults[[f]] <- c(table(subset_report$OA_label),table(subset_report$OA_label_detail))
}

### PER HOOP-AREA ###
HOOP <- read_excel("data/HOOPgebieden-test.xlsx")
colnames(HOOP)[colnames(HOOP) == 'Contributors > Organisations > Organisational unit-0'] <- "org_unit"

hoop_areas <- levels(as.factor(HOOP$HOOP))
hoop_areas <- hoop_areas[hoop_areas!="n.v.t."]

for(h in hoop_areas){
  # which departments are included?
  depts <- filter(HOOP, HOOP==h) %>% pull(org_unit)
  subset <- filter(all_pubs, org_unit%in%depts)
  subset_report <- deduplicate(subset)
  # save records for manual checks in case incomplete information exceeds 5%
  checkthese <- infocheck(subset,checkthese)
  allresults[[h]] <- c(table(subset_report$OA_label),table(subset_report$OA_label_detail))
}

allresults <- bind_rows(!!!allresults, .id="id")
checkthese <- deduplicate(checkthese)

lbls <- c("CLOSED","GOLD","GREEN","HYBRID")
neworder <- c(1,3,4,2)
piecols <- c("gray88","gold1","chartreuse3","orange3")
piecols <- piecols[neworder]

## MAKE PIECHARTS ## 
for(n in 1:nrow(allresults)){
  title <- allresults[n,"id"]
  slices <- unlist(allresults[n,lbls],use.names=F)
  ns <- paste0("n=",slices)
  labs <- paste(lbls,ns)
  # reorder
  ns <- ns[neworder]
  labs <- labs[neworder]
  slices <- slices[neworder]
  # name
  ttl <- str_replace(title," ","_")
  ttl <- str_replace(ttl,"&","en")
  fn <- paste0("img/",ttl,".png")
  png(filename=fn,width=750,height=750,res=130)
  pie(slices,labels=ns,main=title,init.angle=90,col=piecols,border=0)
  dev.off()
}




#write_csv(checkthese,"data/checkthese.csv")
write_csv(allresults,"data/allresults.csv")
#filter(all_pubs, org_unit=="Faculteit Sociale Wetenschappen") %>% write_csv("data/fsw.csv")

write_csv(all_pubs,"data/finaldataset.csv")
write_csv(all_pubs_report,"data/finaldataset_deduplicated.csv")




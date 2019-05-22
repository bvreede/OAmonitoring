## load libraries
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)
library(magrittr)
library(ggplot2)


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




write_csv(checkthese,"data/checkthese.csv")
write_csv(allresults,"data/allresults.csv")
filter(all_pubs, org_unit=="Faculteit Sociale Wetenschappen") %>% write_csv("data/fsw.csv")

write_csv(all_pubs,"data/finaldataset.csv")
write_csv(all_pubs_report,"data/finaldataset_deduplicated.csv")




#### Step two of the Open Access monitoring pipeline
#### Cleaned and indexed files are read and interpreted
#### with this script to write the reports.
#### Input required from the user: a config
#### file with the organisation units that need
#### to be reported on.
####
#### Written by: Barbara Vreede
#### Contact: b.vreede@gmail.com


## load libraries
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(jsonlite)
library(httr)
library(magrittr)
library(ggplot2)
library(docstring)
library(testthat)


## read data
# read all csv files in the output folder
infiles = list.files(path = "./output", pattern="*_clean.csv")
infiles = paste0("output/",infiles)
myfiles = lapply(infiles, read_csv)
# add source filename to myfiles
for(i in 1:length(infiles)){
  myfiles[[i]] <- myfiles[[i]] %>% mutate(source = infiles[i])
}
# bind all together to one large dataframe
df <- bind_rows(myfiles)


report <- function(nrecords_post,nrecords,method){
  cat("Deduplicating by ")
  cat(method)
  cat(" from ")
  cat(nrecords)
  cat(" records, ")
  cat(nrecords-nrecords_post)
  cat(" were deleted.")
  cat(" This df now contains ")
  cat(nrecords_post)
  cat(" records.")
  cat("\n\n")
}


deduplicate_method <- function(df,method){
  #' @title Deduplicate publication entries
  #' 
  #' Deduplication function that takes a dataframe and returns
  #' the deduplicated dataframe based on either the combination of source file and
  #' their internal ID in that source, or DOI.
  #' Reports on the deduplication performed.
  #' @param df The dataframe that needs to be deduplicate
  #' @param method The deduplication method, can be either "doi" or "internal"
  #' @return The deduplicated dataframe
  ### tests ###
  # input is a dataframe
  expect_that(df, is_a("data.frame"))
  # column doi exists
  # expect_true("doi"%in%names(df))
  # column doi does not contain NA
  # expect_that(df$doi )
  ###############
  nrecords <- nrow(df)
  if(method == "internal"){
    df <- distinct(df,system_id,source,.keep_all = T)
    method_print = "internal ID"
  } else if(method == "doi"){
    df <- distinct(df,doi,.keep_all = T)
    method_print = "DOI" 
  }
  nrecords_post <- nrow(df)
  report(nrecords_post,nrecords,method_print)
  return(df)
}


deduplicate <- function(df){
  #' @title Deduplication of publication entries
  #' 
  #' Deduplication function that uses a single dataframe and applies a variety
  #' of deduplication functions to subsets of the dataframe.
  #' Required to use before reporting on a subset of publications, so that no bias is created
  #' in the results if a publication is entered multiple times by different groups, eg.
  #' @param df The dataframe that needs to be deduplicated.
  #' @return The deduplicated dataframe.
  #################################
  # first determine whether there is a mix of multiple source files
  sourcefiles <- df$source %>% as.factor() %>% levels()
  # if a single source file has been used, deduplication can be performed on system ID
  if(length(sourcefiles) < 2){ 
    df <- deduplicate_method(df,method="internal")
  }
  else{
    # separate between entries with and without doi
    df_nondoi <- df %>% filter(is.na(doi))
    df_doi <- df %>% filter(!is.na(doi))
    # deduplicate both individually
    df_doi <- deduplicate_method(df_doi,method="doi")
    df_nondoi <- deduplicate_method(df_nondoi,method="internal")
    # combine
    df <- full_join(df_doi,df_nondoi)
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



reporting <- read_file("./config/rapportage_faculteit.txt") %>% str_split("\n")
reporting <- reporting[[1]]

#count_data_results <- data.frame()

for(r in reporting){
  units = str_split(r, ", ")[[1]]
  df_r <- df %>% filter(org_unit%in%units) %>% deduplicate()
  #dplyr::count(df_r, OA_label) %>% print()
  countdata <- data.frame (dplyr::count(df_r, OA_label))
#  count_data_results <- rbind(countdata)
  outfilename <-paste("./output/",r,"countdata.csv")
  write_csv(countdata,outfilename)
}





#### BELOW TAKEN FROM PIPELINE ####

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




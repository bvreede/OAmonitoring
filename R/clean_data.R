open_everything <- function(allfiles){
  alldata <- list()

  for(col in allfiles[2:length(allfiles)]){
    # extract file name and extension
    fn <- col[allfiles$File_info=="Filename"]
    fn_ext <- col[allfiles$File_info=="Format (tsv, csv, xls, or xlsx)"]
    fn_ext <- str_replace(fn_ext,'[:punct:]','')

    # test if the column contains NAs; in this case the file will not be read
    if(sum(is.na(col))>1){
      warning("The information for file ", fn, " is not filled out. This file cannot be processed.\n")
      next
    } 
    # skip filenames without valid extensions
    if(!fn_ext %in% c("xlsx","xls","csv","tsv")){
      warning("The filename ", fn, " does not have a valid extension provided. This file cannot be processed.\n")
      next
    }
    
    # open the file, clean columns, and save to the alldata list
    alldata[[fn]] <- open_clean(col)
  }
  
  # remove excess variables, bind to dataframe
  df <- bind_rows(alldata)
  
  # check the system IDs for duplicates
  system_id_check(df)
  
  return(df)
}

read_ext <- function(fn, ext="", dir="data/"){
  # opening a file, with method depending on the extension
  # extract extension and put together filename
  if(ext == ""){
  fn_ext <- str_split(fn,"\\.")[[1]]
  ext <- fn_ext[-1]
  }
  
  # only paste on the data directory if the path itself does not exist
  if(file.exists(fn)){
    fn_path <- fn
  } else{
    fn_path <- paste0(dir,fn)
  }
  
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
    df <- read_delim(fn_path,delim="\t", escape_double = FALSE, trim_ws = TRUE)
  } else if(ext=="xls"|ext=="xlsx"){
    df <- read_excel(fn_path)
  }
  return(df)
}

column_rename <- function(data,col_config){
  # rename column names
  id_column <- col_config[allfiles$File_info=="Internal unique identifier"]
  issn_column <- col_config[allfiles$File_info=="ISSN"]
  eissn_column <- col_config[allfiles$File_info=="EISSN (electronic ISSN)"]
  doi_column <- col_config[allfiles$File_info=="DOI"]
  org_column <- col_config[allfiles$File_info=="Departments and/or faculties"]
  colnames(data)[colnames(data) == id_column] <- "system_id"
  colnames(data)[colnames(data) == issn_column] <- "issn"
  # if there is no EISSN, generate a column
  if(!is.na(colnames(data) == eissn_column)){
    colnames(data)[colnames(data) == eissn_column] <- "eissn"
  } else{
      data <- mutate(data, eissn = NA)
    }
  colnames(data)[colnames(data) == doi_column] <- "doi"
  colnames(data)[colnames(data) == org_column] <- "org_unit"
  # return renamed data
  return(data)
}

select_columns <- function(data,col_keep){
  data <- data %>%
    select(system_id,
           issn,
           eissn,
           doi,
           org_unit,
           all_of(col_keep))
  return(data)
}

number_to_issn <- function(number){
  # ensure ISSN has two elements, with a hyphen in between
  if(is.na(number)){
    return(NA)
  }
  if(str_length(number)!=8){
    return(NA)
  }
  part1 <- str_sub(number, start = 1L, end = 4L) 
  part2 <- str_sub(number, start = 5L, end = 8L) 
  return(paste0(part1,"-",part2))
}

# cleaning DOIs and ISSN columns
clean_issn <- function(column){
  column <- str_replace(column,'\\s+','') #remove spaces from ISSN
  column <- str_replace_all(column,'[:punct:]','') #remove all punctuation
  # ensure ISSN has two elements, with a hyphen in between
  column <- mapply(number_to_issn,column)
  return(column)
}

clean_doi <- function(column){
  column <- str_extract(column,"10\\..+") #ensure only dois are kept, without url information
  column <- str_replace(column,'\\s+','') #remove spaces from DOI
  column <- tolower(column) #Change DOI to lowercase only
  column <- str_replace(column,",.+","") #remove duplicate DOIs separated with a comma
}

open_clean <- function(col_config){
  # extract file name
  fn <- col_config[allfiles$File_info=="Filename"]
  fn_ext <- col_config[allfiles$File_info=="Format (tsv, csv, xls, or xlsx)"]
  fn_ext <- str_replace(fn_ext,'[:punct:]','')
  
  # what columns to keep?
  col_keep <- col_config[allfiles$File_info=="Other columns to include"]
  col_keep <- str_split(col_keep,", ") %>% unlist()
  
  # open the file and adjust the column names to the config input
  df <- read_ext(fn,ext=fn_ext) %>% 
    column_rename(col_config)
  
  # reduce number of columns, except when the user wants to keep all
  if(!col_keep=="all"){
    df <- select_columns(df, col_keep)
  }
  
  # clean DOI and ISSN, remove spaces and hyperlinks, change uppercase to lowercase etc.
  # also add source file column
  df <- df %>% mutate(issn = clean_issn(issn),
                      eissn = clean_issn(eissn),
                      doi = clean_doi(doi),
                      source = fn)
  
  return(df)
}


system_id_check <- function(df){
  #' Checks that there are no duplicate system IDs between
  #' multiple source files, as this may be accidental and cause problems
  #' with deduplication and other assignments later on.
  sources <- df$source %>% 
    as.factor() %>% 
    levels()
  all_ids <- c()
  duplicates <- c()
  for(s in sources){
    ids <- df %>% 
      filter(source == s) %>% 
      pull(system_id)
    duplicates <- c(duplicates,ids[ids%in%all_ids])
    all_ids <- c(all_ids,ids)
  }
  if(length(duplicates) > 0){
    df %>% filter(system_id%in%duplicates) %>%
      write_csv("output/confirm_duplicate_IDs.csv")
    warning("
  Duplicate IDs exist between different imported datasets.
  Please ensure that these refer to the same files.
  For your convenience, all lines corresponding to duplicate IDs are saved in output/confirm_duplicate_IDs.csv")
  }
}

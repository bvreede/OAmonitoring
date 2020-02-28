read_ext <- function(fn, dir="data/"){
  # opening a file, with method depending on the extension
  # extract extension and put together filename
  fn_ext <- str_split(fn,"\\.")[[1]]
  ext <- fn_ext[-1]
  fn_path <- paste0(dir,fn)
  
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
  # ensure ISSN has two elements, with a hyphen in between
  if(column[5]!='-'){
    column <- paste0(column[1:4],"-",column[5:8])
  }
  return(column)
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
  column <- str_replace(column,'https://','') #remove https:// from DOI
  column <- str_replace(column,'doi.org/','') #remove doi.org/ from DOI
  column <- str_replace(column,'\\s+','') #remove spaces from DOI
  column <- tolower(column) #Change DOI to lowercase only
  column <- str_replace(column,",.+","") #remove duplicate DOIs separated with a comma
}


open_clean <- function(fn){
  # open the file and adjust the column names to the config input
  df <- read_ext(fn) %>% column_rename(col)
  
  # clean DOI and ISSN, remove spaces and hyperlinks, change uppercase to lowercase etc.
  # also add source file column
  df <- df %>% mutate(issn = clean_issn(issn),
                      doi = clean_doi(doi),
                      source = fn)
  
  return(df)
}

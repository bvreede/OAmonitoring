################################## DEDUPLICATION #########################################
#'@title Report on deduplication effort
report <- function(nrecords_post,nrecords,method){
  message <- paste("Deduplicating by",
                   method,
                   "from",
                   nrecords,
                   "records,",
                   nrecords-nrecords_post,
                   "were deleted.\n",
                   "This df now contains",
                   nrecords_post,
                   "records.\n\n")
  cat(message)
}

#' @title Deduplicate publication entries
#' 
#' Deduplication function that takes a dataframe and returns
#' the deduplicated dataframe based on either the combination of source file and
#' their internal ID in that source, or DOI.
#' Reports on the deduplication performed.
#' @param df The dataframe that needs to be deduplicate
#' @param method The deduplication method, can be either "doi" or "internal"
#' @return The deduplicated dataframe
deduplicate_method <- function(df,method){
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

#' @title Deduplication of publication entries
#' 
#' Deduplication function that uses a single dataframe and applies a variety
#' of deduplication functions to subsets of the dataframe.
#' Required to use before reporting on a subset of publications, so that no bias is created
#' in the results if a publication is entered multiple times by different groups, eg.
#' @param df The dataframe that needs to be deduplicated.
#' @return The deduplicated dataframe.
deduplicate <- function(df){
  # first determine whether there is a mix of multiple source files
  sourcefiles <- df$source %>% as.factor() %>% levels()
  # if a single source file has been used, deduplication can be performed on system ID
  if(length(sourcefiles) < 2){ 
    df <- deduplicate_method(df,method="internal")
  }
  else{
    # ensure there are only atomic columns in the dataset
    df <- df %>% select_if(is.atomic)
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

#' Deduplicate a dataframe for each level org_unit
#' 
#' Resulting dataframe ensures no duplication exists within
#' organization units, but duplication remains between.
deduplicate_per_unit <- function(df){
  df_dedup <- NULL
  for(unit in levels(as.factor(df$org_unit))){
    df_sub <- df %>%
      filter(org_unit==unit) %>%
      deduplicate()
    df_dedup <- bind_rows(df_dedup,df_sub)
  }
  return(df_dedup)
}

############################### REQUEST CUSTOMIZATION ###############################
check_all <- function(df){
  checkthese <- NULL
  checkthese <- infocheck(df,checkthese)
  
  for(cat in levels(as.factor(df$org_unit))){
    df_temp <- df %>% filter(org_unit==cat)
    checkthese <- infocheck(df_temp,checkthese)
  }
  
  outname <- paste0("output/check_these_manually_",lubridate::today(),".csv")
  checkthese %>% deduplicate() %>% write_csv(outname)
}

infocheck <- function(df,checkthese){
  df <- deduplicate(df)
  f_mis <- sum(df$OA_label_explainer=="NONE")/nrow(df)
  if(f_mis>cutoff_missing){
    checkthese <- rbind(checkthese,filter(df,OA_label_explainer=="NONE"))
  }
  return(checkthese)
}

#################################### DATA REFORMATTING FOR REPORT ###########################
get_first_word <- function(sentence){
  words <- str_split(sentence, " ")
  first_word <- words[[1]][1]
  return(first_word)
}

reduce_categories <- function(df){
  df <- df %>% mutate(
    OA_label_explainer_short = mapply(get_first_word,OA_label_explainer)
  )
  return(df)
}

###################################### REPORTING #####################################
report_to_dataframe <- function(df){
  ## Write a general report for the entire dataset
  df_report <- df %>%
    group_by(org_unit, OA_label) %>% 
    summarise(n_papers = n())
  # deduplicate the dataset and score irrespective of org_unit
  df_all <- df %>%
    deduplicate() %>% 
    group_by(OA_label) %>% 
    summarise(n_papers = n()) %>% 
    mutate(org_unit = "all")
  # add all columns to the report
  df_report <- bind_rows(df_report,df_all)
  # transform the data
  df_report <- df_report %>% pivot_wider(names_from=OA_label,values_from=n_papers)
  # add percentages
  df_report <- df_report %>% mutate(
    Total_papers = sum(CLOSED,GOLD,GREEN,HYBRID, na.rm=T),
    gold_percent = round(GOLD/Total_papers*100,1),
    hybrid_percent = round(HYBRID/Total_papers*100,1),
    green_percent = round(GREEN/Total_papers*100,1),
    total_OA_percent = round((1 - CLOSED/Total_papers)*100,1)
  )
  # generate a report for explainer column
  df_explainer <- df %>%
    group_by(org_unit, OA_label_explainer) %>%
    summarise(n_papers = n())
  # deduplicate the dataset and score irrespective of org_unit - for explainer
  df_explainer_all <- df %>%
    deduplicate() %>% 
    group_by(OA_label_explainer) %>% 
    summarise(n_papers = n()) %>% 
    mutate(org_unit = "all")
  # combine to single dataframe
  df_report_explainer <- bind_rows(df_explainer,df_explainer_all)
  # transform the data
  df_report_explainer <- df_report_explainer %>% pivot_wider(names_from=OA_label_explainer,values_from=n_papers)
  # join both reports
  df_report <- left_join(df_report,df_report_explainer,by="org_unit")
  return(df_report)
}


##################################### IMAGING ###############################################

report_to_image <- function(df,title){
  oacols <- c("gray88","chartreuse3","orange3","gold1")
  title_slug <- str_replace(title," ","_")
  outfile <- paste0("output/plot_",title_slug)
  out_prop <- paste0(outfile,"_prop_",lubridate::today(),".png")
  out_num <- paste0(outfile,"_number_",lubridate::today(),".png")
  
  # ensure levels of df are in order: closed/green/hybrid/gold
  df$OA_label <- factor(df$OA_label, levels = c("CLOSED","GREEN","HYBRID","GOLD"))
  
  p <- ggplot(df, aes(x = org_unit, fill = OA_label)) +
    scale_fill_manual(values = oacols) +
    theme_bw() +
    labs(title = paste("Accessibility for",title,"publications"),
      x = "",
      fill = "Access type") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  # PLOT PROPORTION
  plot_prop <- p +
    ylab("proportion of papers") +
    geom_bar(position="fill")
  
  ggsave(filename = out_prop, plot = plot_prop, device=png())
  dev.off()
  
  # PLOT ACTUAL NUMBER
  plot_num <- p + 
    ylab("number of papers") +
    geom_bar()
  
  ggsave(filename = out_num, plot = plot_num, device=png())
  dev.off()
}

#' Make an alluvial diagram with the data
report_to_alluvial <- function(df,name){
  oacols <- c("gray88","chartreuse3","orange3","gold1")
  
  title_slug <- str_replace(name," ","_")
  outfile <- paste0("output/alluvial_",title_slug,"_",lubridate::today(),".png")
  
  df_sum <- df %>%
    reduce_categories() %>%
    deduplicate() %>%
    group_by(org_unit,OA_label,OA_label_explainer_short) %>%
    summarise(n_papers = n()) %>%
    # ensure levels of df are in order: closed/green/hybrid/gold
    as.data.frame() %>%
    mutate(
      OA_label = fct_relevel(OA_label, "CLOSED","GREEN","HYBRID","GOLD"),
      OA_label_explainer_short = fct_relevel(OA_label_explainer_short, "VSNU","DOAJ",after=Inf),
      OA_label_explainer_short = fct_relevel(OA_label_explainer_short,"NONE")
    )
  
  plt_alluv <- ggplot(df_sum,
         aes(y = n_papers,
             axis1 = OA_label_explainer_short, axis2 = OA_label)) +
    geom_alluvium(aes(fill = OA_label),
                  width = 0, knot.pos = 0, reverse = FALSE) +
    guides(fill = FALSE) +
    geom_stratum(width = 1/8, reverse = FALSE) +
    geom_text(stat = "stratum", infer.label = TRUE, reverse = FALSE) +
    scale_x_continuous(breaks = 1:2, labels = c("OA Strategy", "OA Status")) +
    scale_fill_manual(values = oacols) +
    theme_bw() +
    labs(title = paste("Open Access publication strategies for", name),
         y = "Number of papers")
  
  ggsave(filename = outfile, plot = plt_alluv, device=png())
  dev.off()
}


open_reporting_file <- function(path){
  reporting <- read_excel(path)
  reporting <- reporting[2:ncol(reporting)]
  return(reporting)
}

commandline_report <- function(name){
  name_upper <- str_to_upper(name)
  message <- paste(
    "\n\n#### GENERATING REPORT FOR",
    name_upper,
    "####\n\n"
    )
  cat(message)
}

#' Generate a full report
#' 
#' Function to generate both a dataset and corresponding
#' figure, reporting on the classification found
#' in the data. Requires a name to save the report and figure.
full_report <- function(df,name="all"){
  commandline_report(name)
  name_slug <- str_replace(name," ","_")
  outfilename <- paste0("./output/report_",name_slug,"_",lubridate::today(),".csv")
  df <- deduplicate_per_unit(df)
  report_to_dataframe(df) %>% write_csv(outfilename)
  report_to_image(df,name)
  report_to_alluvial(df,name)
}

#' Generate many individual reports
#' 
#' The custom-made reporting sheet is used to
#' write individual reports and figures for
#' each set of units in the reporting sheet.
individual_reports <- function(path_report){
  reporting <- open_reporting_file(path_report)
  for(r in seq_along(reporting)){
    name <- colnames(reporting)[r]
    col <- pull(reporting, name)
    units <- col[!is.na(col)]
    df_r <- df %>% filter(org_unit%in%units)
    full_report(df_r,name)
  }
}

################################### HOOP AREAS #######################################

#' Report for HOOP areas
#' 
#' Go over HOOP areas that were filled out with existing organization units
#' and generate reports on the areas overall.
hoop_report <- function(df){
  hoopfile <- read_ext(path_hoop,dir="")
  for(h in seq_along(hoopfile)){
    name <- colnames(hoopfile)[h]
    col <- pull(hoopfile, name)
    units <- col[!is.na(col)]
    if(length(units) < 1){next}
    df <- df %>% mutate(
      org_unit = case_when(
        org_unit %in% units ~ name,
        TRUE ~ org_unit
      )
    )
  }
  df <- df %>%
    # remove any remaining org_unit entries that were not replaced
    filter(org_unit %in% colnames(hoopfile))
  full_report(df,name="HOOP")
}




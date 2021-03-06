# PATHS TO INPUT AND CONFIG FILES
path_vsnu <- "data/mockdata_VSNUdeal.xlsx"
path_custom <- "data/custom_labels.xlsx"
path_doaj <- "data/clean/doaj_from_issn_2020-02-27.csv"
path_upw <- "data/clean/upw_from_doi_2020-02-27.csv"
path_hoop <- "config/HOOP_areas.xlsx"
path_report <- "config/reports.xlsx"
path_allfiles <- "config/config_pub_files.xlsx"

#Fill in a valid email for use in the Unpaywall API
email_address <- "b.m.i.vreede@uu.nl" 

# Fill in cutoff for checking missing info
cutoff_missing <- 0.05

# What year is the report for?
report_year <- 2019

# For Unpaywall and the DOAJ:
# Do you want to re-run API or use saved data?
# Be sure when using saved data that this was acquired using the
# same input data as you now want to use!
use_doaj <- "saved" # "saved" or "api"
use_upw <- "saved" # "saved" or "api"

# Do you want to add custom entries (green OA)? For yes: enter TRUE.
customized <- TRUE

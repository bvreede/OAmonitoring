# Reporting Open Access output

This project takes publication data from a single year and determines per article its open access status, using various sources available. It uses peer reviewed journal articles registered in CRIS systems as input. It was created in a collaboration between Utrecht University Library and Leiden University Library. Specifics for Utrecht and Leiden can be found in the Wiki.

## Installation instructions

### Software and packages required
The program runs in R through Rstudio. Ensure you have both installed:
1. Install R via [cran.rstudio.com](https://cran.rstudio.com/)
1. Install Rstudio via [rstudio.com](https://www.rstudio.com/products/rstudio/download/#download)

The following packages are required:
- tidyverse
- stringr
- readxl
- jsonlite
- httr
- magrittr
- lubridate
- docstring
- testthat

You can install them in Rstudio by running the following in the console:
```r
install.packages("tidyverse")
install.packages("stringr")
install.packages("readxl")
install.packages("jsonlite")
install.packages("httr")
install.packages("magrittr")
install.packages("lubridate")
install.packages("docstring")
install.packages("testthat")
```

### Download and open the project
1. Download this project [through this zipfile](https://github.com/bvreede/OAmonitoring/archive/master.zip).
1. Unzip the download.
1. Open `OAmonitoring.rproj`, which will launch Rstudio.

## Running the Open Access Monitor on your own data
### Prepare your data
Multiple data files can be processed together in this pipeline. Formats can be `.xls`, `.xlsx`, `.tsv`, and `.csv`.

Your data will need to have the following columns to successfully make it through the pipeline:
- Journal ISSN
- paper DOI
- A unique ID (given by your CRIS)
- Organization unit (e.g. faculty, department, etc)

NB: The columns may have missing data, but must exist.
The exception to this is the unique internal ID, which must be complete. 

1. Place your input file(s) in the folder `data`.
1. Open the file `config_pub_files.xlsx` (in the folder `config`) in Excel.
1. For each input file, fill out the grid:
  - Place the file name of an input file in the field 'filename'.
  - Place the header of each column that contains the data needed in the fields below.
  - If there is only a single column with ISSN or EISSN, ensure it is placed in the ISSN field.
  - Indicate in the last field what additional columns you want to retain in the output file. If you want to keep all columns, put 'all'; otherwise make a selection of columns, and separate them with a comma and a space (e.g. `Publisher, Authors`).
  - NB: Don't forget to remove the examples that are currently present in the file! This configuration file may have only data in the columns that describe your own input data, and must be empty otherwise.

### Prepare the VSNU list
This is an excel sheet with DOIs of papers published under the OA Publishing deals through the VSNU, that is available on request (contact b.m.i.vreede@uu.nl if you need this list).
1. Place the file in the folder `data`.
1. Open the file `config.R` (in the folder `config`). You can do this directly in Rstudio.
1. Ensure the location of the file and the correct file name are placed behind the variable `path_vsnu`.

### Set preferences and configurations
1. Change the email address in `config.R` to your own.
1. If you are running this for the first time: make sure the variables `use_doaj` and `use_upw` are set to `api`.
1. Once the script has run successfully, and files have been generated with the results from this api (they will show up in the folder `data/clean`), you can edit the paths under `path_doaj` and `path_upw` to reflect the location and name of these saved files. The variables `use_doaj` and `use_upw` can now be set to `saved`.

### Specify the reports
#### HOOP areas
Reporting publication output needs to happen for each HOOP-area (this is the division of Dutch Higher Education and Research into eight categories: Landbouw (Agriculture), Natuur (Nature), Techniek (Technique), Gezondheid (Health), Gedrag en Maatschappij (Behaviour and Society), Economie (Economics), Recht (Law), Taal en Cultuur (Language and Culture). Each faculty or department is assigned to one HOOP category. 

You can edit the file `HOOP_areas.xlsx`, in the folder `config`, to contain the names of organization units (e.g. departments or faculties) that are classified under each HOOP area.
Ensure all names are spelled identical to their appearance in your data.
(Don't forget to remove the mock data entries here.)

#### Custom reports
Edit the file `reports.xlsx`, in the folder `config`, with any custom reports you may want to generate. 
The title of the report is up to you, but the organization units included must contain names spelled identically to the organization units in your data.
For example: a report for the Faculty of Science would look like this:

| **Title of the report** | Faculty of Science |
| --------------------:|:----- |
| **Organization units included** | Departement Biologie |
| | Departement Scheikunde |
| | Departement Natuurkunde |
| | Departement Informatica |

All reports can be written up in this file, simply by adding more columns.

### Run the project
Open `reporting_pipeline.Rmd`, and run the code.
You can run this item by item by clicking on the green triangle at the top-right corner of every "chunk", or run all at once by clicking the down-arrow next to the button `-> Run`, and selecting `Run All`.

Results of your run will appear in the folder `output`.

## What data is used, and how?
### Gathering OA information
The pipeline harvests OA information from the following sources:
- Directory of Open Access Journals, ([DOAJ](http://doaj.org/))
- [Unpaywall](http://unpaywall.org/)
- Use of VSNU Open Access publishing deals (an excel sheet available through the VSNU)

### Assigning OA status
Each paper in your data will get a OA label assigned based on the OA information above, and applied in sequence:

1. Is the journal present in the DOAJ? -> label: GOLD
1. Is the DOI present in the VSNU list? -> label: HYBRID
1. Is the DOI classified as gold or hybrid by Unpaywall? -> label: HYBRID
1. *(Optional: is the system ID present in a custom list supplied by the user? -> label: GREEN)*
1. Is the DOI classified as green by Unpaywall? -> label: GREEN

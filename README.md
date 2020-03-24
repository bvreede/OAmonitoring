# Monitoring Open Access at UU in 2019
This project takes publication data from a single year at Utrecht University / UMC Utrecht, and determines per article its open access status, using various sources available. It uses peer reviewed journal articles registered in PURE at UU and UMCU as input.

## UU and UMCU output

UU and UMCU peer reviewed articles are retrieved from the CRIS (both organizations have a separate PURE CRIS). Information retrieved from Pure includes the following fields that are essential for the OA monitor.  
1. ID: for deduplication and returning results from manual check
2. Organisations > Organisational unit: for reporting purposes (faculty/HOOP).  
3. Title of the contribution in original language: for performing manual checks
4. Journal > ISSN: for DOAJ data retrieval
5. Electronic version(s) of this work > DOI (Digital Object Identifier): for VSNU and UPW data retrieval
6. Open Access status: for manual check
7. Electronic version(s) of this work > Document version: for manual check
8. Electronic version(s) of this work > File > File name: for manual check



### UU Pure Data
Source: Functioneel Beheer Pure, via M&A  
Date: March 9th 2020  
Content: All peer reviewed journal articles with publication year 2019 for each organisational unit (duplicate articles when co-authors are from different organisational units)

### UMCU Pure Data
Source: Functioneel Beheer Pure, via M&A  
Date: March 9th 2020  
Content: All peer reviewed journal articles with publication year 2019

## Gathering OA information
The pipeline harvests OA information from the following sources:

### DOAJ Data
Source: On SURF Drive, OA Monitoring  
Date: December 31st 2019  
Content: all journals listed on DOAJ and therefore labeled as Full OA  
Result: doaj = TRUE/FALSE

### VSNU data
Source: compiled from VSNU OA data on Surfdrive, OA Deals (2016 up until january 2020)  
Date: March 4th 2019  
Content: Cumulative list of all OA articles published within the Netherlands (not restricted to Utrecht) as part of the VSNU OA deal, including DOI, publisher and publication year.  
Result: vsnu = TRUE/FALSE

### Unpaywall data
OA status according to Unpaywall, retrieved using their API (http://unpaywall.org/products/api).  
Result: ups = bronze/closed/doaj/gold/green/hybrid/NA

### OA data in Pure
Source: data available within UU/UMCU Pure report  
Date: March 9th 2019  
Content:  
Taverne keyword in Pure, used to determine Green OA Status and identify the Taverne share in Green OA.  
OA Status in Pure and file and file availability in repository. OA status is determined by employees of Utrecht University Library who manually check each publication added to Pure, add the full text and determine if it is OA available (Open/Embargoed/Closed).  
OA status is at this moment not part of the pipeline but is being used to identify items for the manual check.  
Result: taverne = TRUE/FALSE

## Assigning OA labels
Each item has a OA label assigned based on the OA information harvested in a specific order:

doaj=TRUE: GOLD | DOAJ  
vsnu=TRUE: HYBRID | VSNU  
upw=BRONZE: CLOSED | UPW (bronze)  
upw=GOLD: HYBRID | UPW (gold)  
upw=HYBRID: HYBRID | UPW (hybrid)
upw=GREEN: GREEN | UPW (GREEN)  
upw=CLOSED: CLOSED | UPW (closed)  
None of the above: CLOSED | NONE  

## UU specific checks, not part of the pipeline UU Only


### OA status and file availability in Pure

taverne=TRUE: GREEN | TAVERNE  
All items that have OA status = OPEN in Pure and have a file attached in the repository are considered OA available, either Hybrid or Green.  
cris_green=TRUE: GREEN | CUSTOM  

### Other items
All other items with the OA label CLOSED | NONE, where these items represent at least 5% of the total number of titles within that organizational unit, are checked for OA availability using a Google / Google Scholar search.  
1. OA article in journal: HYBRID 
2. OA article in trusted repository: GREEN

## Reporting OA status

The OA status (GOLD/HYBRID/GREEN/CLOSED) is reported regarding three organizational levels:  
- UU/UMCU total  
- For each faculty  
- For each HOOP-gebied (the division of Dutch Higher Education and Research into eight categories: Landbouw (Agriculture), Natuur (Nature), Techniek (Technique), Gezondheid (Health), Gedrag en Maatschappij (Behaviour and Society), Economie (Economics), Recht (Law), Taal en Cultuur (Language and Culture). Each faculty or department is assigned to one HOOP category. 

## Future or bespoke use of this script
When re-using this script, adjust the following things:

- At the top of the script, adjust the file paths for your source data (use of a `data` folder is strongly recommended);  
- Consider that this script was written for two datasets (UU and UMCU); they are labeled as such in the script. If you have a single source file you may need to manually adjust the script to reflect this, however, you can also get in touch if you need a single source file version of this script, and are unable to adjust the script yourself;  
- Adjust the column names in the section 'renaming columns ...', to reflect those used in your source files.  
- In the `upw_api` function, adjust the email address to reflect the current user; 
- In the 'unpaywall' section of 'OA labelling', choose 'api' or 'csv' for the variable `api_csv`, depending on whether you want to load existing unpaywall data ('csv'), or re-run the unpaywall analysis via their api ('api')



## Wishlist
- Test the sources for quality
- Consider making a table that would allow user to input column names. E.g. title = 'Title of the contribution in original language-2'; this way, that information does not have to be hard coded but can be provided by a user less comfortable in R.
- What happens if we use the Core db instead of Unpaywall? https://core.ac.uk/services/api/

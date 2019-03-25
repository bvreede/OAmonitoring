# Monitoring Open Access at UU in 2018
This project takes publication data from a single year at Utrecht University / UMC Utrecht, and determines per article its open access status, using various sources available. It uses peer reviewed journal articles registered in PURE at UU and UMCU as input.

## UU and UMCU output:

### UU Pure Data
Source: Functioneel Beheer Pure, via M&A
Date: March 1st 2019
Content: All peer reviewed journal articles with publication year 2018 for each organisational unit (duplicate articles when co-authors are from different organisational units)

### UMCU Pure Data
Source: Functioneel Beheer Pure, via M&A
Date: March 1st 2019
Content: All peer reviewed journal articles with publication year 2018

## Matching to obtain Open Access status
The pipeline tries to classify all publications according to its presence or absence in various check lists. In sequence:

1. match the journal ISSN with a list from the Directory of Open Access Journals (DOAJ). If the journal matches, the publication is Gold OA
2. match the DOI with a list obtained from VSNU. If the journal matches, the publication is Hybrid OA
3. obtain the OA status from Unpaywall. If the status is publisher, the publication is Hybrid OA. If the status is repository, the publication is Green OA.
4. obtain the OA status from Pure. If the status is Open, the publication is Hybrid OA, unless
4a. a version of the publication is Open, but not the publisher version: the publication is Green OA.
4b. the publisher version is open, but was previously under closed under embargo: the publication is Green OA.
4. if publication matches none of the above, the publication has an unknown OA status and is considered closed.

### DOAJ Data
Source: On SURF Drive, OA Monitoring
Date: December 31st 2018
Content: all journals listed on DOAJ and therefore labeled as Full OA

### VSNU data
Source: compiled from VSNU OA data on Surfdrive, OA Deals (2016 up until 2018)
Date: March 4th 2018
Content: Cumulative list of all OA articles published within the Netherlands (not restricted to Utrecht) as part of the VSNU OA deal, including DOI, publisher and publication year.

### Unpaywall data
Retrieve OA status according to Unpaywall, using their API (http://unpaywall.org/products/api).

### OA Status in Pure
OA Status in Pure is determined by employees of Utrecht University Library who manually check each publication added to Pure, add the full text and determine if it is OA available (Open/Embargoed/Closed). Distinguishing between types of OA availability is at this moment not part of this procedure.

## Manual check for OA status
A manual check might be benificiary for those titles that do not have a DOI. We only check these titles when within an organizational unit the number of titles without a DOI is at least 5% of the total number of titles within that organizational unit.

## Reporting OA status

The OA status (4 categories) is reported regarding three organizational levels:
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

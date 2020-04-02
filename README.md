# Monitoring Open Access from a single year
This project takes publication data from a single year and determines per article its open access status, using various sources available. It uses peer reviewed journal articles registered in CRIS systems as input. It was created in a collaboration between Utrecht University Library and Leiden University Library. Specifics for Utrecht and Leiden can be found in the Wiki.

## Gathering OA information
The pipeline harvests OA information from the following sources:

### DOAJ Data
Content: all journals listed on DOAJ and therefore labeled as Full OA  
Result: doaj = TRUE/FALSE

### VSNU data
Content: Cumulative list of all OA articles published within the Netherlands as part of the VSNU OA deal, including DOI, publisher and publication year.  
Result: vsnu = TRUE/FALSE

### Unpaywall data
OA status according to Unpaywall, retrieved using their API (http://unpaywall.org/products/api).  
Result: ups = bronze/closed/doaj/gold/green/hybrid/NA

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

## Reporting OA status

The OA status (GOLD/HYBRID/GREEN/CLOSED) is reported regarding three organizational levels:  
- Universty/University Medical Center total  
- For each faculty  
- For each HOOP-gebied (the division of Dutch Higher Education and Research into eight categories: Landbouw (Agriculture), Natuur (Nature), Techniek (Technique), Gezondheid (Health), Gedrag en Maatschappij (Behaviour and Society), Economie (Economics), Recht (Law), Taal en Cultuur (Language and Culture). Each faculty or department is assigned to one HOOP category. 

## Future or bespoke use of this script
When re-using this script, adjust the following things:

- In the `upw_api` function, adjust the email address to reflect the current user; 
- In the 'unpaywall' section of 'OA labelling', choose 'api' or 'csv' for the variable `api_csv`, depending on whether you want to load existing unpaywall data ('csv'), or re-run the unpaywall analysis via their api ('api')

## Wishlist
- Test the sources for quality
- What happens if we use the Core db instead of Unpaywall? https://core.ac.uk/services/api/

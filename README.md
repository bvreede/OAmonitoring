# Reporting Open Access output

This project takes publication data from a single year and determines per article its open access status, using various sources available. It uses peer reviewed journal articles registered in CRIS systems as input. It was created in a collaboration between Utrecht University Library and Leiden University Library. Specifics for Utrecht and Leiden can be found in the Wiki.

## Your data
Your data will need to have the following fields to successfully make it through the pipeline:
- Journal ISSN
- paper DOI
- System ID (a unique ID given by your CRIS)
- Organization unit (e.g. faculty, department, etc)

NB: The columns may have missing data, but must exist.

## Gathering OA information
The pipeline harvests OA information from the following sources:
- Directory of Open Access Journals, ([DOAJ](http://doaj.org/))
- [Unpaywall](http://unpaywall.org/)
- Use of VSNU Open Access publishing deals (an excel sheet with DOIs that is available on request via j.deboer@uu.nl)

## Assigning OA status
Each paper in your data will get a OA label assigned based on the OA information above, and applied in sequence:

1. Is the journal present in the DOAJ? -> label: GOLD
1. Is the DOI present in the VSNU list? -> label: HYBRID
1. Is the DOI classified as gold or hybrid by Unpaywall? -> label: HYBRID
*1. (Optional: is the system ID present in a custom list supplied by the user? -> label: GREEN)*
1. Is the DOI classified as green by Unpaywall? -> label: GREEN

## Reporting OA status

The OA status (GOLD/HYBRID/GREEN/CLOSED) is reported regarding three organizational levels:  
- Universty/University Medical Center total  
- For each faculty  
- For each HOOP-gebied (the division of Dutch Higher Education and Research into eight categories: Landbouw (Agriculture), Natuur (Nature), Techniek (Technique), Gezondheid (Health), Gedrag en Maatschappij (Behaviour and Society), Economie (Economics), Recht (Law), Taal en Cultuur (Language and Culture). Each faculty or department is assigned to one HOOP category. 

## Future or bespoke use of this script
When re-using this script, adjust the following things:

- In the `upw_api` function, adjust the email address to reflect the current user; 
- In the 'unpaywall' section of 'OA labelling', choose 'api' or 'csv' for the variable `api_csv`, depending on whether you want to load existing unpaywall data ('csv'), or re-run the unpaywall analysis via their api ('api')



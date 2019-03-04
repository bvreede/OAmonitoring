# Monitoring Open Access at UU in 2018
This project takes publication data from a single year at Utrecht University / UMC Utrecht, and determines per article its open access status, using various sources available. It uses peer reviewed journal articles registered in PURE at UU and UMCU as input.

## UU and UMCU output:

###UU Pure Data
Source: Functioneel Beheer Pure, via M&A
Date: March 1st 2019
Content: All peer reviewed journal articles with publication year 2018 for each organisational unit (duplicate articles when co-authors are from different organisational units)

###UMCU Pure Data
Source: Functioneel Beheer Pure, via M&A
Date: March 1st 2019
Content: All peer reviewed journal articles with publication year 2018

## Matching to obtain Open Access status
The pipeline tries to classify all publications according to its presence or absence in various check lists. In sequence:

1. match the journal ISSN with a list from the Directory of Open Access Journals (DOAJ). If the journal matches, the publication is Gold OA
2. match the DOI with a list obtained from VSNU. If the journal matches, the publication is Hybrid OA
3. obtain the OA status from Unpaywall. If the status is publisher, the publication is Hybrid OA. If the status is repository, the publication is Green OA.
4. if publication matches none of the above, the publication has an unknown OA status and is considered closed.

###DOAJ Data
Source: On SURF Drive, OA Monitoring
Date: December 31st 2018
Content: all journals listed on DOAJ and therefore labeled as Full OA

###VSNU data
Source: compiled from VSNU OA data on Surfdrive, OA Deals (2016 up until 2018)
Date: March 4th 2018
Content: Cumulative list of all OA articles published within the Netherlands (not restricted to Utrecht) as part of the VSNU OA deal, including DOI, publisher and publication year.

###Unpaywall data
Retrieve OA status according to Unpaywall, using their API (http://unpaywall.org/products/api).

##Manual check for OA status
A manual check is necessary for two reasons:
-some publications do not have a DOI
-Unpaywall does not have the OA status for all OA available publications.

Publications that meet the following two criteria must be checked manually for their OA status:
-unknown/closed OA status (not matching DOAJ, VSNU and/or Unpaywall)
-has an electronic file in our repository

##Reporting OA status

...



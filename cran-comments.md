## Resubmission
This is a resubmission. In this version I have:
     
* Updated CRAN URLs to be canonical
* Added years and DOIs to references of implemented methods
      
## Test environments
* OS X 10.11.5, R 3.4.3
* ubuntu 16.04, R 3.4.3
* win-builder,  R 3.4.3

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

Found the following (possibly) invalid DOIs:
  DOI: 10.1002/(SICI)1097-0258(19980715)17:13<1495::AID-SIM863
    From: DESCRIPTION
    Status: Not Found
    Message: 404

This is the result of a parsing bug in check_package_CRAN_incoming when the
DOI has a '>' character

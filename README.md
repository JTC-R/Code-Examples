# Code-Examples

This repository is intended to host a variety of coding examples from my personal projects. 

Both of these relate to the creation and verification of stock reprots produced automatically everyday. The reports are exported as PDF and contain specialized information and data. 

The GET_REPORT_INFORMATION script is designed to allow a signular access point for the data stream into the report creation process. This ensures that all data entering the reports are properly formatted and if the data is missing and/or corrupted, that a standardized error handling, reporting, and repair processes are present. 

The PDF_VERIFICATION script is designed to ensure that the PDF's are accurate, represent the data they are supposed to (per their file name), and that the directory in which they are located is free from any non-PDF files. This function works by first reading the PDF, extracting the STOCK name and then compares it to the file name. If the two do not match, a corrupted file is logged and eventually re-produced. 


The example PDF (SPY_report_20220509) is an Example PDF for the stock $SPY. Approximately 650+ reports are created daily and are the cummulation of several distinct data streams and data procssing that must occur prior to the creation of the reports. 

The PYTHON JOBS APP was designed to web-scrape various job-hunting websites with user-input. It was mainly used as a learning example for Python but it is functional as of 06/2021. 
The user is able to specify the websites they want searched, the type of jobs, the amount of queries, and set the location for the search. As of now, the websites searched are:

    Monster.com,
    Indeed.com,
    Gettinghired.com,
    Flexjobs.com,
    Snagajob.com, and
    Glassdoor.com

All websites except for Glassdoor are straight-forward scrapes. However, Glassdoor has safeguards that have to be overcome - thus the prolonged mechanism for the creation of the appropriate Glassdoor URL can be noted.

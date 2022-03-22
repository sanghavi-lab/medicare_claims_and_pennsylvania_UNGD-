# Code and data availability

All code and certain de-identified datasets are currently available at (removed for blinded review). The outline below walks through the code used to analyze data and generate visualizations and model output.

Below is a table of code descriptions including the input and output from the code.

| Steps | Script | Description | Input | Output | 
| ------------- | ------------- || ------------- | ------------- |------------- |
| 1 | FIN-count-cases-with-mbsf.py | Merge MedPAR and MBSF data, filter to those with Parts A and B coverage, and count the number of cases per outcome group per zip code per year. |       Outcomes to study and corresponding ICD-9 and ICD-10 codes
    FIN-outcomes-groups.csv
    FIN-outcome-group-ICD10s.csv
    FIN-outcome-group-ICD10s-exclusion.csv
      List of included ZIP codes in Pennsylvania & New York study regions
    included-zipcodes-list.csv
      MedPAR files (inpatient claims), one CSV file per year, each CSV contains all records from entire PA & NY states in one year
    THES-DID-INP-REF-whole-state-filtered-medpar-{year}.csv
      MBSF files (all Medicare beneficiaries), one CSV per year, each CSV filtered to all beneficiaries from just the ZIP codes of study
    THES-DID-INP-REF-ZIP-mbsf-zipcode-subset-{year}.csv
 |       Two CSV files per year: one counting cases using 2 diagnosis columns only, one using all 25 diagnosis columns. CSVs contain columns for the outcome (e.g. "AMI"), the ZIP code, and the number of cases.
    FIN-icd-counts-per-zipcode-with-mbsf-{year}-using-{num}-dgns-columns.csv
 | 
| 2 | #1 | #2 | #3 | #4 |
| 3 | #1 | #2 | #3 | #4 |


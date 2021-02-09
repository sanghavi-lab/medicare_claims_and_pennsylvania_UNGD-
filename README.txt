

Date: July 15, 2020
Author: Kevin Trickey


This README describes the four scripts used to analyze health data in relation to UNGD activity in the Marcellus region.
The root directory contains three subdirectories: Code, Data, and Output.
Certain input and output datasets (where any cell has a count of 11 or fewer) are not available due to identifiability concerns, and are noted below.
De-identified data are available in the Data and Output folders.

Note: two control regions were presented in the published paper; five were analyzed here (defined in included-zipcodes-list.csv)
but omitted after observing poorer demographic similarities with the study region. They are included here for any further analysis desired.



Here is a description of the overall input data used, and the locations referenced in our code:

  Outcomes to study and corresponding ICD-9 codes
    * Data/FIN-outcomes-group-ICDs.csv
  List of included ZIP codes in Pennsylvania & New York study regions
    * Data/included-zipcodes-list.csv
  MedPAR files (inpatient claims), one CSV file per year, each CSV contains all records from entire PA & NY states in one year (NOT PUBLIC)
    * Output/THES-DID-INP-REF-whole-state-filtered-medpar/THES-DID-INP-REF-whole-state-filtered-medpar-{year}.csv
  MBSF files (all Medicare beneficiaries), one CSV per year, each CSV filtered to all beneficiaries from just the ZIP codes of study (NOT PUBLIC)
    * Output/THES-DID-INP-REF-ZIP-mbsf-zipcode-subsets/THES-DID-INP-REF-ZIP-mbsf-zipcode-subset-{year}.csv
  UNGD spuds aggregated over ZIP codes
    * Output/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv



Here is a description of the scripts, which you may find in the directory:

1) Merge MedPAR and MBSF data, filter to those with Parts A and B coverage, and count up the number of cases per outcome group per zip code.
    Code: FIN-count-cases-with-mbsf.py

    Input data:
      Outcomes to study and corresponding ICD-9 codes
        * Data/FIN-outcomes-group-ICDs.csv
      List of included ZIP codes in Pennsylvania & New York study regions
        * Data/included-zipcodes-list.csv
      MedPAR files (inpatient claims), one CSV file per year, each CSV contains all records from entire PA & NY states in one year
        * Output/THES-DID-INP-REF-whole-state-filtered-medpar/THES-DID-INP-REF-whole-state-filtered-medpar-{year}.csv (NOT PUBLIC)
      MBSF files (all Medicare beneficiaries), one CSV per year, each CSV filtered to all beneficiaries from just the ZIP codes of study
        * Output/THES-DID-INP-REF-ZIP-mbsf-zipcode-subsets/THES-DID-INP-REF-ZIP-mbsf-zipcode-subset-{year}.csv (NOT PUBLIC)

    Output:
      Two CSV files per year: one counting cases using 2 diagnosis columns only, one using all 25 diagnosis columns.
      CSVs contain columns for the outcome (e.g. "AMI"), the ZIP code, and the number of cases.
        * Output/FIN-icd-counts-per-zipcode-with-mbsf/FIN-icd-counts-per-zipcode-with-mbsf-{year}-using-{num}-dgns-columns.csv (NOT PUBLIC)


2) Aggregate MBSF subset data into zip code-level bins, recording total populations and some average statistics, and merge with aggregated ICD codes.
    Code: FIN-join-zipcodes-mbsf.py

    Input data:
      Counted cases per ZIP code, per year, per # of diagnosis columns (Output from (1))
        * Output/FIN-icd-counts-per-zipcode-with-mbsf/FIN-icd-counts-per-zipcode-with-mbsf-{year}-using-{num}-dgns-columns.csv (NOT PUBLIC)
      MBSF files (all Medicare beneficiaries), one CSV per year, each CSV filtered to all beneficiaries from just the ZIP codes of study
        * Output/THES-DID-INP-REF-ZIP-mbsf-zipcode-subsets/THES-DID-INP-REF-ZIP-mbsf-zipcode-subset-{year}.csv (NOT PUBLIC)
      List of included ZIP codes in Pennsylvania & New York study regions
        * Data/included-zipcodes-list.csv

    Output:
      Aggregate statistics per ZIP code of beneficiary demographics (age distributions, sex, race, duals, etc.) from MBSF file, for each year
        * Output/FIN-mbsf-agg-statistics/FIN-mbsf-agg-statistics-{year}.csv (NOT PUBLIC)
      Merged CSV file for each year, each number of diagnosis columns, with hospitalization counts AND MBSF aggregate statistics (i.e. denominators) together
        * Output/FIN-icd-rates-per-zipcode/FIN-icd-rates-per-zipcode-{year}-using-{num}-dgns-columns.csv (NOT PUBLIC)


3) Plot time series of raw incidence rates, aggregated over study regions (NY Mid, NY Border, PA Border).
    Code: FIN-plot-raw-rates.R
    
    Input data: 
      Hospitalization counts and aggregate statistics (Output from (2))
        * Output/FIN-icd-rates-per-zipcode/FIN-icd-rates-per-zipcode-{year}-using-{num}-dgns-columns.csv (NOT PUBLIC)
      List of included ZIP codes in Pennsylvania & New York study regions
        * Data/included-zipcodes-list.csv
      UNGD spuds aggregated over ZIP codes
        * Output/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv
      Outcomes to study and corresponding ICD-9 codes
        * Data/FIN-outcomes-group-ICDs.csv
	
    Output data:
      "Master dataframe" containing all ZIP code data (hospitalization counts per outcome per year, etc) in one table
        * Output/FIN-raw-rates-plots/generating-tables/MASTER-DF.csv (NOT PUBLIC)
      Plots of demographic trends (age, race, sex, total benes) over time, along with accompanying dataframe
        * Output/FIN-raw-rates-plots/demographics-plots/
        * Output/FIN-raw-rates-plots/generating-tables/DEMOGRAPHICS-DF.csv (NOT PUBLIC)
      Dataframe of hospitalizations summed over ZIP codes in study regions (PA Border, NY Border, NY Mid)
        * Output/FIN-raw-rates-plots/generating-tables/STATES-DF.csv
      Various other informative plots to visualize data, not for final publication
        * Output/FIN-raw-rates-plots/ (SOME NOT PUBLIC, OTHERS AVAILABLE)
      Final, publishable plot with time-series of hospitalization rates stacked on top of UNGD development over time
        * Output/FIN-raw-rates-plots/publishable_plot_column.eps


4) Run negative binomial models on our panel dataset, with different specifications.
    Code: PUB-ZIP-continuous-models.R

    Input data:
      UNGD spuds aggregated over ZIP codes
        * Output/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv
      Cleaned MedPAR/MBSF data with health outcomes (Output from (3))
        * Output/FIN-raw-rates-plots/generating-tables/MASTER-DF.csv (NOT PUBLIC)
      Outcomes to study and corresponding ICD-9 codes
        * Data/FIN-outcomes-group-ICDs.csv
      
    Output: 
      Spreadsheet with one row of model output for each model specification (outcome, # diagnosis cols used, which control region)
        * Output/FIN-continuous-models-output.csv






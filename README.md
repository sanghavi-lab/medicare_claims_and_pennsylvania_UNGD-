# Code and data availability

All code and certain de-identified datasets are currently available at (removed for blinded review). The outline below walks through the code used to analyze data and generate visualizations and model output.

Below is a table of code descriptions including the input and output from the code.

### 1. Merge MedPAR and MBSF data, filter to those with Parts A and B coverage, and count the number of cases per outcome group per zip code per year.

| Scripts| Input| Output|
|-|-|-|
| **FIN-count-cases-with-mbsf.py** | FIN-outcomes-groups.csv | FIN-icd-counts-per-zipcode-with-mbsf-{year}-using-{num}-dgns-columns.csv           |
|                              | FIN-outcome-group-ICD10s.csv||
|                              | FIN-outcome-group-ICD10s-exclusion.csv||
|                              | included-zipcodes-list.csv||
|                              | THES-DID-INP-REF-whole-state-filtered-medpar-{year}.csv||
|                              | THES-DID-INP-REF-ZIP-mbsf-zipcode-subset-{year}.csv||

### 2. Aggregate MBSF subset data into zip code-level bins, recording total populations and some average statistics, and merge with aggregated ICD codes.

| Scripts| Input| Output|
|-|-|-|
| **FIN-join-zipcodes-mbsf.py**| PUB-ZIP-icd-counts-per-zipcode-with-mbsf-{year}-using-2-dgns-columns.csv | FIN-mbsf-agg-statistics-{year}.csv                                                        |
|                          | THES-DID-INP-REF-ZIP-mbsf-zipcode-subset-{year}.csv| FIN-icd-rates-per-zipcode-{year}-using-{num}-dgns-columns.csv|
|                          | included-zipcodes-list.csv   ||

### 3. Plot time series of raw incidence rates, aggregated over study regions (NY Mid, NY Border, PA Border).

| Scripts| Input| Output|
|-|-|-|
| **FIN-plot-raw-rates.R** | FIN-icd-rates-per-zipcode-{year}-using-{num}-dgns-columns.csv            | MASTER-DF.csv (this was changed to MASTER-DF-UPDATE.csv after adding the last three month of data in 2015) |
|                              | included-zipcodes-list.csv                                               | DEMOGRAPHICS-DF.csv                                                                                        |
|                              | THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv                        | publishable_plot_column.eps                                                                                |
|                              | FIN-outcomes-groups.csv                                                  |                                                                                                            |

### 4. Run difference-in-difference anlaysis on our panel dataset usind the did package developed by Callaway et al.

| Scripts| Input| Output|
|-|-|-|
| **CALLAWAY_DID.R**               | THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv                        | CALLAWAY_FINAL_DID_RESULTS_0810ONLY.csv                                                                    |
|                              | MASTER-DF-UPDATE.csv                                                     | CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiod2009_BOOTSTRAPPED.csv                                         |
|                              | FIN-outcomes-groups.csv                                                  | CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiodVARYING.csv                                                   |
|                              |                                                                          | pretrend_NYMid_basedperiod2009.eps                                                                         |
|                              |                                                                          | pretrend_NYMid_basedperiodVARYING.eps                                                                      |

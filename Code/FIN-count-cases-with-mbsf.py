# 7/6/20

# This file counts cases per zip code per outcome group per year, using a specified number of diagnosis columns (either 2 or 25).
# Filters to benes with Part A & B coverage by joining with MBSF data.


import pandas as pd
import numpy as np




# GET ZIP CODE AND HEALTH OUTCOME INFORMATION ====================

# Read in zip codes used in study
zipcodes_of_interest = pd.read_csv('/gpfs/data/sanghavi-lab/Kevin/Data/included-zipcodes-list.csv', dtype='object')

study_group2zipcodes = {}   # dict from study group (e.g. 'PA BORDER') to list of zipcodes (as strings)
for index, row in zipcodes_of_interest.iterrows():
    sg = row['StudyGroup']
    zc = row['ZipCode']
    if sg in study_group2zipcodes.keys():
        study_group2zipcodes[sg].append(zc)
    else:
        study_group2zipcodes[sg] = [zc]
print('study_group2zipcodes:')
print(study_group2zipcodes)


# Read in outcomes for study and associated ICD-9 codes
outcome_group_ICD9s = pd.read_csv('/gpfs/data/sanghavi-lab/Kevin/Data/FIN-outcome-group-ICDs.csv', dtype='object')

outcome2icd9s = {}   # dict from outcome group (e.g. 'air-cardiovascular-all') to list of ICD9 codes (e.g. ['410', '411', ...])
for index, row in outcome_group_ICD9s.iterrows():
    key = row['OutcomeGroup']
    element = row['ICD9'].replace('.', '')  # get rid of any periods, since these aren't reported in medpar data
    if key in outcome2icd9s.keys():
        outcome2icd9s[key].append(element)
    else:
        outcome2icd9s[key] = [element]
print('outcome2icd9s:')
print(outcome2icd9s)



# CONVENIENCE FUNCTIONS ===========================================

def intersection(lst1, lst2): 
    lst3 = [value for value in lst1 if value in lst2] 
    return lst3 

def encode_list(lst):
    return list(map(lambda x: x.encode(), lst))

def isFloat(string):
    try:
        float(string)
        return True
    except ValueError:
        return False



# FOR EACH YEAR, COUNT UP THE NUMBER OF CASES FOR EACH OUTCOME =============================

# Loop through years
for year in range(2002, 2016):
    print('\n\nSTARTING YEAR {} =======\n'.format(str(year)))

    # Read in MedPAR data (inpatient claims) for that year
    medpar = pd.read_csv('/gpfs/data/sanghavi-lab/Kevin/Output/THES-DID-INP-REF-whole-state-filtered-medpar/THES-DID-INP-REF-whole-state-filtered-medpar-{0}.csv'.format(str(year)),
              dtype='object')
    print('Read in medpar data:')
    print(medpar.head())
    print(medpar.shape)

    # Filter to just the zipcodes of interest
    medpar = medpar[medpar['BENE_MLG_CNTCT_ZIP_CD'].isin(zipcodes_of_interest['ZipCode'])]
    print('\nFiltered to zipcodes of interest:')
    print(medpar.head())
    print(medpar.shape)

    # Merge with mbsf data
    mbsf = pd.read_csv('/gpfs/data/sanghavi-lab/Kevin/Output/THES-DID-INP-REF-ZIP-mbsf-zipcode-subsets/THES-DID-INP-REF-ZIP-mbsf-zipcode-subset-{}.csv'.format(year), 
            dtype='object', engine='python', encoding='latin')
    col_prefix = ('BENE_' if year <= 2010 else '') + 'MDCR_ENTLMT_BUYIN_IND_'

    mbsf = mbsf[['BENE_ID'] + [(col_prefix + '{:02d}').format(i) for i in range(1, 13)]]
    medpar = medpar.merge(mbsf, on='BENE_ID', how='left')
    print('Merged with mbsf subset:')
    print(medpar.head())
    print(medpar.shape)

    # Filter to just those with Part A and B in their admission month
    medpar = medpar.assign(admission_month = medpar['ADMSN_DT'].str[2:5])   # will be JAN, FEB, MAR, etc.
    medpar = medpar[(((medpar['admission_month'] == 'JAN') & (medpar[col_prefix + '01'] == '3')) | 
                     ((medpar['admission_month'] == 'FEB') & (medpar[col_prefix + '02'] == '3')) |
                     ((medpar['admission_month'] == 'MAR') & (medpar[col_prefix + '03'] == '3')) |
                     ((medpar['admission_month'] == 'APR') & (medpar[col_prefix + '04'] == '3')) |
                     ((medpar['admission_month'] == 'MAY') & (medpar[col_prefix + '05'] == '3')) |
                     ((medpar['admission_month'] == 'JUN') & (medpar[col_prefix + '06'] == '3')) |
                     ((medpar['admission_month'] == 'JUL') & (medpar[col_prefix + '07'] == '3')) |
                     ((medpar['admission_month'] == 'AUG') & (medpar[col_prefix + '08'] == '3')) |
                     ((medpar['admission_month'] == 'SEP') & (medpar[col_prefix + '09'] == '3')) |
                     ((medpar['admission_month'] == 'OCT') & (medpar[col_prefix + '10'] == '3')) |
                     ((medpar['admission_month'] == 'NOV') & (medpar[col_prefix + '11'] == '3')) |
                     ((medpar['admission_month'] == 'DEC') & (medpar[col_prefix + '12'] == '3')))]
    print('Filtered to FFS:')
    print(medpar.head())
    print(medpar.shape)


    # Now, within each year, repeat the process twice (once using only 2 diagnosis columns, once using all 25)
    for DIAGNOSIS_COLS in [[1,2], range(1, 26)]:

        # Mutate all diagnosis columns that we're using to get prefixes that would match an icd9 code of interest, i.e. 3-5 characters long.
        # If any of the new prefix columns contains an icd code of interest, we count that beneficiary.
        all_prefix_cols = []
        medpar_prefixed = medpar[(['BENE_MLG_CNTCT_ZIP_CD', 'ADMTG_DGNS_CD'] + 
                intersection(['DGNS_{}_CD'.format(i) for i in DIAGNOSIS_COLS] +
                    ['DGNS_E_{}_CD'.format(i) for i in DIAGNOSIS_COLS], list(medpar.columns)))] # intersect
        for c in DIAGNOSIS_COLS:
            # First prefix admitting diagnosis
            medpar_prefixed = medpar_prefixed.astype({'ADMTG_DGNS_CD': 'string_'})
            medpar_prefixed = medpar_prefixed.assign(**{'admtg_dgns_prefix3': medpar_prefixed['ADMTG_DGNS_CD'].str[0:3],
                                                        'admtg_dgns_prefix4': medpar_prefixed['ADMTG_DGNS_CD'].str[0:4],
                                                        'admtg_dgns_prefix5': medpar_prefixed['ADMTG_DGNS_CD'].str[0:5]})
            # Uncomment to include admitting diagnosis column:
            # all_prefix_cols = all_prefix_cols + ['admtg_dgns_prefix3', 'admtg_dgns_prefix4', 'admtg_dgns_prefix5']

            if 'DGNS_{}_CD'.format(c) in medpar_prefixed.columns:
                medpar_prefixed = medpar_prefixed.astype({'DGNS_{}_CD'.format(c): 'string_'})  # make sure it's a string - should be unnecessary
                medpar_prefixed = medpar_prefixed.assign(**{'dgns_{}_prefix3'.format(c): medpar_prefixed['DGNS_{}_CD'.format(c)].str[0:3],
                                                            'dgns_{}_prefix4'.format(c): medpar_prefixed['DGNS_{}_CD'.format(c)].str[0:4],
                                                            'dgns_{}_prefix5'.format(c): medpar_prefixed['DGNS_{}_CD'.format(c)].str[0:5]})
                all_prefix_cols = all_prefix_cols + ['dgns_{}_prefix3'.format(c), 'dgns_{}_prefix4'.format(c), 'dgns_{}_prefix5'.format(c)]


        # Iterate through ICD codes of interest and count number per zip code for each.
        # May be doable with groupby/agg, but with prefixes and multiple diagnoses etc., this is more straightforward.
        outcome_groups = []
        zips = []
        counts = []

        print('medpar_prefixed:')
        print(medpar_prefixed.head())
        print(medpar_prefixed.shape)

        for zipcode in zipcodes_of_interest['ZipCode']:
            print('  Beginning zip code {}'.format(zipcode))

            # Get only the rows from that zip code with that icd code
            medpar_subset_zip = medpar_prefixed[medpar_prefixed['BENE_MLG_CNTCT_ZIP_CD'] == zipcode]
            print('   Filtered for zip code {}:'.format(zipcode))
            print(medpar_subset_zip.head())
            print(medpar_subset_zip.shape)

            for outcome_group, icd9_list in outcome2icd9s.items():
                print('Begining outcome group {} with icd9 codes:'.format(outcome_group))
                print(icd9_list)
                
                medpar_subset_icd = medpar_subset_zip[(medpar_subset_zip[all_prefix_cols].isin(encode_list(icd9_list))).any(1)]  # note: encoding ICD from string to bytes type
                print('   Filtered for icd codes {}:'.format(icd9_list))
                print(medpar_subset_icd.head())
                print(medpar_subset_icd.shape)

                # Stroke has particular exclusion criteria we need to consider:
                if outcome_group == 'stroke':
                    # Iterate through diagnosis columns to filter subset down to those who aren't excluded
                    which_rows_to_include = [True] * medpar_subset_icd.shape[0]   # start off by including all, then mark those indices to exclude as False
                    for col_to_check in intersection(['DGNS_{}_CD'.format(i) for i in range(1, 26)], list(medpar_subset_zip.columns)):
                        col_as_strs = [w.decode() for w in list(medpar_subset_icd[col_to_check])]
                        print(col_as_strs)
                        col_as_floats = [(0.0 if w=='nan' else (float(w[:3] + '.' + w[3:]) if isFloat(w) else (801.0 if w[:3] == 'V57' else 0.0))) for w in col_as_strs]  # set as arbitrary unsatisfactory float if it's V57xx
                        print(col_as_floats)
                        col_satisfactory = [icd < 800 or icd > (804.9 and icd < 850) or icd > 854.1 for icd in col_as_floats]
                        print(col_satisfactory)
                        which_rows_to_include = [t[0] and t[1] for t in zip(which_rows_to_include, col_satisfactory)]

                    # filter down to those that haven't met exclusion criteria
                    medpar_subset_icd = medpar_subset_icd[which_rows_to_include]
                    print('Filtered out {} rows that met exclusion criteria for stroke'.format(sum(which_rows_to_include)))
                    print(medpar_subset_icd.head())
                    print(medpar_subset_icd.shape)



                outcome_groups.append(outcome_group)
                zips.append(zipcode)
                counts.append(medpar_subset_icd.shape[0])
                print('  ADDING TO DATAFRAME: OUTCOMEGROUP: {0}, ZIP: {1}, COUNT: {2}'.format(outcome_group, zipcode, medpar_subset_icd.shape[0]))

        # Compile into one dataframe and save to file
        counted_df = pd.DataFrame({'OutcomeGroup': outcome_groups, 'ZipCode': zips, 'NumClaims': counts})
        print('Counted up everything!')
        print('Num diagnosis columns used: ' + str(len(DIAGNOSIS_COLS)))
        print(counted_df)
        counted_df.to_csv('/gpfs/data/sanghavi-lab/Kevin/Output/FIN-icd-counts-per-zipcode-with-mbsf/FIN-icd-counts-per-zipcode-with-mbsf-{0}-using-{1}-dgns-columns.csv'.format(year, len(DIAGNOSIS_COLS)), index=False)
        print('Written to file!\n')

## RUN THIS PART OF CODE SEPARATELY TO ADD THE LAST THREE MONTHS OF HOSPIATLIZATIONS IN 2015 (WHICH IS DROPPED FROM THE LAST CODE)
# # GET ZIP CODE AND HEALTH OUTCOME INFORMATION ====================

# # Read in zip codes used in study
# zipcodes_of_interest = pd.read_csv('/gpfs/data/cms-share/duas/54200/Kevin/Data/included-zipcodes-list.csv', dtype='object')

# study_group2zipcodes = {}  # dict from study group (e.g. 'PA BORDER') to list of zipcodes (as strings)
# for index, row in zipcodes_of_interest.iterrows():
#     sg = row['StudyGroup']
#     zc = row['ZipCode']
#     if sg in study_group2zipcodes.keys():
#         study_group2zipcodes[sg].append(zc)
#     else:
#         study_group2zipcodes[sg] = [zc]
# # print('study_group2zipcodes:')
# # print(study_group2zipcodes)

# # Read in outcomes for study and associated ICD-9 codes
# outcome_group_ICD10s = pd.read_csv('/gpfs/data/cms-share/duas/54200/Zoey/ICD10/Data/FIN-outcome-group-ICD10s.csv', dtype='object')

# outcome2icd10s = {}  # dict from outcome group (e.g. 'air-cardiovascular-all') to list of ICD9 codes (e.g. ['410', '411', ...])
# for index, row in outcome_group_ICD10s.iterrows():
#     key = row['OutcomeGroup']
#     element = row['ICD10']
#     if key in outcome2icd10s.keys():
#         outcome2icd10s[key].append(element)
#     else:
#         outcome2icd10s[key] = [element]
# # print('outcome2icd10s:')
# # print(outcome2icd10s)

# ## read in icd codes to exclude for stroke
# icd_exclude = pd.read_csv('/gpfs/data/cms-share/duas/54200/Zoey/ICD10/Data/FIN-outcome-group-ICD10s-exclusion.csv', dtype='object')



# # FOR EACH YEAR, COUNT UP THE NUMBER OF CASES FOR EACH OUTCOME =============================

# year = 2015
# print('\n\nSTARTING YEAR {} =======\n'.format(str(year)))

# # Read in MedPAR data (inpatient claims) for that year
# medpar = pd.read_csv(
#     '/gpfs/data/cms-share/duas/54200/Zoey/ICD10/Output/THES-DID-INP-REF-whole-state-filtered-medpar-{0}.csv'.format(
#         str(year)),
#     dtype='object')

# # print('Read in medpar data:')
# # print(medpar.head())
# # print(medpar.shape)

# # Filter to just the zipcodes of interest
# medpar = medpar[medpar['BENE_MLG_CNTCT_ZIP_CD'].isin(zipcodes_of_interest['ZipCode'])]
# # print('\nFiltered to zipcodes of interest:')
# # print(medpar.head())
# # print(medpar.shape) #26119

# # Merge with mbsf data
# mbsf = pd.read_csv(
#     '/gpfs/data/cms-share/duas/54200/Kevin/Output/THES-DID-INP-REF-ZIP-mbsf-zipcode-subsets/THES-DID-INP-REF-ZIP-mbsf-zipcode-subset-{}.csv'.format(
#         year),
#     dtype='object', engine='python', encoding='latin')
# col_prefix = ('BENE_' if year <= 2010 else '') + 'MDCR_ENTLMT_BUYIN_IND_'

# mbsf = mbsf[['BENE_ID'] + [(col_prefix + '{:02d}').format(i) for i in range(1, 13)]]
# medpar = medpar.merge(mbsf, on='BENE_ID', how='left')
# # print('Merged with mbsf subset:')
# # print(medpar.head())
# # print(medpar.shape)

# # Filter to just those with Part A and B in their admission month
# medpar = medpar.assign(admission_month=medpar['ADMSN_DT'].str[2:5])  # will be JAN, FEB, MAR, etc.
# medpar = medpar[(((medpar['admission_month'] == 'JAN') & (medpar[col_prefix + '01'] == '3')) |
#                  ((medpar['admission_month'] == 'FEB') & (medpar[col_prefix + '02'] == '3')) |
#                  ((medpar['admission_month'] == 'MAR') & (medpar[col_prefix + '03'] == '3')) |
#                  ((medpar['admission_month'] == 'APR') & (medpar[col_prefix + '04'] == '3')) |
#                  ((medpar['admission_month'] == 'MAY') & (medpar[col_prefix + '05'] == '3')) |
#                  ((medpar['admission_month'] == 'JUN') & (medpar[col_prefix + '06'] == '3')) |
#                  ((medpar['admission_month'] == 'JUL') & (medpar[col_prefix + '07'] == '3')) |
#                  ((medpar['admission_month'] == 'AUG') & (medpar[col_prefix + '08'] == '3')) |
#                  ((medpar['admission_month'] == 'SEP') & (medpar[col_prefix + '09'] == '3')) |
#                  ((medpar['admission_month'] == 'OCT') & (medpar[col_prefix + '10'] == '3')) |
#                  ((medpar['admission_month'] == 'NOV') & (medpar[col_prefix + '11'] == '3')) |
#                  ((medpar['admission_month'] == 'DEC') & (medpar[col_prefix + '12'] == '3')))]
# # print('Filtered to FFS:')
# # print(medpar.head())
# # print(medpar.shape) #20123

# # Now, within each year, repeat the process twice (once using only 2 diagnosis columns, once using all 25)
# for DIAGNOSIS_COLS in [[1, 2], range(1, 26)]:

#     dgns_cols = ['DGNS_{}_CD'.format(i) for i in DIAGNOSIS_COLS]
#     all_dgns_cols = ['DGNS_{}_CD'.format(i) for i in range(1, 26)]

#     count_df_lst = []
#     for outcome in outcome2icd10s.keys():
#         count_df = pd.DataFrame({'ZipCode': [],
#                                  'NumClaims': []})

#         icd_code = outcome2icd10s[outcome]
#         ## select claims with certain icd codes
#         medpar_subset = medpar[medpar[dgns_cols].isin(icd_code).any(axis=1)]

#         if outcome == 'stroke':## exclude claims with certain diagnosis codes
#             medpar_subset = medpar_subset[~medpar_subset[all_dgns_cols].isin(icd_exclude['Exclusion']).any(axis=1)]
#             medpar_subset = medpar_subset[medpar_subset['DGNS_1_CD'] != 'Z5189']
#         count_by_zipcode = medpar_subset.groupby('BENE_MLG_CNTCT_ZIP_CD')['MEDPAR_ID'].count().rename('NumClaims').reset_index()
#         count_df['ZipCode'] = count_by_zipcode['BENE_MLG_CNTCT_ZIP_CD']
#         count_df['NumClaims'] = count_by_zipcode['NumClaims']
#         count_df['OutcomeGroup'] = outcome
#         count_df_lst.append(count_df)

#     count_df_final = pd.concat(count_df_lst)
#     count_df_final['Year'] = 2015
#     count_df_final['NumDiagnosisColsUsed'] = len(DIAGNOSIS_COLS)
#     count_df_final.to_csv('/gpfs/data/cms-share/duas/54200/Zoey/ICD10/Output/icd10-counts-per-zipcode-with-mbsf-2015-using-{}-dgns-columns.csv'.format(len(DIAGNOSIS_COLS)),
#     index=False)

# ## ADD ICD 10 COUNT DATA TO MASTER-DF AND WRITE NEW DATA AS MASTER-DF-UPDATE
# ## read in original MASTER-DF with no ICD 10 count data in 2015
# master_df = pd.read_csv('/gpfs/data/cms-share/duas/54200/Kevin/Output/FIN-raw-rates-plots/generating-tables/MASTER-DF.csv')

# ## read in ICD 10 count data
# icd10_count_lst = [pd.read_csv('/gpfs/data/cms-share/duas/54200/Zoey/ICD10/Output/icd10-counts-per-zipcode-with-mbsf-2015-using-{}-dgns-columns.csv'.format(i))
#                    for i in [2, 25]]
# icd10_count_df = pd.concat(icd10_count_lst)

# ## select 2015 data from master-df
# master_df2015 = master_df[master_df['Year']==2015]
# master_df_prior2015 = master_df[master_df['Year'] < 2015]
# master_df2015 = master_df2015.merge(icd10_count_df, on=['ZipCode', "NumDiagnosisColsUsed", "OutcomeGroup", "Year"],
#                                     suffixes=['_icd9', '_icd10'],
#                                     how='left')

# master_df2015['NumClaims_icd10'] = master_df2015['NumClaims_icd10'].fillna(0)
# master_df2015['NumClaims'] = master_df2015['NumClaims_icd9'] + master_df2015['NumClaims_icd10']

# master_df2015 = master_df2015.drop(columns=['NumClaims_icd9', 'NumClaims_icd10'])


# master_df_new = pd.concat([master_df_prior2015, master_df2015])
# master_df_new.to_csv('/gpfs/data/cms-share/duas/54200/Zoey/ICD10/Output/MASTER-DF-UPDATE.csv',
#                      index=False)

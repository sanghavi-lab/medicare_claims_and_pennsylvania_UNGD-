# 7/7/20

# This file summarizes (aggregates) MBSF data by zip code, e.g. total number of beneficiaries per zip code,
# then joins it with the aggregated MedPAR data, i.e. the number of ICD diagnoses for each outcome per zip code.



import pandas as pd
import numpy as np


month2num = pd.DataFrame({'monthstr': ['JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC'],
    'months_alive': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]})


for year in range(2002, 2016):

    print('Starting year {} ==========='.format(year))

    # Read in mbsf
    mbsf = pd.read_csv('/gpfs/data/sanghavi-lab/Kevin/Output/THES-DID-INP-REF-ZIP-mbsf-zipcode-subsets/THES-DID-INP-REF-ZIP-mbsf-zipcode-subset-{}.csv'.format(year),
            dtype='object', engine='python', encoding='latin')
    print('Read in mbsf:')
    print(mbsf.head())
    print(mbsf.shape)

    # Filter mbsf to those with Part A and B for as many months as they were alive that year
    mbsf = mbsf.merge(month2num, how='left', left_on=mbsf['BENE_DEATH_DT'].str[2:5], right_on='monthstr')
    mbsf.loc[mbsf['months_alive'].isna(), 'months_alive'] = 12

    mbsf = mbsf[(mbsf['months_alive'] == mbsf['BENE_SMI_CVRAGE_TOT_MONS'].astype('int32')) &
            (mbsf['months_alive'] == mbsf['BENE_HI_CVRAGE_TOT_MONS'].astype('int32'))]

    print('Filtered to FFS:')
    print(mbsf.head())
    print(mbsf.shape)

    # Aggregate mbsf to get number per zip code
    sex_col = ('BENE_' if year <= 2010 else '') + 'SEX_IDENT_CD'
    age_col = ('BENE_' if year <= 2010 else '') + 'AGE_AT_END_REF_YR'
    race_col = 'BENE_RACE_CD'
    esrd_col = ('BENE_' if year <= 2010 else '') + 'ESRD_IND'

    if year < 2006 or year == 2009 or year == 2010:   # only difference is duals
        mbsf_agg = (mbsf.astype({age_col: 'int32'})
            .groupby('ZIP_CD').agg(
                nBenes = pd.NamedAgg(column='BENE_ID', aggfunc='size'),
                nDeaths = pd.NamedAgg(column='BENE_DEATH_DT', aggfunc=(lambda series: sum(series.notna()))),
                nMales = pd.NamedAgg(column=sex_col, aggfunc=(lambda series: sum(series == '1'))),
                meanAge = pd.NamedAgg(column=age_col, aggfunc=np.mean),
                medianAge = pd.NamedAgg(column=age_col, aggfunc=np.median),
                firstQuartileAge = pd.NamedAgg(column=age_col, aggfunc=(lambda series: np.quantile(series, 0.25))),
                thirdQuartileAge = pd.NamedAgg(column=age_col, aggfunc=(lambda series: np.quantile(series, 0.75))),
                nAge65_69 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(65, 70))))),
                nAge70_74 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(70, 75))))),
                nAge75_79 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(75, 80))))),
                nAge80_84 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(80, 85))))),
                nAge85_89 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(85, 90))))),
                nAge90 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series >= 90))),
                nUnknownRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '0'))),
                nWhiteRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '1'))),
                nBlackRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '2'))),
                nOtherRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '3'))),
                nAsianRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '4'))),
                nHispanicRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '5'))),
                nNorthAmericanNativeRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '5'))),
                nESRD = pd.NamedAgg(column=esrd_col, aggfunc=(lambda series: sum(series == 'Y')))
            )
            .assign(nDuals=np.NaN))

    else:
        mbsf_agg = (mbsf.astype({age_col: 'int32'})
            .groupby('ZIP_CD').agg(
                nBenes = pd.NamedAgg(column='BENE_ID', aggfunc='size'),
                nDeaths = pd.NamedAgg(column='BENE_DEATH_DT', aggfunc=(lambda series: sum(series.notna()))),
                nMales = pd.NamedAgg(column=sex_col, aggfunc=(lambda series: sum(series == '1'))),
                meanAge = pd.NamedAgg(column=age_col, aggfunc=np.mean),
                medianAge = pd.NamedAgg(column=age_col, aggfunc=np.median),
                firstQuartileAge = pd.NamedAgg(column=age_col, aggfunc=(lambda series: np.quantile(series, 0.25))),
                thirdQuartileAge = pd.NamedAgg(column=age_col, aggfunc=(lambda series: np.quantile(series, 0.75))),
                nAge65_69 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(65, 70))))),
                nAge70_74 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(70, 75))))),
                nAge75_79 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(75, 80))))),
                nAge80_84 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(80, 85))))),
                nAge85_89 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series.isin(range(85, 90))))),
                nAge90 = pd.NamedAgg(column=age_col, aggfunc=(lambda series: sum(series >= 90))),
                nUnknownRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '0'))),
                nWhiteRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '1'))),
                nBlackRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '2'))),
                nOtherRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '3'))),
                nAsianRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '4'))),
                nHispanicRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '5'))),
                nNorthAmericanNativeRace = pd.NamedAgg(column=race_col, aggfunc=(lambda series: sum(series == '5'))),
                nESRD = pd.NamedAgg(column=esrd_col, aggfunc=(lambda series: sum(series == 'Y'))),
                nDuals = pd.NamedAgg(column='DUAL_STUS_CD_06', aggfunc = (lambda series: sum(series.notna())))
            ))

    mbsf_agg.to_csv('/gpfs/data/sanghavi-lab/Kevin/Output/FIN-mbsf-agg-statistics/FIN-mbsf-agg-statistics-{}.csv'.format(year))

    
    for num_dgns_cols in [2, 25]:
        # Read in zip code outcomes data and merge to these denominators
        zipcodes = pd.read_csv('/gpfs/data/sanghavi-lab/Kevin/Output/FIN-icd-counts-per-zipcode-with-mbsf/FIN-icd-counts-per-zipcode-with-mbsf-{0}-using-{1}-dgns-columns.csv'.format(year, num_dgns_cols),
                dtype={'ZipCode': 'object'})

        zipcodes = zipcodes.merge(mbsf_agg, how='left', left_on='ZipCode', right_on='ZIP_CD') # .fillna(0, downcast='infer')
        zipcodes = zipcodes.assign(Incidence = zipcodes['NumClaims'] / zipcodes['nBenes'])
        print('Assigned incidences to zipcodes:')
        print(zipcodes.head())
        print(zipcodes.shape)

        # Join with zip codes dataset to get which state and region each is in
        zipcode_states = pd.read_csv('/gpfs/data/sanghavi-lab/Kevin/Data/included-zipcodes-list.csv', dtype='object')
        zipcodes = zipcodes.merge(zipcode_states, how='left', on='ZipCode')

        # Write to file
        zipcodes.to_csv('/gpfs/data/sanghavi-lab/Kevin/Output/FIN-icd-rates-per-zipcode/FIN-icd-rates-per-zipcode-{0}-using-{1}-dgns-columns.csv'.format(year, num_dgns_cols), index=False)
        print('Written to file.')







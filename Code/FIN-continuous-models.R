## This script is to build 
## several DID models to study the effect of well development on a few health outcomes
## by comparing NY and PA

library(dplyr)
library(stringr)
library(tidyr)
library(MASS)   # for negative binomial model
library(mfx)    # for clustered SEs in neg bin model
library(ggplot2)
options(tibble.print_max = 200)
options(nwarnings = 10000)

# READ AND ARRANGE DATA =======================

# Spud data
agg_spuds <- read.csv("/gpfs/data/sanghavi-lab/TRANSFER_DESTROY/dua54200/Kevin/Output/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv") %>%
  tbl_df()

# Cleaned MedPAR/MBSF data
master_df <- read.csv("/gpfs/data/sanghavi-lab/TRANSFER_DESTROY/dua54200/Kevin/Output/FIN-raw-rates-plots/generating-tables/MASTER-DF.csv") %>%
  tbl_df() %>%
  filter(Year >= 2002) %>%
  left_join(agg_spuds, by=c("Year" = "year", "ZipCode" = "zipcode")) %>%
  replace_na(list(NewSpuds = 0, CumulativeSpuds = 0, NewSpudDensity = 0, CumulativeSpudDensity = 0)) %>%
  mutate(NewSpudDensity = NewSpudDensity * 2589988.1103, CumulativeSpudDensity = CumulativeSpudDensity * 2589988.1103) %>%    # convert from spuds/m^2 to spuds/mi^2
  mutate(NewSpudsPerBene = NewSpuds / nBenes, CumulativeSpudsPerBene = CumulativeSpuds / nBenes) %>%
  mutate(ZipCode = as.factor(ZipCode))

# Names of outcomes we're studying
# Get outcomes as list (effectively a dictionary from outcome group name, e.g. "stroke", to list of ICD9 codes)
outcomes_df <- read.csv("/gpfs/data/sanghavi-lab/TRANSFER_DESTROY/dua54200/Kevin/Data/FIN-outcome-group-ICDs.csv") %>%
  tbl_df() %>%
  group_by(OutcomeGroup) %>%
  summarize(ICD9s_string = paste0(ICD9, collapse="/"))
outcomes <- setNames(strsplit(outcomes_df$ICD9s_string, "/"), outcomes_df$OutcomeGroup)


# # SELECT ONLY ZIP CODES FIRST GOT WELL DEVELOPMENT IN 2008 AND AFTERWARDS ---------------------------------
distinct_zip_year =
  master_df %>%
  filter(StudyGroup=='PA Border') %>%
  distinct(ZipCode, Year, NewSpuds, CumulativeSpuds, CumulativeSpudDensity) %>%
  arrange(ZipCode, Year, NewSpuds, CumulativeSpuds) %>%
  group_by(ZipCode) %>%
  mutate(growth = NewSpuds/lag(NewSpuds)) %>%
  mutate(growth_lag = growth/lag(growth))

first_spuds = master_df %>%
  filter(StudyGroup=='PA Border') %>%
  distinct(ZipCode, Year, NewSpuds) %>%
  filter(NewSpuds!=0) %>%
  arrange(ZipCode, Year) %>%
  distinct(ZipCode, .keep_all = T)

treatment_df = first_spuds %>% filter(Year>=2008)


# ## plot the trend of hospitalization rate --------
# plot_df <- 
#   master_df %>%
#   filter(!is.na(Incidence)) %>%
#   filter((ZipCode %in%
#             treatment_df$ZipCode) |
#            (StudyGroup %in% c("NY Mid", "NY Border"))) %>%
#   filter(NumDiagnosisColsUsed == 2) %>% 
#   group_by(StudyGroup, Year, OutcomeGroup) %>% 
#   summarise(claims = sum(NumClaims),
#             benes = sum(nBenes)) %>% 
#   mutate(HosRate100 = ifelse(Year==2015,
#                              claims/benes * 365/237 * 100,
#                              100*claims/benes),
#          Year = as.factor(Year)) %>% 
#   filter(OutcomeGroup!="no-ami-ihd") %>% 
#   mutate(OutcomeGroup = case_when(OutcomeGroup == "ami" ~ "AMI (subset of IHD)",
#                                   OutcomeGroup == "ihd" ~ "Ischemic heart disease (IHD, including AMI)",
#                                   OutcomeGroup == "copd" ~ "Chornic obstructive pulmonary disease (COPD)",
#                                   OutcomeGroup == "heart-failure" ~ "Heart Failure",
#                                   OutcomeGroup == "stroke" ~ "Stroke"))
# 
# p <- 
#   ggplot(plot_df, 
#          aes(x = Year, y = HosRate100, color = StudyGroup)) +
#   geom_point() +
#   geom_path(aes(group = StudyGroup)) +
#   facet_wrap(OutcomeGroup~. , scales = "free_y") +
#   labs(y = "Hospitalizations Per Hundred Beneficiaries",
#        caption = "Notes: the health outcome is determined by the first 2 diagnosis codes in MedPAR hospital admission data") +
#   scale_color_manual(name="Study Region", 
#                      values=c("#004488", "#BB5566", "#DDAA33"),
#                      breaks=c("NY Mid", "NY Border", "PA Border"),
#                      labels=c("Main control", "Alt. control (bordering)", "Exposed")) +
#   scale_shape_manual(name="Study Region",
#                      values=c(15, 16, 17),
#                      breaks=c("NY Mid", "NY Border", "PA Border"),
#                      labels=c("Main control", "Alt. control (bordering)", "Exposed")) +
#   theme_bw() + 
#   theme(legend.box.background = element_rect(color="black", size=1),
#         plot.caption = element_text(hjust = 0, size = 20),
#         axis.title = element_text(size = 24),
#         legend.position = c(0.95, 0.2), legend.justification = c(0.95, 0.2),
#         legend.key.height = unit(2, 'cm'), #change legend key height
#         legend.key.width = unit(2, 'cm'), #change legend key width
#         legend.title = element_text(size=20), #change legend title font size
#         legend.text = element_text(size=20))
# 
# ggsave(p, width = 18, height = 15,
#        filename = '/gpfs/data/sanghavi-lab/Zoey/gardner/fracing/Final_Code_Output/FIN-hospitalization-trends.png')
# 

# # RUN DID REGRESSION WITH TWO-YEAR WASHOUT PERIOD ONLY 2008 AND AFTERWARDS-------------------------

## Build the model based on zipcode-level data

did_output_df <- tibble(
  Treatment = character(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Formula = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimate = numeric(0),
  StdErr = numeric(0),
  ZVal = numeric(0),
  PUnadjusted = numeric(0),
  RateRatio = numeric(0),
  RateRatioStdErr = numeric(0),
  PVal = numeric(0)
)


for (outcome_name in names(outcomes)) {
  for (control_region in c("NY Border", "NY Mid")) {
    for (num_dgns_cols in c(2, 25)) {

      print(paste0(outcome_name, control_region, num_dgns_cols, "---------", sep = " "))
      model_df <-
        master_df %>%
        filter(!is.na(Incidence)) %>%
        filter((ZipCode %in%
                  treatment_df$ZipCode) |
                 StudyGroup==control_region) %>%
        filter(NumDiagnosisColsUsed == num_dgns_cols) %>%
        filter(OutcomeGroup==outcome_name) %>%
        mutate(treated = ifelse(StudyGroup=='PA Border', 1, 0),
               post = ifelse(Year>=2008, 1, 0),
               OffsetBenes = offset(log(nBenes)),
               Year = as.factor(Year)) %>%
        filter(!Year %in% c(2008, 2009))
      
      ## run the simple DID model
      treatment = "treated:post"
      formula <- paste0("NumClaims ~ treated*post + OffsetBenes")

      mfx_res <- negbinirr(formula, model_df, clustervar1="ZipCode")
      model <- mfx_res$fit
      coefficients <- summary(model)$coefficients

      did_output_df <- did_output_df %>%
        add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                ModelSpecification = "Neg. binomial, two-year washout period (08&09) DiD, clustered SEs by zip code",
                Formula = formula, Package = "mfx::negbinirr",
                ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                Estimate = coefficients[treatment, "Estimate"],
                StdErr = coefficients[treatment, "Std. Error"],
                ZVal = coefficients[treatment, "z value"],
                PUnadjusted = coefficients[treatment, "Pr(>|z|)"],
                RateRatio = mfx_res$irr[treatment, "IRR"],
                RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                PVal = mfx_res$irr[treatment, "P>|z|"])
      ## run Zip Code FE with binary treatment
      treatment = "treated:post"
      formula <- paste0("NumClaims ~ treated:post + post + ZipCode + OffsetBenes")

      mfx_res <- negbinirr(formula, model_df, clustervar1="ZipCode")
      model <- mfx_res$fit
      coefficients <- summary(model)$coefficients

      did_output_df <- did_output_df %>%
        add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                ModelSpecification = "Neg. binomial, two-year washout period (08&09) Zip Code FE with binary treatment, clustered SEs by zip code",
                Formula = formula, Package = "mfx::negbinirr",
                ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                Estimate = coefficients[treatment, "Estimate"],
                StdErr = coefficients[treatment, "Std. Error"],
                ZVal = coefficients[treatment, "z value"],
                PUnadjusted = coefficients[treatment, "Pr(>|z|)"],
                RateRatio = mfx_res$irr[treatment, "IRR"],
                RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                PVal = mfx_res$irr[treatment, "P>|z|"])
      ## run year FE with binary treatment
      treatment = "treated:post"
      formula <- paste0("NumClaims ~ treated:post + treated + Year + OffsetBenes")

      mfx_res <- negbinirr(formula, model_df, clustervar1="ZipCode")
      model <- mfx_res$fit
      coefficients <- summary(model)$coefficients

      did_output_df <- did_output_df %>%
        add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                ModelSpecification = "Neg. binomial, two-year washout period (08&09) Year FE with binary treatment for large, medium and small development Zip Codes in PA as treatmnet, clustered SEs by zip code",
                Formula = formula, Package = "mfx::negbinirr",
                ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                Estimate = coefficients[treatment, "Estimate"],
                StdErr = coefficients[treatment, "Std. Error"],
                ZVal = coefficients[treatment, "z value"],
                PUnadjusted = coefficients[treatment, "Pr(>|z|)"],
                RateRatio = mfx_res$irr[treatment, "IRR"],
                RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                PVal = mfx_res$irr[treatment, "P>|z|"])
      ## run the twfe with binary treatment
      treatment = "treated:post"
      formula <- paste0("NumClaims ~ treated:post + Year + ZipCode + OffsetBenes")

      mfx_res <- negbinirr(formula, model_df, clustervar1="ZipCode")
      model <- mfx_res$fit
      coefficients <- summary(model)$coefficients

      did_output_df <- did_output_df %>%
        add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                ModelSpecification = "Neg. binomial, two-year washout period (08&09) TWFE with binary treatment, clustered SEs by zip code",
                Formula = formula, Package = "mfx::negbinirr",
                ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                Estimate = coefficients[treatment, "Estimate"],
                StdErr = coefficients[treatment, "Std. Error"],
                ZVal = coefficients[treatment, "z value"],
                PUnadjusted = coefficients[treatment, "Pr(>|z|)"],
                RateRatio = mfx_res$irr[treatment, "IRR"],
                RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                PVal = mfx_res$irr[treatment, "P>|z|"])
    }
  }
}


print('regression is complete')
write.csv(did_output_df,
          file = '/gpfs/data/sanghavi-lab/Zoey/gardner/fracing/Final_Code_Output/FIN_DID_results_washout_only2008andAfter_ZipcodeLevel.csv')

# # RUN PLACEBO TEST FOR DID REGRESSION WITH TWO-YEAR WASHOUT PERIOD ONLY 2008 AND AFTERWARDS-------------------------

## Build the model based on state-level data, aggregating Zip Codes in PA and NY

## Build the model based on zipcode-level data

did_output_df <- tibble(
  Treatment = character(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Formula = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimate = numeric(0),
  StdErr = numeric(0),
  ZVal = numeric(0),
  PUnadjusted = numeric(0),
  RateRatio = numeric(0),
  RateRatioStdErr = numeric(0),
  PVal = numeric(0)
)


for (outcome_name in names(outcomes)) {
  for (control_region in c("NY Border", "NY Mid")) {
    for (num_dgns_cols in c(2, 25)) {
      # zip <- rbind(large, medium, small)
      # zip <- zip %>% filter(Yearfirst >= 2008)
      print(paste0(outcome_name, control_region, num_dgns_cols, "---------", sep = " "))

      model_df <-
        master_df %>%
        filter(!is.na(Incidence)) %>%
        filter((ZipCode %in%
                  treatment_df$ZipCode) |
                 StudyGroup==control_region) %>%
        filter(NumDiagnosisColsUsed == num_dgns_cols) %>%
        filter(OutcomeGroup==outcome_name) %>%
        filter(Year < 2008) %>%
        mutate(treated = ifelse(StudyGroup=='PA Border', 1, 0),
               post = ifelse(Year>=2004, 1, 0),
               OffsetBenes = offset(log(nBenes)),
               Year = as.factor(Year)) %>%
        filter(!Year %in% c(2004, 2005))
      ## run the simple DID model
      treatment = "treated:post"
      formula <- paste0("NumClaims ~ treated*post + OffsetBenes")

      mfx_res <- negbinirr(formula, model_df, clustervar1="ZipCode")
      model <- mfx_res$fit
      coefficients <- summary(model)$coefficients

      print(mfx_res$irr)
      did_output_df <- did_output_df %>%
        add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                ModelSpecification = "Neg. binomial, two-year washout period (04&05) DiD, clustered SEs by zip code",
                Formula = formula, Package = "mfx::negbinirr",
                ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                Estimate = coefficients[treatment, "Estimate"],
                StdErr = coefficients[treatment, "Std. Error"],
                ZVal = coefficients[treatment, "z value"],
                PUnadjusted = coefficients[treatment, "Pr(>|z|)"],
                RateRatio = mfx_res$irr[treatment, "IRR"],
                RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                PVal = mfx_res$irr[treatment, "P>|z|"])
      ## run Zip Code FE with binary treatment
      treatment = "treated:post"
      formula <- paste0("NumClaims ~ treated:post + post + ZipCode + OffsetBenes")

      mfx_res <- negbinirr(formula, model_df, clustervar1="ZipCode")
      model <- mfx_res$fit
      coefficients <- summary(model)$coefficients
      print(summary(warnings()))
      # print(mfx_res$irr)
      did_output_df <- did_output_df %>%
        add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                ModelSpecification = "Neg. binomial, two-year washout period (04&05) Zip Code FE with binary treatment, clustered SEs by zip code",
                Formula = formula, Package = "mfx::negbinirr",
                ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                Estimate = coefficients[treatment, "Estimate"],
                StdErr = coefficients[treatment, "Std. Error"],
                ZVal = coefficients[treatment, "z value"],
                PUnadjusted = coefficients[treatment, "Pr(>|z|)"],
                RateRatio = mfx_res$irr[treatment, "IRR"],
                RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                PVal = mfx_res$irr[treatment, "P>|z|"])
      ## run year FE with binary treatment
      treatment = "treated:post"
      formula <- paste0("NumClaims ~ treated:post + treated + Year + OffsetBenes")

      mfx_res <- negbinirr(formula, model_df, clustervar1="ZipCode")
      model <- mfx_res$fit
      coefficients <- summary(model)$coefficients
      print(summary(warnings()))

      # print(mfx_res$irr)
      did_output_df <- did_output_df %>%
        add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                ModelSpecification = "Neg. binomial, two-year washout period (04&05) Year FE with binary treatment for large, medium and small development Zip Codes in PA as treatmnet, clustered SEs by zip code",
                Formula = formula, Package = "mfx::negbinirr",
                ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                Estimate = coefficients[treatment, "Estimate"],
                StdErr = coefficients[treatment, "Std. Error"],
                ZVal = coefficients[treatment, "z value"],
                PUnadjusted = coefficients[treatment, "Pr(>|z|)"],
                RateRatio = mfx_res$irr[treatment, "IRR"],
                RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                PVal = mfx_res$irr[treatment, "P>|z|"])
      ## run the twfe with binary treatment
      treatment = "treated:post"
      formula <- paste0("NumClaims ~ treated:post + Year + ZipCode + OffsetBenes")

      mfx_res <- negbinirr(formula, model_df, clustervar1="ZipCode")
      model <- mfx_res$fit
      coefficients <- summary(model)$coefficients
      print(summary(warnings()))

      # print(mfx_res$irr)
      did_output_df <- did_output_df %>%
        add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                ModelSpecification = "Neg. binomial, two-year washout period (04&05) TWFE with binary treatment, clustered SEs by zip code",
                Formula = formula, Package = "mfx::negbinirr",
                ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                Estimate = coefficients[treatment, "Estimate"],
                StdErr = coefficients[treatment, "Std. Error"],
                ZVal = coefficients[treatment, "z value"],
                PUnadjusted = coefficients[treatment, "Pr(>|z|)"],
                RateRatio = mfx_res$irr[treatment, "IRR"],
                RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                PVal = mfx_res$irr[treatment, "P>|z|"])
    }
  }
}

print('regression is complete')
write.csv(did_output_df,
          file = '/gpfs/data/sanghavi-lab/Zoey/gardner/fracing/Final_Code_Output/FIN_placebo_results_washout_only2008andAfter_ZipcodeLevel.csv')












# # RUN PLACEBO TEST FOR DID REGRESSION WITH TWO-YEAR WASHOUT PERIOD ONLY 2008 AND AFTERWARDS-------------------------

## Build the model based on zipcode-level data

did_output_df <- tibble(
  Treatment = character(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Formula = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimate = numeric(0),
  StdErr = numeric(0),
  ZVal = numeric(0),
  PUnadjusted = numeric(0),
  RateRatio = numeric(0),
  RateRatioStdErr = numeric(0),
  PVal = numeric(0)
)


for (outcome_name in names(outcomes)) {
  for (control_region in c("NY Border", "NY Mid")) {
    for (num_dgns_cols in c(2, 25)) {
      model_df <-
        master_df %>%
        filter(!is.na(Incidence)) %>%
        filter((ZipCode %in%
                  treatment_df$ZipCode) |
                 StudyGroup==control_region) %>%
        filter(NumDiagnosisColsUsed == num_dgns_cols) %>%
        filter(OutcomeGroup==outcome_name) %>%
        mutate(treated = ifelse(StudyGroup=='PA Border', 1, 0),
               post = ifelse(Year>=2008, 1, 0),
               OffsetBenes = offset(log(nBenes)),
               Year = as.factor(Year)) %>%
        filter(!Year %in% c(2008, 2009)) %>%
        mutate(Year2003 = ifelse(Year==2003, 1, 0), ## sometimes the reference level of interaction is not set at treated:2002
               Year2004 = ifelse(Year==2004, 1, 0),
               Year2005 = ifelse(Year==2005, 1, 0),
               Year2006 = ifelse(Year==2006, 1, 0),
               Year2007 = ifelse(Year==2007, 1, 0),
               Year2010 = ifelse(Year==2010, 1, 0),
               Year2011 = ifelse(Year==2011, 1, 0),
               Year2012 = ifelse(Year==2012, 1, 0),
               Year2013 = ifelse(Year==2013, 1, 0),
               Year2014 = ifelse(Year==2014, 1, 0),
               Year2015 = ifelse(Year==2015, 1, 0))
      ## run the twfe with binary treatment
      formula <- paste0("NumClaims ~ treated:Year2003 + treated:Year2004 + treated:Year2005 + treated:Year2006 + treated:Year2007 +
                         treated:Year2010 + treated:Year2011 + treated:Year2012 + treated:Year2013 + treated:Year2014 +
                         treated:Year2015 + Year + ZipCode + OffsetBenes")

      mfx_res <- negbinirr(formula, model_df, clustervar1="ZipCode")
      model <- mfx_res$fit
      coefficients <- summary(model)$coefficients
      # print(mfx_res$irr)
      for (year in c(2003, 2004, 2005, 2006, 2007, 2010, 2011, 2012, 2013, 2014, 2015)){

        treatment = paste0("treated:Year", year)
        print(treatment)
        did_output_df <- did_output_df %>%
          add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                  ModelSpecification = "Neg. binomial, two-year washout period (08-09) TWFE with binary treatment, clustered SEs by zip code",
                  Formula = formula, Package = "mfx::negbinirr",
                  ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                  Estimate = coefficients[treatment, "Estimate"],
                  StdErr = coefficients[treatment, "Std. Error"],
                  ZVal = coefficients[treatment, "z value"],
                  PUnadjusted = coefficients[treatment, "Pr(>|z|)"],
                  RateRatio = mfx_res$irr[treatment, "IRR"],
                  RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                  PVal = mfx_res$irr[treatment, "P>|z|"])
      }
    }
  }
}


print('regression is complete')
write.csv(did_output_df,
          file = '/gpfs/data/sanghavi-lab/Zoey/gardner/fracing/Final_Code_Output/FIN_placebo_DID_results_only2008andAfter_ZipcodeLevel.csv')

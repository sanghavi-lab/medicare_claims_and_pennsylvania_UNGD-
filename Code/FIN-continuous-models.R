# 7/7/20

# This script estimates negative binomial models and creates a spreadsheet comparing the results
# of the various model specifications, essentially tables 2 and 3 in the publication.

# The models vary in:
#  * Treatment: new well density, cumulative well density
#    - For new well density, models are also run controlling for existing well density in the zip code
#  * Outcome: ICD9 codes/outcomes vary
#  * Geographical regions included in the model: different unexposed regions (NY Border, NY Mid) are paired with the exposed region (PA Border)

# Reads in:
#  Cleaned/organized hospitalization data:    /gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/generating-tables/MASTER-DF.csv
#  Cleaned spud data:                         /gpfs/data/sanghavi-lab/Kevin/Output/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv
#  List of health outcomes:                   /gpfs/data/sanghavi-lab/Kevin/Data/FIN-outcome-group-ICDs.csv

# Writes out:
#  Spreadsheet with model results:            /gpfs/data/sanghavi-lab/Kevin/Output/FIN-continuous-models-output.csv

# For prior code version see PUB-ZIP-continuous-models.R





library(dplyr)
library(stringr)
library(tidyr)
library(MASS)   # for negative binomial model
library(mfx)    # for clustered SEs in neg bin model




# READ AND ARRANGE DATA =======================


# Spud data
agg_spuds <- read.csv("/gpfs/data/sanghavi-lab/Kevin/Output/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv") %>%
  tbl_df()

# Cleaned MedPAR/MBSF data
master_df <- read.csv("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/generating-tables/MASTER-DF.csv") %>%
  tbl_df() %>%
  filter(Year >= 2002) %>%
  left_join(agg_spuds, by=c("Year" = "year", "ZipCode" = "zipcode")) %>%
  replace_na(list(NewSpuds = 0, CumulativeSpuds = 0, NewSpudDensity = 0, CumulativeSpudDensity = 0)) %>%
  mutate(NewSpudDensity = NewSpudDensity * 2589988.1103, CumulativeSpudDensity = CumulativeSpudDensity * 2589988.1103) %>%    # convert from spuds/m^2 to spuds/mi^2
  mutate(NewSpudsPerBene = NewSpuds / nBenes, CumulativeSpudsPerBene = CumulativeSpuds / nBenes) %>%
  mutate(ZipCode = as.factor(ZipCode))
print("Got master df (joined):")
print(master_df)

# Names of outcomes we're studying
# Get outcomes as list (effectively a dictionary from outcome group name, e.g. "stroke", to list of ICD9 codes)
outcomes_df <- read.csv("/gpfs/data/sanghavi-lab/Kevin/Data/FIN-outcome-group-ICDs.csv") %>%
  tbl_df() %>%
  group_by(OutcomeGroup) %>%
  summarize(ICD9s_string = paste0(ICD9, collapse="/"))
outcomes <- setNames(strsplit(outcomes_df$ICD9s_string, "/"), outcomes_df$OutcomeGroup)
print("Got outcomes list:")
print(outcomes)









# START MODELS =================================


# Keep the following table of information produced by each model, including RRs, standard errors, etc.

model_output_df <- tibble(
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
  RateRatio = numeric(0),
  RateRatioStdErr = numeric(0),
  PVal = numeric(0),
  Gamma = numeric(0),
  GammaStdErr = numeric(0),
  GammaPVal = numeric(0),
  FittedHospitalizationRateInExposedRegion = numeric(0),
  FittedHospitalizationRateInControlRegion = numeric(0),
  PredictedCounterfactualRateInExposedRegion = numeric(0),
  FittedTotalCasesInExposedRegion = numeric(0),
  FittedTotalCasesInControlRegion = numeric(0),
  PredictedTotalCasesCounterfactualInExposedRegion = numeric(0)
)


# Iterate through different model specifications that we're performing and record results in the table

for (treatment in c("NewSpudDensity", "CumulativeSpudDensity")) {
  for (outcome_name in names(outcomes)) {
    for (control_region in c("NY Border", "NY Mid")) {
      for (num_dgns_cols in c(2, 25)) {


        print('====================')
        print(paste(treatment, outcome_name, control_region))
        print('\n\n\n')
          
        # Filter to relevant data frame -
        # Each row is one zipcode-year, with incidence rate, StudyGroup, ...
        model_df <- master_df %>%
          filter(OutcomeGroup == outcome_name) %>%
          filter(StudyGroup == "PA Border" | StudyGroup == control_region) %>%
          filter(NumDiagnosisColsUsed == num_dgns_cols) %>%
          filter(!is.na(Incidence))

        counterfactual_df <- model_df %>%    # used for counterfactual predictions (if there were no fracking present)
          mutate(NewSpuds = 0, CumulativeSpuds = 0,
                 NewSpudDensity = 0, CumulativeSpudDensity = 0,
                 NewSpudsPerBene = 0, CumulativeSpudsPerBene = 0)


        # RUN NEG BINOMIAL MODELS:
        

        # Model: Zip code FE, no state indicators, clustering by zip code, Negative Binomial
        model_df_mfx <- model_df %>%   # need to do this since including functions in the formula call gives an error when using clustering, for some reason
          mutate(Year = as.factor(Year), OffsetBenes = offset(log(nBenes)))   # ZipCode already a factor
        formula <- paste0("NumClaims ~ ", treatment, " + Year + ZipCode + OffsetBenes")
        mfx_res <- negbinirr(formula, model_df_mfx, clustervar1="ZipCode")
        model <- mfx_res$fit
        coefficients <- summary(model)$coefficients

        counterfactual_df_mfx <- counterfactual_df %>%
          mutate(Year = as.factor(Year), OffsetBenes = offset(log(nBenes)))
        zipcode_predictions_tbl <- tibble(
          ZipCode = model_df_mfx$ZipCode,
          Year = model_df_mfx$Year,
          nBenes = model_df_mfx$nBenes,
          OffsetBenes = model_df_mfx$OffsetBenes,
          ExposedRegion = model_df_mfx$StudyGroup == "PA Border",
          FittedHospitalizationCount = fitted(model),
          PredictedCounterfactualCount = predict(model, newdata=counterfactual_df_mfx, type="response")
        )
        exposed_predictions_tbl <- zipcode_predictions_tbl %>% filter(ExposedRegion)
        control_predictions_tbl <- zipcode_predictions_tbl %>% filter(!ExposedRegion)
        model_output_df <- model_output_df %>%
          add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                  ModelSpecification = "Neg. binomial, zip code FE, clustered SEs by zip code",
                  Formula = formula, Package = "mfx::negbinirr",
                  ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                  Estimate = coefficients[treatment, "Estimate"],
                  StdErr = coefficients[treatment, "Std. Error"],
                  RateRatio = mfx_res$irr[treatment, "IRR"],
                  RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                  PVal = mfx_res$irr[treatment, "P>|z|"],
                  Gamma = NA,
                  GammaStdErr = NA,
                  GammaPVal = NA,
                  FittedHospitalizationRateInExposedRegion = sum(exposed_predictions_tbl$FittedHospitalizationCount) / sum(exposed_predictions_tbl$nBenes),
                  FittedHospitalizationRateInControlRegion = sum(control_predictions_tbl$FittedHospitalizationCount) / sum(control_predictions_tbl$nBenes),
                  PredictedCounterfactualRateInExposedRegion = sum(exposed_predictions_tbl$PredictedCounterfactualCount) / sum(exposed_predictions_tbl$nBenes),
                  FittedTotalCasesInExposedRegion = sum(exposed_predictions_tbl$FittedHospitalizationCount),
                  FittedTotalCasesInControlRegion = sum(control_predictions_tbl$FittedHospitalizationCount),
                  PredictedTotalCasesCounterfactualInExposedRegion = sum(exposed_predictions_tbl$PredictedCounterfactualCount))


        # If doing a "new UNGD exposure" instead of a "cumulative UNGD exposure", 
        # run an additional model controlling for existing UNGD activity.
        if (grepl("New", treatment)) {
          corresponding_cumulative_treatment <- str_replace(treatment, "New", "Cumulative")   # e.g. "NewSpudDensity" becomes "CumulativeSpudDensity"
          model_df_new <- model_df_mfx %>%
            mutate(PreviousTreatment = (!!as.name(corresponding_cumulative_treatment)) - (!!as.name(treatment)))
                # e.g. CumulativeSpudDensity - NewSpudDensity = spud density prior to this year
          formula <- paste0("NumClaims ~ ", treatment, " + PreviousTreatment + Year + ZipCode + OffsetBenes")
          mfx_res <- negbinirr(formula, model_df_new, clustervar1="ZipCode")
          model <- mfx_res$fit
          coefficients <- summary(model)$coefficients

          counterfactual_df_new <- counterfactual_df_mfx %>%
            mutate(PreviousTreatment = 0)
          zipcode_predictions_tbl <- tibble(
            ZipCode = model_df_mfx$ZipCode,
            Year = model_df_mfx$Year,
            nBenes = model_df_mfx$nBenes,
            OffsetBenes = model_df_mfx$OffsetBenes,
            ExposedRegion = model_df_mfx$StudyGroup == "PA Border",
            FittedHospitalizationCount = fitted(model),
            PredictedCounterfactualCount = predict(model, newdata=counterfactual_df_new, type="response")
          )
          exposed_predictions_tbl <- zipcode_predictions_tbl %>% filter(ExposedRegion)
          control_predictions_tbl <- zipcode_predictions_tbl %>% filter(!ExposedRegion)
          model_output_df <- model_output_df %>%
            add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
                    ModelSpecification = "Neg. binomial, controlling for prior years' exposure, zip code FE, clustered SEs by zip code",
                    Formula = formula, Package = "mfx::negbinirr",
                    ClusteringAtZipCode = TRUE, ControlRegion = control_region,
                    Estimate = coefficients[treatment, "Estimate"],
                    StdErr = coefficients[treatment, "Std. Error"],
                    RateRatio = mfx_res$irr[treatment, "IRR"],
                    RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
                    PVal = mfx_res$irr[treatment, "P>|z|"],
                    Gamma = coefficients["PreviousTreatment", "Estimate"],
                    GammaStdErr = coefficients["PreviousTreatment", "Std. Error"],
                    GammaPVal = mfx_res$irr["PreviousTreatment", "Std. Err."],
                    FittedHospitalizationRateInExposedRegion = sum(exposed_predictions_tbl$FittedHospitalizationCount) / sum(exposed_predictions_tbl$nBenes),
                    FittedHospitalizationRateInControlRegion = sum(control_predictions_tbl$FittedHospitalizationCount) / sum(control_predictions_tbl$nBenes),
                    PredictedCounterfactualRateInExposedRegion = sum(exposed_predictions_tbl$PredictedCounterfactualCount) / sum(exposed_predictions_tbl$nBenes),
                    FittedTotalCasesInExposedRegion = sum(exposed_predictions_tbl$FittedHospitalizationCount),
                    FittedTotalCasesInControlRegion = sum(control_predictions_tbl$FittedHospitalizationCount),
                    PredictedTotalCasesCounterfactualInExposedRegion = sum(exposed_predictions_tbl$PredictedCounterfactualCount))
        }


        # COMMENTED OUT BELOW:
        # Non-clustering models

        # # Model: Zip code FE, no state indicators, no clustering, Negative Binomial - using mfx package
        # model_df_mfx <- model_df %>%   # need to do this since including functions in the formula call gives an error when using clustering, for some reason
        #   mutate(Year = as.factor(Year), OffsetBenes = offset(log(nBenes)))   # ZipCode already a factor
        # formula <- paste0("NumClaims ~ ", treatment, " + Year + ZipCode + OffsetBenes")
        # mfx_res <- negbinirr(formula, model_df_mfx)
        # model <- mfx_res$fit
        # coefficients <- summary(model)$coefficients

        # counterfactual_df_mfx <- counterfactual_df %>%
        #   mutate(Year = as.factor(Year), OffsetBenes = offset(log(nBenes)))
        # zipcode_predictions_tbl <- tibble(
        #   ZipCode = model_df_mfx$ZipCode,
        #   Year = model_df_mfx$Year,
        #   nBenes = model_df_mfx$nBenes,
        #   OffsetBenes = model_df_mfx$OffsetBenes,
        #   ExposedRegion = model_df_mfx$StudyGroup == "PA Border",
        #   FittedHospitalizationCount = fitted(model),
        #   PredictedCounterfactualCount = predict(model, newdata=counterfactual_df_mfx, type="response")
        # )
        # exposed_predictions_tbl <- zipcode_predictions_tbl %>% filter(ExposedRegion)
        # control_predictions_tbl <- zipcode_predictions_tbl %>% filter(!ExposedRegion)
        # model_output_df <- model_output_df %>%
        #   add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
        #           ModelSpecification = "Neg. binomial, zip code FE (mfx)",
        #           Formula = formula, Package = "mfx::negbinirr",
        #           ClusteringAtZipCode = FALSE, ControlRegion = control_region,
        #           Estimate = coefficients[treatment, "Estimate"],
        #           StdErr = coefficients[treatment, "Std. Error"],
        #           RateRatio = mfx_res$irr[treatment, "IRR"],
        #           RateRatioStdErr = mfx_res$irr[treatment, "Std. Err."],
        #           PVal = mfx_res$irr[treatment, "P>|z|"],
        #           FittedHospitalizationRateInExposedRegion = sum(exposed_predictions_tbl$FittedHospitalizationCount) / sum(exposed_predictions_tbl$nBenes),
        #           FittedHospitalizationRateInControlRegion = sum(control_predictions_tbl$FittedHospitalizationCount) / sum(control_predictions_tbl$nBenes),
        #           PredictedCounterfactualRateInExposedRegion = sum(exposed_predictions_tbl$PredictedCounterfactualCount) / sum(exposed_predictions_tbl$nBenes),
        #           FittedTotalCasesInExposedRegion = sum(exposed_predictions_tbl$FittedHospitalizationCount),
        #           FittedTotalCasesInControlRegion = sum(control_predictions_tbl$FittedHospitalizationCount),
        #           PredictedTotalCasesCounterfactualInExposedRegion = sum(exposed_predictions_tbl$PredictedCounterfactualCount))


        # # Model: Zip code FE, no state indicators, no clustering, Negative Binomial - using MASS package (glm.nb)
        # formula <- paste0("NumClaims ~ ", treatment, " + as.factor(Year) + as.factor(ZipCode) + offset(log(nBenes))")
        # model <- glm.nb(formula, data=model_df)
        # coefficients <- summary(model)$coefficients

        # zipcode_predictions_tbl <- tibble(
        #   ZipCode = model_df$ZipCode,
        #   Year = model_df$Year,
        #   nBenes = model_df$nBenes,
        #   ExposedRegion = model_df$StudyGroup == "PA Border",
        #   FittedHospitalizationCount = fitted(model),
        #   PredictedCounterfactualCount = predict(model, newdata=counterfactual_df, type="response")
        # )
        # exposed_predictions_tbl <- zipcode_predictions_tbl %>% filter(ExposedRegion)
        # control_predictions_tbl <- zipcode_predictions_tbl %>% filter(!ExposedRegion)
        # model_output_df <- model_output_df %>%
        #   add_row(Treatment = treatment, Outcome = outcome_name, DiagnosisColsUsed = num_dgns_cols,
        #           ModelSpecification = "Neg. binomial, zip code FE",
        #           Formula = formula, Package = "MASS::glm.nb",
        #           ClusteringAtZipCode = FALSE, ControlRegion = control_region,
        #           Estimate = coefficients[treatment, "Estimate"],
        #           StdErr = coefficients[treatment, "Std. Error"],
        #           RateRatio = exp(coefficients[treatment, "Estimate"]),
        #           RateRatioStdErr = NA,
        #           PVal = coefficients[treatment, "Pr(>|z|)"],
        #           FittedHospitalizationRateInExposedRegion = sum(exposed_predictions_tbl$FittedHospitalizationCount) / sum(exposed_predictions_tbl$nBenes),
        #           FittedHospitalizationRateInControlRegion = sum(control_predictions_tbl$FittedHospitalizationCount) / sum(control_predictions_tbl$nBenes),
        #           PredictedCounterfactualRateInExposedRegion = sum(exposed_predictions_tbl$PredictedCounterfactualCount) / sum(exposed_predictions_tbl$nBenes),
        #           FittedTotalCasesInExposedRegion = sum(exposed_predictions_tbl$FittedHospitalizationCount),
        #           FittedTotalCasesInControlRegion = sum(control_predictions_tbl$FittedHospitalizationCount),
        #           PredictedTotalCasesCounterfactualInExposedRegion = sum(exposed_predictions_tbl$PredictedCounterfactualCount))


      }
    }
  }
}



# WRITE OUTPUT TO FILE ===================

write.csv(model_output_df, "/gpfs/data/sanghavi-lab/Kevin/Output/FIN-continuous-models-output.csv", row.names=F)
print("Written model_output_df:")
print(model_output_df)





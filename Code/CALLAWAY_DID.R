## USE THE did PACAKAGE DEVELOPED BY Brantly Callaway AND Pedro H. C. Sant’Anna
## IN THEIR PAPER OF "DIFFERENCE-IN-DIFFERENCE WITH MULTIPLE TIME PERIODS"

## THE FUNCTION IS UPDATED WITH AN ADDED ARGUMENT SO WE CAN SET UP A UNIVERSAL PRE-TREATMENT PERIOD (WHICH IS 2009 IN OUR STUDY)
## PREVIOUSLY, ALL YEARS AFTER 2009 IS COMPARED WITH 2009, AND ALL YEARS BEFORE WERE COMPARED WITH 1 YEAR EARLIER.
## THIS RESULTS IN A CHANGE IN THE PARALLEL TREND PLOT BECAUSE PREVIOUSLY WE CALCULATE OURSELVES THE PRE-PERIOD TREND

## OTHER THAN THE MAIN RESULTS, WE ALSO RUN THE ANALYSIS WITH SLIGHTLY DIFFERENT SPECIFICATIONS FOR SENSITIVITY ANALYSIS

library(dplyr)
library(stringr)
library(tidyr)
library(did)
library(ggplot2)
options(tibble.print_max = 100)
set.seed(129) # set seed for bootstrapping

# READ AND ARRANGE DATA =======================

# Spud data
agg_spuds <- read.csv("/mnt/labshares/sanghavi-lab/Zoey/fracing/data/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv") %>%
  tbl_df()

# Cleaned MedPAR/MBSF data
master_df <- read.csv("/mnt/labshares/sanghavi-lab/Zoey/fracing/data/MASTER-DF-UPDATE.csv") %>%
  tbl_df() %>%
  filter(Year >= 2002) %>%
  left_join(agg_spuds, by=c("Year" = "year", "ZipCode" = "zipcode")) %>%
  replace_na(list(NewSpuds = 0, CumulativeSpuds = 0, NewSpudDensity = 0, CumulativeSpudDensity = 0)) %>%
  mutate(NewSpudDensity = NewSpudDensity * 2589988.1103, CumulativeSpudDensity = CumulativeSpudDensity * 2589988.1103) %>%    # convert from spuds/m^2 to spuds/mi^2
  mutate(NewSpudsPerBene = NewSpuds / nBenes, CumulativeSpudsPerBene = CumulativeSpuds / nBenes)

# Names of outcomes we're studying
# Get outcomes as list (effectively a dictionary from outcome group name, e.g. "stroke", to list of ICD9 codes)
outcomes_df <- read.csv("/mnt/labshares/sanghavi-lab/Zoey/fracing/data/FIN-outcome-group-ICDs.csv") %>%
  tbl_df() %>%
  group_by(OutcomeGroup) %>%
  summarize(ICD9s_string = paste0(ICD9, collapse="/"))
outcomes <- setNames(strsplit(outcomes_df$ICD9s_string, "/"), outcomes_df$OutcomeGroup)


## divide zip codes into groups based on the time they first got well development
first_spuds = master_df %>%
  filter(StudyGroup=='PA Border') %>% ## PA Border is the treated group
  distinct(ZipCode, Year, NewSpuds) %>%
  filter(NewSpuds!=0) %>% ## exclude years with no UNGD
  arrange(ZipCode, Year) %>% ## order by year to get the first year of well developmend
  distinct(ZipCode, .keep_all = T)
# print(first_spuds %>% group_by(Year) %>% count())
# Year     n
# <int> <int>
#   1  2005     2
# 2  2006     1
# 3  2007     2
# 4  2008    16
# 5  2009    10
# 6  2010    10
# 7  2011     5
# 8  2012     2
# 9  2013     1
# 10  2014     2

# ## write to csv the zipcodes in PA Border that first got well development within 2008 - 2010
# write.csv(first_spuds %>% filter(Year %in% c(2008, 2009, 2010)),
#           "/mnt/labshares/sanghavi-lab/Zoey/fracing/data/included-zip-codes-PA-Border.csv")

master_df <-
  master_df %>% ## merge the year zip codes first got well development with master_df
  left_join(first_spuds %>% dplyr::select(ZipCode, Year),
            by = "ZipCode",
            suffix = c("", "first")) %>%
  mutate(ClaimsPerBene = NumClaims/nBenes *100, ## create outcome variable as the number of health outcomes per beneficiary within a zip code
         Yearfirst = ifelse(is.na(Yearfirst), 0, Yearfirst), ## if the row doesn't have a Yearfirst variable, it belongs to the control group
         ZipCode = as.numeric(ZipCode) ##idname must be numeric in att_gt function
  )

## calculate the weight of each zip code as the Medicare population of the zip code divided by the total Medicare population within the study group
total_benes <-
  master_df %>%
  distinct(ZipCode, StudyGroup, Year, nBenes) %>%
  group_by(StudyGroup, Year) %>%
  summarise(total_bene = sum(nBenes))

master_df <-
  master_df %>%
  left_join(total_benes, by = c("StudyGroup", "Year")) %>%
  mutate(model_weight = nBenes/total_bene)


## CALLAWAY DID YEARFIRST BETWEEN 2008 - 2010 MODEL ---------------------------------------------

## run did model for each health outcome with Yearfirst between 2008 - 2010 ---------------

# create tables to store regression results
group_time_output_df <- tibble(
  Model = character(0),
  TreatmentEffect = character(0),
  Group = numeric(0),
  Time = numeric(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimates = numeric(0),
  StandardError = numeric(0),
  ConfidenceIntervalLower = numeric(0),
  ConfidenceIntervalUpper = numeric(0),
  Significant = logical(0)
)

group_time_output_df_boot <- tibble(
  Model = character(0),
  TreatmentEffect = character(0),
  Group = numeric(0),
  Time = numeric(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimates = numeric(0),
  StandardError = numeric(0),
  ConfidenceIntervalLower = numeric(0),
  ConfidenceIntervalUpper = numeric(0),
  Significant = logical(0)
)

group_time_output_df_varying <- tibble(
  Model = character(0),
  TreatmentEffect = character(0),
  Group = numeric(0),
  Time = numeric(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimates = numeric(0),
  StandardError = numeric(0),
  ConfidenceIntervalLower = numeric(0),
  ConfidenceIntervalUpper = numeric(0),
  Significant = logical(0)
)

group_time_output_df_varying_boot <- tibble(
  Model = character(0),
  TreatmentEffect = character(0),
  Group = numeric(0),
  Time = numeric(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimates = numeric(0),
  StandardError = numeric(0),
  ConfidenceIntervalLower = numeric(0),
  ConfidenceIntervalUpper = numeric(0),
  Significant = logical(0)
)
for (outcome_name in names(outcomes)){ ## run regression for each health outcome, each control region and 2 or 25 diagnosis codes
  for (control_region in c("NY Border", "NY Mid")) {
    for (num_dgns_cols in c(2, 25)) {

      model_df <-
        master_df %>%
        filter(OutcomeGroup==outcome_name & StudyGroup %in% c("PA Border", control_region) & NumDiagnosisColsUsed==num_dgns_cols) %>%
        filter(!is.na(Incidence)) %>%
        filter(Yearfirst %in% c(2008, 2009, 2010, 0)) %>%  ## keep only groups first got UNGD in 2008 - 2010
        mutate(Yearfirst = ifelse(Yearfirst !=0, 2010, 0)) %>% ## pretend treated groups are treated in 2010, a more conservative measure
        mutate(treated = ifelse(StudyGroup=="PA Border", 1, 0),
               post = ifelse(Year >= 2010, 1, 0))
      model = "Weighted, regular CI"
#       # # DID MODEL ---------------------------------------------------------------

      model_results <- ## run the did regression from the did package
        att_gt(yname = "ClaimsPerBene",
               tname = "Year",
               idname = "ZipCode",
               gname = "Yearfirst",
               xformla = ~ 1,
               data = model_df,
               weightsname = "model_weight",
               bstrap = FALSE,
               cband = FALSE,
               base_period = "universal")


      ## store group-time estimates
      group_time_output <- cbind(
        Model = rep(model, length(model_results$att)),
        TreatmentEffect = rep("Group Time Effects", length(model_results$att)),
        Group = model_results$group,
        Time = model_results$t,
        Outcome = rep(outcome_name, length(model_results$att)),
        DiagnosisColsUsed = rep(num_dgns_cols, length(model_results$att)),
        ModelSpecification = rep("Brantly Callaway and Pedro H.C. Sant’Anna", length(model_results$att)),
        Package = rep("did", length(model_results$att)),
        ClusteringAtZipCode = rep(FALSE, length(model_results$att)),
        ControlRegion = rep(control_region, length(model_results$att)),
        Estimates = model_results$att,
        StandardError = model_results$se,
        ConfidenceIntervalLower = model_results$att - model_results$se*model_results$c,
        ConfidenceIntervalUpper = model_results$att + model_results$se*model_results$c,
        Significant = ((model_results$att - model_results$se*model_results$c) * (model_results$att + model_results$se*model_results$c)) > 0
      )

      group_time_output_df <- rbind(group_time_output_df, group_time_output)

      group_time_output_df = group_time_output_df %>%
        mutate(Group = as.numeric(Group),
               Time = as.numeric(Time),
               DiagnosisColsUsed = as.numeric(DiagnosisColsUsed),
               ClusteringAtZipCode = as.logical(ClusteringAtZipCode),
               Estimates = as.numeric(Estimates),
               StandardError = as.numeric(StandardError),
               ConfidenceIntervalLower = as.numeric(ConfidenceIntervalLower),
               ConfidenceIntervalUpper = as.numeric(ConfidenceIntervalUpper),
               Significant = as.logical(Significant)
        )
# #       #     # DID MODEL ADJUST FOR MULTIPLE TESTING---------------------------------------------------------------
      model_results_boot <- ## run the did regression from the did package
        att_gt(yname = "ClaimsPerBene",
               tname = "Year",
               idname = "ZipCode",
               gname = "Yearfirst",
               xformla = ~ 1,
               data = model_df,
               weightsname = "model_weight",
               bstrap = TRUE, ## adjust for multiple testing
               cband = TRUE,
               clustervars = 'ZipCode',
               base_period = "universal")

      ## store group-time estimates
      group_time_output_boot <- cbind(
        Model = rep(model, length(model_results_boot$att)),
        TreatmentEffect = rep("Group Time Effects", length(model_results_boot$att)),
        Group = model_results_boot$group,
        Time = model_results_boot$t,
        Outcome = rep(outcome_name, length(model_results_boot$att)),
        DiagnosisColsUsed = rep(num_dgns_cols, length(model_results_boot$att)),
        ModelSpecification = rep("Brantly Callaway and Pedro H.C. Sant’Anna", length(model_results_boot$att)),
        Package = rep("did", length(model_results_boot$att)),
        ClusteringAtZipCode = rep(TRUE, length(model_results_boot$att)),
        ControlRegion = rep(control_region, length(model_results_boot$att)),
        Estimates = model_results_boot$att,
        StandardError = model_results_boot$se,
        ConfidenceIntervalLower = model_results_boot$att - model_results_boot$se*model_results_boot$c, ## model_results_boot$c ranges from 2.56 - 2.78
        ConfidenceIntervalUpper = model_results_boot$att + model_results_boot$se*model_results_boot$c,
        Significant = ((model_results_boot$att - model_results_boot$se*model_results_boot$c) * (model_results_boot$att + model_results_boot$se*model_results_boot$c)) > 0
      )


      group_time_output_df_boot <- rbind(group_time_output_df_boot, group_time_output_boot)

      group_time_output_df_boot = group_time_output_df_boot %>%
        mutate(Group = as.numeric(Group),
               Time = as.numeric(Time),
               DiagnosisColsUsed = as.numeric(DiagnosisColsUsed),
               ClusteringAtZipCode = as.logical(ClusteringAtZipCode),
               Estimates = as.numeric(Estimates),
               StandardError = as.numeric(StandardError),
               ConfidenceIntervalLower = as.numeric(ConfidenceIntervalLower),
               ConfidenceIntervalUpper = as.numeric(ConfidenceIntervalUpper),
               Significant = as.logical(Significant)
        )
# 
#       # DID MODEL WITH VARYING BASED-PERIOD PRE-TREATMENT ----------------------------------------------------
      model_results_varying <- ## run the did regression from the did package
        att_gt(yname = "ClaimsPerBene",
               tname = "Year",
               idname = "ZipCode",
               gname = "Yearfirst",
               xformla = ~ 1,
               data = model_df,
               weightsname = "model_weight",
               bstrap = FALSE,
               cband = FALSE,
               base_period = "varying")


      group_time_output_varying <- cbind(
        Model = rep(paste0(model, ' varying based-period pretreatment'), length(model_results_varying$att)),
        TreatmentEffect = rep("Group Time Effects", length(model_results_varying$att)),
        Group = model_results_varying$group,
        Time = model_results_varying$t,
        Outcome = rep(outcome_name, length(model_results_varying$att)),
        DiagnosisColsUsed = rep(num_dgns_cols, length(model_results_varying$att)),
        ModelSpecification = rep("Brantly Callaway and Pedro H.C. Sant’Anna", length(model_results_varying$att)),
        Package = rep("did", length(model_results_varying$att)),
        ClusteringAtZipCode = rep(TRUE, length(model_results_varying$att)),
        ControlRegion = rep(control_region, length(model_results_varying$att)),
        Estimates = model_results_varying$att,
        StandardError = model_results_varying$se,
        ConfidenceIntervalLower = model_results_varying$att - model_results_varying$se*model_results_varying$c, ## model_results_boot$c ranges from 2.56 - 2.78
        ConfidenceIntervalUpper = model_results_varying$att + model_results_varying$se*model_results_varying$c,
        Significant = ((model_results_varying$att - model_results_varying$se*model_results_varying$c) * (model_results_varying$att + model_results_varying$se*model_results_varying$c)) > 0
      )

      group_time_output_df_varying <- rbind(group_time_output_df_varying, group_time_output_varying)

      group_time_output_df_varying = group_time_output_df_varying %>%
        mutate(Group = as.numeric(Group),
               Time = as.numeric(Time),
               DiagnosisColsUsed = as.numeric(DiagnosisColsUsed),
               ClusteringAtZipCode = as.logical(ClusteringAtZipCode),
               Estimates = as.numeric(Estimates),
               StandardError = as.numeric(StandardError),
               ConfidenceIntervalLower = as.numeric(ConfidenceIntervalLower),
               ConfidenceIntervalUpper = as.numeric(ConfidenceIntervalUpper),
               Significant = as.logical(Significant)
        )

        model_results_varying_boot <- ## run the did regression from the did package
          att_gt(yname = "ClaimsPerBene",
                 tname = "Year",
                 idname = "ZipCode",
                 gname = "Yearfirst",
                 xformla = ~ 1,
                 data = model_df,
                 weightsname = "model_weight",
                 print_details = T,
                 bstrap = T,
                 cband = T,
                 base_period = "varying")

        group_time_output_varying_boot <- cbind(
          Model = rep(paste0(model, ' varying based-period pretreatment with adjustment for multiple treatment'), length(model_results_varying_boot$att)),
          TreatmentEffect = rep("Group Time Effects", length(model_results_varying_boot$att)),
          Group = model_results_varying_boot$group,
          Time = model_results_varying_boot$t,
          Outcome = rep(outcome_name, length(model_results_varying_boot$att)),
          DiagnosisColsUsed = rep(num_dgns_cols, length(model_results_varying_boot$att)),
          ModelSpecification = rep("Brantly Callaway and Pedro H.C. Sant’Anna", length(model_results_varying_boot$att)),
          Package = rep("did", length(model_results_varying_boot$att)),
          ClusteringAtZipCode = rep(TRUE, length(model_results_varying_boot$att)),
          ControlRegion = rep(control_region, length(model_results_varying_boot$att)),
          Estimates = model_results_varying_boot$att,
          StandardError = model_results_varying_boot$se,
          ConfidenceIntervalLower = model_results_varying_boot$att - model_results_varying_boot$se*model_results_varying_boot$c, ## model_results_boot$c ranges from 2.56 - 2.78
          ConfidenceIntervalUpper = model_results_varying_boot$att + model_results_varying_boot$se*model_results_varying_boot$c,
          Significant = ((model_results_varying_boot$att - model_results_varying_boot$se*model_results_varying_boot$c) * (model_results_varying_boot$att + model_results_varying_boot$se*model_results_varying_boot$c)) > 0
        )

        group_time_output_df_varying_boot <- rbind(group_time_output_df_varying_boot, group_time_output_varying_boot)

        group_time_output_df_varying_boot = group_time_output_df_varying_boot %>%
          mutate(Group = as.numeric(Group),
                 Time = as.numeric(Time),
                 DiagnosisColsUsed = as.numeric(DiagnosisColsUsed),
                 ClusteringAtZipCode = as.logical(ClusteringAtZipCode),
                 Estimates = as.numeric(Estimates),
                 StandardError = as.numeric(StandardError),
                 ConfidenceIntervalLower = as.numeric(ConfidenceIntervalLower),
                 ConfidenceIntervalUpper = as.numeric(ConfidenceIntervalUpper),
                 Significant = as.logical(Significant)
          )
    }
  }
}

## write main did results to csv
write.csv(
  group_time_output_df,
  file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiod2009.csv'
)
write.csv(
  group_time_output_df_boot,
  file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiod2009_BOOTSTRAPPED.csv'
)
#
write.csv(
  group_time_output_df_varying,
  file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiodVARYING.csv'
)
write.csv(
  group_time_output_df_varying_boot,
  file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiodVARYING_BOOTSTRAPPED.csv'
)



      

# ## RUN DID FOR GROUP-TIME AVERGAE EFFECTS AND AGGREGATE FOR CALENDAR TIME AND EVENT TIME AVERAGE EFFECTS --------------
## create tables to store regression results
group_time_output_df <- tibble(
  Model = character(0),
  TreatmentEffect = character(0),
  Group = numeric(0),
  Time = numeric(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimates = numeric(0),
  StandardError = numeric(0),
  ConfidenceIntervalLower = numeric(0),
  ConfidenceIntervalUpper = numeric(0),
  Significant = logical(0)
)

calendar_time_output_df <- tibble(
  Model = character(0),
  TreatmentEffect = character(0),
  Time = numeric(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimates = numeric(0),
  StandardError = numeric(0),
  ConfidenceIntervalLower = numeric(0),
  ConfidenceIntervalUpper = numeric(0),
  Significant = logical(0)
)
event_time_output_df <- tibble(
  Model = character(0),
  TreatmentEffect = character(0),
  Time = numeric(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimates = numeric(0),
  StandardError = numeric(0),
  ConfidenceIntervalLower = numeric(0),
  ConfidenceIntervalUpper = numeric(0),
  Significant = logical(0)
)
#
for (outcome_name in names(outcomes)){ ## run regression for each health outcome, each control region and 2 or 25 diagnosis codes
  for (control_region in c("NY Border", "NY Mid")) {
    for (num_dgns_cols in c(2, 25)) {
      model_df <-
        master_df %>%
        filter(OutcomeGroup==outcome_name & StudyGroup %in% c("PA Border", control_region) & NumDiagnosisColsUsed==num_dgns_cols) %>%
        filter(!is.na(Incidence)) %>%
        filter(Yearfirst %in% c(2008, 2009, 2010, 0)) %>%  ## keep only groups first got UNGD in 2008 - 2010
        mutate(treated = ifelse(StudyGroup=="PA Border", 1, 0))

      model = "Weighted, Regular CI"
      model_results <- ## run the did regression from the did package
        att_gt(yname = "ClaimsPerBene",
               tname = "Year",
               idname = "ZipCode",
               gname = "Yearfirst",
               xformla = ~ 1,
               data = model_df,
               weightsname = "model_weight",
               bstrap = F,
               cband = F)
      ## store group-time estimates
      group_time_output <- cbind(
        Model = rep(model, length(model_results$att)),
        TreatmentEffect = rep("Group Time Effects", length(model_results$att)),
        Group = model_results$group,
        Time = model_results$t,
        Outcome = rep(outcome_name, length(model_results$att)),
        DiagnosisColsUsed = rep(num_dgns_cols, length(model_results$att)),
        ModelSpecification = rep("Brantly Callaway and Pedro H.C. Sant’Anna", length(model_results$att)),
        Package = rep("did", length(model_results$att)),
        ClusteringAtZipCode = rep(FALSE, length(model_results$att)),
        ControlRegion = rep(control_region, length(model_results$att)),
        Estimates = model_results$att,
        StandardError = model_results$se,
        ConfidenceIntervalLower = model_results$att - model_results$se*model_results$c,
        ConfidenceIntervalUpper = model_results$att + model_results$se*model_results$c,
        Significant = ((model_results$att - model_results$se*model_results$c) * (model_results$att + model_results$se*model_results$c)) > 0
      )

      group_time_output_df <- rbind(group_time_output_df, group_time_output)

      group_time_output_df = group_time_output_df %>%
        mutate(Group = as.numeric(Group),
               Time = as.numeric(Time),
               DiagnosisColsUsed = as.numeric(DiagnosisColsUsed),
               ClusteringAtZipCode = as.logical(ClusteringAtZipCode),
               Estimates = as.numeric(Estimates),
               StandardError = as.numeric(StandardError),
               ConfidenceIntervalLower = as.numeric(ConfidenceIntervalLower),
               ConfidenceIntervalUpper = as.numeric(ConfidenceIntervalUpper),
               Significant = as.logical(Significant)
        )

      ## get aggregate treatment effect at calendar time
      agg.ct <- aggte(model_results, type = "calendar")
      calendar_time_output <- cbind(
        Model = rep(model, length(agg.ct$att)),
        TreatmentEffect = rep("Aggregated Calendar Time Effects", length(agg.ct$att)),
        Time = agg.ct$egt,
        Outcome = rep(outcome_name, length(agg.ct$att)),
        DiagnosisColsUsed = rep(num_dgns_cols, length(agg.ct$att)),
        ModelSpecification = rep("Brantly Callaway and Pedro H.C. Sant’Anna", length(agg.ct$att)),
        Package = rep("did", length(agg.ct$att)),
        ClusteringAtZipCode = rep(TRUE, length(agg.ct$att)),
        ControlRegion = rep(control_region, length(agg.ct$att)),
        Estimates = agg.ct$att,
        StandardError = agg.ct$se,
        ConfidenceIntervalLower = agg.ct$att - agg.ct$se*agg.ct$crit.val.egt,
        ConfidenceIntervalUpper = agg.ct$att + agg.ct$se*agg.ct$crit.val.egt,
        Significant = ((agg.ct$att - agg.ct$se*agg.ct$crit.val.egt) * (agg.ct$att + agg.ct$se*agg.ct$crit.val.egt)) > 0
      )

      calendar_time_output_df <- rbind(calendar_time_output_df, calendar_time_output)

      calendar_time_output_df = calendar_time_output_df %>%
        mutate(Time = as.numeric(Time),
               DiagnosisColsUsed = as.numeric(DiagnosisColsUsed),
               ClusteringAtZipCode = as.logical(ClusteringAtZipCode),
               Estimates = as.numeric(Estimates),
               StandardError = as.numeric(StandardError),
               ConfidenceIntervalLower = as.numeric(ConfidenceIntervalLower),
               ConfidenceIntervalUpper = as.numeric(ConfidenceIntervalUpper),
               Significant = as.logical(Significant)
        )
      ## get aggregate treatment effect at event time
      agg.es <- aggte(model_results, type = "dynamic")
      event_time_output <- cbind(
        Model = rep(model, length(agg.es$att)),
        TreatmentEffect = rep("Aggregated Calendar Time Effects", length(agg.es$att)),
        Time = agg.es$egt,
        Outcome = rep(outcome_name, length(agg.es$att)),
        DiagnosisColsUsed = rep(num_dgns_cols, length(agg.es$att)),
        ModelSpecification = rep("Brantly Callaway and Pedro H.C. Sant’Anna", length(agg.es$att)),
        Package = rep("did", length(agg.es$att)),
        ClusteringAtZipCode = rep(TRUE, length(agg.es$att)),
        ControlRegion = rep(control_region, length(agg.es$att)),
        Estimates = agg.es$att,
        StandardError = agg.es$se,
        ConfidenceIntervalLower = agg.es$att - agg.es$se*agg.es$crit.val.egt,
        ConfidenceIntervalUpper = agg.es$att + agg.es$se*agg.es$crit.val.egt,
        Significant = ((agg.es$att - agg.es$se*agg.es$crit.val.egt) * (agg.es$att + agg.es$se*agg.es$crit.val.egt)) > 0
      )

      event_time_output_df <- rbind(event_time_output_df, event_time_output)

      event_time_output_df = event_time_output_df %>%
        mutate(Time = as.numeric(Time),
               DiagnosisColsUsed = as.numeric(DiagnosisColsUsed),
               ClusteringAtZipCode = as.logical(ClusteringAtZipCode),
               Estimates = as.numeric(Estimates),
               StandardError = as.numeric(StandardError),
               ConfidenceIntervalLower = as.numeric(ConfidenceIntervalLower),
               ConfidenceIntervalUpper = as.numeric(ConfidenceIntervalUpper),
               Significant = as.logical(Significant)
        )

    }
  }
}
#
write.csv(group_time_output_df,
          file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_GROUPTIME_DID_RESULTS_0810ONLY_preperiodVARYING.CSV')
write.csv(calendar_time_output_df,
          file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_CALENDARTIME_DID_RESULTS_0810ONLY_preperiodVARYING.CSV')

write.csv(event_time_output_df,
          file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_EVENTTIME_DID_RESULTS_0810ONLY_preperiodVARYING.CSV')


# # # # PLOT PRE TREND ----------------------------------------------------------
pretest_df_2009 <- read.csv('/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiod2009.csv') %>%
  filter(Time < 2009)

pretest_df_2009_boot <- read.csv('/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiod2009_BOOTSTRAPPED.csv') %>%
  filter(Time<2009)

pretest_df_varying <- read.csv('/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiodVARYING.csv') %>%
  filter(Time<=2009)

pretest_df_varying_boot <- read.csv('/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/CALLAWAY_FINAL_DID_RESULTS_0810ONLY_preperiodVARYING_BOOTSTRAPPED.csv') %>%
  filter(Time<=2009)

plot_pre_trend <- function(df, filename, base_period='2009'){
  if (base_period==2009){
    df <- df %>% mutate(Estimates = Estimates*(-1), ConfidenceIntervalLower = ConfidenceIntervalLower*(-1), ConfidenceIntervalUpper = ConfidenceIntervalUpper*(-1))
  }
  pNYMid <-
    df %>%
    filter(ControlRegion=="NY Mid") %>%
    filter(Outcome != "no-ami-ihd") %>%
    mutate(DiagnosisColsUsed = ifelse(DiagnosisColsUsed==2, "Primary diagnoses", "Any diagnosis"),
           Outcome = case_when(Outcome == "ami" ~ "AMI",
                               Outcome == "copd" ~ "COPD",
                               Outcome == "ihd" ~ "IHD (including AMI)",
                               Outcome == "heart-failure" ~ "Heart failure",
                               Outcome == "stroke" ~ "Stroke")) %>%
    ggplot(aes(x = Time, y = Estimates)) +
    geom_errorbar(aes(ymin=ConfidenceIntervalLower, ymax=ConfidenceIntervalUpper), color = "grey", width = 0.2) +
    geom_path(aes(group = 1), color = "grey") +
    geom_point() +
    facet_grid(rows = vars(Outcome),
               cols = vars(DiagnosisColsUsed),
               scale = "free_y") +
    labs(x = "Year", y = "Difference-in-differences estimates of hospitalization rates \nper hundred Medicare beneficiaries") +
    theme(axis.title.x = element_text(size = 20, face="bold"),
          axis.title.y = element_text(size = 20, face="bold")) +
    theme_bw()

  pNYBorder <-
    df %>%
    filter(ControlRegion=="NY Border") %>%
    filter(Outcome != "no-ami-ihd") %>%
    mutate(DiagnosisColsUsed = ifelse(DiagnosisColsUsed==2, "Primary diagnoses", "Any diagnosis"),
           Outcome = case_when(Outcome == "ami" ~ "AMI",
                               Outcome == "copd" ~ "COPD",
                               Outcome == "ihd" ~ "IHD (including AMI)",
                               Outcome == "heart-failure" ~ "Heart failure",
                               Outcome == "stroke" ~ "Stroke")) %>%
    ggplot(aes(x = Time, y = Estimates)) +
    geom_errorbar(aes(ymin=ConfidenceIntervalLower, ymax=ConfidenceIntervalUpper), color = "grey", width = 0.2) +
    geom_path(aes(group = 1), color = "grey") +
    geom_point() +
    facet_grid(rows = vars(Outcome),
               cols = vars(DiagnosisColsUsed),
               scale = "free_y") +
    labs(x = "Year", y = "Difference-in-differences estimates of hospitalization rates \nper hundred Medicare beneficiaries") +
    theme(axis.title.x = element_text(size = 15, face="bold"),
          axis.title.y = element_text(size = 15, face="bold")) +
    theme_bw()

  ggsave(pNYMid,
         filename = paste0('/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/pretrend_NYMid', filename, '.eps'))

  ggsave(pNYBorder,
         filename = paste0('/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/pretrend_NYBorder', filename, '.eps'))

}
#
plot_pre_trend(pretest_df_2009, filename = '_basedperiod2009')

plot_pre_trend(pretest_df_varying, filename = '_basedperiodVARYING')

plot_pre_trend(pretest_df_2009_boot, filename = '_basedperiod2009BOOTSTRAPPED')
plot_pre_trend(pretest_df_varying_boot, filename = '_basedperiodVARYING_BOOTSTRAPPED')



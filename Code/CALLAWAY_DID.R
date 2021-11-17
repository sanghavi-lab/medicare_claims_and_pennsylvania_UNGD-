## USE THE did PACAKAGE DEVELOPED BY Brantly Callaway AND Pedro H. C. Sant’Anna
## IN THEIR PAPER OF "DIFFERENCE-IN-DIFFERENCE WITH MULTIPLE TIME PERIODS"

library(dplyr)
library(stringr)
library(tidyr)
library(did)
library(ggplot2)
options(tibble.print_max = 100)

# READ AND ARRANGE DATA =======================

# Spud data
agg_spuds <- read.csv("/mnt/labshares/sanghavi-lab/Zoey/fracing/data/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv") %>%
  tbl_df()

# Cleaned MedPAR/MBSF data
master_df <- read.csv("/mnt/labshares/sanghavi-lab/Zoey/fracing/data/MASTER-DF.csv") %>%
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
  filter(StudyGroup=='PA Border') %>%
  distinct(ZipCode, Year, NewSpuds) %>%
  filter(NewSpuds!=0) %>%
  arrange(ZipCode, Year) %>%
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


# # CALLAWAY DID YEARFIRST BETWEEN 2008 - 2010 MODEL ---------------------------------------------
#
## run did model for each health outcome with Yearfirst between 2008 - 2010

## create tables to store regression results
self_pretest_df <- tibble(
  Model = character(0),
  Time = numeric(0),
  Outcome = character(0),
  DiagnosisColsUsed = numeric(0),
  ModelSpecification = character(0),
  Package = character(0),
  ClusteringAtZipCode = logical(0),
  ControlRegion = character(0),
  Estimates = numeric(0),
  StandardError = numeric(0),
  CILower = numeric(0),
  CIHigher = numeric(0),
  PValue = numeric(0)
)

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
               post = ifelse(Year >= 2010, 1, 0),
               ClaimsPerBene = ifelse(Year!=2015, ClaimsPerBene, ClaimsPerBene*365/273)) ## scale the outcome measure in 2015 because we dropped the last three months of claims in 2015

# PRE-TREND TEST ----------------------------------------------------------
      for (year in seq(2003, 2009)){ ## pre-trend test using 2002 as pre-treatment year and the rest (2003 - 2009) as the treatment year
        model_df_year = model_df %>% filter(Year == 2002 | Year==year) %>%
          mutate(post = ifelse(Year > 2002, 1, 0))
        model_result <- lm(ClaimsPerBene ~ treated*post, data = model_df_year, weights = model_weight) ## simple linear did regression, with Medicare population weights as the model weight
        self_pretest_df <- self_pretest_df %>% ## store the did results in self_pretest_df
          add_row(
            Model = "Simple DID",
            Time = year,
            Outcome = outcome_name,
            DiagnosisColsUsed = num_dgns_cols,
            ModelSpecification = "Linear regression, comparison year is 2002",
            Package = "did",
            ClusteringAtZipCode = FALSE,
            ControlRegion = control_region,
            Estimates = summary(model_result)$coefficients["treated:post", "Estimate"],
            StandardError = summary(model_result)$coefficients["treated:post", "Std. Error"],
            CILower = summary(model_result)$coefficients["treated:post", "Estimate"] - summary(model_result)$coefficients["treated:post", "Std. Error"]*1.96,
            CIHigher = summary(model_result)$coefficients["treated:post", "Estimate"] + summary(model_result)$coefficients["treated:post", "Std. Error"]*1.96,
            PValue = summary(model_result)$coefficients["treated:post", "Pr(>|t|)"]
          )
      }



# DID MODEL ---------------------------------------------------------------
      model = "Weighted, regular CI"
      model_results <- ## run the did regression from the did package
        att_gt(yname = "ClaimsPerBene",
               tname = "Year",
               idname = "ZipCode",
               gname = "Yearfirst",
               xformla = ~ 1,
               data = model_df,
               weightsname = "model_weight",
               bstrap = FALSE,
               cband = FALSE)
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
    }
  }
}
## write main did results to csv
write.csv(
  group_time_output_df,
  file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/FIN_CALLAWAY/CALLAWAY_FINAL_DID_RESULTS_0810ONLY.csv'
)
## write pre-testing results to csv
write.csv(
  self_pretest_df,
  file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/FIN_CALLAWAY/PRETREND_TEST_REFERENCE2002_0810ONLY.csv'
)



# PLOT PRE TREND ----------------------------------------------------------
self_pretest_df <- read.csv('/mnt/labshares/sanghavi-lab/Zoey/fracing/FIN_CALLAWAY/PRETREND_TEST_REFERENCE2002_0810ONLY.csv')

pNYMid <- 
  self_pretest_df %>% 
  filter(ControlRegion=="NY Mid") %>% 
  filter(Outcome != "no-ami-ihd") %>% 
  mutate(DiagnosisColsUsed = ifelse(DiagnosisColsUsed==2, "Primary diagnoses", "Any diagnosis"),
         Outcome = case_when(Outcome == "ami" ~ "AMI",
                             Outcome == "copd" ~ "COPD",
                             Outcome == "ihd" ~ "IHD (including AMI)",
                             Outcome == "heart-failure" ~ "Heart failure",
                             Outcome == "stroke" ~ "Stroke")) %>% 
  ggplot(aes(x = Time, y = Estimates)) + 
  geom_errorbar(aes(ymin=CILower, ymax=CIHigher), color = "grey", width = 0.2) +
  geom_path(aes(group = 1), color = "grey") +
  geom_point() +
  facet_grid(rows = vars(Outcome),
             cols = vars(DiagnosisColsUsed),
             scale = "free_y") +
  labs(x = "Year", y = "Differences in the trend of hospitalization rate \nper hundred Medicare beneficiaries") +
  theme(axis.title.x = element_text(size = 20, face="bold"),
        axis.title.y = element_text(size = 20, face="bold")) +
  theme_bw()

pNYBorder <- 
  self_pretest_df %>% 
  filter(ControlRegion=="NY Border") %>% 
  filter(Outcome != "no-ami-ihd") %>% 
  mutate(DiagnosisColsUsed = ifelse(DiagnosisColsUsed==2, "Primary diagnoses", "Any diagnosis"),
         Outcome = case_when(Outcome == "ami" ~ "AMI",
                             Outcome == "copd" ~ "COPD",
                             Outcome == "ihd" ~ "IHD (including AMI)",
                             Outcome == "heart-failure" ~ "Heart failure",
                             Outcome == "stroke" ~ "Stroke")) %>% 
  ggplot(aes(x = Time, y = Estimates)) + 
  geom_errorbar(aes(ymin=CILower, ymax=CIHigher), color = "grey", width = 0.2) +
  geom_path(aes(group = 1), color = "grey") +
  geom_point() +
  facet_grid(rows = vars(Outcome),
             cols = vars(DiagnosisColsUsed),
             scale = "free_y") +
  labs(x = "Year", y = "Differences in the trend of hospitalization rate \nper hundred Medicare beneficiaries") +
  theme(axis.title.x = element_text(size = 15, face="bold"),
        axis.title.y = element_text(size = 15, face="bold")) +
  theme_bw()

ggsave(pNYMid,
       filename = '/mnt/labshares/sanghavi-lab/Zoey/fracing/FIN_CALLAWAY/pretrend_NYMid.png')

ggsave(pNYBorder,
       filename = '/mnt/labshares/sanghavi-lab/Zoey/fracing/FIN_CALLAWAY/pretrend_NYBorder.png')




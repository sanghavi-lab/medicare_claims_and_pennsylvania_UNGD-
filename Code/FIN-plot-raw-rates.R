# 7/7/20
#
# 
# This file outputs a few things, among them:
#  (a) Plots of demographic trends for each StudyGroup (demographics-plots)
# and, for each outcome group:
#  (b) Plot using only the primary and second diagnosis columns to calculate rates (two-diagnoses-only);
#  (c) Same as (d), but with a superimposed line for the trend of new spud development that year (two-diagnoses-only-with-spuds);
#  (d) Tables with the raw data for (b) as a CSV (generating-tables).
# Finally, it outputs a publishable stack of time-series plots of hospitalization rates for the different outcomes.
# These appear in the folder Kevin/Output/FIN-raw-rate-plots.

library(dplyr)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)


YEARS <- 2002:2015
N_DGNS_COLS <- c(2, 25)



# UTILITIES ==========================

linetypes = list()  # from NumDiagnosisColsUsed to linetype
linetypes[["2"]] = "solid"
linetypes[["25"]] = "dotted"

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}



# GET DATA AND COMBINE INTO ONE MASTER DATAFRAME ==============================

df_list <- list()
i <- 1
for (yr in YEARS) {
  for (nDgns in N_DGNS_COLS) {
    df <- read.csv(paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-icd-rates-per-zipcode/FIN-icd-rates-per-zipcode-", yr, "-using-", nDgns, "-dgns-columns.csv")) %>%
      tbl_df() %>%
      mutate(Year = yr, NumDiagnosisColsUsed = nDgns)
    df_list[[i]] <- df
    i <- i + 1
  }
}
master_df <- do.call(bind_rows, df_list)

print("Got master df")
print(master_df)
write.csv(master_df, "/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/generating-tables/MASTER-DF.csv", row.names=F)





# PLOT TRENDS IN BENEFICIARY TOTAL POPULATIONS, DEMOGRAPHICS (AGE, SEX, RACE DISTR.) OVER TIME ==============

summ <- function (x) sum(x, na.rm=T)

# Get table of demographic characteristics over time, used to make plots
demographics_df <- master_df %>%
  distinct(ZipCode, nBenes, nDeaths, nMales, meanAge, medianAge, firstQuartileAge, thirdQuartileAge, 
	   nAge65_69, nAge70_74, nAge75_79, nAge80_84, nAge85_89, nAge90,
	   nUnknownRace, nWhiteRace, nBlackRace, nOtherRace, nAsianRace, nHispanicRace,
	   nNorthAmericanNativeRace, nESRD, StudyGroup, Year) %>%
  group_by(StudyGroup, Year) %>%
  summarize(TotalBenes = summ(nBenes), nDeaths = summ(nDeaths), nMales = summ(nMales), meanAge = summ(meanAge*nBenes)/summ(nBenes),
	    avgMedianAge = summ(medianAge*nBenes)/summ(nBenes), avgFirstQuartileAge = summ(firstQuartileAge*nBenes)/summ(nBenes),
	    avgThirdQuartileAge = summ(thirdQuartileAge*nBenes)/summ(nBenes), nAge65_69 = summ(nAge65_69), nAge70_74 = summ(nAge70_74),
	    nAge75_79 = summ(nAge75_79), nAge80_84 = summ(nAge80_84), nAge85_89 = summ(nAge85_89), nAge90 = summ(nAge90),
	    nUnknownRace = summ(nUnknownRace), nWhiteRace = summ(nWhiteRace), nBlackRace = summ(nBlackRace), nOtherRace = summ(nOtherRace), 
	    nAsianRace = summ(nAsianRace), nHispanicRace = summ(nHispanicRace), nNorthAmericanNativeRace = summ(nNorthAmericanNativeRace),
	    nESRD = summ(nESRD)) %>%
  mutate(MortalityRate = nDeaths / TotalBenes, PctMale = nMales / TotalBenes, PctAge65_69 = nAge65_69 / TotalBenes, PctAge70_74 = nAge70_74 / TotalBenes,
         PctAge75_79 = nAge75_79 / TotalBenes, PctAge80_84 = nAge80_84 / TotalBenes, PctAge85_89 = nAge85_89 / TotalBenes,
	 PctAge90 = nAge90 / TotalBenes, PctUnknownRace = nUnknownRace / TotalBenes, PctWhiteRace = nWhiteRace / TotalBenes,
	 PctBlackRace = nBlackRace / TotalBenes, PctOtherRace = nOtherRace / TotalBenes, PctAsianRace = nAsianRace / TotalBenes,
	 PctHispanicRace = nHispanicRace / TotalBenes, PctNorthAmericanNativeRace = nNorthAmericanNativeRace / TotalBenes,
	 PctESRD = nESRD / TotalBenes)
print("Got demographics df")
print(demographics_df)
write.csv(demographics_df, "/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/generating-tables/DEMOGRAPHICS-DF.csv", row.names=F)

# Make plot of total beneficiaries over time
total_benes_plot <- ggplot(demographics_df) +
  aes(x=Year, y=TotalBenes, color=StudyGroup) +
  geom_path(aes(group=StudyGroup)) + 
  labs(title="Total Number of Beneficiaries", y="Beneficiaries") +
  ylim(0, 85000)
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/demographics-plots/FIN-total-benes.eps", 
       plot=total_benes_plot)

# Make plot of pct. male over time
pct_male_plot <- ggplot(demographics_df) +
  aes(x=Year, y=PctMale*100, color=StudyGroup) +
  geom_path(aes(group=StudyGroup)) +
  labs(title="Percent Male Among Beneficiaries", y="Percent of Medicare population")
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/demographics-plots/FIN-bene-pct-male.eps", 
       plot=pct_male_plot)

# Make plots of age distribution over time
age_distr_plot_iqr <- ggplot(demographics_df) +
  aes(x=Year, color=StudyGroup, group=StudyGroup) +
  geom_path(aes(y=avgMedianAge), linetype="solid") +
  geom_path(aes(y=avgFirstQuartileAge), linetype="dashed") +
  geom_path(aes(y=avgThirdQuartileAge), linetype="dashed") +
  labs(title="Age Distributions (Weighted Average)",
       subtitle="Showing median (solid) and IQR (dashed)",
       y="Average Median Age with IQR")
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/demographics-plots/FIN-bene-age-distr-iqr.eps", 
       plot=age_distr_plot_iqr)

age_distr_plot_pct <- ggplot(demographics_df %>% mutate(a65_74=100*(PctAge65_69+PctAge70_74), a75_84=100*(PctAge75_79+PctAge80_84), a85=100*(PctAge85_89+PctAge90))) +
  aes(x=Year, color=StudyGroup, group=StudyGroup) +
  geom_path(aes(y=a65_74), linetype="solid") +
  geom_path(aes(y=a75_84), linetype="dashed") +
  geom_path(aes(y=a85), linetype="dotted") +
  labs(title="Zip Code Age Distributions",
       subtitle="Showing % 65-74 (solid), % 75-85 (dashed), % 85+ (dotted)",
       y="Percent of Medicare population") +
  ylim(0, 45)
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/demographics-plots/FIN-bene-age-distr-pct.eps", 
       plot=age_distr_plot_pct)

# Make plot of pct. white over time
pct_race_plot <- ggplot(demographics_df) +
  aes(x=Year, color=StudyGroup, group=StudyGroup) +
  geom_path(aes(y=100*PctWhiteRace), linetype="solid") +
  labs(title="Percent White Among Beneficiaries", y="Percent of Medicare Population")
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/demographics-plots/FIN-bene-pct-white.eps", 
       plot=pct_race_plot)


# Make combined panel of demographic plots
four_demographic_plots <- grid.arrange(arrangeGrob(total_benes_plot + theme(legend.position = "none"),
				                   pct_male_plot + theme(legend.position = "none"), 
				                   age_distr_plot_iqr + theme(legend.position = "none"),
				                   pct_race_plot + theme(legend.position = "none")),
				       get_legend(total_benes_plot),
				       ncol=2, widths=c(8, 1.5))
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/demographics-plots/FIN-four-demographic-plots.eps", 
       plot=four_demographic_plots,
       width=8.5, height=6.5, units="in")





# AGGREGATE HOSPITALIZATION COUNTS ACROSS STUDY REGIONS (SUM ACROSS ALL ZIP CODES IN PA BORDER REGION, ETC) ==============

states_df <- master_df %>%
  group_by(OutcomeGroup, Year, StudyGroup, NumDiagnosisColsUsed) %>%
  summarize(TotalClaimsRaw = sum(NumClaims, na.rm=T),
            TotalBenes = sum(nBenes, na.rm=T)) %>%
  mutate(TotalClaims = ifelse(Year == 2015, TotalClaimsRaw / 273 * 365, TotalClaimsRaw))    # adjust for Sept 30, 2015 cutoff
print('states_df:')
print(states_df)
write.csv(states_df, "/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/generating-tables/STATES-DF.csv", row.names=F)




# GET SPUD DATA (TO COMPARE UNGD TRAJECTORY WITH HOSPITALIZATION RATES) ================================

included_zips <- read.csv("/gpfs/data/sanghavi-lab/Kevin/Data/included-zipcodes-list.csv", colClasses=c("ZipCode" = "character"))

raw_spuds <- read.csv("/gpfs/data/sanghavi-lab/Kevin/Output/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv", colClasses=c("zipcode" = "character")) %>%
  tbl_df() %>%
  filter(zipcode %in% included_zips$ZipCode)

spuds <- raw_spuds %>%
  group_by(year) %>%
  summarize(NewSpuds = sum(NewSpuds),
	    CumulativeSpuds = sum(CumulativeSpuds)) 


print("Spuds:")
print(spuds)





# PLOT OVERALL EXPOSURE PER ZIP CODE (not used in final analysis, just interesting) ================

exposure_df <- master_df %>%
  filter(StudyGroup == "PA Border") %>%
  distinct(ZipCode, Year, nBenes) %>%
  mutate(ZipCode = as.character(ZipCode)) %>%
  left_join(raw_spuds, by=c("Year" = "year", "ZipCode" = "zipcode")) %>%
  mutate(CumulativeSpudsPerBene = CumulativeSpuds / nBenes)
agg_exposure_df <- exposure_df %>%
  group_by(Year) %>%
  summarize(CumulativeSpuds = sum(CumulativeSpuds), nBenes = sum(nBenes)) %>%
  mutate(CumulativeSpudsPerBene = CumulativeSpuds / nBenes)

print("Exposure df:")
print(exposure_df)
cumspudsperbene_plot <- ggplot(exposure_df %>% filter(nBenes >= 11)) + 
  aes(x=Year, y=CumulativeSpudsPerBene) +
  geom_path(aes(group=ZipCode), size=0.5, color="darkgrey") +
  geom_path(data=agg_exposure_df, color="red", size=1.5) +
  labs(y="Cumulative Spuds per Beneficiary")
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/cumulative-spuds-per-beneficiary-time-series.eps", plot=cumspudsperbene_plot)

cumspuds_plot <- ggplot(exposure_df) +
  aes(x=Year, y=CumulativeSpuds) +
  geom_path(aes(group=ZipCode), size=0.5, color="darkgrey") +
  geom_path(data=agg_exposure_df, size=1.5, color="red") +
  labs(y="Cumulative Spuds")
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/cumulative-spuds-time-series.eps", plot=cumspuds_plot)

both_spuds_plots <- grid.arrange(cumspuds_plot, cumspudsperbene_plot, ncol=2)
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/cumulative-spuds-both-time-series.eps", plot=both_spuds_plots, width=8, height=4, units="in")




# PLOT AND SAVE VARIOUS PLOTS FOR EACH OUTCOME OF STUDY ============================================


# This function takes the outcome group string and a string of ICD9 codes (just for labeling/title purposes).
# It saves a bunch of informative plots of the data for reference, but ultimately only returns
# the final plot (time series) for publication.

plot_and_save <- function (outcome_group, ICD9s_string) {
  print(paste0("Plot_and_save: ", outcome_group))

  plot_df <- states_df %>%    # sum over all ICD codes in the group of ICDs for one outcome
    filter(OutcomeGroup == outcome_group) %>%
    mutate(CasesPerHundred = TotalClaims / TotalBenes * 100)
  print("Plot_df:")
  print(plot_df)
  # Write generating table
  write.csv(plot_df, paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/generating-tables/FIN-raw-rates-table-", outcome_group, ".csv"), row.names=F)
  

  # Plot with both lines (2 and 25 dgns cols)
  plot <- ggplot(plot_df) +
    aes(x=Year, y=CasesPerHundred, color=StudyGroup) +
    geom_point() +
    labs(title=paste('Raw incidence per 100:', outcome_group),
	 subtitle=paste("ICD Codes:", ICD9s_string))
  for (nDgns in N_DGNS_COLS) {
    nDgns_filtered_df <- plot_df %>% filter(NumDiagnosisColsUsed == nDgns)
    plot <- plot +
      geom_path(data=nDgns_filtered_df, aes(group=StudyGroup), linetype=linetypes[[as.character(nDgns)]])
  }
  ggsave(paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/with-all-diagnoses/FIN-raw-rates-plot-", outcome_group, ".eps"), 
	 plot=plot)
  
  # Also plot just the primary + first secondary diagnosis codes
  second_dgns_df <- plot_df %>%
    filter(NumDiagnosisColsUsed==2) %>% 
    ungroup()
  second_dgns_plot <- ggplot(second_dgns_df) +
    aes(x=Year, y=CasesPerHundred, color=StudyGroup) + 
    geom_point() +
    geom_path(aes(group=StudyGroup)) +
    labs(title=paste('Raw incidence per 100:', outcome_group, '(2 diagnosis codes)'),
	 subtitle=paste("ICD Codes:", ICD9s_string))
  ggsave(paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/two-diagnoses-only/FIN-raw-rates-plot-", outcome_group, ".eps"), 
	 plot=second_dgns_plot)

  # Also plot the line for fracking new spuds on top of second_dgns_plot
  maxnewspuds <- max(spuds$NewSpuds)
  maxcases <- max(second_dgns_df$CasesPerHundred)
  spuds_adjusted <- spuds %>%
    mutate(NewSpudsAdjusted = NewSpuds / maxnewspuds * maxcases * 0.75)
  second_dgns_plot_with_spuds <- second_dgns_plot +
    geom_path(data=spuds_adjusted, aes(x=year, y=NewSpudsAdjusted, color=NULL), color="black", linetype="dashed")
  ggsave(paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/two-diagnoses-only-with-spuds/FIN-raw-rates-plot-", outcome_group, ".eps"), 
	 plot=second_dgns_plot_with_spuds)


  # Also plot just PA and NY Border groups, with fracking trajectory, from 2002–2015
  pub_df_1 <- second_dgns_df %>%
    filter(StudyGroup %in% c("PA Border", "NY Border")) %>%
    filter(Year >= 2002) %>%
    mutate(StudyGroup = factor(StudyGroup, levels=c("PA Border", "NY Border")))
  pub_plot_1 <- ggplot(pub_df_1) +
    aes(x=Year, y=CasesPerHundred, color=StudyGroup) +
    geom_point(aes(shape=StudyGroup)) +
    geom_path(aes(group=StudyGroup)) +
    geom_path(data=spuds_adjusted, aes(x=year, y=NewSpudsAdjusted, color=NULL), color="black", linetype="dashed") +
    xlim(2002, 2015) +
    scale_color_manual(values=c("red3", "seagreen3"), breaks=c("PA Border", "NY Border")) + 
    scale_shape_manual(values=c(17, 16), breaks=c("PA Border", "NY Border")) +
    labs(title=paste0("Incidence per 100: ", outcome_group),
         subtitle=paste("ICD-9-CM ", ICD9s_string),
         y="Hospitalizations per Hundred Beneficiaries")
  ggsave(paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/pa-ny-borders-2002/FIN-raw-rates-plot-", outcome_group, ".eps"), 
         plot=pub_plot_1)
    
  # Also plot just PA Border and NY Mid groups, with fracking trajectory, from 2002–2015
  pub_df_2 <- second_dgns_df %>%
    filter(StudyGroup %in% c("PA Border", "NY Mid")) %>%
    filter(Year >= 2002) %>%
    mutate(StudyGroup = factor(StudyGroup, levels=c("PA Border", "NY Mid")))
  pub_plot_2 <- ggplot(pub_df_2) +
    aes(x=Year, y=CasesPerHundred, color=StudyGroup) +
    geom_point(aes(shape=StudyGroup)) +
    geom_path(aes(group=StudyGroup)) +
    geom_path(data=spuds_adjusted, aes(x=year, y=NewSpudsAdjusted, color=NULL), color="black", linetype="dashed") +
    xlim(2002, 2015) +
    scale_color_manual(values=c("red3", "seagreen3"), breaks=c("PA Border", "NY Mid")) + 
    scale_shape_manual(values=c(17, 16), breaks=c("PA Border", "NY Mid")) +
    labs(title=paste0("Incidence per 100: ", outcome_group),
         subtitle=paste("ICD-9-CM ", ICD9s_string),
         y="Hospitalizations per Hundred Beneficiaries")
  ggsave(paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/alt-controls-2002/ny-mid-", outcome_group, ".eps"), 
         plot=pub_plot_2)


  # Also plot just PA Border and NY Northeastern groups, with fracking trajectory, from 2002–2015
  pub_df_3 <- second_dgns_df %>%
    filter(StudyGroup %in% c("PA Border", "NY Northeastern")) %>%
    filter(Year >= 2002) %>%
    mutate(StudyGroup = factor(StudyGroup, levels=c("PA Border", "NY Northeastern")))
  pub_plot_3 <- ggplot(pub_df_3) +
    aes(x=Year, y=CasesPerHundred, color=StudyGroup) +
    geom_point(aes(shape=StudyGroup)) +
    geom_path(aes(group=StudyGroup)) +
    geom_path(data=spuds_adjusted, aes(x=year, y=NewSpudsAdjusted, color=NULL), color="black", linetype="dashed") +
    xlim(2002, 2015) +
    scale_color_manual(values=c("red3", "seagreen3"), breaks=c("PA Border", "NY Northeastern")) + 
    scale_shape_manual(values=c(17, 16), breaks=c("PA Border", "NY Northeastern")) +
    labs(title=paste0("Incidence per 100: ", outcome_group),
         subtitle=paste("ICD-9-CM ", ICD9s_string),
         y="Hospitalizations per Hundred Beneficiaries")
  ggsave(paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/alt-controls-2002/ny-northeastern-", outcome_group, ".eps"), 
         plot=pub_plot_3)

  # Also plot just PA Border and PA Mid groups, with fracking trajectory, from 2002–2015
  pub_df_4 <- second_dgns_df %>%
    filter(StudyGroup %in% c("PA Border", "PA Mid")) %>%
    filter(Year >= 2002) %>%
    mutate(StudyGroup = factor(StudyGroup, levels=c("PA Border", "PA Mid")))
  pub_plot_4 <- ggplot(pub_df_4) +
    aes(x=Year, y=CasesPerHundred, color=StudyGroup) +
    geom_point(aes(shape=StudyGroup)) +
    geom_path(aes(group=StudyGroup)) +
    geom_path(data=spuds_adjusted, aes(x=year, y=NewSpudsAdjusted, color=NULL), color="black", linetype="dashed") +
    xlim(2002, 2015) +
    scale_color_manual(values=c("red3", "seagreen3"), breaks=c("PA Border", "PA Mid")) + 
    scale_shape_manual(values=c(17, 16), breaks=c("PA Border", "PA Mid")) +
    labs(title=paste0("Incidence per 100: ", outcome_group),
         subtitle=paste("ICD-9-CM ", ICD9s_string),
         y="Hospitalizations per Hundred Beneficiaries")
  ggsave(paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/alt-controls-2002/pa-mid-", outcome_group, ".eps"), 
         plot=pub_plot_4)

  # Also plot just PA Border and PA Southern groups, with fracking trajectory, from 2002–2015
  pub_df_5 <- second_dgns_df %>%
    filter(StudyGroup %in% c("PA Border", "PA Southern")) %>%
    filter(Year >= 2002) %>%
    mutate(StudyGroup = factor(StudyGroup, levels=c("PA Border", "PA Southern")))
  pub_plot_5 <- ggplot(pub_df_5) +
    aes(x=Year, y=CasesPerHundred, color=StudyGroup) +
    geom_point(aes(shape=StudyGroup)) +
    geom_path(aes(group=StudyGroup)) +
    geom_path(data=spuds_adjusted, aes(x=year, y=NewSpudsAdjusted, color=NULL), color="black", linetype="dashed") +
    xlim(2002, 2015) +
    scale_color_manual(values=c("red3", "seagreen3"), breaks=c("PA Border", "PA Southern")) + 
    scale_shape_manual(values=c(17, 16), breaks=c("PA Border", "PA Southern")) +
    labs(title=paste0("Incidence per 100: ", outcome_group),
         subtitle=paste("ICD-9-CM ", ICD9s_string),
         y="Hospitalizations per Hundred Beneficiaries")
  ggsave(paste0("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/alt-controls-2002/pa-southern-", outcome_group, ".eps"), 
         plot=pub_plot_5)
  

  # Return a publishable plot with NY Border, NY Mid, PA Border lines
  publishable_df <- second_dgns_df %>%
    filter(StudyGroup %in% c("PA Border", "NY Border", "NY Mid")) %>%
    filter(Year >= 2002) %>%
    mutate(StudyGroup = factor(StudyGroup, levels=c("NY Mid", "NY Border", "PA Border")))  # get them in order
  publishable_plot <- ggplot(publishable_df) +
    aes(x=Year, y=CasesPerHundred, color=StudyGroup) +
    geom_point(aes(shape=StudyGroup)) +
    geom_path(aes(group=StudyGroup)) + 
    xlim(2002, 2015) +
    scale_color_manual(name="Study Region", 
                       values=c("#004488", "#BB5566", "#DDAA33"),
                       breaks=c("NY Mid", "NY Border", "PA Border"),
                       labels=c("Main control", "Alt. control (bordering)", "Exposed")) +
    scale_shape_manual(name="Study Region",
                       values=c(15, 16, 17),
                       breaks=c("NY Mid", "NY Border", "PA Border"),
                       labels=c("Main control", "Alt. control (bordering)", "Exposed")) +
    theme_bw() + 
    theme(legend.position=c(0.1, 0.9), legend.box.background = element_rect(color="black", size=1)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()) + 
    ylim(0, NA)   # added 11/1/20


  return (publishable_plot)
  
}



# Now iterate through all the outcomes to produce all the plots

# This function reduces [433, 434, 415.1] to "433-4, 415.1"
reduce_icdstrings <- function (icdstrs) {
  if (any(is.na(suppressWarnings(as.numeric(icdstrs))))) {  # not all numbers (i.e. E codes)
    return (paste0(icdstrs, collapse=", "))
  }
  last_num <- 0
  running_str <- ""
  for (icd in icdstrs) {
    icdnum <- as.numeric(icd)
    if (floor(icdnum) != icdnum) { # not an integer
      running_str <- paste0(running_str, ", ", icd)
      last_num <- 0
    } else {
      if (icdnum == last_num + 1) {  # attach -x to last number
        last_digit <- icdnum %% 10
        if (substr(running_str, nchar(running_str)-1, nchar(running_str)-1) == "-") { # if last part of running string is -y, excise it first
          running_str <- substr(running_str, 1, nchar(running_str)-2)
	}
        running_str <- paste0(running_str, "-", last_digit)
      } else {
        running_str <- paste0(running_str, ", ", icd)
      }
      last_num <- icdnum
    }
  }
  return (substr(running_str, 3, nchar(running_str)))  # trim leading comma and space
}

# Load all outcomes and ICDs available
outcomes2icd9s_df <- read.csv("/gpfs/data/sanghavi-lab/Kevin/Data/FIN-outcome-group-ICDs.csv", stringsAsFactors=F) %>%
  tbl_df()
print(outcomes2icd9s_df)
outcomes2icd9s_df <- outcomes2icd9s_df %>%
  group_by(OutcomeGroup) %>%
  summarize(ICD9s_string = reduce_icdstrings(ICD9))

print("outcomes2icd9s_df:")
print(outcomes2icd9s_df)


# However, we will only actually plot the outcomes listed here, in the order listed here, along with the common legend:

gg_copd <- plot_and_save("copd", outcomes2icd9s_df$ICD9s_string[which(outcomes2icd9s_df$OutcomeGroup=="copd")]) + 
  labs(y="COPD") +
  theme(legend.position="none")
gg_ihd <- plot_and_save("ihd", outcomes2icd9s_df$ICD9s_string[which(outcomes2icd9s_df$OutcomeGroup=="ihd")]) + 
  labs(y="Ischemic heart disease (IHD)") +
  theme(legend.position="none")
gg_ami <- plot_and_save("ami", outcomes2icd9s_df$ICD9s_string[which(outcomes2icd9s_df$OutcomeGroup=="ami")]) + 
  labs(y="AMI (subset of IHD)") +
  theme(legend.position="none")
gg_stroke <- plot_and_save("stroke", outcomes2icd9s_df$ICD9s_string[which(outcomes2icd9s_df$OutcomeGroup=="stroke")]) + 
  labs(y="Stroke") +
  theme(legend.position="none")
gg_heart_failure <- plot_and_save("heart-failure", outcomes2icd9s_df$ICD9s_string[which(outcomes2icd9s_df$OutcomeGroup=="heart-failure")]) + 
  labs(y="Heart Failure") +
  theme(legend.position=c(0, 0), legend.justification=c(0,0))  # bottom-left corner
common_legend <- get_legend(gg_heart_failure)
gg_heart_failure <- gg_heart_failure + 
  theme(legend.position="none")


# Now add the fracking plot at bottom
fracking_plot <- ggplot(spuds) +
  aes(x=year, y=CumulativeSpuds) +
  geom_area(color="darkgoldenrod4", fill="#DDAA33") + 
  xlim(2002, 2015) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=14)) +
  labs(y="Cumulative UNGD Wells")
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/fracking-plot.eps",
       plot=fracking_plot, width=4, height=3)




# FINAL PUBLICATION-READY TIME SERIES PLOT =====================================


# Combine them all into one column of plots
aligned_plots <- align_plots(gg_copd, NULL, #common_legend,
                             gg_ihd, gg_heart_failure, 
                             gg_ami, gg_stroke, 
                             fracking_plot, fracking_plot+theme(axis.title.y=element_blank()),
                             align="vh")
stacked_plots <- do.call(plot_grid, c(aligned_plots, ncol=2)) + 
  draw_grob(common_legend, x=0.6, y=0.8)
ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/stack-tmp.eps", plot=stacked_plots, width=7, height=11)
common_y_axis <- textGrob("Hospitalizations per Hundred Beneficiaries", gp=gpar(fontsize=16), hjust=0.2, rot=90)
common_x_axis <- textGrob("Year", gp=gpar(fontsize=16))
publishable_plot_column <- grid.arrange(arrangeGrob(stacked_plots, left=common_y_axis, bottom=common_x_axis))

ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/publishable_plot_column.eps", 
       plot=publishable_plot_column, width=7, height=11)

ggsave("/gpfs/data/sanghavi-lab/Kevin/Output/FIN-raw-rates-plots/publishable_plot_column.pdf", 
       plot=publishable_plot_column, width=7, height=11)





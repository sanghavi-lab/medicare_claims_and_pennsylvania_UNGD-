library(dplyr)
library(tidyr)
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
# this is the original master dataframe. I updated it with the last three-month of data in 2015.
# 
# df_list <- list()
# i <- 1
# for (yr in YEARS) {
#   for (nDgns in N_DGNS_COLS) {
#     df <- read.csv(paste0("/gpfs/data/sanghavi-lab/TRANSFER_DESTROY/dua54200/Kevin/Output/FIN-icd-rates-per-zipcode/FIN-icd-rates-per-zipcode-", yr, "-using-", nDgns, "-dgns-columns.csv")) %>%
#       tbl_df() %>%
#       mutate(Year = yr, NumDiagnosisColsUsed = nDgns)
#     df_list[[i]] <- df
#     i <- i + 1
#   }
# }
# master_df <- do.call(bind_rows, df_list)
# 
# # print("Got master df")
# # print(master_df)
# # write.csv(master_df, "/gpfs/data/sanghavi-lab/Zoey/garnder/fracing/Final_Code_Output/FIN-raw-rates-plots/generating-tables/MASTER-DF.csv", row.names=F)

## read in the updated master-df
master_df <- read.csv('/mnt/labshares/sanghavi-lab/Zoey/fracing/data/MASTER-DF-UPDATE.csv')

summ <- function (x) sum(x, na.rm=T)

# Get table of demographic characteristics over time
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
write.csv(demographics_df, "/gpfs/data/sanghavi-lab/Zoey/garnder/fracing/Final_Code_Output/FIN-raw-rates-plots/generating-tables/DEMOGRAPHICS-DF.csv", row.names=F)

## READ IN ZIP CODES OF PA BORDER THAT FIRST GOT WELL DEVELOPMENT BETWEEN 2008 - 2010
pa_border_zip <- read.csv('/mnt/labshares/sanghavi-lab/Zoey/fracing/data/included-zip-codes-PA-Border.csv')

## CREATE A TABLE TO SUMMARIZE FINAL ANALYTICAL SAMPLE
su_table <- 
  master_df %>% 
  filter((StudyGroup != "PA Border") | (ZipCode %in% pa_border_zip$ZipCode)) %>% 
  filter(OutcomeGroup != 'no-ami-ihd') %>% 
  group_by(StudyGroup, NumDiagnosisColsUsed, OutcomeGroup) %>% 
  summarize(TotalClaims = sum(NumClaims, na.rm=T))
# write.csv(su_table %>% spread(key = OutcomeGroup, value = TotalClaims),
#           file = '/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/ClaimsCount.csv')

# AGGREGATE HOSPITALIZATION COUNTS ACROSS STUDY REGIONS (SUM ACROSS ALL ZIP CODES IN PA BORDER REGION, ETC) ==============

states_df <- master_df %>%
  filter((StudyGroup != "PA Border") | (ZipCode %in% pa_border_zip$ZipCode)) %>%
  group_by(OutcomeGroup, Year, StudyGroup, NumDiagnosisColsUsed) %>%
  summarize(TotalClaims = sum(NumClaims, na.rm=T),
            TotalBenes = sum(nBenes, na.rm=T))

# GET SPUD DATA (TO COMPARE UNGD TRAJECTORY WITH HOSPITALIZATION RATES) ================================

included_zips <- read.csv("/mnt/labshares/sanghavi-lab/Zoey/fracing/data/included-zipcodes-list.csv", colClasses=c("ZipCode" = "character"))

raw_spuds <- read.csv("/mnt/labshares/sanghavi-lab/Zoey/fracing/data/THES-DID-INP-REF-ZIP-zipcode-aggregated-spuds.csv", colClasses=c("zipcode" = "character")) %>%
  tbl_df()

raw_spuds <- raw_spuds %>%
  filter(zipcode %in% pa_border_zip$ZipCode )

spuds <- raw_spuds %>%
  group_by(year) %>%
  summarize(NewSpuds = sum(NewSpuds),
            CumulativeSpuds = sum(CumulativeSpuds))
print(spuds)

# # PLOT AND SAVE VARIOUS PLOTS FOR EACH OUTCOME OF STUDY ============================================


# This function takes the outcome group string and a string of ICD9 codes (just for labeling/title purposes).
# It saves a bunch of informative plots of the data for reference, but ultimately only returns
# the final plot (time series) for publication.

plot_and_save <- function (outcome_group, ICD9s_string) {
  print(paste0("Plot_and_save: ", outcome_group))

  plot_df <- states_df %>%    # sum over all ICD codes in the group of ICDs for one outcome
    filter(OutcomeGroup == outcome_group) %>%
    mutate(CasesPerHundred = TotalClaims / TotalBenes * 100)

  second_dgns_df <- plot_df %>%
    filter(NumDiagnosisColsUsed==2) %>%
    ungroup()

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
                       labels=c("NY Mid", "NY Border", "PA Border")) +
    scale_shape_manual(name="Study Region",
                       values=c(15, 16, 17),
                       breaks=c("NY Mid", "NY Border", "PA Border"),
                       labels=c("NY Mid", "NY Border", "PA Border")) +
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
outcomes2icd9s_df <- read.csv("/mnt/labshares/sanghavi-lab/Zoey/fracing/data/FIN-outcome-group-ICDs.csv", stringsAsFactors=F) %>%
  tbl_df()
# print(outcomes2icd9s_df)
outcomes2icd9s_df <- outcomes2icd9s_df %>%
  group_by(OutcomeGroup) %>%
  summarize(ICD9s_string = reduce_icdstrings(ICD9))

# print("outcomes2icd9s_df:")
# print(outcomes2icd9s_df)


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
ggsave("/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/fracking-plot.eps",
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
ggsave("/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/stack-tmp.eps", plot=stacked_plots, width=7, height=11)
common_y_axis <- textGrob("Hospitalizations per Hundred Medicare Beneficiaries", gp=gpar(fontsize=16), hjust=0.2, rot=90)
common_x_axis <- textGrob("Year", gp=gpar(fontsize=16))
publishable_plot_column <- grid.arrange(arrangeGrob(stacked_plots, left=common_y_axis, bottom=common_x_axis))

ggsave("/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/publishable_plot_column.eps",
       plot=publishable_plot_column, width=7, height=11)

ggsave("/mnt/labshares/sanghavi-lab/Zoey/fracing/UPDATE_CALLAWAY/publishable_plot_column.pdf",
       plot=publishable_plot_column, width=7, height=11)


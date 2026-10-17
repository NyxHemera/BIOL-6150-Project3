############################################################
################          Step 1         ###################
################        Load Packages    ###################
############################################################

rm(list=ls())
set.seed(13)

library("data.table")
library("dplyr")
library("parallel")
library('stringr')
library('ggplot2')

############################################################
################          Step 2         ###################
##############       Look at our Paricipants    ############
############################################################


# Load the files for our cohort.
cohort = as.data.frame(fread("~/biol6150/Data2/EHR/ParticipantCohort.tsv"))
range(cohort$ParticipantID)
dim(cohort)
head(cohort)

ehr_records = as.data.frame(fread("~/biol6150/Data2/EHR/ParticipantEHR.tsv"))
dim(ehr_records)
head(ehr_records)

#Explore the data a little bit.
cohort %>% count(Sex)
cohort %>% group_by(Sex) %>% summarize(MeanAge = mean(Age))

# Visualize the density of age distribution.
ggplot(cohort, aes(x = Age, fill = Sex, color = Sex)) + 
  geom_density(alpha = 0.3, linewidth = 1) + 
  scale_fill_manual(values = c("F" = "orange", "M" = "blue")) +
  scale_color_manual(values = c("F" = "orange", "M" = "blue")) +
  theme_bw(base_size = 20) + 
  theme(legend.position = "bottom")

############################################################
################          Step 3         ###################
##################       Phecodes      #####################
############################################################

# Let's have a look at the EHR records.
print("Number of rows in our EHR df: ")
dim(ehr_records)
head(ehr_records)

# See the number of people in the EHR data.
print("Number of participants that have EHR records: ")
length(unique(ehr_records$ParticipantID))

print("Number of unique EHR records: ")
length(unique(ehr_records$ICD10CM))

# Read the phecode map for ICD10CM codes.
# This map file is downloaded from the PheWAS Catalog: https://phewascatalog.org/
phecode_map = as.data.frame(fread("~/biol6150/Data//EHR/PhecodeMap.csv"))
dim(phecode_map)
head(phecode_map)

# Extract the IDC10CM codes for our disease of interest: Type 2 Diabetes.
phecode_map_t2d = phecode_map %>% filter(phecode == 250.2)
dim(phecode_map_t2d)
head(phecode_map_t2d,100)

# Extract the IDC10CM codes for the T2D Exclusions
phecode_map_exclusions <- phecode_map %>% filter(between(phecode, 249, 250.99) & phecode != 250.2)
dim(phecode_map_exclusions)
head(phecode_map_exclusions,50)

phecode_map_exclusions %>% filter(icd10cm == 'E09.36')


#This should be empty if above excludes T2D codes
phecode_map_exclusions %>% filter(icd10cm == 'E11.622')


#Label if an EHR entry is for T2D or not.
ehr_records = ehr_records %>% mutate(T2D_Code = case_when(ICD10CM %in% phecode_map_t2d$icd10cm ~ 1, TRUE ~ 0))
dim(ehr_records)
head(ehr_records,25)

#Label if an EHR entry is within the exclusion criteria
ehr_records = ehr_records %>% mutate(T2D_Exclusion_Code = case_when(ICD10CM %in% phecode_map_exclusions$icd10cm ~ 1, TRUE ~ 0))
dim(ehr_records)
head(ehr_records,25)

ehr_records %>% filter(ParticipantID == 72855012)
ehr_records %>% filter(ParticipantID == 10055570)
ehr_records %>% filter(ParticipantID == 10008475)
ehr_records %>% filter(ParticipantID == 96872475)


case_control_cohort <- ehr_records %>%
                        group_by(ParticipantID) %>%
                        summarize(T2DCount = sum(T2D_Code), T2DExclusionCount = sum(T2D_Exclusion_Code)) %>%
                        as.data.frame()

dim(case_control_cohort)
head(case_control_cohort)

case_control_cohort %>% filter(T2DExclusionCount > 0)


#include demographic information
case_control_cohort = merge(case_control_cohort, cohort, by = "ParticipantID")
dim(case_control_cohort)
head(case_control_cohort)

#count the number of cases/controls per sex group.
case_control_cohort_counts = case_control_cohort %>% group_by(Sex) %>% count(T2DCount)
case_control_cohort_counts



ambiguous_cohort   <- case_control_cohort %>% filter(T2DCount == 1)
ambiguous_cohort

case_cohort   <- case_control_cohort %>% filter(T2DCount > 1)
case_cohort

excluded_cohort   <- case_control_cohort %>% filter(T2DCount == 0 & T2DExclusionCount > 0)
excluded_cohort

control_cohort   <- case_control_cohort %>% filter(T2DCount == 0 & T2DExclusionCount == 0)
control_cohort

case_cohort %>% filter(ParticipantID == 10008475)
ehr_records %>% filter(ParticipantID == 10008475)

case_cohort %>% filter(ParticipantID == 96872475)


ehr_records %>% filter(ParticipantID == 22673416)


#you can also check for the prevalence of T2D per sex group
case_control_cohort_counts %>% 
    group_by(Sex) %>%
    summarise(
        Total = sum(n),
        Cases = sum(n[T2DStatus == 1]),
        Prevalence = Cases / Total * 100 
    )



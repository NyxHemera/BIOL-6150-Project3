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
cohort = as.data.frame(fread("~/biol6150/Data/EHR/ParticipantCohort.tsv"))
range(cohort$ParticipantID)
dim(cohort)
head(cohort)

ehr_records = as.data.frame(fread("~/biol6150/Data/EHR/ParticipantEHR.tsv"))
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

# ACTIVE LEARNING [01] //================================
# 1. What does as.data.frame do?
# 2. What's the dimension of our cohort dataset? What are the columns?
# 3. What's the dimension of our EHR dataset? What are the columns?
# 4. Which gender has a higher age in our cohort?

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

# ACTIVE LEARNING [02] //================================
# 1. Why would dim() and length(unique(ParticipantID)) give different numbers?
# 2. What are all the columns in the phecode table? Explain each one in detail.

# Extract the IDC10CM codes for our disease of interest: Type 2 Diabetes.
phecode_map_t2d = phecode_map %>% filter(phecode_str == "Type 2 diabetes")
dim(phecode_map_t2d)
head(phecode_map_t2d,25)

#Label if an EHR entry is for T2D or not.
ehr_records = ehr_records %>% mutate(T2D_Code = case_when(ICD10CM %in% phecode_map_t2d$icd10cm ~ 1, TRUE ~ 0))
dim(ehr_records)
head(ehr_records)

ehr_records %>% filter(ParticipantID == 72855012)
ehr_records %>% filter(ParticipantID == 10055570)

case_control_cohort <- ehr_records %>%
                        group_by(ParticipantID) %>%
                        summarize(T2DStatus = max(T2D_Code)) %>%
                        as.data.frame()

dim(case_control_cohort)
head(case_control_cohort)

#include demographic information
case_control_cohort = merge(case_control_cohort, cohort, by = "ParticipantID")
dim(case_control_cohort)
head(case_control_cohort)

#count the number of cases/controls per sex group.
case_control_cohort_counts = case_control_cohort %>% group_by(Sex) %>% count(T2DStatus)
case_control_cohort_counts

#you can also check for the prevalence of T2D per sex group
case_control_cohort_counts %>% 
    group_by(Sex) %>%
    summarise(
        Total = sum(n),
        Cases = sum(n[T2DStatus == 1]),
        Prevalence = Cases / Total * 100 
    )

# ACTIVE LEARNING [03] //================================
# 1. What does a 0 signify? What does a 1 signify? What type of variable do we call this? How does exclude range relate to this?
# 2. Which gender has more cases of T2D?

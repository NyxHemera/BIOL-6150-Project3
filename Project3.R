#
#
# Group 4
# Chaimae Echcharef and Jordan Rust
#
#

# Load Packages and clear environment

rm(list=ls())
set.seed(13)

library("data.table")
library("dplyr")
library("parallel")
library('stringr')
library('ggplot2')

# Read Participant Records

ehr_records = as.data.frame(fread("~/biol6150/Data2/EHR/ParticipantEHR.tsv"))
dim(ehr_records)
head(ehr_records)

# In the end, our total summed cohort should equal this number:
print("Number of participants that have EHR records: ")
length(unique(ehr_records$ParticipantID))


# Read the phecode map for ICD10CM codes.
# This map file is downloaded from the PheWAS Catalog: https://phewascatalog.org/
phecode_map = as.data.frame(fread("~/biol6150/Data//EHR/PhecodeMap.csv"))
dim(phecode_map)
head(phecode_map)

# Extract the IDC10CM codes for Type 2 Diabetes.
phecode_map_t2d = phecode_map %>% filter(phecode == 250.2)
dim(phecode_map_t2d)
head(phecode_map_t2d,100)

# Extract the IDC10CM codes for the T2D Exclusions
phecode_map_exclusions <- phecode_map %>% filter(between(phecode, 249, 250.99) & phecode != 250.2)
dim(phecode_map_exclusions)
head(phecode_map_exclusions,50)


# This is an example of a valid entry in exclusions, it has phecodes of 249.0 and 250.7, both within the exclusion range of T2D
phecode_map_exclusions %>% filter(icd10cm == 'E09.36')


# This should be empty if above excludes T2D codes and E11.622 has a phecode of 250.2
phecode_map_exclusions %>% filter(icd10cm == 'E11.622')


# Label if an EHR entry is for T2D or not.
ehr_records = ehr_records %>% mutate(T2D_Code = case_when(ICD10CM %in% phecode_map_t2d$icd10cm ~ 1, TRUE ~ 0))
dim(ehr_records)
head(ehr_records,25)

# Label if an EHR entry is within the exclusion criteria
ehr_records = ehr_records %>% mutate(T2D_Exclusion_Code = case_when(ICD10CM %in% phecode_map_exclusions$icd10cm ~ 1, TRUE ~ 0))
dim(ehr_records)
head(ehr_records,25)

# We now have two new columns, both booleans, one for if a row is a T2D phecode, the other if it is within the T2D exclusion range

# This is an example of a user that has a ton of T2D phecode records and a single exclusion phecode
ehr_records %>% filter(ParticipantID == 10465750)

# This will create two new columns within case_control_cohort: T2DCount and T2DExclusionCount. These are the sum of the records under a particular
# participant, showing how many T2D or Exclusion records they have respectively. We will use this in filtering later on.
case_control_cohort <- ehr_records %>%
                        group_by(ParticipantID) %>%
                        summarize(T2DCount = sum(T2D_Code), T2DExclusionCount = sum(T2D_Exclusion_Code)) %>%
                        as.data.frame()

dim(case_control_cohort)
head(case_control_cohort)

# This shows all of our users with exclusion rows
case_control_cohort %>% filter(T2DExclusionCount > 0)


# Below are our four filtering groups: ambiguous, case, excluded, and control.
# Ambiguous and excluded will be combined into one group. Notice we are filtering on 
# T2DCount and T2DExclusionCount

ambiguous_cohort   <- case_control_cohort %>% filter(T2DCount == 1)
ambiguous_cohort

case_cohort   <- case_control_cohort %>% filter(T2DCount > 1)
case_cohort

excluded_cohort   <- case_control_cohort %>% filter(T2DCount == 0 & T2DExclusionCount > 0)
excluded_cohort

control_cohort   <- case_control_cohort %>% filter(T2DCount == 0 & T2DExclusionCount == 0)
control_cohort

print("Total Case: ")
length(case_cohort$ParticipantID)

print("Total Control: ")
length(control_cohort$ParticipantID)

print("Total Excluded: ")
length(ambiguous_cohort$ParticipantID) + length(excluded_cohort$ParticipantID)



# library
library(tidyverse)
library(here)

# data
df_doctors <- read.csv("df_doctors_v20220321 - df_doctors_v20220321.csv", header = T)
df_patients <- read.csv("df_patients_v20220321 - df_patients_v20220321.csv", header = T)
df_sofa_traj <- read.csv("df_traj_v20220321 - df_traj_v20220321.csv", header = T)
df_eval360 <- read.csv("df_eval360_v20220321 - df_eval360_v20220321.csv", header = T)

# Check missing
colSums(is.na(df_doctors))
colSums(is.na(df_patients))
colSums(is.na(df_sofa_traj))

# remove M9: education in df_doctors
df_doctors <- df_doctors %>% select(-M9)

# rename SOFA in df_patients
df_patients <- df_patients %>% rename( SOFA_admission = SOFA)

# convert categorical variables to factors if needed
# df_doctors: all except "overall_score"
df_doctors$icu_sites <- as.factor(df_doctors$icu_sites)
df_doctors$leadership_role <- as.factor(df_doctors$leadership_role)
df_doctors$physician_rank <- as.factor(df_doctors$physician_rank)
df_doctors$resident_rank <- as.factor(df_doctors$resident_rank)
df_doctors$physician_sex <- as.factor(df_doctors$physician_sex)
df_doctors$domain <- as.factor(df_doctors$domain)
df_doctors$physician_age <- as.factor(df_doctors$physician_age)

# df_patients: age/admission_response/sex/discharge_status/primary_diagnosis
df_patients$patient_age <- as.factor(df_patients$patient_age)
df_patients$admission_response <- as.factor(df_patients$admission_response)
df_patients$icu_dept <- as.factor(df_patients$icu_dept)
df_patients$patient_sex <- as.factor(df_patients$patient_sex)
df_patients$discharge_status <- as.factor(df_patients$discharge_status)
df_patients$pri_diag <- as.factor(df_patients$pri_diag)

# convert to Binary Encoding if needed
df_doctors$physician_sex_binary <- ifelse(df_doctors$physician_sex == "Male", 1, 0)
df_patients$patient_sex_binary <- ifelse(df_patients$patient_sex == "Male", 1, 0)

# Join datasets
df_join <- left_join(df_patients, df_doctors, by = "DocID")
df_join <- inner_join(df_join, df_sofa_traj, by = "PtID")

# rename SOFA in df_join to SOFA_daily, SOFA is from df_sofa_traj originally
df_join <- df_join %>% rename( SOFA_daily = SOFA)

#write updated csv files
write_csv(df_doctors,"df_doctors_v20220321 - df_doctors_v20220321_updated.csv")
write_csv(df_patients,"df_patients_v20220321 - df_patients_v20220321_updated.csv")
# write.csv(df_sofa_traj,"df_traj_v20220321 - df_traj_v20220321_updated.csv")
# write.csv(df_eval360,"df_eval360_v20220321 - df_eval360_v20220321_updated.csv")

# write the joined dataset
write_csv(df_join,"merged_df.csv")

# -----------------------------------------------------------------------------------------
# Victoria's part
# -----------------------------------------------------------------------------------------
library(dplyr)
library(stringr)

# rename SOFA to SOFA_daily 
df_join <- df_join %>% rename(SOFA_daily = SOFA)

# categorical vars w/ multiple categories 
icu_dept_map <- data.frame(Category = unique(df_join$icu_dept), Encoded_Value = as.numeric(factor(unique(df_join$icu_dept))))
pri_diag_map <- data.frame(Category = unique(df_join$pri_diag), Encoded_Value = as.numeric(factor(unique(df_join$pri_diag))))
domain_map <- data.frame(Category = unique(df_join$domain), Encoded_Value = as.numeric(factor(unique(df_join$domain))))

# character variables to binary (except docID and icu_sites)
df_join <- df_join %>%
  mutate(
    DocID = as.numeric(str_extract(DocID,"\\d+")),  
    patient_age = ifelse(patient_age == "<60",0,1),
    admission_response = ifelse(admission_response == "no_op",0,1),
    patient_sex = ifelse(patient_sex == "M",0,1),
    discharge_status = ifelse(discharge_status == "A",0,1),
    icu_sites = ifelse(icu_sites == "1",1,2),
    leadership_role = ifelse(leadership_role == "leader_x",0,1),
    physician_rank = ifelse(physician_rank == "junior",0,1),
    physician_age = ifelse(physician_age == "<50",0,1),
    physician_sex = ifelse(physician_sex == "M",0,1),
    
    # categorical vars
    icu_dept = as.numeric(factor(icu_dept)),
    pri_diag = as.numeric(factor(pri_diag)),
    domain = as.numeric(factor(domain)))

head(df_join)

write.csv(df_join,"merged_data_numeric.csv",row.names=FALSE)


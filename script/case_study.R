# -----------------------------------------------------------------------------------------
# Yu Yan's part
# -----------------------------------------------------------------------------------------

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

# convert to Binary Encoding if needed (adding new col)
# df_doctors$physician_sex_binary <- ifelse(df_doctors$physician_sex == "M", 0, 1)
# df_patients$patient_sex_binary <- ifelse(df_patients$patient_sex == "M", 0, 1)

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


# -----------------------------------------------------------------------------------------
# rebekah's part - exploratory tables
# -----------------------------------------------------------------------------------------
library(table1)

#use df_exploratory for exploratory plots and tables (has categorical variables as
# factors and has variables with labels)
df_exploratory <- df_join

#change the labels for categorical variables for visual/display purposes
df_exploratory$patient_age<-factor(df_exploratory$patient_age,levels=c(0,1),
                                   labels=c("<60",">=60"))
df_exploratory$admission_response<-factor(df_exploratory$admission_response,levels=c(0,1),
                                          labels=c("No rapid response","Rapid response needed"))
df_exploratory$icu_dept<-factor(df_exploratory$icu_dept,levels=c(1,2,3,4),                                    
                                labels=c("Medical","Neuro","Surgical","Trauma"))
df_exploratory$patient_sex <-factor(df_exploratory$patient_sex ,levels=c(0,1),                                   
                                    labels=c("Male","Female"))
df_exploratory$discharge_status<-factor(df_exploratory$discharge_status,levels=c(0,1),                          
                                        labels=c("Alive","Dead"))
df_exploratory$pri_diag<-factor(df_exploratory$pri_diag,levels=c(1,2,3,4,5),                        
                                labels=c("Cardivascular","Gastrointestinal","Neuro","Respiratory","Trauma"))
df_exploratory$icu_sites<-factor(df_exploratory$icu_sites,levels=c(1,2),                            
                                 labels=c("1","2+"))
df_exploratory$leadership_role<-factor(df_exploratory$leadership_role,levels=c(0,1),                            
                                       labels=c("No Role","Leadership Role"))
df_exploratory$physician_rank<-factor(df_exploratory$physician_rank,levels=c(0,1),                              
                                     labels=c("Junior","Senior"))
df_exploratory$physician_sex<-factor(df_exploratory$physician_sex,levels=c(0,1),                                
                                     labels=c("Male","Female"))
df_exploratory$domain<-factor(df_exploratory$domain,levels=c(1,2,3,4,5,6),                                    
                              labels=c("Anesthesia","Emergency","Internal medicine",
                                       "Medicine","Neurology","Pulmonary medicine"))
df_exploratory$physician_age<-factor(df_exploratory$physician_age,levels=c(0,1),                            
                                     labels=c("<50","50+"))
#label the variables
label(df_exploratory$patient_age)<-"Patient age"
label(df_exploratory$admission_response)<-"Admission response"
label(df_exploratory$icu_dept)<-"ICU department"
label(df_exploratory$charlson_score)<-"Charlson comorbidity Score"
label(df_exploratory$apache_score)<-"APACHE-II admission score "
label(df_exploratory$SOFA_admission)<-"SOFA admission score"
label(df_exploratory$discharge_status)<-"Discharge status"
label(df_exploratory$patient_sex)<-"Patient sex"
label(df_exploratory$ICU_total_stay)<-"ICU length of stay"
label(df_exploratory$pri_diag )<-"Primary diagnosis"
label(df_exploratory$icu_sites )<-"Physician ICU work sites"
label(df_exploratory$leadership_role)<-"Leadership role present"
label(df_exploratory$physician_rank)<-"Physician rank"
label(df_exploratory$overall_score)<-"Overall physician score"
label(df_exploratory$resident_rank)<-"Resident evaluation of physician"
label(df_exploratory$physician_sex)<-"Physician sex"
label(df_exploratory$domain )<-"Physician training domain"
label(df_exploratory$physician_age )<-"Physician age group"
label(df_exploratory$day_of_ICU  )<-"Day of ICU"
label(df_exploratory$SOFA_daily)<-"Daily SOFA score"

# Create an exploratory tables using table1
table1(~ patient_age +  admission_response  + icu_dept+
        charlson_score   +  apache_score +  SOFA_admission +  patient_sex  +   discharge_status +   
        ICU_total_stay     +  pri_diag  + icu_sites    +  
        leadership_role +  physician_rank    +  overall_score   +   resident_rank  +
         physician_sex   +   domain   +   physician_age   + 
         day_of_ICU  + SOFA_daily , data = df_exploratory,caption="Overall Exploratory Data")

#patient outcomes by physician traits
table1(~ ICU_total_stay + SOFA_admission + SOFA_daily + discharge_status+ pri_diag+admission_response|physician_rank, 
       data = df_exploratory,caption="Patient Outcomes by Physician Rank")

table1(~ ICU_total_stay + SOFA_admission + SOFA_daily + discharge_status+pri_diag+admission_response| leadership_role, 
       data = df_exploratory,caption="Patient Outcomes by Physician Leadership Role")

table1(~ ICU_total_stay + SOFA_admission + SOFA_daily + discharge_status+pri_diag +admission_response| physician_sex, 
       data = df_exploratory,caption="Patient Outcomes by Physician Sex")

table1(~ ICU_total_stay + SOFA_admission + SOFA_daily + discharge_status +pri_diag+admission_response| icu_sites, 
       data = df_exploratory, caption="Patient Outcomes by Physician ICU Sites Worked")

table1(~ ICU_total_stay + SOFA_admission + SOFA_daily + discharge_status+pri_diag+admission_response| physician_age, 
       data = df_exploratory, caption="Patient Outcomes by Physician Age Group")

table1(~ ICU_total_stay + SOFA_admission + SOFA_daily + discharge_status+pri_diag +admission_response| domain, 
       data = df_exploratory, caption="Patient Outcomes by Physician Training Domain")


##extra exploratory tables
# table1(~overall_score | physician_rank, data=df_exploratory, caption="Physician Rank and Overall Score")
# 
# table1(~ICU_total_stay + SOFA_admission + SOFA_daily + discharge_status |icu_dept, 
#        data=df_exploratory, caption="Patient Outcomes by ICU Department")
# 
# 
# plot(df_exploratory$physician_rank,df_exploratory$ICU_total_stay)
# plot(df_exploratory$physician_rank,df_exploratory$SOFA_admission)
# plot(df_exploratory$physician_rank,df_exploratory$SOFA_daily)

# -----------------------------------------------------------------------------------------
# vincent's part 
# -----------------------------------------------------------------------------------------
# check unique
df_join <- df_join %>% distinct()




























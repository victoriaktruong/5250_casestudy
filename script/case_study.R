# -----------------------------------------------------------------------------------------
# vincent's part 
# -----------------------------------------------------------------------------------------
library(tidyverse)
library(here)

#directly downloaded data from https://ssc.ca/en/case-study/developing-a-physician-performance-model-critical-care-assessing-quality-and-value
df_doctors <- read.csv("data/df_doctors_v20220321 - df_doctors_v20220321.csv", header = T) 
df_patients <- read.csv("data/df_patients_v20220321 - df_patients_v20220321.csv", header = T)
df_sofa_traj <- read.csv("data/df_traj_v20220321 - df_traj_v20220321.csv", header = T)
df_eval360 <- read.csv("data/df_eval360_v20220321 - df_eval360_v20220321.csv", header = T)
df_patients <- df_patients %>%
  rename(
    patient_age = P1,
    admission_response = P2,
    icu_dept = P3,
    charlson_score = P4,
    apache_score = P5,
    SOFA = P6,
    patient_sex = P7,
    discharge_status = P8,
    ICU_total_stay = P9,
    pri_diag = P10
  )
df_doctors <- df_doctors %>%
  rename(
    icu_sites = M1,
    leadership_role = M2,
    physician_rank = M3,
    overall_score = M4,
    resident_rank = M5,
    physician_sex = M6,
    domain = M7,
    physician_age = M8,
    M9 = M9  # Keeping M9 unchanged if needed
  )
df_sofa_traj <-df_sofa_traj%>%
  rename(day_of_ICU = day)

# check unique
#df_join <- df_join %>% distinct()


# -----------------------------------------------------------------------------------------
# Yu Yan's part
# -----------------------------------------------------------------------------------------

# library
library(tidyverse)
library(here)

# data
#df_doctors <- read.csv("df_doctors_v20220321 - df_doctors_v20220321.csv", header = T)
#df_patients <- read.csv("df_patients_v20220321 - df_patients_v20220321.csv", header = T)
#df_sofa_traj <- read.csv("df_traj_v20220321 - df_traj_v20220321.csv", header = T)
#df_eval360 <- read.csv("df_eval360_v20220321 - df_eval360_v20220321.csv", header = T)

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
#write_csv(df_doctors,"df_doctors_v20220321 - df_doctors_v20220321_updated.csv")
#write_csv(df_patients,"df_patients_v20220321 - df_patients_v20220321_updated.csv")
# write.csv(df_sofa_traj,"df_traj_v20220321 - df_traj_v20220321_updated.csv")
# write.csv(df_eval360,"df_eval360_v20220321 - df_eval360_v20220321_updated.csv")

#process from Yu's code and write updated csv files by vincent
write_csv(df_doctors,"data/df_doctors_updated.csv")
write_csv(df_patients,"data/df_patients_updated.csv")
write.csv(df_sofa_traj,"data/df_traj_updated.csv")
write.csv(df_eval360,"data/df_eval360_updated.csv")
write_csv(df_join,"data/merged_df.csv")
# write the joined dataset
write_csv(df_join,"merged_df.csv")

# -----------------------------------------------------------------------------------------
# Victoria's part
# -----------------------------------------------------------------------------------------
library(dplyr)
library(stringr)

# categorical vars w/ multiple categories 
icu_dept_map <- data.frame(Category = unique(df_join$icu_dept), Encoded_Value = as.numeric(factor(unique(df_join$icu_dept))))
pri_diag_map <- data.frame(Category = unique(df_join$pri_diag), Encoded_Value = as.numeric(factor(unique(df_join$pri_diag))))
domain_map <- data.frame(Category = unique(df_join$domain), Encoded_Value = as.numeric(factor(unique(df_join$domain))))

# character variables to binary (except docID and icu_sites)
df_join <- df_join %>%
  mutate(
    DocID = as.numeric(str_extract(DocID,"\\d+")),  
    patient_age = ifelse(patient_age == "<lt60",0,1),
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
# Analysis: vincent's part 
# -----------------------------------------------------------------------------------------
# Load Required Libraries
# -----------------------------
library(survival)     # Survival analysis (Cox model, Surv objects)
library(survminer)    # Plotting survival curves

# -----------------------------
# A. Survival Analysis
# -----------------------------
a_data <- df_join %>%
  mutate(
    pri_diag     = factor(pri_diag),
    domain = factor(domain)
  )
cox_mod <- coxph(
  Surv(ICU_total_stay, discharge_status) ~ pri_diag
  + domain
  + admission_response,
  data = a_data
)
summary(cox_mod)

# 12345 cardiovascular, gastrointestinal, neuro, respiratory, and trauma classes
km_diag <- survfit(Surv(ICU_total_stay, discharge_status) ~ pri_diag, data = a_data)
ggsurvplot(
  km_diag,
  data       = a_data,
  pval       = TRUE,
  risk.table = TRUE,
  title      = "Survival Curves by Primary Diagnosis",
  xlab       = "Time (days)",
  ylab       = "Survival Probability"
)

# 1213456 Emergency, Respiratory, Internal Medicine, Pulmonary, Neurology. 
km_domain <- survfit(Surv(ICU_total_stay, discharge_status) ~ domain, data = a_data)
ggsurvplot(
  km_domain,
  data       = a_data,
  pval       = TRUE,
  risk.table = TRUE,
  title      = "Survival Curves by Physician Domain",
  xlab       = "Time (days)",
  ylab       = "Survival Probability"
)
# -----------------------------
# B. Correlation Analysis
# -----------------------------
corr_data <- df_join %>%
  select(apache_score, SOFA_admission, charlson_score, ICU_total_stay, admission_response)

cov(corr_data)
cor(corr_data)


# -----------------------------
# 2. Survival Analysis
# -----------------------------
# Assume that 'time' is ICU length in days and 'status' indicates the event.
cox_model <- coxph(Surv(ICU_total_stay, discharge_status) ~ apache_score + SOFA_admission + charlson_score + admission_response, data = df_join)
summary(cox_model)

ph_test <- cox.zph(cox_model)
print(ph_test)

# mutated based on the median value.
sv_data <- df_join %>%
  mutate(APACHEII_group = ifelse(apache_score >= median(apache_score, na.rm = TRUE), "High", "Low"),
         SOFA_admission_group = ifelse(SOFA_admission >= median(SOFA_admission, na.rm = TRUE), "High", "Low"),
         charlson_score_group = ifelse(charlson_score >= median(charlson_score, na.rm = TRUE), "High", "Low"))

km_fit_APACHEII <- survfit(Surv(ICU_total_stay, discharge_status) ~ APACHEII_group, data = sv_data)

# Plot the survival curves with risk tables and p-value.
ggsurvplot(km_fit_APACHEII, 
           data = sv_data, 
           pval = TRUE, 
           risk.table = TRUE, 
           title = "Survival Curves by APACHEII Group")

km_fit_SOFA <- survfit(Surv(ICU_total_stay, discharge_status) ~ SOFA_admission_group, data = sv_data)

ggsurvplot(km_fit_SOFA, 
           data = sv_data, 
           pval = TRUE, 
           risk.table = TRUE, 
           title = "Survival Curves by SOFA Group")

km_fit_charlson <- survfit(Surv(ICU_total_stay, discharge_status) ~ charlson_score_group, data = sv_data)

ggsurvplot(km_fit_charlson, 
           data = sv_data, 
           pval = TRUE, 
           risk.table = TRUE, 
           title = "Survival Curves by charlson Group")

# -----------------------------------------------------------------------------------------
# Analysis: rebekah's part - logistic regression
# -----------------------------------------------------------------------------------------

#discharge status 0=alive 1=dead
#fit logistic regression to see how predictors like primary diagnosis and physicians domain #influence prob of death
log_model<-glm(discharge_status~pri_diag+admission_response+icu_dept+patient_age+SOFA_admission+apache_score+charlson_score+patient_sex+leadership_role,data=df_exploratory,family=binomial)

#summary of model
summary(log_model)

#95% Wald CI and OR estimate
coefs_lr<-coef(log_model)
se_lr<-summary(log_model)$coefficients[,'Std. Error']
lower_lr<-coefs_lr-1.96*se_lr
upper_lr<-coefs_lr+1.96*se_lr
lr_model<-data.frame(Odds_Ratio = round(exp(coefs_lr),4),
                     Lower_CI=round(exp(lower_lr),4), 
                     Upper_CI=round(exp(upper_lr),4))
#display odds ratios and CI
lr_model

# Gastro intestinal patients had 2.55 times higher odds of death compared to cardiovascular patients. Neuro has 2.47 times higher odds of death compared to cardiovascular patients. Respiratory primary diagnosis has the highest odds of death comparing other diagnosis. Trauma patients have a 1.7 times higher odds of death vs cardiovascular. 
# Patients with a rapid response needed at admission had 48% higher odds of death compared to patients with no rapid response needed. 
# ICU Neuro department had not statistically significant odds of death, suggesting that the neuro and medical icu have similar odds of death. Surgical had the highest odds of death (1.52 times) compared to the medical icu. ICU trauma patients also had an increase in odds of death compared to medical.
# 
# Patients older than 60 had a16% higher odds of death compared to those < 60.
# An increase in SOFA score by 1 point at admission increases odds of death by 11%. An increase in APACHE II score by 1 point increases the odds of death by 19%. The Charlson score increase reduced odds of death by 0.13. Female patients have 1.11 times higher odds of death.
# Patients under physicians with a leadership role have 61% lower odds of death





#how well model discriminates between patients that are alive (=0) or dead (=1)
library(pROC)
#prediction
pred_lr<-predict(log_model, type = "response")
#roc and plot it
roc_lr<-roc(df_exploratory$discharge_status,pred_lr)
plot(roc_lr, main="ROC Curve",col="black")
#display auc val
auc(roc_lr)
#AUC value of 0.70+ so we have good discrimination

#checking multicollinearity in logistic model
library(car)
vif(log_model)
#all VIFs are well below 5, so no concerns of multicollinearity

# -----------------------------------------------------------------------------------------
# Yu's part : Join df_eval360 & K-means clustering 
# -----------------------------------------------------------------------------------------
# STEP 1: Aggregate patient outcomes per physician

df_patient_agg <- read.csv("cluster_data.csv")

df_patient_agg <- df_join %>%
  group_by(DocID) %>%
  summarize(
    n_patients = n(),
    mortality_rate = mean(discharge_status == 1, na.rm = TRUE),
    avg_los = mean(ICU_total_stay, na.rm = TRUE),
    avg_apache = mean(apache_score, na.rm = TRUE),
    avg_sofa = mean(SOFA_admission, na.rm = TRUE)
  )
df_patient_agg <- df_patient_agg %>%
  mutate(DocID = sprintf("doc-%02d", DocID)) 


# Imputation
library(mice)
df_360 <- df_eval360 %>% select(-Q20)
qs_to_impute <- paste0("Q", c(1:19, 21:23))

imputed <- mice(df_360[, qs_to_impute], m = 5, method = "midastouch", seed = 2025)
df_360_imputed <- complete(imputed, 1)
# check
range(df_360_imputed) # should be 1-5
all(apply(df_360_imputed, c(1,2), function(x) x %% 1 == 0)) # should be T

# Combine with non-imputed columns (DocID and position)
df_360_final <- bind_cols(df_eval360 %>% select(DocID, position), df_360_imputed)
colSums(is.na(df_360_final))

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# STEP 2: Aggregate 360 scores per physician
df_360_agg <- df_360_final %>%
  group_by(DocID) %>%
  summarize(
    position = get_mode(position),
    across(starts_with("Q"), mean, na.rm = TRUE),
    .groups = "drop"
  )
# Aggregate dataset
df_cluster_data <- inner_join(df_360_agg, df_patient_agg, by = "DocID")

# Correlation
cluster_data <- df_cluster_data %>%
  mutate(
    Medical_Expert = rowMeans(select(., Q1:Q3), na.rm = TRUE),
    Advocacy = Q4,
    Science_Knowledge = Q5,
    Professionalism = rowMeans(select(., Q6:Q9), na.rm = TRUE),
    Communication = rowMeans(select(., Q10:Q14), na.rm = TRUE),
    Collaboration = rowMeans(select(., Q15:Q18), na.rm = TRUE),
    Management = rowMeans(select(., Q19:Q22), na.rm = TRUE),
    Overall = Q23
  )
feedback_vars <- cluster_data %>%
  select(Medical_Expert, Advocacy, Science_Knowledge, Professionalism, Communication, Collaboration, Management,
         avg_los, mortality_rate, avg_sofa, avg_apache)

cor_matrix <- cor(feedback_vars, use = "complete.obs")
round(cor_matrix, 2)

library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix: 360 Domains vs. Patient Outcomes", mar = c(0,0,2,0))
# lm if need

# K- means Clustering
library(factoextra)
fviz_nbclust(scale(feedback_vars), kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal k")
set.seed(1)
k_result <- kmeans(scale(feedback_vars), centers = 5, nstart = 10)

# Add cluster labels
cluster_data$cluster <- as.factor(k_result$cluster)
pca_res <- prcomp(scale(feedback_vars), center = TRUE, scale. = TRUE)

fviz_pca_ind(pca_res,
             habillage = cluster_data$cluster,
             # addEllipses = TRUE,
             label = "none",
             palette = "jama",
             title = "PCA of Physician Profiles by Cluster")

cluster_data %>%
  group_by(cluster) %>%
  summarize(across(c(Medical_Expert:avg_apache), mean, .names = "mean_{.col}")) %>%
  pivot_longer(-cluster, names_to = "Variable", values_to = "Mean") %>%
  ggplot(aes(x = Variable, y = Mean, fill = cluster)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  coord_flip() +
  labs(title = "Cluster Profiles: Feedback & Outcomes")


# doc-22&23
cluster_2 <- cluster_data %>% filter(cluster == 2)


























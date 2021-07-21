#Project: Vitamin D
#By: Yijin Xiang
#Start Date: 09/21/2020
rm(list=ls())
##############################################
########load the relevant packages############
#readxl
#install.packages('readxl')
library("readxl")
#tibble
#install.packages("tibble")
library("tibble")
#dplyr
# install.packages('dplyr')
library('dplyr')
#tidyr
#install.packages("tidyr")
library("tidyr")
## tableone package itself
#install.packages("tableone")
library(tableone)
##epitools - to gain the risk ratio
#install.packages("epitools")
library(epitools)
#ggplot - to plot the data
library(ggplot2)
# ggpubr - FOR PUBLICATION READY PLOTS
# install.packages("ggpubr")
library(ggpubr)
#lme4 - to generate linear mixed models
library(lme4)
#doBy - generate estimate for linear mixed models
library(doBy)
#rstatix - generate ancova test
#install.packages("rstatix")
library(rstatix)
#emmeans - generate Post-hoc test
#install.packages("emmeans")
library(emmeans)
##############################################


##############################################
######Set working directory###################
getwd()
setwd('C:/Users/yxian33/OneDrive - Emory University/Projects_Yijin_Xiang/From_Tracy/Vitamin_D/In_Data')
outpat <- "C:/Users/yxian33/OneDrive - Emory University/Projects_Yijin_Xiang/From_Tracy/Vitamin_D/Output"
######Load the dataset########################
raw_data<-as_tibble(read_xlsx('VitaminD_analysis.xlsx', na = c("n/a","ND","NND","Nd","No response"))) # Indicate that  and NA represent missing value
head(raw_data)
month_3_raw <-as_tibble(read_xlsx('VitaminD_3_months.xlsx', na = c("n/a","ND","NND","Nd","MD","No response","SKIP","skip","Skip"))) # Indicate that  and NA represent missing value
head(month_3_raw)
month_6_raw <-as_tibble(read_xlsx('VitaminD_6_months.xlsx', na = c("n/a","ND","b","NND","Nd","MD","No response","SKIP","skip","Skip"))) # Indicate that  and NA represent missing value
head(month_6_raw)
##########################################
##########################################


#####################Data clean#####################################################

###############Baseline Raw Data cleaning##########
###Summry statistics
summary(raw_data)
####Continuous variables
# Calculate an age variable
raw_data$Age_cor <- as.numeric((raw_data$Consented - raw_data$DOB) / 365.25)
summary(raw_data$Age_cor)
data_with_age_issue <- raw_data[raw_data$Age_cor > 19,c(1,2,8,52)]
# write.csv(data_with_age_issue, file = paste(outpat,"/data_with_age_issue.csv",sep = ""))
# coercion the non-numeric value in Calcrium
# raw_data$Calcium
raw_data$Calcium_num <- as.numeric(raw_data$Calcium)
raw_data$cor_Calcium <- raw_data$Calcium_num + 0.8*(4-raw_data$Albumin)
# coercion the non-numeric value in Vit D
# raw_data$'Vit D'
raw_data$VitD_num <- as.numeric(raw_data$'Vit D')
raw_data$VitD_cat <- ifelse(raw_data$VitD_num>=30,"Normal",ifelse(is.na(raw_data$VitD_num), NA,"Abnormal"))
# table(raw_data$VitD_cat)
#####Categorical variables
#gender
# table(raw_data$Gender)
# table(toupper(raw_data$Gender))
raw_data$Gender_new<- toupper(raw_data$Gender)
#Multivitamin
# table(raw_data$Multivitamin)
# table(toupper(raw_data$Multivitamin))
raw_data$Multivitamin_new<- toupper(raw_data$Multivitamin)
#Vitamin_D
# table(raw_data$`Vitamin D`)
# table(toupper(raw_data$`Vitamin D`))
raw_data$Vitamin_D_new<- toupper(raw_data$`Vitamin D`)
#Iron
# table(raw_data$`Iron (capsule)`)
# table(toupper(raw_data$`Iron (capsule)`))
raw_data$Iron_new<- toupper(raw_data$`Iron (capsule)`)
#ESA
# table(raw_data$ESA)
# table(toupper(raw_data$ESA))
raw_data$ESA_new<- toupper(raw_data$ESA)
#Create a variable for ckd stage
raw_data$CKD_group2 <- ifelse(raw_data$Clinic %in% c("HD","PD"),"Dialysis",ifelse(raw_data$Clinic == "Transplant","Transplant","Pre-dialysis"))
# table(raw_data$CKD_group2,raw_data$Clinic)
raw_data$CKD_group1 <- ifelse(raw_data$CKD_group2 %in%c("Dialysis","Transplant"), raw_data$CKD_group2, ifelse(raw_data$eGFR<15,"CKD stage 5",ifelse(raw_data$eGFR<=29,"CKD stage 4",ifelse(raw_data$eGFR<=59,"CKD stage 3",NA))))
#Check the categorization
# table(raw_data$CKD_group2,raw_data$CKD_group1,useNA = "ifany")
# tapply(raw_data$eGFR, raw_data$CKD_group2,summary)
table(raw_data$`U P/Cr`,useNA = "ifany")
table(month_3_raw$`Urine P/Cr`,useNA = "ifany")
table(month_6_raw$`Urine P/Cr`,useNA = "ifany")
########exclude those rows in red
enroll_data <- as_tibble(read_xlsx('enrollment.xlsx', na = c("")))
analysis_data <- raw_data[raw_data$ID %in% enroll_data$ID[enroll_data$Enroll==1],]
exclude_Data <-  raw_data[raw_data$ID %in% enroll_data$ID[enroll_data$Enroll==0],]
###############################################################
############Prepare dataset to combine longitudinal dataset 
#Rename the columns for combine more data
names(analysis_data)
analysis_data_1 <- analysis_data   %>% 
  select(c(ID,Treatment_group,CKD_group2,CKD_group1,VitD_num, VitD_cat, cor_Calcium)) %>% 
  rename_with(~paste0(., "_basline"),VitD_num:cor_Calcium) 
exclude_Data <- exclude_Data  %>% rename_with(~paste0(., "_basline"),'Time spent Outdoors':VitD_cat)
final_data_Exclude <- merge(merge(exclude_Data,month_3_raw,by="ID"),month_6_raw,by="ID")
###############################################################
###############Month 3 Raw Data cleaning##########

###Summary statistics
# summary(month_3_raw)
####Continuous variables
# coercion the non-numeric value in Calcium
# month_3_raw$Calcium
month_3_raw$Calcium_num <- as.numeric(month_3_raw$Calcium)
# coercion the non-numeric value in Albumin
# month_3_raw$Albumin
month_3_raw$Albumin_num <- as.numeric(month_3_raw$Albumin)
month_3_raw$cor_Calcium <- month_3_raw$Calcium_num + 0.8*(4-month_3_raw$Albumin_num)

# coercion the non-numeric value in Vit D
month_3_raw$'Vit D'
month_3_raw$VitD_num <- as.numeric(month_3_raw$'Vit D')
month_3_raw$VitD_cat <- ifelse(month_3_raw$VitD_num>=30,"Normal",ifelse(is.na(month_3_raw$VitD_num), NA,"Abnormal"))
table(month_3_raw$VitD_cat)
#Rename the columns for combine more data
# month_3 <- month_3_raw  %>% rename_with(~paste0(., "_month3"),'Time spent Outdoors':VitD_cat)
month_3 <- month_3_raw   %>% 
  select(c(ID,VitD_num, VitD_cat, cor_Calcium)) %>% 
  rename_with(~paste0(., "_month3"),VitD_num:cor_Calcium) 

###############################################################
###############Month 6 Raw Data cleaning########## 

###Summary statistics
summary(month_6_raw)
####Continuous variables
month_6_raw$cor_Calcium <- month_6_raw$Calcium + 0.8*(4-month_6_raw$Albumin)
month_6_raw$VitD_cat <- ifelse(month_6_raw$'Vit D'>=30,"Normal",ifelse(is.na(month_6_raw$'Vit D'), NA,"Abnormal"))
#Rename the columns for combine more data
# month_6 <- month_6_raw  %>% rename_with(~paste0(., "_month6"),'Time spent Outdoors':VitD_cat)
month_6 <- month_6_raw   %>% 
  select(c(ID,'Vit D', VitD_cat, cor_Calcium)) %>% 
  rename_with(~paste0(., "_month6"),'Vit D':cor_Calcium) 

###############################################################
##################Merge three dataset together
final_data <- merge(merge(analysis_data_1,month_3,by="ID"),month_6,by="ID")
table( final_data$Treatment_group,final_data$VitD_cat_month6)
table( final_data$Treatment_group,final_data$VitD_cat_month3)
# names(final_data1_vit)

###########################Prepare long format dataset
final_data1_ca <-final_data %>% select("ID","Treatment_group","CKD_group2","CKD_group1","cor_Calcium_basline","cor_Calcium_month3","cor_Calcium_month6")
final_data1_ca_long <- final_data1_ca %>% gather(month,Cal,cor_Calcium_basline: cor_Calcium_month6, factor_key=TRUE)
final_data1_vit <-final_data %>% select("ID","Treatment_group","CKD_group2","CKD_group1","VitD_num_basline","VitD_num_month3","Vit D_month6","VitD_cat_basline","VitD_cat_month3","VitD_cat_month6")
final_data1_vit_con_long <- final_data1_vit %>% gather(month,VitD,VitD_num_basline:'Vit D_month6', factor_key=TRUE)
final_data1_vit_cat_long <- final_data1_vit %>% gather(month,VitD,VitD_cat_basline:VitD_cat_month6, factor_key=TRUE)
final_data1_vit_cat_long$VitD <- as.factor(final_data1_vit_cat_long$VitD )
###################################################

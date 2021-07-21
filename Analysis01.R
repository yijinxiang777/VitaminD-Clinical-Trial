
##########################################
## Vector of variables to summarize
myVars <- c("Age_cor","Weight","BMI","SCr", "Ht", "eGFR",
            "Calcium_num","Albumin","cor_Calcium","Phosphorus","Alk Phos",
            "Creatinine","Intact PTH", "VitD_num", "Iron",
            "Iron Saturation", "Ferritin", "Hemoglobin","U P/Cr",
            "Gender_new", "Race", "Ethnicity",
            "Clinic", "Time spent Outdoors", "Multivitamin_new", "Vitamin_D_new",
            "Iron_new", "ESA_new","CKD_group2","CKD_group1","VitD_cat")
## Vector of continuous variables
conVars<-c("Age_cor","Weight","BMI","SCr", "Ht", "eGFR",
           "Calcium_num","Albumin","cor_Calcium","Phosphorus","Alk Phos",
           "Creatinine","Intact PTH", "VitD_num", "Iron",
           "Iron Saturation", "Ferritin", "Hemoglobin")
## Vector of categorical variables that need transformation
catVars <- c("Gender_new", "Race", "Ethnicity",
             "Clinic", "Time spent Outdoors",  "Multivitamin_new", "Vitamin_D_new",
             "Iron_new", "ESA_new","U P/Cr","CKD_group2","CKD_group1","VitD_cat")
######################Intention to treat (ITT) population#####################
#cat
tab1_cat <- CreateCatTable(vars = catVars, strata = "Treatment_group" , 
                           data = raw_data,test = TRUE,includeNA = FALSE,addOverall = TRUE)
tab1_cat_final<-print(tab1_cat, quote = FALSE, noSpaces = TRUE, 
                      showAllLevels = TRUE, exact = c("Race","Ethnicity", "Clinic", "Time spent Outdoors","CKD_group2","CKD_group1"))
write.csv(tab1_cat_final, file = paste(outpat,"/tab1_cat_2.csv",sep = ""))
#con
#Want to see the n and missing 
tab1_con <- CreateContTable(vars = conVars, strata = "Treatment_group", 
                            data = raw_data,test = FALSE,funcNames = c("n", "miss", "mean", "sd"))
tab1_con_final<-summary(tab1_con,showAllLevels = TRUE)
#Want to see the n and missing 
tab1_con <- CreateContTable(vars = conVars, strata = "Treatment_group", 
                            data = raw_data,test = FALSE,addOverall = TRUE)
tab1_con_final<-print(tab1_con, quote = FALSE, noSpaces = TRUE)
write.csv(tab1_con_final, file = paste(outpat,"/tab1_con2.csv",sep = ""))

#Total data
tab1 <- CreateTableOne(vars = myVars, strata = "Treatment_group", data = raw_data, 
                       factorVars = catVars, test = TRUE,addOverall = TRUE)

tab1_final <- print(tab1, quote = FALSE, noSpaces = TRUE, exact = c("Race","Ethnicity", "Clinic", "Time spent Outdoors","CKD_group2","CKD_group1"), showAllLevels = TRUE)
write.csv(tab1_final, file = paste(outpat,"/tab1_final_P-VALUE.csv",sep = ""))
#####################################################################
######################Per protocal (PP) population#####################
#cat
tab1_cat <- CreateCatTable(vars = catVars, strata = "Treatment_group" , 
                           data = analysis_data,test = FALSE,includeNA = TRUE)
tab1_cat_final<-print(tab1_cat, quote = FALSE, noSpaces = TRUE, 
                      showAllLevels = TRUE, missing = TRUE)
write.csv(tab1_cat_final, file = paste(outpat,"/tab1_cat.csv",sep = ""))
#con
#Want to see the n and missing 
tab1_con <- CreateContTable(vars = conVars, strata = "Treatment_group", 
                            data = analysis_data,test = FALSE,funcNames = c("n", "miss", "mean", "sd"))
tab1_con_final<-summary(tab1_con,showAllLevels = TRUE)
#Want to see the n and missing 
tab1_con <- CreateContTable(vars = conVars, strata = "Treatment_group", 
                            data = analysis_data,test = FALSE)
tab1_con_final<-print(tab1_con, quote = FALSE, noSpaces = TRUE)
write.csv(tab1_con_final, file = paste(outpat,"/tab1_con.csv",sep = ""))

#Total data
tab1 <- CreateTableOne(vars = myVars, strata = "Treatment_group", data = analysis_data, 
                       factorVars = catVars, test = TRUE,addOverall = TRUE)

tab1_final <- print(tab1, quote = FALSE, noSpaces = TRUE, exact = c("Race","Ethnicity", "Clinic", "Time spent Outdoors","CKD_group2","CKD_group1"), showAllLevels = TRUE)
write.csv(tab1_final, file = paste(outpat,"/tab1_2_p-value.csv",sep = ""))

###################################################################################################
##########################Comparison by treatment in different time points#########################
###Comparison of Vitamin D level between different time points within different rgoups
# paired t-test
# t.test(final_data$`Vit D_month6`,final_data$VitD_num_basline,paired=TRUE)
# final_data %>% 
#   summarise(
#     count = n(),
#     mean = mean(final_data$"Vit D_month6",na.rm = TRUE),
#     sd = sd(final_data$"Vit D_month6",na.rm = TRUE)
# )
# final_data %>%
#   summarise(
#     count = n(),
#     mean = mean(VitD_num_basline, na.rm = TRUE),
#     sd = sd(VitD_num_basline, na.rm = TRUE)
#   )
# ##############
# # group A
# # paired t-test
t.test(final_data$`Vit D_month6`[final_data$Treatment_group=="A"],final_data$VitD_num_basline[final_data$Treatment_group=="A"],paired=TRUE)
t.test(final_data$VitD_num_month3[final_data$Treatment_group=="A"],final_data$VitD_num_basline[final_data$Treatment_group=="A"],paired=TRUE)
t.test(final_data$cor_Calcium_month3[final_data$Treatment_group=="A"],final_data$cor_Calcium_basline[final_data$Treatment_group=="A"],paired=TRUE)
t.test(final_data$cor_Calcium_month6[final_data$Treatment_group=="A"],final_data$cor_Calcium_basline[final_data$Treatment_group=="A"],paired=TRUE)

mean(final_data$`Vit D_month6`[final_data$Treatment_group=="A"], na.rm = TRUE)
sd(final_data$`Vit D_month6`[final_data$Treatment_group=="A"], na.rm = TRUE)

final_data %>% filter(Treatment_group=="A")%>%
  summarise(
    count = n(),
    mean = mean(VitD_num_basline, na.rm = TRUE),
    sd = sd(VitD_num_basline, na.rm = TRUE)
  )
# ##############
# # t-test
t.test(final_data$`Vit D_month6`[final_data$Treatment_group=="A"],final_data$`Vit D_month6`[final_data$Treatment_group=="B"])
t.test(final_data$VitD_num_month3[final_data$Treatment_group=="A"],final_data$VitD_num_month3[final_data$Treatment_group=="B"])
t.test(final_data$cor_Calcium_month3[final_data$Treatment_group=="A"],final_data$cor_Calcium_month3[final_data$Treatment_group=="B"])
t.test(final_data$cor_Calcium_month6[final_data$Treatment_group=="A"],final_data$cor_Calcium_month6[final_data$Treatment_group=="B"])

# # group B
# # paired t-test
t.test(final_data$`Vit D_month6`[final_data$Treatment_group=="B"],final_data$VitD_num_basline[final_data$Treatment_group=="B"],paired=TRUE)
t.test(final_data$VitD_num_month3[final_data$Treatment_group=="B"],final_data$VitD_num_basline[final_data$Treatment_group=="B"],paired=TRUE)
t.test(final_data$cor_Calcium_month3[final_data$Treatment_group=="B"],final_data$cor_Calcium_basline[final_data$Treatment_group=="B"],paired=TRUE)
t.test(final_data$cor_Calcium_month6[final_data$Treatment_group=="B"],final_data$cor_Calcium_basline[final_data$Treatment_group=="B"],paired=TRUE)

mean(final_data$`Vit D_month6`[final_data$Treatment_group=="B"], na.rm = TRUE)
sd(final_data$`Vit D_month6`[final_data$Treatment_group=="B"], na.rm = TRUE)

final_data %>% filter(Treatment_group=="B")%>%
  summarise(
    count = n(),
    mean = mean(VitD_num_basline, na.rm = TRUE),
    sd = sd(VitD_num_basline, na.rm = TRUE)
  )

######################Comparison of Vitamin D at 6 month##################
###########################continuous outcome
#######################ANCOVA-control for baseline vitamin D status############################
data_ancova <- final_data %>% 
  select(ID,Treatment_group,VitD_num_basline,'Vit D_month6')%>% 
  rename(basline =VitD_num_basline, month_6 ='Vit D_month6' )
results <- data_ancova %>% anova_test(month_6 ~ basline+Treatment_group)
acova <- data_ancova %>% emmeans_test(month_6 ~ Treatment_group, covariate = basline,
                                      conf.level = 0.95,detailed = TRUE,
                                      comparisons = list(c("B", "A")))
get_emmeans(acova)
###########################Categorical outcome
####Frequency table
prop.table(table(final_data$VitD_cat_basline,final_data$VitD_cat_month6))
vit_all_data <-
  matrix(c(3, 2, 15, 55),
         nrow = 2,
         dimnames = list("Baseline" = c("Abnormal", "Normal"),
                         "Month 6" = c("Abnormal", "Normal")))
vit_all_data
mcnemar.test(vit_all_data)
##############cmh
table(final_data$VitD_cat_basline,useNA = "ifany")
table(final_data$VitD_cat_month6,final_data$Treatment_group,useNA = "ifany")
table(final_data$Treatment_group,useNA = "ifany")
table(final_data$Treatment_group,final_data$VitD_cat_month6,final_data$VitD_cat_basline,useNA = "ifany")
table_vit<-array(c(16,7,4,25,5,4,7,7),
                 dim = c(2,2,2),
                 dimnames = list(treatment=c("A","B"),
                                 month6=c("Abnormal","Normal"),
                                 baseline = c("Abnormal","Normal")))
mantelhaen.test(table_vit)
partial_table <- margin.table(as.table(final_data),c(9,129,97))
################
table(final_data$VitD_cat_basline[final_data$Treatment_group=="B"],final_data$VitD_cat_month6[final_data$Treatment_group=="B"])
vit_all_data <-
  matrix(c(2, 1, 8, 32),
         nrow = 2,
         dimnames = list("Baseline" = c("Abnormal", "Normal"),
                         "Month 6" = c("Abnormal", "Normal")))
vit_all_data
mcnemar.test(vit_all_data)
################
table(final_data$VitD_cat_basline[final_data$Treatment_group=="A"],final_data$VitD_cat_month6[final_data$Treatment_group=="A"])
vit_all_data <-
  matrix(c(1, 1, 7, 23),
         nrow = 2,
         dimnames = list("Baseline" = c("Abnormal", "Normal"),
                         "Month 6" = c("Abnormal", "Normal")))
vit_all_data
mcnemar.test(vit_all_data)
####################################################

#################################Change over time
####################################################
#Summary score
final_data1_vit_con_long %>% group_by(Treatment_group,month)%>%
  summarize(mean = mean(VitD, na.rm = TRUE),
            sd = sd(VitD, na.rm = TRUE))%>%
  mutate(count=count)
fit1 <- lmer(VitD~Treatment_group+month+month*Treatment_group+(1|ID), data=final_data1_vit_con_long)
results1 <- summary(fit1)[["coefficients"]]
anova(fit1) 
write.csv(results1, file = paste(outpat,"/vit_estimate.csv",sep = ""))
#Intercept
lambda <- c(1,0,0,0,0,0)
est <- esticon(fit1,lambda)
#Treatment_groupB
lambda <- c(0,1,0,0,0,0)
est1<- rbind(est,esticon(fit1,lambda))
#monthVitD_num_month3 
lambda <- c(0,0,1,0,0,0)
est2<- rbind(est1,esticon(fit1,lambda))
#monthVit D_month6
lambda <- c(0,0,0,1,0,0)
est3<- rbind(est2,esticon(fit1,lambda))
#Treatment_groupB:monthVitD_num_month3
lambda <- c(0,0,0,0,1,0)
est4<- rbind(est3,esticon(fit1,lambda))
#Treatment_groupB:monthVit D_month6 
lambda <- c(0,0,0,0,0,1)
est5<- rbind(est4,esticon(fit1,lambda))
write.csv(est5, file = paste(outpat,"/vit_estimate_p_value.csv",sep = ""))
coef(fit1)
# The difference between two groups across time
#Baseline
lambda1 <- c(0,1,0,0,0,0)
est <- esticon(fit1,lambda1)
#at month 3
lambda2 <- c(0,1,0,0,1,0)
est1<- rbind(est,esticon(fit1,lambda2))
#at month 6
lambda3 <- c(0,1,0,0,0,1)
est2<- rbind(est1,esticon(fit1,lambda3))
write.csv(est2, file = paste(outpat,"/vit_trt_diff_at_3_timepoints.csv",sep = ""))
# The difference between time among group A
#month 3 versus Baseline
lambda1 <- c(0,0,1,0,0,0)
est <- esticon(fit1,lambda1)
#month 6 versus Baseline
lambda2 <- c(0,0,0,1,0,0)
est1<- rbind(est,esticon(fit1,lambda2))
# The difference between time among group B
#month 3 versus Baseline
lambda1 <- c(0,0,1,0,1,0)
est <- esticon(fit1,lambda1)
#month 6 versus Baseline
lambda2 <- c(0,0,0,1,0,1)
est1<- rbind(est,esticon(fit1,lambda2))

#########################################################
########################Subgroup analysis ##############
# fit2_1 <- lmer(VitD~Treatment_group+month+CKD_group2+month*Treatment_group+(1|ID), data=final_data1_vit_con_long)
# results1_1 <- summary(fit2_1)[["coefficients"]]
# #Intercept
# lambda <- c(1,0,0,0,0,0,0,0)
# est <- esticon(fit2_1,lambda)
# #Treatment_groupB
# lambda <- c(0,1,0,0,0,0,0,0)
# est1<- rbind(est,esticon(fit2_1,lambda))
# #monthVitD_num_month3 
# lambda <- c(0,0,1,0,0,0,0,0)
# est2<- rbind(est1,esticon(fit2_1,lambda))
# #monthVit D_month6
# lambda <- c(0,0,0,1,0,0,0,0)
# est3<- rbind(est2,esticon(fit2_1,lambda))
# #Treatment_groupB:monthVitD_num_month3
# lambda <- c(0,0,0,0,1,0,0,0)
# est4<- rbind(est3,esticon(fit2_1,lambda))
# #Treatment_groupB:monthVit D_month6 
# lambda <- c(0,0,0,0,0,1,0,0)
# est5<- rbind(est4,esticon(fit2_1,lambda))
# write.csv(est5, file = paste(outpat,"/vit_estimate_p_value.csv",sep = ""))
# # The difference between two groups across time
# #Baseline
# lambda1 <- c(0,1,0,0,0,0,0,0)
# est <- esticon(fit2_1,lambda1)
# #at month 3
# lambda2 <- c(0,1,0,0,0,0,1,0)
# est1<- rbind(est,esticon(fit2_1,lambda2))
# #at month 6
# lambda3 <- c(0,1,0,0,0,0,0,1)
# est2<- rbind(est1,esticon(fit2_1,lambda3))


###################Calcium##########################################
final_data1_ca_long %>% group_by(month)%>%
  summarize(mean = mean(Cal, na.rm = TRUE),
            sd = sd(Cal, na.rm = TRUE))

fit2 <- lmer(Cal~Treatment_group+month+(1|ID), data=final_data1_ca_long)
results2 <- summary(fit2)[["coefficients"]]
####################################################
#Sum
fit1 <- lmer(Cal~Treatment_group+month+(1|ID), data=final_data1_ca_long)
results1 <- summary(fit1)[["coefficients"]]
anova(fit1) 
write.csv(results1, file = paste(outpat,"/vit_estimate.csv",sep = ""))

#Treatment_groupB
lambda <- c(0,1,0,0)
est1<- esticon(fit1,lambda)
#monthVitD_num_month3 
lambda <- c(0,0,1,0)
est2<- rbind(est1,esticon(fit1,lambda))
#monthVit D_month6
lambda <- c(0,0,0,1)
est3<- rbind(est2,esticon(fit1,lambda))
#Treatment_groupB:monthVitD_num_month3
lambda <- c(0,0,0,0,1,0)
est4<- rbind(est3,esticon(fit1,lambda))
#Treatment_groupB:monthVit D_month6 
lambda <- c(0,0,0,0,0,1)
est5<- rbind(est4,esticon(fit1,lambda))
write.csv(est5, file = paste(outpat,"/vit_estimate_p_value.csv",sep = ""))
coef(fit1)
# The difference between two groups across time
#Baseline
lambda1 <- c(0,1,0,0,0,0)
est <- esticon(fit1,lambda1)
#at month 3
lambda2 <- c(0,1,0,0,1,0)
est1<- rbind(est,esticon(fit1,lambda2))
#at month 6
lambda3 <- c(0,1,0,0,0,1)
est2<- rbind(est1,esticon(fit1,lambda3))
write.csv(est2, file = paste(outpat,"/vit_trt_diff_at_3_timepoints.csv",sep = ""))
# The difference between time among group A
#month 3 versus Baseline
lambda1 <- c(0,0,1,0,0,0)
est <- esticon(fit1,lambda1)
#month 6 versus Baseline
lambda2 <- c(0,0,0,1,0,0)
est1<- rbind(est,esticon(fit1,lambda2))
# The difference between time among group B
#month 3 versus Baseline
lambda1 <- c(0,0,1,0,1,0)
est <- esticon(fit1,lambda1)
#month 6 versus Baseline
lambda2 <- c(0,0,0,1,0,1)
est1<- rbind(est,esticon(fit1,lambda2))
write.csv(results2, file = paste(outpat,"/cal_estimate_update.csv",sep = ""))
#############################################


#Categorical Vitamin D
final_data1_vit_cat_long <- final_data1_vit %>% gather(month,VitD,VitD_cat_basline:VitD_cat_month6, factor_key=TRUE)
final_data1_vit_cat_long$VitD <- as.factor(final_data1_vit_cat_long$VitD )
table(final_data1_vit_cat_long$month,final_data1_vit_cat_long$VitD,final_data1_vit_cat_long$Treatment_group)
table(final_data1_vit_cat_long$Treatment_group)
#Summary score
fit3 <- glmer(VitD~Treatment_group+month+(1|ID),family = binomial(link="logit") ,data=final_data1_vit_cat_long)
results3 <- summary(fit3)[["coefficients"]]
write.csv(results3, file = paste(outpat,"/vit_cat_estimate.csv",sep = ""))
lambda1 <- c(0,1,0,0)
est <- esticon(fit3,lambda1)
result <- cbind(exp(cbind(est$estimate,est$lwr,est$upr)),est$p.value)
#Summary score
fit3_1 <- glmer(VitD~Treatment_group+month+CKD_group2+(1|ID),family = binomial(link="logit") ,data=final_data1_vit_cat_long)
# fit3_2 <- glmer(VitD~Treatment_group+month*CKD_group2+(1|ID),family = binomial(link="logit") ,data=final_data1_vit_cat_long)

results3_1 <- summary(fit3_1)[["coefficients"]]
# results3_2 <- summary(fit3_2)[["coefficients"]]
write.csv(results3, file = paste(outpat,"/vit_cat_estimate_ADJ.csv",sep = ""))
lambda1 <- c(0,1,0,0,0,0)
est <- esticon(fit3_1,lambda1)
result <- cbind(exp(cbind(est$estimate,est$lwr,est$upr)),est$p.value)
# 
# #Baseline
# lambda1 <- c(0,1,0,0,0,0)
# est <- esticon(fit3,lambda1)
# result <- cbind(exp(cbind(est$estimate,est$lwr,est$upr)),est$p.value)
# #at month 3
# lambda2 <- c(0,0,1,0)
# est1<- esticon(fit3,lambda2)
# result <- rbind(result,cbind(exp(cbind(est1$estimate,est1$lwr,est1$upr)),est1$p.value))
# #at month 6
# lambda3 <- c(0,0,0,1)
# est2<- esticon(fit3,lambda3)
# result <- rbind(result,cbind(exp(cbind(est2$estimate,est2$lwr,est2$upr)),est2$p.value))
# write.csv(result, file = paste(outpat,"/vit_cat_trt_diff_at_3_timepoints.csv",sep = ""))

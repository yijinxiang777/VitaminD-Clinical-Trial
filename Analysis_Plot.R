#Project: Vitamin D
#By: Yijin Xiang
#Start Date: 09/21/2020
rm(list=ls())

#############################################
# Prepare for plot with confidence interval
Forplot <-matrix(NA,6,2)
Forplot[1,1]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "VitD_num_basline"&final_data1_vit_con_long$Treatment_group=="A"],mu=0)$conf.int[1]
Forplot[1,2]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "VitD_num_basline"&final_data1_vit_con_long$Treatment_group=="A"],mu=0)$conf.int[2]
Forplot[2,1]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "VitD_num_month3"&final_data1_vit_con_long$Treatment_group=="A"],mu=0)$conf.int[1]
Forplot[2,2]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "VitD_num_month3"&final_data1_vit_con_long$Treatment_group=="A"],mu=0)$conf.int[2]
Forplot[3,1]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "Vit D_month6"&final_data1_vit_con_long$Treatment_group=="A"],mu=0)$conf.int[1]
Forplot[3,2]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "Vit D_month6"&final_data1_vit_con_long$Treatment_group=="A"],mu=0)$conf.int[2]
Forplot[4,1]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "VitD_num_basline"&final_data1_vit_con_long$Treatment_group=="B"],mu=0)$conf.int[1]
Forplot[4,2]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "VitD_num_basline"&final_data1_vit_con_long$Treatment_group=="B"],mu=0)$conf.int[2]
Forplot[5,1]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "VitD_num_month3"&final_data1_vit_con_long$Treatment_group=="B"],mu=0)$conf.int[1]
Forplot[5,2]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "VitD_num_month3"&final_data1_vit_con_long$Treatment_group=="B"],mu=0)$conf.int[2]
Forplot[6,1]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "Vit D_month6"&final_data1_vit_con_long$Treatment_group=="B"],mu=0)$conf.int[1]
Forplot[6,2]<-t.test(final_data1_vit_con_long$VitD[final_data1_vit_con_long$month == "Vit D_month6"&final_data1_vit_con_long$Treatment_group=="B"],mu=0)$conf.int[2]
as.data.frame(Forplot)
Forplot_final<-cbind(final_data1_vit_con_long %>% group_by(Treatment_group,month)%>%
                       summarize(mean = mean(VitD, na.rm = TRUE),
                                 sd = sd(VitD, na.rm = TRUE)),as.data.frame(Forplot))
Forplot_final %>%
  ggplot(aes(x = month, y = mean,group = Treatment_group, color = Treatment_group)) +
  geom_errorbar(aes(ymin=V1, ymax = V2),width = .1) + 
  geom_line()+
  geom_point()  +
  theme_light()+ 
  scale_x_discrete(name ="Follow-up Time", labels=c("VitD_num_basline" = "Baseline", "VitD_num_month3" = "3 Months",
                                                    "Vit D_month6" = "6 Months"),expand=c(0,1))+
  scale_color_discrete(name = "Treatment Group", labels = c("Treatment group randomized to 1000 IU","Treatment group randomized to 4000 IU"))+ guides(linetype = FALSE, group = FALSE)+
  ylab ("Plasma 25(OH)D (ng/mL)")+
  geom_text(aes(label = paste(round(mean,2)," (",round(V1,2),"-",round(V2,2),")",sep = ""),hjust=-0.03,vjust=-0.5),  size=4.5)+
  theme(text=element_text(size=16,colour = "black"),
        axis.text.x = element_text(color="black",size=16),
        axis.text.y = element_text(color="black",size=16),
        axis.title.y = element_text(color="black",size=16)
  ) +
  geom_text(x = "Vit D_month6", y = 44.7, label = "***", colour = "black")+
  geom_text(x = "VitD_num_month3", y = 47.64, label = "**", colour = "black")
tiff("figure3_0614.tiff", width = 13, height = 4.5, units = 'in', res = 300, compression = 'none')

dev.off()

#####################################################################
# Prepare for plot with confidence interval
Forplot <-matrix(NA,6,2)
#groupwise analysis for month 6
table(final_data$VitD_cat_month6,final_data$Treatment_group)
table(final_data$VitD_cat_month3,final_data$Treatment_group)
table(final_data$VitD_cat_basline,final_data$Treatment_group)
Forplot[1,1]<-100*binom.test(12,34,0.5,alternative="two.sided",conf.level=0.95)$conf.int[1]
Forplot[1,2]<-100*binom.test(12,34,0.5,alternative="two.sided",conf.level=0.95)$conf.int[2]
Forplot[2,1]<-100*binom.test(12, 30, 0.5, alternative="two.sided",conf.level=0.95)$conf.int[1]
Forplot[2,2]<-100*binom.test(12, 30, 0.5, alternative="two.sided",conf.level=0.95)$conf.int[2]
Forplot[3,1]<-100*binom.test(11, 33,0.5,alternative="two.sided", conf.level=0.95)$conf.int[1]
Forplot[3,2]<-100*binom.test(11, 33,0.5,alternative="two.sided", conf.level=0.95)$conf.int[2]
Forplot[4,1]<-100*binom.test(12, 44,0.5,alternative="two.sided", conf.level=0.95)$conf.int[1]
Forplot[4,2]<-100*binom.test(12, 44,0.5,alternative="two.sided", conf.level=0.95)$conf.int[2]
Forplot[5,1]<-100*binom.test(27, 36,0.5, alternative="two.sided",conf.level=0.95)$conf.int[1]
Forplot[5,2]<-100*binom.test(27, 36,0.5, alternative="two.sided",conf.level=0.95)$conf.int[2]
Forplot[6,1]<-100*binom.test(32, 43,0.5,alternative="two.sided",conf.level=0.95)$conf.int[1]
Forplot[6,2]<-100*binom.test(32, 43,0.5,alternative="two.sided",conf.level=0.95)$conf.int[2]
as.data.frame(Forplot)
Forplot_final<-cbind(final_data1_vit_cat_long %>% group_by(Treatment_group,month)%>%
                       summarize(mean = 100*mean(VitD_num, na.rm = TRUE),
                                 sd = sqrt(mean*(100-mean)/length(VitD_num))),
                     as.data.frame(Forplot))
Forplot_final %>%
  ggplot(aes(x = month, y = mean,group = Treatment_group, color = Treatment_group)) +
  geom_errorbar(aes(ymin=V1, ymax = V2),width = .1) + 
  geom_line()+
  geom_point()  +
  theme_light()+
  scale_x_discrete(name ="Follow-up Time", labels=c("VitD_cat_basline" = "Baseline", "VitD_cat_month3" = "3 Months",
                                                    "VitD_cat_month6" = "6 Months"),expand=c(0,1))+
  scale_color_discrete(name = "Treatment group", labels = c("Treatment group randomized to 1000 IU","Treatment group randomized to 4000 IU"))+
  geom_text(aes(label = paste(round(mean,2),"%(",round(V1,2),"%-",round(V2,2),"%)",sep = ""),hjust=-0.03,vjust=0.1),  size=4.5)+
  ylab (paste("Plasma 25(OH)D",stringi::stri_replace_all_fixed(
    c(">= 30"), 
    c(">="), 
    c("\u2265"), 
    vectorize_all = F
  ), "ng/mL (%)",sep = " "))+
  theme(text=element_text(size=16,colour = "black"),
        axis.text.x = element_text(color="black",size=16),
        axis.text.y = element_text(color="black",size=16),
        axis.title.y = element_text(color="black",size=16)
  ) +
  geom_text(x = "VitD_cat_month6", y = 87.5, label = "***", colour = "black")+
  geom_text(x = "VitD_cat_month3", y = 88.9, label = "**", colour = "black")
tiff("figure2_0614.tiff", width = 13, height = 4.5, units = 'in', res = 300, compression = 'none')

dev.off()

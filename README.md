# Ovarian-Cancer
Survival Analysis of Ovarian Cancer patients
head(ovarian)
 km<-with(ovarian,Surv(REC_DTH,EVENT))
km 


kapm_fit<-survfit(Surv(REC_DTH,EVENT)~1,data=ovarian)
kapm_fit
autoplot(kapm_fit)

kpm_trt<-survfit(Surv(REC_DTH,EVENT)~TREAT,data=ovarian)
autoplot(kpm_trt)
summary(kpm_trt)$table
#KaplaN Meier Plot
plot_1<-ggsurvplot(kpm_trt,pval = TRUE,conf.int = TRUE, risk.table = TRUE
           ,risk.table.col="strata",linetype = "strata"
           ,surv.median.line = "hv",ggtheme = theme_bw(),palette = c("#E7B670",
                                                                     "#3E9FDF"))
plot_1

plot_2<-ggsurvplot(kpm_trt,pval = TRUE,
          xlab="Time",ggtheme = theme_light(),risk.table = "abs_pct",
           risk.table.y.text.col=T,risk.table.y.text= FALSE,ncensor.plot=TRUE,
           surv.median.line = "hv",legend.labs=c("Standard","Investigational"),
           palette = c("#E7B670","#3E9FDF"))
plot_2

#Cumulative Hazard plot
plot_3<-ggsurvplot(kpm_trt,conf.int = TRUE,risk.table.col = "strata",
           ggtheme = theme_bw(),palette = c("#E7B670","#3E9FDF"),fun = "cumhaz")
plot_3

#Log-Rank Test
surv_diff<-survdiff(Surv(REC_DTH,EVENT)~TREAT,data=ovarian)
surv_diff

#AAlen additive model
aa_fit<-aareg(Surv(REC_DTH,EVENT)~TREAT+AGE+IN_RES_D+STAGING+NCLASS+PERFORM
              +NT_SRCE+GRADE, data=ovarian)
aa_fit
autoplot(aa_fit)
#ranger model(Random Forest Model)
r_for<-ranger(Surv(REC_DTH,EVENT)~TREAT+AGE+IN_RES_D+STAGING+NCLASS+PERFORM
              +NT_SRCE+GRADE,data = ovarian,
              mtry = 4,splitrule = "extratrees",importance = "permutation",
              verbose = TRUE)
r_for
#Average survival models
death_time<-r_for$unique.death.times
survival_prob<-data.frame(r_for$survival)
average_prob<-sapply(survival_prob,mean)


fi<-data.frame(sort(round(r_for$variable.importance,4),decreasing = TRUE))
names(fi)<- "importance"
head(fi)

#Cox Proportional 
cox_prop<-coxph(Surv(REC_DTH,EVENT)~TREAT+AGE+IN_RES_D+STAGING+NCLASS+PERFORM+
                  NT_SRCE+GRADE,data = ovarian)
summary(cox_prop)
cox_m<-survfit(cox_prop)
autoplot(cox_m)

#Model comparison
kap_m<-rep("KM",length(kapm_fit$time))
kapm<-data.frame(kapm_fit$time,kapm_fit$surv,kap_m)
names(kapm)<-c("Time","Surv","Model")

coxy<-rep("Cox",length(cox_m$time))
cox_grph<-data.frame(cox_m$time,cox_m$surv,coxy)
names(cox_grph)<-c("Time","Surv","Model")

rfi<-rep("RF",length(r_for$unique.death.times))
rf_grph<-data.frame(r_for$unique.death.times,avg_prob,rfi)
names(rf_grph)<-c("Time","Surv","Model")

plot_grph<-rbind(kapm,cox_grph,rf_grph)

p<-ggplot(plot_df,aes(x=Time,y=Surv,color=Model))
p+geom_line()

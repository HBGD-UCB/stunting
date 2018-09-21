#-----------------------------------
# haz analysis
# Objective 1a
# Calculate point prevalence at
# Birth, 3, 6, 12, 18, and 24 mo of age

# Prevalence pooled using random effects
#-----------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(metafor)
theme_set(theme_bw())

# load base functions
source("U:/Scripts/haz/2-analyses/0_st_basefunctions.R")

load("U:/Data/haz/haz_data.RData")

# define age windows
d = d %>% 
  arrange(studyid,subjid,agedays) %>%
  mutate(agecat=ifelse(agedays==1,"Birth",
                       ifelse(agedays>2*30.4167 & agedays<4*30.4167,"3 months",
                              ifelse(agedays>5*30.4167 & agedays<7*30.4167,"6 months",
                                     ifelse(agedays>8*30.4167 & agedays<10*30.4167,"9 months",
                                            ifelse(agedays>11*30.4167 & agedays<13*30.4167,"12 months",
                                                   ifelse(agedays>14*30.4167 & agedays<16*30.4167,"15 months",
                                                          ifelse(agedays>17*30.4167 & agedays<19*30.4167,"18 months",
                                                                 ifelse(agedays>20*30.4167 & agedays<22*30.4167,"21 months",
                                                                        ifelse(agedays>23*30.4167& agedays<25*30.4167,"24 months","")))))))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","3 months","6 months","9 months",
                                       "12 months","15 months","18 months","21 months","24 months"))) 

# check age categories

# take mean of multiple measurements within age window
dmn <- d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid,agecat) %>%
  summarise(haz=mean(haz))

# count measurements per study by age
# exclude time points if number of measurements per age
# in a study is <50
haz.data = dmn %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,agecat) %>%
  summarise(nmeas=sum(!is.na(haz)),
            meanhaz=mean(haz),
            varhaz=var(haz)) %>%
  filter(nmeas>=50) 


#RMA functions for continious data
fit.escalc.cont <- function(data, age, yi, vi, meas){
  data=filter(data,agecat==age)
  
  data<-escalc(data=data, yi=data[[yi]], vi=data[[vi]], method="REML", measure="GEN", append=T)
  
  data$se <- sqrt(data$vi)
  data$ci.lb <- data$yi - 1.96 * data$se 
  data$ci.ub <- data$yi + 1.96 * data$se 
  
  return(data)
}

fit.rma.cont <- function(data,age, ni, yi, vi, nlab){
  data=filter(data,agecat==age)

    fit<-rma(yi=data[[yi]], vi=data[[vi]], method="REML", measure="GEN")
  
  out=data %>%
    ungroup() %>%
    summarise(nstudies=length(unique(studyid)),
              nmeas=sum(data[[ni]][agecat==age])) %>%
    mutate(agecat=age,est=fit$beta, se=fit$se, lb=fit$ci.lb, ub=fit$ci.ub,
           nmeas.f=paste0("N=",format(sum(data[[ni]]),big.mark=",",scientific=FALSE),
                          " ",nlab),
           nstudy.f=paste0("N=",nstudies," studies"))
  return(out)
}

# cohort specific results
laz.cohort=lapply(list("Birth","3 months","6 months","9 months","12 months","15 months","18 months","21 months","24 months"),function(x) 
  fit.escalc.cont(data=haz.data,yi="meanhaz", vi="varhaz",age=x))
laz.cohort=as.data.frame(do.call(rbind, laz.cohort))
laz.cohort=cohort.format(laz.cohort,y=laz.cohort$yi,
                          lab=  c("Birth","3m","6m","9m","12m","15m","18m","21m","24m"))


# estimate random effects, format results
laz.res=lapply(list("Birth","3 months","6 months","9 months","12 months","15 months","18 months","21 months","24 months"),function(x) 
  fit.rma.cont(data=haz.data, ni="nmeas", yi="meanhaz", vi="varhaz", nlab="children",age=x))
laz.res=as.data.frame(do.call(rbind, laz.res))
laz.res[,4]=as.numeric(laz.res[,4])
laz.res$agecat=factor(laz.res$agecat,levels=c("Birth","3 months","6 months","9 months","12 months","15 months","18 months","21 months","24 months"))
laz.res$ptest.f=sprintf("%0.0f",laz.res$est)

# plot cohort prevalence
pdf("U:/Figures/haz-ptprev-africa.pdf",width=11,height=5,onefile=TRUE)
ggplot(laz.cohort[laz.cohort$region=="Africa",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  # scale_y_continuous(limits=c(0,96))+
  xlab("Age in months")+
  ylab("Point prevalence (95% CI)")+
  ggtitle("Cohort-specific mean LAZ - Africa")
dev.off()

pdf("U:/Figures/haz-ptprev-latamer-eur.pdf",width=8,height=5,onefile=TRUE)
ggplot(laz.cohort[laz.cohort$region=="Latin America"|
                     laz.cohort$region=="Europe",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  # scale_y_continuous(limits=c(0,96))+
  xlab("Age in months")+
  ylab("Point prevalence (95% CI)")+
  ggtitle("Cohort-specific mean LAZ - Latin America & Europe")
dev.off()

pdf("U:/Figures/haz-ptprev-asia.pdf",width=15,height=7,onefile=TRUE)
ggplot(laz.cohort[laz.cohort$region=="Asia",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  # scale_y_continuous(limits=c(0,96))+
  xlab("Age in months")+
  ylab("Point prevalence (95% CI)")+
  ggtitle("Cohort-specific mean LAZ - Asia")
dev.off()

# plot pooled prevalence
pdf("U:/Figures/haz-ptprev-pool.pdf",width=10,height=4,onefile=TRUE)
ggplot(laz.res,aes(y=est,x=agecat))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Point prevalence (95% CI)")+
  scale_y_continuous(limits=c(-4,60))+
  annotate("text",x=laz.res$agecat,y=0,label=laz.res$nmeas.f,size=3)+
  annotate("text",x=laz.res$agecat,y=-3,label=laz.res$nstudy.f,size=3)+
  annotate("text",label=laz.res$ptest.f,x=laz.res$agecat,
           y=laz.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled mean LAZ")
dev.off()



# export
meanlaz = dmn 

save(meanlaz, laz.res, laz.cohort, file="U:/Data/stunting/st_laz.RData")





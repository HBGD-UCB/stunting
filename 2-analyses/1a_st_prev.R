#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate point prevalence at
# birth, 6 mo and 12, and 24 mo of age

# Prevalence and cumulative incidence 95% CI 
# calculated with exact binomial test - Pearson 
# and Klopper method
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(metafor)
theme_set(theme_bw())

# load meta-analysis functions
source("U:/Scripts/Stunting/2-analyses/0_randomeffects.R")

load("U:/Data/Stunting/stunting_data.RData")

# define age windows
all.data = all.data %>% 
  arrange(studyid,subjid,agedays) %>%
  mutate(agecat=ifelse(agedays==1,"Birth",
      ifelse(agedays>5*30.4167 & agedays<7*30.4167,"6 months",
       ifelse(agedays>11*30.4167 & agedays<13*30.4167,"12 months",
              ifelse(agedays>23*30.4167& agedays<25*30.4167,"24 months",""))))) %>%
    mutate(agecat=factor(agecat,levels=c("Birth","6 months",
                                         "12 months","24 months"))) %>%
    mutate(stunted=ifelse(haz< -2, 1,0),sstunted=ifelse(haz< -3, 1,0))

# check age categories
all.data %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167),
            max=max(agedays/30.4167))

# count measurements per study by age
# exclude time points if number of measurements per age
# in a study is <50
prev.data = all.data %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,agecat) %>%
  summarise(nmeas=sum(!is.na(haz)),
            prev=mean(stunted),
            nxprev=sum(stunted==1)) %>%
  filter(nmeas>=50) 
  

# estimate random effects, format results
prev.res=lapply(list("Birth","6 months","12 months","24 months"),function(x) 
  fit.rma(prev.data,ni="nmeas", xi="nxprev",age=x))
prev.res=as.data.frame(do.call(rbind, prev.res))
prev.res[,4]=as.numeric(prev.res[,4])
prev.res$agecat=factor(prev.res$agecat,levels=c("Birth","6 months","12 months","24 months"))

# plot prevalence
pdf("U:/Figures/stunting-ptprev-pool.pdf",width=8,height=4,onefile=TRUE)
ggplot(prev.res,aes(y=est,x=agecat))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Point prevalence (95% CI)")+
  scale_y_continuous(limits=c(-0.04,0.6))+
  annotate("text",x=prev.res$agecat,y=0,label=prev.res$nmeas.f,size=3)+
  annotate("text",x=prev.res$agecat,y=-0.03,label=prev.res$nstudy.f,size=3)
dev.off()


#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate cumulative incidence (ever stunted) at
# 6, 12, 18, and 24 mo of age

# Cumulative incidence pooled using random effects
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load random effects function
source("U:/Scripts/Stunting/2-analyses/0_randomeffects.R")

load("U:/Data/Stunting/stunting_data.RData")

# define age windows
d = d %>% 
  mutate(agecat=ifelse(agedays<=6*30.4167,"6 months",
         ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                       ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months",""))))) %>%
  mutate(agecat=factor(agecat,levels=c("6 months","12 months","18 months","24 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167),
            max=max(agedays/30.4167))

# identify ever stunted children
  evs = d %>%
    filter(!is.na(agecat)) %>%
    group_by(studyid,subjid) %>%
    arrange(studyid,subjid) %>%
    #create variable with minhaz by age category, cumulatively
    mutate(minhaz=ifelse(agecat=="6 months",min(haz[agecat=="6 months"]),
        ifelse(agecat=="12 months",min(haz[agecat=="6 months"|agecat=="12 months"]),
               ifelse(agecat=="18 months",min(haz[agecat=="6 months"|agecat=="12 months"|agecat=="18 months"]),
          min(haz))))) %>%
    # create indicator for whether the child was ever stunted
    # by age category
    group_by(studyid,subjid,agecat) %>%
    mutate(evst=ifelse(minhaz< -2,1,0)) 
  
# count incident cases per study by age
# exclude time points if number of measurements per age
# in a study is <50  
cuminc.data= evs%>%
    group_by(studyid,agecat) %>%
    summarise(
      nchild=length(unique(subjid)),
      nstudy=length(unique(studyid)),
      ncases=sum(evst),
      N=sum(length(evst))) %>%
    filter(N>=50)
  
cuminc.data
  
# estimate random effects, format results
ci.res=lapply(list("6 months","12 months","18 months","24 months"),function(x)
    fit.rma(data=cuminc.data,ni="N", xi="ncases",age=x))
ci.res=as.data.frame(do.call(rbind, ci.res))
ci.res[,4]=as.numeric(ci.res[,4])
ci.res$agecat=factor(ci.res$agecat,levels=c("6 months","12 months","18 months","24 months"))
ci.res$ptest.f=sprintf("%0.02f",ci.res$est)

ci.res

# plot pooled cumulative incidence
pdf("U:/Figures/stunting-cuminc-pool.pdf",width=8,height=3,onefile=TRUE)
ggplot(ci.res,aes(y=est,x=agecat))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_y_continuous(limits=c(0.13,0.7))+
  xlab("Age category")+
  ylab("Cumulative incidence (95% CI)")+
  annotate("text",x=ci.res$agecat,y=0.18,label=ci.res$nmeas.f,size=3)+
  annotate("text",x=ci.res$agecat,y=0.15,label=ci.res$nstudy.f,size=3)+
  annotate("text",label=ci.res$ptest.f,x=ci.res$agecat,
           y=ci.res$est,hjust=-0.3,size=3)
dev.off()



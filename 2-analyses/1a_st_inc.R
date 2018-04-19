#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate incidence at
# 6 mo and 12, and 24 mo of age

# Prevalence and cumulative incidence 95% CI 
# calculated with exact binomial test - Pearson 
# and Klopper method
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(epitools)
theme_set(theme_bw())

# load random effects function
source("U:/Scripts/Stunting/2-analyses/0_randomeffects.R")

load("U:/Data/Stunting/stunting_data.RData")


# define age windows
all.data = all.data %>% 
  mutate(agecat=ifelse(agedays==1,"Birth",
    ifelse(agedays<=6*30.4167,"6 months",
                       ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                              ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months",""))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","6 months","12 months","24 months")))

# check age categories
all.data %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167),
            max=max(agedays/30.4167))

# ---------------------------------------
# flag incident cases and define risk set
# ---------------------------------------
inc.prep = all.data %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  
  # create id for measurement within person
  mutate(measid=seq_along(subjid)) %>%
  # duration between measurements 
  mutate(agedayslag=lag(agedays)) %>%
  mutate(agedayslag=ifelse(is.na(agedayslag),0,agedayslag)) %>%
  mutate(deltat=ifelse(measid==1 & agecat=="Birth",0,agedays-agedayslag)) %>%

  # create indicator for whether haz at t < haz at t-1
  mutate(hazlag=lag(haz)) %>%
  mutate(newcase=ifelse(measid==1,ifelse(haz< -2, 1,0),
                    ifelse(hazlag>= -2 & haz< -2,1,0))) %>%
  mutate(newcaselag=lag(newcase))%>%
  mutate(newcaselag=ifelse(measid==1,0,newcaselag))%>%
  mutate(cnewcaselag=cumsum(newcaselag)) %>%
  
  # create at risk variable
  mutate(atrisk=ifelse(cnewcaselag>=1,0,1)) %>%
  # create inc case variable
  mutate(inccase=ifelse(cnewcaselag>=1,0,newcase)) %>%
    
  # clean up
  select(-c(hazlag,newcase,newcaselag, cnewcaselag,agedayslag))


inc.prep[inc.prep$studyid=="ki1000108-CMC-V-BCS-2002"& inc.prep$agecat=="Birth",
    c("subjid","measid","agedays","haz","deltat")][1:20,]



inc.prep[inc.prep$studyid=="ki1000110-WASH-Bangladesh"& inc.prep$agecat=="6 months",
         c("subjid","measid","agedays","haz","deltat")][1:20,]


inc.prep[inc.prep$studyid=="ki1000110-WASH-Bangladesh" & inc.prep$subjid==1610800,
         c("subjid","agedays","agedayslag","haz","deltat")]


# count incident cases and sum person time at risk per study by age
# exclude time points if number of children per age
# in a study is <50  

# at birth
inc.data.0 = inc.prep %>%
  filter(agecat=="Birth") %>%
  group_by(studyid) %>%
  summarise(ptar=sum(atrisk),
            ncase=sum(inccase),
            nchild=length(unique(subjid)),
            nstudy=length(unique(studyid))) %>%
  filter(nchild>=50) %>%
  mutate(agecat="Birth") %>%
  select(studyid,agecat,everything())

# not at birth
inc.data.1 = inc.prep %>%
  filter(agecat!="Birth") %>%
  group_by(studyid,agecat) %>%
  summarise(ptar=sum(deltat*atrisk),
            ncase=sum(inccase),
            nchild=length(unique(subjid)),
            nstudy=length(unique(studyid))) %>%
  filter(nchild>=50)

inc.data=bind_rows(inc.data.0,inc.data.1) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","6 months","12 months","24 months"))) %>%
  arrange(studyid,agecat)
          
# estimate random effects, format results
ir.res=lapply(list("Birth","6 months","12 months","24 months"),function(x)
  fit.rma(data=inc.data,ni="ptar", xi="ncase",age=x))
ir.res=as.data.frame(do.call(rbind, ir.res))
ir.res[,4]=as.numeric(ir.res[,4])
ir.res$agecat=factor(ir.res$agecat,levels=c("Birth","6 months","12 months","24 months"))

ir.res

pdf("U:/Figures/stunting-inc-pool.pdf",width=8,height=4,onefile=TRUE)
ggplot(ir.res,aes(y=est*1000,x=agecat))+
  geom_point()+
  geom_errorbar(aes(ymin=lb*1000,ymax=ub*1000),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Incidence rate per 1,000 child-days (95% CI)")+
  scale_y_continuous(limits=c(0,18))+
  annotate("text",x=ir.res$agecat,y=0.02,label=ir.res$nmeas.f,size=3)+
  annotate("text",x=ir.res$agecat,y=0.005,label=ir.res$nstudy.f,size=3)
dev.off()




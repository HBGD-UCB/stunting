#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth 

# Cohort specific estimates & 
# Pooled estimates using random effects

# What is the mean duration of time for 
# reversing stunting (measured in cohorts 
# with at least monthly measurement)? 
#-----------------------------------
# OPEN ISSUE: there are multiple measurements for the same kid in jvita3, maybe others
# waiting on vishak's response 
# > rev3[rev3$studyid=="kiGH5241-JiVitA-3" & rev3$subjid==9683,
#        +      c("subjid","measid","agedays","agecat","haz")]

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load random effects function
source("U:/Scripts/Stunting/2-analyses/0_randomeffects.R")

# load 
load("U:/Data/Stunting/st_rec_interim.RData")
load("U:/Data/Stunting/st_rec.RData")

# subset to monthly cohorts
#CHANGE THIS WHEN ANDREW SENDS DATA


# create variable for whether child is stunted or recovered or not stunted
rev3 <- rev3 %>%
  mutate(hazlag=lag(haz),
         status=ifelse(haz>= -2 & hazlag< -2,"Recovered",
       ifelse(haz>= -2 & hazlag>= -2,"Not stunted",
          ifelse(haz< -2 & hazlag< -2,"Stunted",
            ifelse(haz< -2 & hazlag>= -2, "Stunted","?"))))) %>%
  mutate(status=ifelse(is.na(status) & haz< -2,"Stunted",
                          ifelse(is.na(status) & haz>= -2, "Not stunted",status))) %>%
  mutate(agedayslag=lag(agedays)) 

# get min and max age among children who are stunted
dur <- rev3 %>%
  group_by(studyid,country,subjid,status) %>%
  mutate(x=seq_along(status)) %>%
  # subset to first stunting and first recov
  filter((status=="Stunted" & x==1) | (status=="Recovered" & x==1)) %>%
  group_by(studyid,country,subjid) %>%
  # calculate duration between stunting onset and recovery
  mutate(agedayslag=lag(agedays),
         duration=agedays-agedayslag) %>%
  # remove unnecessary rows
  filter(!is.na(duration) & status=="Recovered") %>%
  select(-c(meas_age,maxmeas_age,hazlag,newcase,newcaselag,rec3m,rec6m,
            rec9m,rec12m,rec18m,rec24m,x))

dur.data= dur %>%
  # make region
  mutate(region = ifelse(country=="BANGLADESH" | country=="INDIA"|
                           country=="NEPAL" | country=="PAKISTAN"|
                           country=="PHILIPPINES" ,"Asia",
                         ifelse(country=="BURKINA FASO"|
                                  country=="GUINEA-BISSAU"|
                                  country=="MALAWI"|
                                  country=="SOUTH AFRICA"|
                                  country=="TANZANIA, UNITED REPUBLIC OF"|
                                  country=="ZIMBABWE"|
                                  country=="GAMBIA","Africa",
                                ifelse(country=="BELARUS","Europe",
                                       "Latin America")))) %>%
  # concatenate country and study
  mutate(study_country=paste0(studyid,"-",country)) %>%
  group_by(region,study_country) %>%
  summarise(mn=mean(duration),
            se=sem(duration),
            Nmeas=n(),
            Nchild=sum(length(unique(subjid))))%>%
  mutate(lb=mn-qnorm(0.975)*se,
         ub=mn+qnorm(0.975)*se) 

# estimate random effects, format results
pool.fit=rma(yi=dur.data$mn, 
             sei=dur.data$se,
             method="REML")

# sort by mean age
dur.data$study_country=factor(dur.data$study_country, 
            levels = dur.data$study_country[order(dur.data$mn)])


# results:
c(est=pool.fit$beta, se=pool.fit$se, lb=pool.fit$ci.lb, ub=pool.fit$ci.ub)


pdf("U:/Figures/stunting-rec-dur.pdf",width=10,height=5,onefile=TRUE)
ggplot(dur.data,aes(x=study_country,y=mn))+
  geom_point(aes(size=Nchild,col=region))+
  geom_errorbar(aes(ymin=lb,ymax=ub,col=region))+
  coord_flip()+
  geom_hline(yintercept=pool.fit$ci.lb,linetype="dashed")+
  geom_hline(yintercept=pool.fit$ci.ub,linetype="dashed")+
  geom_hline(yintercept=pool.fit$beta)+
  scale_y_continuous(breaks=seq(0,160,10),labels=seq(0,160,10))+
  xlab("Study & Country") + ylab("Mean days until recovery")+
  ggtitle("Mean days from stunting onset to recovery before age 24 months")
dev.off()


rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==12,
     c("subjid","measid","agedays","agedayslag","agecat","haz","hazlag",
       "rec9m","status")][1:20,]

rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==9,
     c("subjid","measid","agedays","agedayslag","agecat","haz","hazlag",
       "status")][1:20,]

x[x$studyid=="ki1000108-CMC-V-BCS-2002" & x$subjid==9,
     c("subjid","measid","agedays","agedayslag","duration","agecat","haz","hazlag",
       "status","x")]

# export
dur <- dur %>% select(studyid,country,subjid,tr,haz,agecat,duration)

save(dur,file="U:/Data/Stunting/st_dur.RData")
save(dur,file="U:/UCB-Superlearner/Stunting rallies/st_dur.RData")


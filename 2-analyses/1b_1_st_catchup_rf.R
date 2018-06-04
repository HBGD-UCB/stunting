#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth

# Cohort specific estimates & 
# Pooled estimates using random effects

# create dataset for risk factor analyses: 
# stunting from birth to 3 months, recover by 24 months

# Not used for descriptive analyses
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load base functions
source("U:/Scripts/Stunting/2-analyses/0_st_basefunctions.R")

load("U:/Data/Stunting/stunting_data.RData")

# convert subjid to character
d <- d %>% 
  ungroup() %>%
  mutate(subjid=as.character(subjid))

# subset to monthly data
d <- d %>% filter(measurefreq=="monthly")

# create age in months
d <- d %>% mutate(agem=agedays/30.4167)

# sort data
d <- d %>% arrange(studyid, country, subjid, agedays)

# define age windows with 2 week buffer around age point
# (ie, for 6 months, consider recovery in the window  up to 7 months)
d = d %>% 
  mutate(agecat=ifelse(agedays==1,"Birth",
                       ifelse(agedays>1 & agedays<=3.5*30.4167,"3 months",
                              ifelse(agedays>3.5*30.4167 & agedays<=6.5*30.4167,"6 months",
                                     ifelse(agedays>6.5*30.4167 & agedays<=9.5*30.4167,"9 months",
                                            ifelse(agedays>9.5*30.4167 & agedays<=12.5*30.4167,"12 months",
                                                   ifelse(agedays>12.5*30.4167 & agedays<=18.5*30.4167,"18 months",
                                                          ifelse(agedays>18.5*30.4167& agedays<=24.5*30.4167,"24 months","")))))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","3 months","6 months","9 months","12 months","18 months","24 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167,na.rm=TRUE),
            max=max(agedays/30.4167))

# subset to stunted between birth and 3 months
stunt.03 <- d %>%
  filter(agecat=="Birth" | agecat=="3 months") %>%
  group_by(studyid,country,subjid) %>%
  summarise(minlaz03=min(haz)) %>%
  mutate(stunted03=ifelse(minlaz03< -2, 1, 0)) 

rec.24 <- d %>%
  filter(agecat=="24 months") %>%
  # identify last two measurements prior to 24 months
  group_by(studyid,country,subjid) %>%
  mutate(rank=min_rank(-agedays)) %>%
  filter(rank<= 2) %>%
  # flag kids with 2 measurements not stunted
  mutate(rec=ifelse(haz>= -2,1,0)) %>%
  mutate(recsum=cumsum(rec)) %>%
  # one row for each kid, indicator for recovered
  summarise(maxrec=max(recsum)) %>%
  mutate(rec24=ifelse(maxrec==2,1,0)) %>%
  select(-c(maxrec))

rev <- full_join(stunt.03, rec.24,by=c("studyid","country","subjid")) %>%
  mutate(s03rec24=ifelse(stunted03==1 & rec24==1,1,0)) %>%
  select(studyid, country,subjid, s03rec24)

# prepare data for pooling 
rev.data <- rev %>%
  group_by(studyid,country) %>%
  summarise(mn=mean(s03rec24,na.rm=TRUE),
            n=sum(s03rec24,na.rm=TRUE),
            N=sum(!is.na(s03rec24)))

# estimate random effects, format results
rev.fit=rma(ni=rev.data$N, xi=rev.data$n, 
            method="REML", measure="PR")
rev.res=data.frame(est=rev.fit$beta, se=rev.fit$se, lb=rev.fit$ci.lb, ub=rev.fit$ci.ub)

# number of cohorts
nrow(rev.data)

# number of children
sum(rev.data$N)

# export
save(rev,file="U:/Data/Stunting/st_rec.RData")
save(rev,file="U:/ucb-superlearner/Stunting rallies/st_rec.RData")


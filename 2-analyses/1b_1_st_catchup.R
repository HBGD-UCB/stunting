#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth at
# 3, 6, 12, 18, and 24 mo of age

# Cohort specific estimates & 
# Pooled estimates using random effects
#-----------------------------------

# will be defined as a LAZ change from below to -2 or greater 
# among children who are currently stunted or severely stunted. 
# We will estimate stunting reversal by each age of interest and define
# stunting as reversed if a child’s LAZ measurements change from < -2 
# to >= -2 by that age, ignoring any changes in LAZ at future older
# ages. Children will only be considered “at risk” for reversal 
# after their LAZ falls below -2. 



# What is the proportion 
# reversible after 3, 6, 9, 12 and 18 mos. of age?

# What is the 
# mean duration of time for reversing stunting (measured in cohorts 
# with at least monthly measurement)? 

# What is the proportion of 
# children who were ever stunted within the first 2 years of life, 
# who are stunted (or not) at 24, 36, and 48 mos. of age?

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
  mutate(agecat=ifelse(agedays==1,"Birth",
                   ifelse(agedays>1 & agedays<=3*30.4167,"3 months",
                       ifelse(agedays>3*30.4167 & agedays<=6*30.4167,"6 months",
                              ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                                     ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                                            ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months",""))))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","3 months","6 months","12 months","18 months","24 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167,na.rm=TRUE),
            max=max(agedays/30.4167))

# What is the proportion of stunting at birth, 3, 6, and 12, and 
# 18 mos. of age that is reversible? 

# identify children with catchup growth
rev = d %>%
  filter(!is.na(agecat)) %>%
  
  # create sequence of measid within agecat
  group_by(studyid,subjid,agecat) %>%
  mutate(meas_age=seq_along(subjid),
         maxmeas_age=max(meas_age)) %>%
  
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid) %>%
  
  # create indicator for whether haz at t < haz at t-1
  mutate(hazlag=lag(haz)) %>%
  mutate(newcase=ifelse(measid==1,ifelse(haz< -2, 1,0),
                        ifelse(hazlag>= -2 & haz< -2,1,0))) %>%
  mutate(newcaselag=lag(newcase))%>%
  mutate(newcaselag=ifelse(measid==1,0,newcaselag))%>%
  mutate(cnewcaselag=cumsum(newcaselag)) %>%
  
  # create at risk variable for reversal
  mutate(atrisk=ifelse(cnewcaselag>=1,1,0)) %>%
  # create inc case variable
  mutate(inccase=ifelse(cnewcaselag>=1,0,newcase)) %>%
  
  # flag each time haz>=-2
  mutate(rec_age=ifelse(atrisk==1 & haz>=-2 & meas_age==maxmeas_age,1,0))
  
rev.child= rev %>%
  # identify first recovery episode
  group_by(studyid,agecat,subjid) %>%
  filter(rec_age==1) %>%
  mutate(countrec=seq_along(subjid)) %>%
  filter(countrec==1) 
  
# next steps: figure out why countrec is not working 
# as a sequence. once it's working, subset to conutrec=1
# then maybe merge the rev.child back on

         firstrec=
         rec_3m=ifelse(rec_age)) 
  

rev.sum= rev %>%
  group_by(studyid,country,subjid) %>%
  summarise(rec3m=)
  
rev[rev$studyid=="ki1000108-CMC-V-BCS-2002" & rev$subjid==9,
    c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
      "meas_age","maxmeas_age","rec_age","firstrec")][1:20,]

rev.child[rev.child$studyid=="ki1000108-CMC-V-BCS-2002" & rev.child$subjid==9,
          c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
            "rec_age","countrec")]

# recovered
rev[rev$studyid=="ki1000108-CMC-V-BCS-2002" & rev$subjid==1,
    c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
      "meas_age","maxmeas_age","rec3m_row","rec3m_age")][1:20,]


  
  

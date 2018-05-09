#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth at
# 3, 6, 9, 12, 18, and 24 mo of age

# Cohort specific estimates & 
# Pooled estimates using random effects

# What proportion of children stunted at birth were no longer stunted at 3 months? 
# What proportion of children stunted at 3 months were no longer stunted at 6 months?
# What proportion of children stunted at 6 months were no longer stunted at 9 months? 
# What proportion of children stunted at 9 months were no longer stunted at 12 months? 
#-----------------------------------
# What is the 
# mean duration of time for reversing stunting (measured in cohorts 
# with at least monthly measurement)? 

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

load("U:/Data/Stunting/stunting_data.RData")

# define age windows
d = d %>% 
  mutate(agecat=ifelse(agedays==1,"Birth",
                   ifelse(agedays>1 & agedays<=3*30.4167,"3 months",
                       ifelse(agedays>3*30.4167 & agedays<=6*30.4167,"6 months",
                              ifelse(agedays>6*30.4167 & agedays<=9*30.4167,"9 months",
                                  ifelse(agedays>9*30.4167 & agedays<=12*30.4167,"12 months",
                                     ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                                            ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months","")))))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","3 months","6 months","9 months","12 months","18 months","24 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167,na.rm=TRUE),
            max=max(agedays/30.4167))

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
  # mutate(inccase=ifelse(cnewcaselag>=1,0,newcase)) %>%
  
  # flag each time haz>=-2
  mutate(rec_age=ifelse(atrisk==1 & haz>=-2 & meas_age==maxmeas_age,1,0))
  
# identify first recovery episode
rev.child= rev %>%
  group_by(studyid,subjid) %>%
  filter(rec_age==1) %>%
  mutate(firstrec=seq_along(subjid)) %>%
  filter(firstrec==1) %>%
  select(studyid, subjid,agedays,firstrec)

# indicator for incident case in age group, flag
# on last measurement in that age group
inc.child= rev %>%
  group_by(studyid,subjid,agecat) %>%
  summarise(minhaz=min(haz)) %>%
  mutate(minhazlag=lag(minhaz))

# merge indicator for first recovery back onto main data
rev2= left_join(rev,rev.child,by=c("studyid","subjid","agedays"))
  # mutate(firstrec=ifelse(is.na(firstrec),0,1)) 

# merge on indicator for incidence case in age group 
rev3 = left_join(rev2, inc.child, by=c("studyid","subjid","agecat")) %>%
  # recovery at 3 months for stunting at birth
  mutate(rec3m=ifelse(minhazlag< -2 & rec_age==1 & agecat=="3 months",1,0),
         rec6m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="6 months",1,0),
         rec9m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="9 months",1,0),
         rec12m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="12 months",1,0),
         rec18m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="18 months",1,0),
         rec24m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="24 months",1,0))

rev.data=rev3 %>%
  # subset to kids at risk
  filter(atrisk==1) %>%
  # child level data 
  group_by(agecat,studyid,country,subjid) %>%
  summarise_at(.vars=c("rec3m", "rec6m","rec9m", "rec12m", "rec18m", "rec24m"),
               .funs=max) %>%
  # organize into single column
  mutate(rec=ifelse(rec3m==1,1,
                    ifelse(rec6m==1,1,
                        ifelse(rec9m==1,1,
                           ifelse(rec12m==1,1,
                                  ifelse(rec18m==1,1,
                                         ifelse(rec24m==1,1,0))))))) %>%
  # study specific means
  group_by(studyid,country,agecat) %>%
  summarise(n.rec=length(subjid[rec==1]),N=n())


# estimate random effects, format results
rev.res=lapply(list("3 months","6 months","9 months","12 months","18 months",
                "24 months"),function(x) 
  fit.rma(rev.data,ni="N", xi="n.rec",age=x))
rev.res=as.data.frame(do.call(rbind, rev.res))
rev.res[,4]=as.numeric(rev.res[,4])
rev.res = rev.res %>%
  mutate(est=est*100,lb=lb*100,ub=ub*100)
rev.res$agecat=factor(rev.res$agecat,levels=c("Birth","3 months","6 months","9 months",
                                              "12 months","18 months","24 months"))
rev.res$agecat.f=as.factor(ifelse(rev.res$agecat=="3 months","Recover by\n 3 months",
      ifelse(rev.res$agecat=="6 months","Recover by\n6 months",
           ifelse(rev.res$agecat=="9 months","Recover by\n9 months",
               ifelse(rev.res$agecat=="12 months","Recover by\n12 months",
                      ifelse(rev.res$agecat=="18 months","Recover by\n18 months",
                             ifelse(rev.res$agecat=="24 months","Recover by\n24 months","")))))))
rev.res$agecat.f=factor(rev.res$agecat.f,levels=c("Recover by\n 3 months",
      "Recover by\n6 months", "Recover by\n9 months",
      "Recover by\n12 months","Recover by\n18 months","Recover by\n24 months"))
rev.res$ptest.f=sprintf("%0.0f",rev.res$est)

rev.res

# NOTE THE PLOT SAYS MEASUREMENTS BUT IT IS KIDS< NEED TO FIX FUNCTION

# plot % recovered by age
pdf("U:/Figures/stunting-rec-pool.pdf",width=10,height=4,onefile=TRUE)
ggplot(rev.res,aes(y=est,x=agecat.f))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Percentage (95% CI)")+
  scale_y_continuous(limits=c(0,35))+
  annotate("text",x=rev.res$agecat.f,y=2.5,label=rev.res$nmeas.f,size=3)+
  annotate("text",x=rev.res$agecat.f,y=0,label=rev.res$nstudy.f,size=3)+
  annotate("text",label=rev.res$ptest.f,x=rev.res$agecat.f,
           y=rev.res$est,hjust=-1.1,size=3)+
  ggtitle("Percentage of children who recovered from stunting")
dev.off()

#   
# rev[rev$studyid=="ki1000108-CMC-V-BCS-2002" & rev$subjid==9,
#     c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
#       "meas_age","maxmeas_age","rec_age")][1:20,]
# 
# rev.child[rev.child$studyid=="ki1000108-CMC-V-BCS-2002" & rev.child$subjid==9,
#           c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
#             "rec_age","countrec")]
# inc.child[inc.child$studyid=="ki1000108-CMC-V-BCS-2002" & inc.child$subjid==9,]
# 
# 
# 
# rev2[rev2$studyid=="ki1000108-CMC-V-BCS-2002" & rev2$subjid==9,
#     c("subjid","measid","agedays","agecat","haz","minhaz","atrisk",
#       "meas_age","maxmeas_age","rec_age","firstrec")][1:20,]
# rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==9,
#        c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
#           "meas_age","maxmeas_age","rec_age","minhaz","minhazlag",
#          "rec9m")][1:20,]
# 
# rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==9,
#      c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
#        "meas_age","maxmeas_age","rec_age","firstrec","inc_age",
#       "rec9m")][1:20,]
# rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==12,
#      c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
#        "meas_age","maxmeas_age","rec_age","minhaz","minhazlag",
#        "rec9m")][1:20,]
# 
# # recovered
# rev[rev$studyid=="ki1000108-CMC-V-BCS-2002" & rev$subjid==1,
#     c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
#       "meas_age","maxmeas_age","rec3m_row","rec3m_age")][1:20,]
# 
# rev3[rev3$studyid=="kiGH5241-JiVitA-3" & rev3$subjid==9683,
#      c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
#        "meas_age","maxmeas_age","rec_age","firstrec","inc_age",
#        "rec3m","rec6m")]
  
# export
rec=rev3 %>%
  # subset to kids at risk
  filter(atrisk==1) %>%
  # child level data 
  group_by(agecat,studyid,country,subjid) %>%
  summarise_at(.vars=c("rec3m", "rec6m","rec9m", "rec12m", "rec18m", "rec24m"),
               .funs=max) %>%
  # organize into single column
  mutate(rec=ifelse(rec3m==1,1,
                    ifelse(rec6m==1,1,
                           ifelse(rec9m==1,1,
                                  ifelse(rec12m==1,1,
                                         ifelse(rec18m==1,1,
                                                ifelse(rec24m==1,1,0))))))) %>%
  select(studyid,subjid,country,agecat,rec)

save(rev3,file="U:/Data/Stunting/st_rec_interim.RData")
save(rec,file="U:/Data/Stunting/st_rec.RData")
save(rec,file="U:/UCB-Superlearner/Stunting rallies/st_rec.RData")



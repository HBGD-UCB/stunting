#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth at
# 3, 6, 9, 12, 18, and 24 mo of age

# Cohort specific estimates & 
# Pooled estimates using random effects

# What is the proportion of 
# children who were ever stunted within the first 2 years of life, 
# who are stunted (or not) at 24, 36, and 48 mos. of age?
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

load("U:/Data/Stunting/stunting_data.RData")

# define age windows incidence
d = d %>%
  mutate(agecat=ifelse(agedays==1,"Birth",
                       ifelse(agedays>1 & agedays<=3*30.4167,"3 months",
                              ifelse(agedays>3*30.4167 & agedays<=6*30.4167,"6 months",
                                     ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                                            ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                                                   ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months",
                                                          ifelse(agedays>24*30.4167 & agedays<=36*30.4167,"36 months",
                                                                 ifelse(agedays>36*30.4167 & agedays<=48*30.4167,"48 months",
                                                                        ifelse(agedays>48*30.4167 & agedays<=60*30.4167,"60 months","Over 60 months")))))))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","3 months","6 months",
        "12 months","18 months","24 months","36 months","48 months",
        "60 months","Over 60 months"))) %>%
  
  # define age windows for recovery
  mutate(agecatr=ifelse(agedays>23*30.4167& agedays<=24*30.4167,"24 months",
                           ifelse(agedays>25*30.4167 & agedays<=37*30.4167,"36 months",
                                ifelse(agedays>47*30.4167 & agedays<=49*30.4167,"48 months",
                                      ifelse(agedays>59*30.4167 & agedays<=61*30.4167,"60 months",""))))) %>%
  mutate(agecatr=factor(agecat,levels=c("24 months","36 months","48 months",
                                       "60 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167,na.rm=TRUE),
            max=max(agedays/30.4167))

# identify ever stunted children
evs = d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid) %>%
  #create variable with minhaz by age category, cumulatively
  mutate(minhaz=ifelse(agecat=="3 months",min(haz[agecat=="3 months"]),
                       ifelse(agecat=="6 months",min(haz[agecat=="3 months" | agecat=="6 months"]),
                              ifelse(agecat=="12 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"]),
                                     ifelse(agecat=="18 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"|agecat=="18 months"]),
                                            min(haz)))))) %>%
  # create indicator for whether the child was ever stunted
  # by age category
  group_by(studyid,agecat,subjid) %>%
  summarise(minhaz=min(minhaz)) %>%
  mutate(ever_stunted=ifelse(minhaz< -2,1,0))

# merge ever_stunted back onto main data
d2 <- left_join(d,evs,by=c("studyid","agecat","subjid"))

# calculate max age of measurement for each child
maxage <- d %>%
  group_by(studyid,country,subjid) %>%
  summarise(maxagem=max(agedays)/30.4167)

# merge maxagem back onto main data
d3 <- left_join(d2, maxage,by=c("studyid","country","subjid"))

# function to create indicator for recovery by certain age
rec.age=function(age){
  x <- d3 %>%
    # subset to kids with measurement at agedays 24 months
    # who were ever stunted
    filter(maxagem>=age & ever_stunted==1 & agedays<=age*30.4167 &
             agecatr==paste0(age," months")) %>%
    # calculate recovery at 24 months
    group_by(studyid,country,subjid) %>%
    summarise(maxhaz=max(haz)) %>%
    mutate(rec = ifelse(maxhaz>=-2,1,0),
           agecatr = paste0(age," months"))
  return(x)
}

d.age=rbind(rec.age(24),rec.age(36),rec.age(48),rec.age(60))

# cohort specific means
rev.data=d.age %>%
  group_by(studyid,country,agecatr) %>%
  summarise(n.rec=length(subjid[rec==1]),N=n()) %>%
  rename(agecat=agecatr)

# estimate random effects, format results
rev.res=lapply(list("24 months","36 months","48 months","60 months"),function(x) 
  fit.rma(rev.data,ni="N", xi="n.rec",age=x))
rev.res=as.data.frame(do.call(rbind, rev.res))
rev.res[,4]=as.numeric(rev.res[,4])
rev.res = rev.res %>%
  mutate(est=est*100,lb=lb*100,ub=ub*100)
rev.res$agecat=factor(rev.res$agecat,
      levels=c("24 months","36 months","48 months","60 months"))
rev.res$agecat.f=as.factor(ifelse(rev.res$agecat=="24 months","Recover by\n 24 months",
      ifelse(rev.res$agecat=="36 months","Recover by\n36 months",
         ifelse(rev.res$agecat=="48 months","Recover by\n48 months",
              ifelse(rev.res$agecat=="60 months","Recover by\n60 months","")))))
rev.res$agecat.f=factor(rev.res$agecat.f,levels=c("Recover by\n 24 months",
          "Recover by\n36 months", "Recover by\n48 months",
          "Recover by\n60 months"))
rev.res$ptest.f=sprintf("%0.0f",rev.res$est)

rev.res

# NOTE THE PLOT SAYS MEASUREMENTS BUT IT IS KIDS< NEED TO FIX FUNCTION

# plot % recovered by age
pdf("U:/Figures/stunting-rec-everst-pool.pdf",width=9,height=4,onefile=TRUE)
ggplot(rev.res,aes(y=est,x=agecat.f))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Percentage (95% CI)")+
  scale_y_continuous(limits=c(0,80))+
  annotate("text",x=rev.res$agecat.f,y=8,label=rev.res$nmeas.f,size=3)+
  annotate("text",x=rev.res$agecat.f,y=2,label=rev.res$nstudy.f,size=3)+
  annotate("text",label=rev.res$ptest.f,x=rev.res$agecat.f,
           y=rev.res$est,hjust=-1.1,size=3)+
  ggtitle("Percentage of children who recovered from stunting")
dev.off()


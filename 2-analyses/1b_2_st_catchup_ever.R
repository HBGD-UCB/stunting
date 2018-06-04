#-----------------------------------
# Stunting analysis
# Objective 1b
# Catch up growth 

# Cohort specific estimates & 
# Pooled estimates using random effects

# What is the proportion of 
# children who were ever stunted within the first 2 years of life, 
# who are stunted (or not) at 24, 36, and 48 mos. of age?
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

# subset to monthly data
d <- d %>% filter(measurefreq=="monthly")

# create age in months
d <- d %>% mutate(agem=agedays/30.4167)

# sort data
d <- d %>% arrange(studyid, country, subjid, agedays)

# make subjid a character
d <- d %>% 
  ungroup() %>%
  mutate(subjid=as.character(subjid))

# define age windows incidence
d = d %>%
  mutate(agem=agedays/30.4167) %>%
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
  mutate(agecatr=case_when(
    agem >23 & agem<=25 ~ "24 months",
    agem >35 & agem<=37 ~ "36 months",
    agem >47 & agem<=49 ~ "48 months",
    agem >59 & agem<=61 ~ "60 months",
    TRUE                ~ ""
  )) %>%
  
 mutate(agecatr=factor(agecatr,levels=c("24 months","36 months","48 months",
                                       "60 months","")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167,na.rm=TRUE),
            max=max(agedays/30.4167))

d %>%
  group_by(agecatr) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167,na.rm=TRUE),
            max=max(agedays/30.4167))

# indicator for stunted by 24 months
stunt.24 <- d %>%
  filter(agem<=25) %>%
  group_by(studyid,country,subjid) %>%
  summarise(minlaz24=min(haz)) %>%
  mutate(stunted24=ifelse(minlaz24< -2, 1, 0)) 

# recovery by x months
rec.age <- d %>%
  filter(agecatr!="") %>%
  mutate(agecatr=droplevels(agecatr)) %>%
  arrange(studyid,country,subjid,agecatr,agedays) %>%
  # since about half of children have only 1 
  # measurement in the age window, using all
  # available measurements to define recovery
  group_by(studyid,country,subjid,agecatr) %>%
  # flag kids with 2 measurements not stunted
  summarise(minhaz=min(haz))%>%
  mutate(rec=ifelse(minhaz>= -2,1,0)) 

rev <- full_join(stunt.24, rec.age,by=c("studyid","country","subjid")) %>%
  mutate(s24rec=ifelse(stunted24==1 & rec==1,1,0)) %>%
  select(studyid, country,subjid, agecatr, s24rec) %>%
  filter(!is.na(agecatr))

# cohort specific means
rec.data=rev %>%
  group_by(studyid,country,agecatr) %>%
  summarise(n.rec=length(subjid[s24rec==1]),
            N=n()) %>%
  rename(agecat=agecatr)


rec.cohort=lapply(list("24 months","36 months","48 months","60 months"),function(x) 
  fit.escalc(data=rec.data,ni="N", xi="n.rec",age=x,meas="PR"))
rec.cohort=as.data.frame(do.call(rbind, rec.cohort))
rec.cohort=cohort.format(rec.cohort, y=rec.cohort$yi,
    lab=  c("24 months","36 months","48 months","60 months"))

# estimate random effects, format results
rev.res=lapply(list("24 months","36 months","48 months","60 months"),function(x) 
  fit.rma(rec.data,ni="N", xi="n.rec",age=x,measure="PR",nlab=" children"))
rev.res=as.data.frame(do.call(rbind, rev.res))
rev.res[,4]=as.numeric(rev.res[,4])
rev.res = rev.res %>%
  mutate(est=est*100,lb=lb*100,ub=ub*100)
rev.res$agecat=factor(rev.res$agecat,
                      levels=c("24 months","36 months","48 months","60 months"))
rev.res$agecat.f=as.factor(ifelse(rev.res$agecat=="24 months","Not stunted at\n 24 months",
                                  ifelse(rev.res$agecat=="36 months","Not stunted at\n36 months",
                                         ifelse(rev.res$agecat=="48 months","Not stunted at\n48 months",
                                                ifelse(rev.res$agecat=="60 months","Not stunted at\n60 months","")))))
rev.res$agecat.f=factor(rev.res$agecat.f,levels=c("Not stunted at\n 24 months",
                                                  "Not stunted at\n36 months", "Not stunted at\n48 months",
                                                  "Not stunted at\n60 months"))
rev.res$ptest.f=sprintf("%0.0f",rev.res$est)

rev.res

# plot cohort prevalence

lab.af=rec.cohort[rec.cohort$region=="Africa",] %>% 
  group_by(cohort) %>% summarise(N=sum(N))
lab.af.f=paste0("N=",lab.af$N)

pdf("U:/Figures/stunting-rec-everst-africa.pdf",width=11,height=5,onefile=TRUE)
ggplot(rec.cohort[rec.cohort$region=="Africa",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(-40,90))+
  xlab("Age category")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who were stunted by 24 months who were not stunted at later ages - Africa")+
  annotate("text", x=3.2,y=80,label=lab.af.f,size=4)
dev.off()


pdf("U:/Figures/stunting-rec-everst-latamer-eur.pdf",width=8,height=5,onefile=TRUE)
ggplot(rec.cohort[rec.cohort$region=="Latin America"|
                    rec.cohort$region=="Europe",],
       aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(-40,90))+
  xlab("Age category")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who were stunted by 24 months who were not stunted at later ages - Latin America")+
  annotate("text", x=4.2,y=80,label=lab.af.f,size=4)
dev.off()

lab.asia=rec.cohort[rec.cohort$region=="Asia",] %>% 
  group_by(cohort) %>% summarise(N=sum(N))
lab.asia.f=paste0("N=",lab.asia$N)

pdf("U:/Figures/stunting-rec-everst-asia.pdf",width=11,height=7,onefile=TRUE)
ggplot(rec.cohort[rec.cohort$region=="Asia",],
  aes(y=y,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(-40,90))+
  xlab("Age category")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who were stunted by 24 months who were not stunted at later ages - Asia")+
  annotate("text", x=4.2,y=80,label=lab.asia.f,size=4)
dev.off()


# plot % recovered by age
pdf("U:/Figures/stunting-rec-everst-pool.pdf",width=9,height=4,onefile=TRUE)
ggplot(rev.res,aes(y=est,x=agecat.f))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Percentage (95% CI)")+
  scale_y_continuous(limits=c(0,60))+
  annotate("text",x=rev.res$agecat.f,y=8,label=rev.res$nmeas.f,size=3)+
  annotate("text",x=rev.res$agecat.f,y=2,label=rev.res$nstudy.f,size=3)+
  annotate("text",label=rev.res$ptest.f,x=rev.res$agecat.f,
           y=rev.res$est,hjust=-1.1,size=3)+
  ggtitle("Percentage of children who were stunted by 24 months who were not stunted at later ages")
dev.off()


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
# What proportion of children stunted at 12 months were no longer stunted at 18 months? 
# What proportion of children stunted at 18 months were no longer stunted at 24 months? 
#-----------------------------------
# OPEN ISSUE: there are multiple measurements for the same kid in jvita3, maybe others
# waiting on vishak's response 

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load random effects function
source("U:/Scripts/Stunting/2-analyses/0_randomeffects.R")

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


# create indicators for stunting
rev <- d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  # 
  mutate(stunted=ifelse(haz< -2,1,0),
         lagstunted=lag(stunted))  %>%
  # unique stunting episode
  mutate(sepisode=ifelse(lagstunted==0 & stunted==1 |
                         stunted==1 & measid==1,1,0))

#------------------------------------------
# number of stunting episodes per child
#------------------------------------------
episode.child <- rev %>%
  group_by(studyid,country,subjid) %>%
  summarise(nrev=sum(sepisode))

labs=paste0(sprintf("%0.1f",prop.table(table(episode.child$nrev))*100),"%")

pdf("U:/Figures/stunting-n-episodes.pdf",width=8,height=4,onefile=TRUE)
ggplot(episode.child, aes(nrev))+geom_histogram(binwidth=0.5,col="black",fill="gray")+
  scale_x_continuous(labels=seq(0,7),breaks=seq(0,7)) +
  annotate("text",x=seq(0,7),y=as.numeric(table(episode.child$nrev)),
           label=labs, vjust=-0.4)+
  scale_y_continuous(limits=c(0,7000),labels=seq(0,7000,500),
                     breaks=seq(0,7000,500))+
  xlab("Number of stunting episodes per child")+
  ylab("Number of children")
dev.off()

#------------------------------------------
# create indicator recovery in each age cat
#------------------------------------------
rev.agecat <- rev %>%
  filter(agecat!="Birth") %>%
  group_by(studyid,country,subjid,agecat) %>%
  summarise(min_stunted=min(stunted),
            max_prev_stunted=max(lagstunted)) %>%
  # create indicator for recovery
  # NA means that it was the age cat of first measurement 
  mutate(recover=ifelse(min_stunted==0 & max_prev_stunted==1,1,0))

# prepare data for pooling 
rev.data <- rev.agecat %>%
  group_by(studyid,country,agecat) %>%
  summarise(mn=mean(recover,na.rm=TRUE),
            n=sum(recover,na.rm=TRUE),
            N=sum(!is.na(recover)))


# estimate random effects, format results
rev.res=lapply(list("3 months","6 months","9 months","12 months","18 months",
                    "24 months"),function(x) 
                      fit.rma(rev.data,ni="N", xi="n",age=x))
rev.res=as.data.frame(do.call(rbind, rev.res))
rev.res[,4]=as.numeric(rev.res[,4])
rev.res = rev.res %>%
  mutate(est=est*100,lb=lb*100,ub=ub*100)
rev.res$agecat=factor(rev.res$agecat,levels=c("Birth","3 months","6 months","9 months",
                                              "12 months","18 months","24 months"))
rev.res$agecat.f=as.factor(ifelse(rev.res$agecat=="3 months","Stunted at birth,\nnot stunted by\n 3 months",
      ifelse(rev.res$agecat=="6 months","Stunted by 3 months,\nnot stunted by\n6 months",
         ifelse(rev.res$agecat=="9 months","Stunted by 6 months,\nnot stunted by\n9 months",
              ifelse(rev.res$agecat=="12 months","Stunted by 9 months,\nnot stunted by\n12 months",
                  ifelse(rev.res$agecat=="18 months","Stunted by 12 months,\nnot stunted by\n18 months",
                     ifelse(rev.res$agecat=="24 months","Stunted by 18 months,\nnot stunted by\n24 months","")))))))
rev.res$agecat.f=factor(rev.res$agecat.f,levels=c("Stunted at birth,\nnot stunted by\n 3 months",
                                                  "Stunted by 3 months,\nnot stunted by\n6 months", 
                                                  "Stunted by 6 months,\nnot stunted by\n9 months",
                                                  "Stunted by 9 months,\nnot stunted by\n12 months",
                                                  "Stunted by 12 months,\nnot stunted by\n18 months",
                                                  "Stunted by 18 months,\nnot stunted by\n24 months"))
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
  scale_y_continuous(limits=c(0,25))+
  annotate("text",x=rev.res$agecat.f,y=1.5,label=rev.res$nmeas.f,size=3)+
  annotate("text",x=rev.res$agecat.f,y=0,label=rev.res$nstudy.f,size=3)+
  annotate("text",label=rev.res$ptest.f,x=rev.res$agecat.f,
           y=rev.res$est,hjust=-1.1,size=3)+
  ggtitle("Percentage of children who recovered from stunting")
dev.off()

#------------------------------------------
# distribution of stunting among those who 
# have recovered and those who have not 
#------------------------------------------
rdist=full_join(rev,rev.agecat,by=c("studyid","country","subjid","agecat")) %>%
  select(studyid, subjid, country, agedays, haz, stunted,lagstunted,recover) %>%
  # identify row with onset of stunting
  mutate(laghaz=lag(haz),
         st_onset=ifelse(laghaz>= -2 & haz< -2,1,0)) %>%
  # identify row with onset of recovery
  mutate(rec_onset=ifelse(laghaz< -2 & haz>= -2,1,0)) 

# plot dist among stunted
pdf("U:/Figures/stunting-rec-onset-st-dist.pdf",width=8,height=4,onefile=TRUE)
ggplot(rdist %>% filter(st_onset==1), aes(x=haz))+
  geom_histogram(binwidth=0.05,col="black",fill="gray")+
  xlab("LAZ at stunting onset")+ylab("Number of children")+
  scale_x_continuous(breaks=seq(-6,-2,1),labels=seq(-6,-2,1))
dev.off()

# plot dist among recovered
pdf("U:/Figures/stunting-rec-onset-rec-dist.pdf",width=8,height=4,onefile=TRUE)
ggplot(rdist %>% filter(rec_onset==1), aes(x=haz))+
  geom_histogram(binwidth=0.1,col="black",fill="gray")+
  xlab("LAZ at first measurement in which child is not stunted")+
  ylab("Number of children")+
  scale_x_continuous(breaks=seq(-2,6,1),labels=seq(-2,6,1))
dev.off()


# rdist %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.01728116478713e-320") 
# 
# 
# # example of recovery at 3m
# rev %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.01728116478713e-320") 
# 
# rev.agecat %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.01728116478713e-320") 
# 
# # example of recovery at 6m
# d %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.0049295236411e-320") 
# 
# rev %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.0049295236411e-320") 
# 
# rev.agecat %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.0049295236411e-320") 
# 
# 
# 
# rev %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.48219693752374e-323") 
# 
# ### OLD CODE: 
# 
# 
# # create indicators for each recovery
# rev = d %>%
#   filter(!is.na(agecat)) %>%
#   
#   # create sequence of measid within agecat
#   group_by(studyid,subjid,agecat) %>%
#   mutate(meas_age=seq_along(subjid),
#          maxmeas_age=max(meas_age)) %>%
#   
#   group_by(studyid,subjid) %>%
#   arrange(studyid,subjid) %>%
#   
#   # create indicator for whether haz at t < haz at t-1
#   mutate(hazlag=lag(haz)) %>%
#   mutate(newcase=ifelse(measid==1,ifelse(haz< -2, 1,0),
#                         ifelse(hazlag>= -2 & haz< -2,1,0))) %>%
#   mutate(newcaselag=lag(newcase))%>%
#   mutate(newcaselag=ifelse(measid==1,0,newcaselag))%>%
#   mutate(cnewcaselag=cumsum(newcaselag)) %>%
#   
#   # create at risk variable for reversal
#   mutate(atrisk=ifelse(cnewcaselag>=1,1,0)) %>%
#   # create inc case variable
#   # mutate(inccase=ifelse(cnewcaselag>=1,0,newcase)) %>%
#   
#   # flag each time haz>=-2
#   mutate(rec_age=ifelse(atrisk==1 & haz>=-2 & meas_age==maxmeas_age,1,0))
# 
# # example of recovery at 3m
# rev %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.01728116478713e-320") %>%
#   select(agedays,agem,haz,hazlag,agecat,atrisk,meas_age,maxmeas_age,rec_age)
# 
# # example of recovery at 6m
# rev %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.0049295236411e-320") %>%
#   select(agedays,agem,haz,hazlag,newcaselag,agecat,atrisk,meas_age,maxmeas_age,rec_age)
# 
# 
# # identify first recovery episode
# rev.child= rev %>%
#   group_by(studyid,subjid) %>%
#   filter(rec_age==1) %>%
#   mutate(firstrec=seq_along(subjid)) %>%
#   filter(firstrec==1) %>%
#   select(studyid, subjid,agedays,firstrec)
# 
# # indicator for incident case in age group, flag
# # on last measurement in that age group
# inc.child= rev %>%
#   group_by(studyid,subjid,agecat) %>%
#   summarise(minhaz=min(haz)) %>%
#   mutate(minhazlag=lag(minhaz))
# 
# # merge indicator for first recovery back onto main data
# rev2= left_join(rev,rev.child,by=c("studyid","subjid","agedays"))
#   # mutate(firstrec=ifelse(is.na(firstrec),0,1)) 
# 
# # merge on indicator for incidence case in age group 
# rev3 = left_join(rev2, inc.child, by=c("studyid","subjid","agecat")) %>%
#   # recovery at 3 months for stunting at birth
#   mutate(rec3m=ifelse(minhazlag< -2 & rec_age==1 & agecat=="3 months",1,0),
#          rec6m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="6 months",1,0),
#          rec9m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="9 months",1,0),
#          rec12m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="12 months",1,0),
#          rec18m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="18 months",1,0),
#          rec24m=ifelse(minhazlag< -2  & rec_age==1 & agecat=="24 months",1,0))
# 
# # example of recovery at 3m
# rev3 %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#                 subjid=="1.01728116478713e-320") %>%
#   select(agedays,haz,hazlag,minhazlag,agecat,rec_age,rec3m,rec6m,rec9m,rec12m)
# 
# # example of recovery at 6m
# rev3 %>%  group_by(studyid, subjid) %>%
#   filter(studyid=="ki0047075b-MAL-ED"&
#            subjid=="1.0049295236411e-320") %>%
#   select(agedays,haz,hazlag,minhazlag,agecat,rec_age,rec3m,rec6m)
# 
# rev.data=rev3 %>%
#   # subset to kids at risk
#   filter(atrisk==1) %>%
#   # child level data 
#   group_by(agecat,studyid,country,subjid) %>%
#   summarise_at(.vars=c("rec3m", "rec6m","rec9m", "rec12m", "rec18m", "rec24m"),
#                .funs=max) %>%
#   # organize into single column
#   mutate(rec=ifelse(rec3m==1,1,
#                     ifelse(rec6m==1,1,
#                         ifelse(rec9m==1,1,
#                            ifelse(rec12m==1,1,
#                                   ifelse(rec18m==1,1,
#                                          ifelse(rec24m==1,1,0))))))) %>%
#   # study specific means
#   group_by(studyid,country,agecat) %>%
#   summarise(n.rec=length(subjid[rec==1]),N=n())
# 
# 
# # estimate random effects, format results
# rev.res=lapply(list("3 months","6 months","9 months","12 months","18 months",
#                 "24 months"),function(x) 
#   fit.rma(rev.data,ni="N", xi="n.rec",age=x))
# rev.res=as.data.frame(do.call(rbind, rev.res))
# rev.res[,4]=as.numeric(rev.res[,4])
# rev.res = rev.res %>%
#   mutate(est=est*100,lb=lb*100,ub=ub*100)
# rev.res$agecat=factor(rev.res$agecat,levels=c("Birth","3 months","6 months","9 months",
#                                               "12 months","18 months","24 months"))
# rev.res$agecat.f=as.factor(ifelse(rev.res$agecat=="3 months","Recover by\n 3 months",
#       ifelse(rev.res$agecat=="6 months","Recover by\n6 months",
#            ifelse(rev.res$agecat=="9 months","Recover by\n9 months",
#                ifelse(rev.res$agecat=="12 months","Recover by\n12 months",
#                       ifelse(rev.res$agecat=="18 months","Recover by\n18 months",
#                              ifelse(rev.res$agecat=="24 months","Recover by\n24 months","")))))))
# rev.res$agecat.f=factor(rev.res$agecat.f,levels=c("Recover by\n 3 months",
#       "Recover by\n6 months", "Recover by\n9 months",
#       "Recover by\n12 months","Recover by\n18 months","Recover by\n24 months"))
# rev.res$ptest.f=sprintf("%0.0f",rev.res$est)
# 
# rev.res
# 
# # NOTE THE PLOT SAYS MEASUREMENTS BUT IT IS KIDS< NEED TO FIX FUNCTION
# 
# # plot % recovered by age
# pdf("U:/Figures/stunting-rec-pool.pdf",width=10,height=4,onefile=TRUE)
# ggplot(rev.res,aes(y=est,x=agecat.f))+
#   geom_point(size=3)+
#   geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
#   scale_color_manual(values=tableau10)+xlab("Age category")+
#   ylab("Percentage (95% CI)")+
#   scale_y_continuous(limits=c(0,35))+
#   annotate("text",x=rev.res$agecat.f,y=2.5,label=rev.res$nmeas.f,size=3)+
#   annotate("text",x=rev.res$agecat.f,y=0,label=rev.res$nstudy.f,size=3)+
#   annotate("text",label=rev.res$ptest.f,x=rev.res$agecat.f,
#            y=rev.res$est,hjust=-1.1,size=3)+
#   ggtitle("Percentage of children who recovered from stunting")
# dev.off()
# 
# #   
# # rev[rev$studyid=="ki1000108-CMC-V-BCS-2002" & rev$subjid==9,
# #     c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
# #       "meas_age","maxmeas_age","rec_age")][1:20,]
# # 
# # rev.child[rev.child$studyid=="ki1000108-CMC-V-BCS-2002" & rev.child$subjid==9,
# #           c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
# #             "rec_age","countrec")]
# # inc.child[inc.child$studyid=="ki1000108-CMC-V-BCS-2002" & inc.child$subjid==9,]
# # 
# # 
# # 
# # rev2[rev2$studyid=="ki1000108-CMC-V-BCS-2002" & rev2$subjid==9,
# #     c("subjid","measid","agedays","agecat","haz","minhaz","atrisk",
# #       "meas_age","maxmeas_age","rec_age","firstrec")][1:20,]
# # rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==9,
# #        c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
# #           "meas_age","maxmeas_age","rec_age","minhaz","minhazlag",
# #          "rec9m")][1:20,]
# # 
# # rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==9,
# #      c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
# #        "meas_age","maxmeas_age","rec_age","firstrec","inc_age",
# #       "rec9m")][1:20,]
# rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==12,
#      c("subjid","measid","agedays","agecat","haz","minhazlag","atrisk",
#        "rec_age", "rec3m","rec6m")][1:20,]
# # 
# # # recovered
# # rev[rev$studyid=="ki1000108-CMC-V-BCS-2002" & rev$subjid==1,
# #     c("subjid","measid","agedays","agecat","haz","inccase","atrisk",
# #       "meas_age","maxmeas_age","rec3m_row","rec3m_age")][1:20,]
# # 
# rev3[rev3$studyid=="kiGH5241-JiVitA-3" & rev3$subjid==9683,
#      c("subjid","measid","agedays","agecat","haz","minhazlag","atrisk",
#        "rec_age","firstrec","inc_age",
#        "rec3m","rec6m")]
  


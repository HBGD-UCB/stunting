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


# max measureid for each child
maxid = d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  summarise(maxid=max(measid))

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

# make indicator for max measurement
rev2 <- full_join(rev, maxid, by=c("studyid","country","subjid"))  %>%
  # determine the start and end of each stunting episode
  mutate(start=ifelse(stunted==1 & lag(stunted)==0,1,0),
         end=ifelse(stunted==1 & maxid==measid,1,
                    ifelse(stunted==0 & lag(stunted)==1,1,0))) %>%
  # keep starting and ending ages
  filter(start==1 | end==1) %>%
  # round age in months
  mutate(agem=round(agedays/30.4167)) %>%
  # calculate difference in age
  mutate(agemlag=lag(agem),
         agediff=agem-agemlag)  %>%
  filter(!is.na(agediff))

# plot distribution of stunting duration prior to 24 months
labs=paste0(sprintf("%0.1f",prop.table(table(rev2$agediff))*100),"%")[1:6]

pdf("U:/Figures/stunting-duration.pdf",width=8,height=4,onefile=TRUE)
ggplot(rev2, aes(x=agediff))+
  geom_histogram(binwidth=1,col="black",fill="gray")+
  scale_x_continuous(labels=seq(0,24),breaks=seq(0,24)) +
  annotate("text",x=seq(0,5),y=as.numeric(table(rev2$agediff)[1:6]),
           label=labs, vjust=-0.4,size=2.25)+
  xlab("Duration of stunting episodes in months")+
  ylab("Number of children")+
  ggtitle("Distribution of the duration of stunting episodes prior to 24 months")
dev.off()

rev2[rev2$studyid=="ki0047075b-MAL-ED" &
       rev2$subjid=="8.34970941471707e-322",
     c("subjid","agedays","measid","maxid",
       "haz","stunted",
       "start","end")][21:25,]

rev2[rev2$studyid=="ki0047075b-MAL-ED" &
       rev2$subjid=="1.01283457397456e-321",
     c("subjid","agedays","measid","maxid",
       "haz","stunted",
       "start","end")][20:35,]

rev2[rev2$studyid=="ki0047075b-MAL-ED" &
      rev2$subjid=="1.01283457397456e-321",
    c("subjid","agem","agemlag","agediff","measid","maxid",
      "haz","stunted",
      "start","end")]

rev2$measid[rev2$studyid=="ki0047075b-MAL-ED" &
       rev2$subjid=="1.01283457397456e-321"]

rev[,c("subjid","agedays","haz","stunted","sep_rank")][31:40,]


d[d$studyid=="ki0047075b-MAL-ED" &
      d$subjid=="1.01283457397456e-321",]


# OLD CODE ############################
# 
# # load random effects function
# source("U:/Scripts/Stunting/2-analyses/0_randomeffects.R")
# 
# # load 
# load("U:/Data/Stunting/st_rec_interim.RData")
# load("U:/Data/Stunting/st_rec.RData")
# 
# # create variable for whether child is stunted or recovered or not stunted
# rev3 <- rev3 %>%
#   mutate(hazlag=lag(haz),
#          status=ifelse(haz>= -2 & hazlag< -2,"Recovered",
#        ifelse(haz>= -2 & hazlag>= -2,"Not stunted",
#           ifelse(haz< -2 & hazlag< -2,"Stunted",
#             ifelse(haz< -2 & hazlag>= -2, "Stunted","?"))))) %>%
#   mutate(status=ifelse(is.na(status) & haz< -2,"Stunted",
#                           ifelse(is.na(status) & haz>= -2, "Not stunted",status))) %>%
#   mutate(agedayslag=lag(agedays)) 
# 
# # get min and max age among children who are stunted
# dur <- rev3 %>%
#   group_by(studyid,country,subjid,status) %>%
#   mutate(x=seq_along(status)) %>%
#   # subset to first stunting and first recov
#   filter((status=="Stunted" & x==1) | (status=="Recovered" & x==1)) %>%
#   group_by(studyid,country,subjid) %>%
#   # calculate duration between stunting onset and recovery
#   mutate(agedayslag=lag(agedays),
#          duration=agedays-agedayslag) %>%
#   # remove unnecessary rows
#   filter(!is.na(duration) & status=="Recovered") %>%
#   select(-c(meas_age,maxmeas_age,hazlag,newcase,newcaselag,rec3m,rec6m,
#             rec9m,rec12m,rec18m,rec24m,x))
# 
# dur.data= dur %>%
#   # make region
#   mutate(region = ifelse(country=="BANGLADESH" | country=="INDIA"|
#                            country=="NEPAL" | country=="PAKISTAN"|
#                            country=="PHILIPPINES" ,"Asia",
#                          ifelse(country=="BURKINA FASO"|
#                                   country=="GUINEA-BISSAU"|
#                                   country=="MALAWI"|
#                                   country=="SOUTH AFRICA"|
#                                   country=="TANZANIA, UNITED REPUBLIC OF"|
#                                   country=="ZIMBABWE"|
#                                   country=="GAMBIA","Africa",
#                                 ifelse(country=="BELARUS","Europe",
#                                        "Latin America")))) %>%
#   # concatenate country and study
#   mutate(study_country=paste0(studyid,"-",country)) %>%
#   group_by(region,study_country) %>%
#   summarise(mn=mean(duration),
#             se=sem(duration),
#             Nmeas=n(),
#             Nchild=sum(length(unique(subjid))))%>%
#   mutate(lb=mn-qnorm(0.975)*se,
#          ub=mn+qnorm(0.975)*se) 
# 
# # estimate random effects, format results
# pool.fit=rma(yi=dur.data$mn, 
#              sei=dur.data$se,
#              method="REML")
# 
# # sort by mean age
# dur.data$study_country=factor(dur.data$study_country, 
#             levels = dur.data$study_country[order(dur.data$mn)])
# 
# 
# # results:
# c(est=pool.fit$beta, se=pool.fit$se, lb=pool.fit$ci.lb, ub=pool.fit$ci.ub)
# 
# 
# pdf("U:/Figures/stunting-rec-dur.pdf",width=10,height=5,onefile=TRUE)
# ggplot(dur.data,aes(x=study_country,y=mn))+
#   geom_point(aes(size=Nchild,col=region))+
#   geom_errorbar(aes(ymin=lb,ymax=ub,col=region))+
#   coord_flip()+
#   geom_hline(yintercept=pool.fit$ci.lb,linetype="dashed")+
#   geom_hline(yintercept=pool.fit$ci.ub,linetype="dashed")+
#   geom_hline(yintercept=pool.fit$beta)+
#   scale_y_continuous(breaks=seq(0,160,10),labels=seq(0,160,10))+
#   xlab("Study & Country") + ylab("Mean days until recovery")+
#   ggtitle("Mean days from stunting onset to recovery before age 24 months")
# dev.off()
# 
# 
# rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==12,
#      c("subjid","measid","agedays","agedayslag","agecat","haz","hazlag",
#        "rec9m","status")][1:20,]
# 
# rev3[rev3$studyid=="ki1000108-CMC-V-BCS-2002" & rev3$subjid==9,
#      c("subjid","measid","agedays","agedayslag","agecat","haz","hazlag",
#        "status")][1:20,]
# 
# x[x$studyid=="ki1000108-CMC-V-BCS-2002" & x$subjid==9,
#      c("subjid","measid","agedays","agedayslag","duration","agecat","haz","hazlag",
#        "status","x")]
# 
# # export
# dur <- dur %>% select(studyid,country,subjid,tr,haz,agecat,duration)
# 
# save(dur,file="U:/Data/Stunting/st_dur.RData")
# save(dur,file="U:/ucb-superlearner/Stunting rallies/st_dur.RData")
# 

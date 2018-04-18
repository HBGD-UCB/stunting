#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate incidence at
# 6 mo and 12, and 24 mo of age

# Prevalence and cumulative incidence 95% CI 
# calculated with exact binomial test - Pearson 
# and Klopper method
#-----------------------------------
# Incident stunting episodes will be defined as a 
# change in LAZ from above -2 z in the prior measurement to below -2 z in 
# the current measurement. Children will only be considered 
# “at risk” for incident stunting episodes, and thus contribute person-time at 
# risk for stunting, if they currently have a z-score ≥ -2 and previously did 
# not have a z-score < -2. Since linear growth faltering that leads to stunting 
# is a cumulative process, if a child’s LAZ score fluctuates above and below -2, 
# we will consider the first measurement below -2 as the incident onset of 
# stunting and then following that the child will no longer be included in the 
# risk set for stunting.

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(epitools)
theme_set(theme_bw())

# load standard error function
source("U:/Scripts/Stunting/2-analyses/0_se_fns.R")

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

# define risk set: current HAZ>=-2, previous HAZ>=-2 in same time window
# identify ever stunted children
evs = all.data %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  # create id for measurement within person
  mutate(measid=seq_along(subjid)) %>%
  # create variable with haz at t-1
  mutate(hazprev=lag(haz)) %>%
  # create indicator for whether haz at t < haz at t-1
  mutate(newcase=ifelse(measid==1,
                    ifelse(haz< -2, 1,0),
                    ifelse(hazprev>= -2 & haz< -2,1,0))) %>%
  # create at risk variable
  mutate(atrisk=ifelse(agecat=="Birth",1,1-cumsum(newcase))) 
  
# # calculate incidence at birth
# inc.birth = evs %>%
#   ungroup() %>%
#   filter(agecat=="Birth" & atrisk==1) %>%
#   summarise(inc.cases=sum(newcase),
#             at.risk=sum(atrisk)) 
# 
# inc1=c("Birth",pois.exact(inc.birth$inc.cases,inc.birth$at.risk)[c("rate","lower","upper")])
#         # getting an error when using mean95CI fn
# 
# # calculate incidence at 6 months
# inc.age = evs %>%
#   filter(agecat!="Birth") %>%
#   group_by(agecat) %>%
#   summarise(inc.cases=sum(newcase),
#             at.risk=sum(atrisk)) 
# 
# inc2=cbind(inc.age$agecat,pois.exact(inc.age$inc.cases,inc.age$at.risk)[,c("rate","lower","upper")])
# names(inc1)=colnames(inc2)
# inc.res=rbind(inc1,inc2)
# colnames(inc.res)=c("agecat","rate","lb","ub")

inc.data = evs %>%
  group_by(agecat) %>%
  summarise(ptar=sum(!is.na(atrisk)),
          nstudy=length(unique(studyid)),
          prev=mean(stunted),
          lb=mean95CI(newcase,atrisk,proportion=F,count=T)[["Lower 95%CI"]],
          ub=mean95CI(newcase,atrisk,proportion=F,count=T)[["Upper 95%CI"]],
          measure="Stunting") %>%
  mutate(nmeas.f=paste0("N=",format(nmeas,big.mark=",",scientific=FALSE),
                        " measurements"),
         nstudy.f=paste0("N=",nstudy," studies"))

mean95CI(Y=evs$newcase,persontime=evs$atrisk,proportion=F,count=T)

pdf("U:/Figures/stunting-inc-pool.pdf",width=8,height=4,onefile=TRUE)
ggplot(inc.res,aes(y=rate,x=agecat))+
  geom_point()+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Incidence rate (95% CI)")+
  scale_y_continuous(limits=c(0,0.3))+
  annotate("text",x=sprev.data$agecat,y=0.03,label=sprev.data$nmeas.f,size=3)+
  annotate("text",x=sprev.data$agecat,y=0.01,label=sprev.data$nstudy.f,size=3)
dev.off()




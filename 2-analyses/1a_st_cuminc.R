#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate cumulative incidence (ever stunted) at
# 6 mo and 12, and 24 mo of age

# Prevalence and cumulative incidence 95% CI 
# calculated with exact binomial test - Pearson 
# and Klopper method
#-----------------------------------
# Cumulative incidence (incidence proportion) of stunting will 
# be defined as the occurrence of any incident stunting event 
# over a defined risk period. For example, we will examine the 
# incidence of any stunting in the first six months of life among 
# children who were not born stunted. 

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
theme_set(theme_bw())

# load standard error function
source("U:/Scripts/Stunting/2-analyses/0_se_fns.R")

load("U:/Data/Stunting/stunting_data.RData")

# define age windows
all.data = all.data %>% 
  mutate(agecat=ifelse(agedays<=6*30.4167,"6 months",
         ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
           ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months","")))) %>%
  mutate(agecat=factor(agecat,levels=c("6 months","12 months","24 months")))

# check age categories
all.data %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167),
            max=max(agedays/30.4167))

# identify ever stunted children
evs = all.data %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,subjid,agecat) %>%
  arrange(studyid,subjid,agecat) %>%
  summarise(minhaz=min(haz)) %>%
  mutate(evst=ifelse(minhaz< -2,1,0)) 

cuminc.data= evs%>%
  group_by(agecat) %>%
  summarise(
  nchild=length(unique(subjid)),
  nstudy=length(unique(studyid)),
  cuminc=mean(evst),
  lb=mean95CI(evst,proportion=T)[["Lower 95%CI"]],
  ub=mean95CI(evst,proportion=T)[["Upper 95%CI"]]) %>%
  mutate(nchild.f=paste0("N=",format(nchild,big.mark=",",scientific=FALSE),
                         " children"),
         nstudy.f=paste0("N=",nstudy," studies"))


#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

# plot pooled cumulative incidence
pdf("U:/Figures/stunting-cuminc-pool.pdf",width=7,height=3,onefile=TRUE)
ggplot(cuminc.data,aes(y=cuminc,x=agecat))+
  geom_point()+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_y_continuous(limits=c(0.15,0.5))+
  xlab("Age category")+
  ylab("Cumulative incidence (95% CI)")+
  annotate("text",x=cuminc.data$agecat,y=0.2,label=cuminc.data$nchild.f,size=3)+
  annotate("text",x=cuminc.data$agecat,y=0.17,label=cuminc.data$nstudy.f,size=3)
dev.off()



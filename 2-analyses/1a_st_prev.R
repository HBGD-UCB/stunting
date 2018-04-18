#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate point prevalence at
# birth, 6 mo and 12, and 24 mo of age

# Prevalence and cumulative incidence 95% CI 
# calculated with exact binomial test - Pearson 
# and Klopper method
#-----------------------------------
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
    mutate(agecat=ifelse(agedays==1,"Birth",
      ifelse(agedays>5*30.4167 & agedays<7*30.4167,"6 months",
       ifelse(agedays>11*30.4167 & agedays<13*30.4167,"12 months",
              ifelse(agedays>23*30.4167& agedays<25*30.4167,"24 months",""))))) %>%
    mutate(agecat=factor(agecat,levels=c("Birth","6 months",
                                         "12 months","24 months"))) %>%
    mutate(stunted=ifelse(haz< -2, 1,0),sstunted=ifelse(haz< -3, 1,0))

# check age categories
all.data %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167),
            max=max(agedays/30.4167))
  

# calculate stunting prevalence by age group
sprev.data = all.data %>%
  filter(!is.na(agecat)) %>%
  group_by(agecat) %>%
  summarise(nprev=sum(!is.na(stunted)),
            prev=mean(stunted),
            lb=mean95CI(stunted,proportion=T)[["Lower 95%CI"]],
            ub=mean95CI(stunted,proportion=T)[["Upper 95%CI"]],
            measure="Stunting") 

# calculate severe stunting prevalence by age group
ssprev.data = all.data %>%
  filter(!is.na(agecat)) %>%
  group_by(agecat) %>%
  summarise(nprev=sum(!is.na(sstunted)),
            prev=mean(sstunted),
            lb=mean95CI(sstunted,proportion=T)[["Lower 95%CI"]],
            ub=mean95CI(sstunted,proportion=T)[["Upper 95%CI"]],
            measure="Severe stunting") 

prev.data=rbind(sprev.data,ssprev.data)
prev.data$measure=factor(prev.data$measure,levels=c("Stunting","Severe stunting"))

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

# plot prevalence
pdf("U:/Figures/stunting-ptprev-pool.pdf",width=7,height=3,onefile=TRUE)
ggplot(prev.data,aes(y=prev,x=agecat,group=measure))+
  geom_point(aes(col=measure))+
  geom_errorbar(aes(ymin=lb,ymax=ub,col=measure),width=0.2) +
  facet_wrap(~measure)+
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Point prevalence (95% CI)")
dev.off()

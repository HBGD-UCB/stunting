
#-----------------------------------
# Stunting webinar plots
#-----------------------------------

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(metafor)
library(ggthemes)
theme_set(theme_bw())

# load base functions
source("U:/Scripts/Stunting/2-analyses/0_st_basefunctions.R")

load("U:/Data/Stunting/st_prev.RData")
load("U:/Data/Stunting/st_cuminc.RData")
load("U:/Data/Stunting/st_rec..RData")



setwd("U:/Figures/Stunting Webinar")


#Prevalence
p1 <- ggplot(prev.res,aes(y=est,x=agecat)) +
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  scale_fill_tableau(drop=TRUE, limits = levels(prev.res$agecat)) +
  scale_colour_tableau(drop=TRUE, limits = levels(prev.res$agecat)) +
  xlab("Age category")+
  ylab("Point prevalence (95% CI)")+
  scale_y_continuous(limits=c(-4,60))+
  annotate("text",x=prev.res$agecat,y=0,label=prev.res$nmeas.f,size=3)+
  annotate("text",x=prev.res$agecat,y=-3,label=prev.res$nstudy.f,size=3)+
  annotate("text",label=prev.res$ptest.f,x=prev.res$agecat,
           y=prev.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled point prevalence of stunting") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 0, hjust = 1)) 

ggsave(p1, file="pooled_prev.png", width=10, height=4)



#Cumulative Incidence

p2 <- ggplot(ci.res,aes(y=est,x=agecat.f))+
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  scale_fill_tableau(drop=TRUE, limits = levels(ci.res$agecat)) +
  scale_colour_tableau(drop=TRUE, limits = levels(ci.res$agecat)) +
  scale_y_continuous(limits=c(0,80))+
  xlab("Age category")+
  ylab("Cumulative incidence per 100 children (95% CI)")+
  annotate("text",x=ci.res$agecat.f,y=5,label=ci.res$nmeas.f,size=3)+
  annotate("text",x=ci.res$agecat.f,y=1,label=ci.res$nstudy.f,size=3)+
  annotate("text",label=ci.res$ptest.f,x=ci.res$agecat.f,
           y=ci.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled cumulative incidence of stunting") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 0, hjust = 1)) 

ggsave(p2, file="pooled_CI.png", width=10, height=4)

#Recovert

ggplot(rev.res,aes(y=est,x=agecat))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Percentage (95% CI)")+
  scale_y_continuous(limits=c(0,20))+
  annotate("text",x=rev.res$agecat,y=1.2,label=rev.res$nmeas.f,size=3)+
  annotate("text",x=rev.res$agecat,y=0.1,label=rev.res$nstudy.f,size=3)+
  annotate("text",label=rev.res$ptest.f,x=rev.res$agecat,
           y=rev.res$est,hjust=-0.4,size=3)+
  ggtitle("Percentage of children who were stunted and recovered within age intervals")


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
library(plyr)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

# load base functions
source("C:/Users/andre/Documents/HBGDki/Stunting/2-analyses/0_st_basefunctions.R")

load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_cuminc.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_incprop.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_prev.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_rec_interim.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/pool_vel.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_rf_res.RData")



setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/")

#-------------------------------------------------------------------------------------------
#Prevalence
#-------------------------------------------------------------------------------------------


p1 <- ggplot(prev.res,aes(y=est,x=agecat)) +
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  # scale_fill_tableau(drop=TRUE, limits = levels(prev.res$agecat)) +
  # scale_colour_tableau(drop=TRUE, limits = levels(prev.res$agecat)) +
  scale_color_manual(values=rep(tableau10[1],20))+  scale_fill_manual(values=rep(tableau10[1],20))+
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
        axis.text.x = element_text(size=12)) 

ggsave(p1, file="pooled_prev.png", width=10, height=4)




#-------------------------------------------------------------------------------------------
# Cumulative Incidence
#-------------------------------------------------------------------------------------------

p2 <- ggplot(ci.res,aes(y=est,x=agecat.f))+
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[2],20))+  scale_fill_manual(values=rep(tableau10[2],20))+
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
        axis.text.x = element_text(size=12)) 

ggsave(p2, file="pooled_CI.png", width=10, height=4)


#-------------------------------------------------------------------------------------------
# Recovery
#-------------------------------------------------------------------------------------------

p3 <- ggplot(rev.res,aes(y=est,x=agecat))+
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[3],20))+  scale_fill_manual(values=rep(tableau10[3],20))+
  xlab("Age category")+ ylab("Percentage (95% CI)")+
  scale_y_continuous(limits=c(0,20))+
  annotate("text",x=rev.res$agecat,y=1.2,label=rev.res$nmeas.f,size=3)+
  annotate("text",x=rev.res$agecat,y=0.1,label=rev.res$nstudy.f,size=3)+
  annotate("text",label=rev.res$ptest.f,x=rev.res$agecat,
           y=rev.res$est,hjust=-0.4,size=3)+
  ggtitle("Percentage of children who were stunted and recovered within age intervals")+
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 

ggsave(p3, file="pooled_rev.png", width=10, height=4)




#-------------------------------------------------------------------------------------------
# Incidence proportion
#-------------------------------------------------------------------------------------------

p4 <- ggplot(df, aes(y=est,x=agecat.f2, fill=Measure, color=Measure))+
              geom_point( size = 4, position=position_dodge(width=0.25)) +
              geom_linerange(aes(ymin=lb, ymax=ub), alpha=0.5, size = 3, position=position_dodge(width=0.25)) +
              scale_y_continuous(limits=c(0,80))+
              xlab("Age category")+
              ylab("Percent stunted (95% CI)") +
              annotate("text",x=df$agecat.f2[1:5],y=74,label=df$nmeas.f[1:5],size=3) +
              annotate("text",x=df$agecat.f2[6:10],y=4,label=df$nmeas.f[6:10],size=3) +
              annotate("text",x=df$agecat.f2[1:5],y=71,label=df$nstudy.f[1:5],size=3) +
              annotate("text",x=df$agecat.f2[6:10],y=1,label=df$nstudy.f[6:10],size=3) +
              annotate("text",x=df$agecat.f2[1],y=78,label="N's for cumulative incidence from birth",size=4, hjust = 0) +
              annotate("text",x=df$agecat.f2[1],y=8,label="N's for incidence proportion within age ranges",size=4, hjust = 0) +
              annotate("text",label=df$ptest.f[1:5],x=df$agecat.f2[1:5], y=df$est[1:5],hjust=2.5,size=3)+
              annotate("text",label=df$ptest.f[6:10],x=df$agecat.f2[6:10], y=df$est[6:10],hjust=-2,size=3)+
              theme(strip.background = element_blank(), strip.text.x = element_text(size=12)) +
              scale_colour_manual(name = "Measure",values=tableau10[c(2,10)]) +
              ggtitle("Pooled cumulative incidence of stunting")+
  theme(strip.background = element_blank(),
        legend.position="right",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p4
ggsave(p4, file="pooled_incprop.png", width=10, height=4)

          
          
  

#-------------------------------------------------------------------------------------------
# Velocity
#-------------------------------------------------------------------------------------------

vel <- rbind(data.frame(measure="Length velocity (cm per month)" , poollencm[poollencm$stratacol=="pooled" & poollencm$region=="Overall",]),
       data.frame(measure="HAZ change (Z-score per month)" , poolhaz[poolhaz$stratacol=="pooled" & poolhaz$region=="Overall",]))

p5 <- ggplot(vel, aes(y=Mean,x=strata))+
  geom_point(aes(fill=strata, color=strata), size = 4) +
  geom_linerange(aes(ymin=Lower.95.CI, ymax=Upper.95.CI, color=strata),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[4],20))+  
  xlab("Age category")+ ylab("")+
  #scale_y_continuous(limits=c(0,20))+
  # annotate("text",x=vel$strata,y=.12,label=vel$nmeas.f,size=3)+
  # annotate("text",x=vel$strata,y=0.1,label=vel$nstudy.f,size=3)+
  facet_wrap(~measure, scales="free_y") +
  ggtitle("")+
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 

ggsave(p5, file="pooled_velocity.png", width=10, height=4)



#-------------------------------------------------------------------------------------------
# Risk factor plots 
#-------------------------------------------------------------------------------------------


setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar")

scaleFUN <- function(x) sprintf("%.2f", x)
  
label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=20, simplify=FALSE), 
        paste, collapse="\n")
}  


#-------------------------------------------------------------------------------------------
# prenatal characteristics
#-------------------------------------------------------------------------------------------


df <- RMAest %>%
      filter(agecat=="0-24 months (no birth st.)\ncumulative incidence"|
               agecat=="0-24 months\ncumulative incidence") %>%
      filter(intervention_variable %in% c("mhtcm","mwtkg","mbmi",
                                          "fhtcm","meducyrs","feducyrs",
                                          "hhwealth_quart"))
df <- droplevels(df)

df$intervention_variable <- factor(df$intervention_variable, levels=c("mhtcm","mwtkg","mbmi",
                                          "fhtcm","meducyrs","feducyrs",
                                          "hhwealth_quart"))
df <- df %>% arrange(intervention_variable)
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))


yticks <- c(2/3, 1, 3/2)

  
df$intervention_level <- revalue(df$intervention_level, c("Overweight or Obese"="Overweight"))

  p <-  ggplot(df, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                       alpha=0.5, size = 3) +
        facet_wrap(~RFlabel, scales="free_x", ncol=4, labeller = label_wrap) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
        geom_text(aes(x=2, y=(max(df$RR.CI2))-.1, label=Nstudies), size=2,  hjust=0) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          axis.text.y = element_text(size=12),
          strip.text.x = element_text(size=10),
          axis.text.x = element_text(size=10, angle = 20, hjust = 1),
          panel.spacing = unit(0, "lines")) +
        #ggtitle("Prenatal characteristics associated with\nstunting from birth to 2 years")
        ggtitle("Outcome: any stunting from birth to 2 years")

  ggsave(p, file="prenatal_RFplot.png",  width=6, height=5.2)
  
  
#-------------------------------------------------------------------------------------------
# at-birth characteristics
#-------------------------------------------------------------------------------------------
  


df <- RMAest %>%
      filter(agecat=="0-24 months (no birth st.)\ncumulative incidence"|
               agecat=="0-24 months\ncumulative incidence") %>%
      filter(intervention_variable %in% c("sex","birthlen", "birthwt", "gagebrth", "hdlvry", "parity"))
df <- droplevels(df)

df$intervention_variable <- factor(df$intervention_variable, levels=
                                     c("sex","gagebrth","birthlen", "birthwt",  "hdlvry", "parity"))
df <- df %>% arrange(intervention_variable)
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))

df$intervention_level <- revalue(df$intervention_level, c("Normal or high birthweight"="Normal birthweight",
                                                          "Full or late term"="Full term"))


yticks <- c(2/3, 1, 3/2,2,3,4)
    
  p <-  ggplot(df, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                       alpha=0.5, size = 3) +
        facet_wrap(~RFlabel, scales="free_x", ncol=4, labeller = label_wrap) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
        geom_text(aes(x=1.5, y=(max(df$RR.CI2))-.1, label=Nstudies), size=2,  hjust=0) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          axis.text.y = element_text(size=12),
          strip.text.x = element_text(size=10),
          axis.text.x = element_text(size=10, angle = 20, hjust = 1),
          panel.spacing = unit(0, "lines")) +
        ggtitle("Outcome: any stunting by 2 years, excluding stunting at birth")

    ggsave(p, file="atbirth_RFplot.png",  width=6, height=5.2)

  
  

#-------------------------------------------------------------------------------------------
# postnatal characteristics
#-------------------------------------------------------------------------------------------

  

df <- RMAest %>%
      #filter(agecat=="6-24 months\ncumulative incidence") %>%
        filter(agecat=="0-24 months (no birth st.)\ncumulative incidence"|
               agecat=="0-24 months\ncumulative incidence") %>%
      filter(intervention_variable %in% c("exclfeed3",
                                          "exclfeed36","exclfeed6","impfloor","impsan",
                                          "nhh","nrooms","perdiar6"))
df <- droplevels(df)

 df$intervention_variable <- factor(df$intervention_variable, levels=
                                      c("exclfeed3","exclfeed36",
                                          "exclfeed6","perdiar6",
                                          "impfloor","impsan",
                                          "nhh","nrooms"))
df <- df %>% arrange(intervention_variable)
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))



yticks <- c(2/3, 1, 3/2,2,3,4)
    
    
  p <-  ggplot(df, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                       alpha=0.5, size = 3) +
        facet_wrap(~RFlabel, scales="free_x", ncol=4, labeller = label_wrap) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
        geom_text(aes(x=0.5, y=(max(df$RR.CI2))-.1, label=Nstudies), size=2,  hjust=0) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          axis.text.y = element_text(size=12),
          strip.text.x = element_text(size=10),
          axis.text.x = element_text(size=10, angle = 20, hjust = 1),
          panel.spacing = unit(0, "lines")) +
        ggtitle("Outcome: any stunting from 6 months to 2 years")
  

    ggsave(p, file="postnatal_RFplot.png",  width=6, height=5.2)
  
  
  
  
  
#-------------------------------------------------------------------------------------------
# wasting as a risk factor
#-------------------------------------------------------------------------------------------
  
  
  
  df <- RMAest %>%
      filter(agecat=="6-24 months\ncumulative incidence") %>%
      filter(intervention_variable %in% c("anywast06","enwast","pers_wast"))
df <- droplevels(df)
  
  
df <- droplevels(df)

 df$intervention_variable <- factor(df$intervention_variable, levels=
                                      c("anywast06","enwast","pers_wast"))
df <- df %>% arrange(intervention_variable)
df$RFlabel[df$intervention_variable=="pers_wast"] <- "Persistent wasting\nbefore 6 months age"
df$RFlabel[df$intervention_variable=="anywast06"] <- "Any wasting\nbefore 6 months age"
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))



yticks <- c(2/3, 1, 3/2,2,3,4)

  
    
  p <-  ggplot(df, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                       alpha=0.5, size = 3) +
        facet_wrap(~RFlabel, scales="free_x", ncol=3, labeller = label_wrap) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
        geom_text(aes(x=0.5, y=(max(df$RR.CI2))-.1, label=Nstudies), size=3,  hjust=0) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4)) +
        scale_colour_manual(values=rep(tableau10,4)) +
        theme(strip.background = element_blank(),
          legend.position="none",
          axis.text.y = element_text(size=12),
          strip.text.x = element_text(size=10),
          axis.text.x = element_text(size=10, angle = 20, hjust = 1),
          panel.spacing = unit(0, "lines")) +
        ggtitle("Associations between wasting before 6 months\nand stunting from 6 months to 2 years")
  

  ggsave(p, file="Wasting_as_a_RF_plots.png",  width=6, height=4.2)
  
  
  
  
  
  
#-------------------------------------------------------------------------------------------
# HAZ curves by WHZ quartiles
#-------------------------------------------------------------------------------------------
  
  load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/HAZ_by_WHZ.RData")
  
  
  p<-ggplot(df, aes(x=agedays, y=haz, group=lag_WHZ_quart, color=lag_WHZ_quart)) + geom_smooth(method = 'gam', formula= y ~ s(x,  k=4, bs = "cs")) +
  facet_wrap(~agecat, scales="free_x", nrow=1) +
  scale_color_manual(values=tableau10, name = "Quartile of WHZ in\nthe prior 3 months")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 1)) +
  xlab("Child age in days") + ylab("HAZ") + 
  ggtitle("Spline curves of HAZ over 3-month age ranges\nstratified by quartile of WHZ in prior 3-month range.") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 0, hjust = 1)) 

ggsave(p, file="HAZcurves_by_WHZ.png", width=8.25, height=3.25)
  



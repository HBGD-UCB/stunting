
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
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_rec24.RData")



setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/")

#-------------------------------------------------------------------------------------------
#Prevalence
#-------------------------------------------------------------------------------------------

clean_nmeans<-function(nmeas){
  nmeas <- round(nmeas/1000)
  nmeas.f <- paste0("N=",nmeas,"K children")
  return(nmeas.f)
}

clean_agecat<-function(agecat){
  agecat <- as.character(agecat)
  agecat <- gsub("months","mo.", agecat)
  agecat <- factor(agecat, levels=unique(agecat))
return(agecat)
}

prev.res$nmeas.f <- clean_nmeans(prev.res$nmeas)
prev.res$agecat <- clean_agecat(prev.res$agecat)


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

ci.res$nmeas.f <- clean_nmeans(ci.res$nmeas)

ci.res$agecat.f <- c("0-3 months", "0-6 months","0-9 months", "0-12 months",  "0-15 months",  "0-18 months", "0-21 months", "0-24 months")
ci.res$agecat.f <- clean_agecat(ci.res$agecat.f)

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

rev.res$agecat <- clean_agecat(rev.res$agecat)


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

df$nmeas.f <- substr(df$nmeas.f, 1, 4)
df$nmeas.f <- gsub(",","",df$nmeas.f)
df$nmeas.f <- paste0(df$nmeas.f, c(rep("K children",8),rep("K at-risk",8)))

df$agecat.f2 <- as.character(df$agecat.f2)
df$agecat.f2 <- gsub("months","mo.",df$agecat.f2)
df$agecat.f2 <- factor(df$agecat.f2, levels=unique(df$agecat.f2))

p4 <- ggplot(df, aes(y=est,x=agecat.f2, fill=Measure, color=Measure))+
              geom_point( size = 4, position=position_dodge(width=0.25)) +
              geom_linerange(aes(ymin=lb, ymax=ub), alpha=0.5, size = 3, position=position_dodge(width=0.25)) +
              scale_y_continuous(limits=c(-2,80))+
              xlab("Age category")+
              ylab("Percent stunted (95% CI)") +
              annotate("text",x=df$agecat.f2[1:8],y=74,label=df$nmeas.f[1:8],size=3) +
              annotate("text",x=df$agecat.f2[9:16],y=1,label=df$nmeas.f[9:16],size=3) +
              annotate("text",x=df$agecat.f2[1:8],y=71,label=df$nstudy.f[1:8],size=3) +
              annotate("text",x=df$agecat.f2[9:16],y=-2,label=df$nstudy.f[9:16],size=3) +
              annotate("text",x=df$agecat.f2[1],y=78,label="N's for cumulative incidence from birth",size=4, hjust = 0) +
              annotate("text",x=df$agecat.f2[1],y=5,label="N's for incidence proportion within age ranges",size=4, hjust = 0) +
              annotate("text",label=df$ptest.f[1:8],x=df$agecat.f2[1:8], y=df$est[1:8],hjust=2.5,size=3)+
              annotate("text",label=df$ptest.f[9:16],x=df$agecat.f2[9:16], y=df$est[9:16],hjust=-2,size=3)+
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
# Incidence proportion
#-------------------------------------------------------------------------------------------

df_nobirth$nmeas.f <- substr(df_nobirth$nmeas.f, 1, 4)
df_nobirth$nmeas.f <- gsub(",","",df_nobirth$nmeas.f)
df_nobirth$nmeas.f <- paste0(df_nobirth$nmeas.f, levels=c(rep("K children",8),rep("K at-risk",8)))

df_nobirth$agecat.f2 <- as.character(df_nobirth$agecat.f2)
df_nobirth$agecat.f2 <- gsub("months","mo.",df_nobirth$agecat.f2)
df_nobirth$agecat.f2 <- factor(df_nobirth$agecat.f2, levels=unique(df_nobirth$agecat.f2))

p5 <- ggplot(df_nobirth, aes(y=est,x=agecat.f2, fill=Measure, color=Measure))+
  geom_point( size = 4, position=position_dodge(width=0.25)) +
  geom_linerange(aes(ymin=lb, ymax=ub), alpha=0.5, size = 3, position=position_dodge(width=0.25)) +
  scale_y_continuous(limits=c(-2,80))+
  xlab("Age category")+
  ylab("Percent stunted (95% CI)") +
  annotate("text",x=df_nobirth$agecat.f2[1:8],y=74,label=df_nobirth$nmeas.f[1:8],size=3) +
  annotate("text",x=df_nobirth$agecat.f2[9:16],y=1,label=df_nobirth$nmeas.f[9:16],size=3) +
  annotate("text",x=df_nobirth$agecat.f2[1:8],y=71,label=df_nobirth$nstudy.f[1:8],size=3) +
  annotate("text",x=df_nobirth$agecat.f2[9:16],y=-2,label=df_nobirth$nstudy.f[9:16],size=3) +
  annotate("text",x=df_nobirth$agecat.f2[1],y=78,label="N's for cumulative incidence from birth",size=4, hjust = 0) +
  annotate("text",x=df_nobirth$agecat.f2[1],y=5,label="N's for incidence proportion within age ranges",size=4, hjust = 0) +
  annotate("text",label=df_nobirth$ptest.f[1:8],x=df_nobirth$agecat.f2[1:8], y=df_nobirth$est[1:8],hjust=2.5,size=3)+
  annotate("text",label=df_nobirth$ptest.f[9:16],x=df_nobirth$agecat.f2[9:16], y=df_nobirth$est[9:16],hjust=-2,size=3)+
  theme(strip.background = element_blank(), strip.text.x = element_text(size=12)) +
  scale_colour_manual(name = "Measure",values=tableau10[c(2,10)]) +
  ggtitle("Pooled cumulative incidence of stunting")+
  theme(strip.background = element_blank(),
        legend.position="right",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p5
ggsave(p5, file="pooled_incprop_nobirth_stunt.png", width=10, height=4)




#-------------------------------------------------------------------------------------------
# Incidence proportion - no birth measurement
#-------------------------------------------------------------------------------------------

df_nobirthmeas$nmeas.f <- substr(df_nobirthmeas$nmeas.f, 1, 4)
df_nobirthmeas$nmeas.f <- gsub(",","",df_nobirthmeas$nmeas.f)
df_nobirthmeas$nmeas.f <- paste0(df_nobirthmeas$nmeas.f, levels=c(rep("K children",8),rep("K at-risk",8)))

df_nobirthmeas$agecat.f2 <- as.character(df_nobirthmeas$agecat.f2)
df_nobirthmeas$agecat.f2 <- gsub("months","mo.",df_nobirthmeas$agecat.f2)
df_nobirthmeas$agecat.f2 <- factor(df_nobirthmeas$agecat.f2, levels=unique(df_nobirthmeas$agecat.f2))

p6 <- ggplot(df_nobirthmeas, aes(y=est,x=agecat.f2, fill=Measure, color=Measure))+
  geom_point( size = 4, position=position_dodge(width=0.25)) +
  geom_linerange(aes(ymin=lb, ymax=ub), alpha=0.5, size = 3, position=position_dodge(width=0.25)) +
  scale_y_continuous(limits=c(-2,80))+
  xlab("Age category")+
  ylab("Percent stunted (95% CI)") +
  annotate("text",x=df_nobirthmeas$agecat.f2[1:8],y=74,label=df_nobirthmeas$nmeas.f[1:8],size=3) +
  annotate("text",x=df_nobirthmeas$agecat.f2[9:16],y=1,label=df_nobirthmeas$nmeas.f[9:16],size=3) +
  annotate("text",x=df_nobirthmeas$agecat.f2[1:8],y=71,label=df_nobirthmeas$nstudy.f[1:8],size=3) +
  annotate("text",x=df_nobirthmeas$agecat.f2[9:16],y=-2,label=df_nobirthmeas$nstudy.f[9:16],size=3) +
  annotate("text",x=df_nobirthmeas$agecat.f2[1],y=78,label="N's for cumulative incidence from birth",size=4, hjust = 0) +
  annotate("text",x=df_nobirthmeas$agecat.f2[1],y=5,label="N's for incidence proportion within age ranges",size=4, hjust = 0) +
  annotate("text",label=df_nobirthmeas$ptest.f[1:8],x=df_nobirthmeas$agecat.f2[1:8], y=df_nobirthmeas$est[1:8],hjust=2.5,size=3)+
  annotate("text",label=df_nobirthmeas$ptest.f[9:16],x=df_nobirthmeas$agecat.f2[9:16], y=df_nobirthmeas$est[9:16],hjust=-2,size=3)+
  theme(strip.background = element_blank(), strip.text.x = element_text(size=12)) +
  scale_colour_manual(name = "Measure",values=tableau10[c(2,10)]) +
  ggtitle("Pooled cumulative incidence of stunting")+
  theme(strip.background = element_blank(),
        legend.position="right",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
p6
ggsave(p6, file="pooled_incprop_nobirth_meas.png", width=10, height=4)

          
  

#-------------------------------------------------------------------------------------------
# Velocity
#-------------------------------------------------------------------------------------------

vel <- rbind(data.frame(measure="Length velocity (cm per month)" , poollencm[poollencm$stratacol=="pooled" & poollencm$region=="Overall",]),
       data.frame(measure="LAZ change (Z-score per month)" , poolhaz[poolhaz$stratacol=="pooled" & poolhaz$region=="Overall",]))


vel$nmeas.f <- clean_nmeans(vel$N)
vel$strata <- clean_agecat(vel$strata)


p7 <- ggplot(vel, aes(y=Mean,x=strata))+
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
        axis.text.x = element_text(size=12, angle = 25, hjust = 1)) 

ggsave(p7, file="pooled_velocity.png", width=10, height=4)









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
        labs(x = "Exposure level", y = "Relative risk") +
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
      filter(intervention_variable %in% c("sex","birthlen", "birthwt", "gagebrth", "hdlvry","vagbrth", "parity"))
df <- droplevels(df)

df$intervention_variable <- factor(df$intervention_variable, levels=
                                     c("sex","gagebrth","birthlen", "birthwt",  "hdlvry","vagbrth", "parity"))
df <- df %>% arrange(intervention_variable)
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))

# df$intervention_level <- revalue(df$intervention_level, c("Normal or high birthweight"="Normal birthweight",
#                                                           "Full or late term"="Full term"))


yticks <- c(2/3, 1, 3/2,2,3,4)
    
  p <-  ggplot(df, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                       alpha=0.5, size = 3) +
        facet_wrap(~RFlabel, scales="free_x", ncol=4, labeller = label_wrap) +
        labs(x = "Exposure level", y = "Relative risk") +
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

  
    df <- df %>% filter((RR.CI1 < 1 & RR.CI2 < 1)|(RR.CI1 > 1 & RR.CI2 > 1))
    as.data.frame(df)
#-------------------------------------------------------------------------------------------
# postnatal characteristics
#-------------------------------------------------------------------------------------------

  

df <- RMAest %>%
      #filter(agecat=="6-24 months\ncumulative incidence") %>%
        filter(agecat=="6-24 months (no birth st.)\ncumulative incidence"|
               agecat=="6-24 months\ncumulative incidence") %>%
      filter(intervention_variable %in% c("exclfeed3",
                                          "exclfeed36","exclfeed6","impfloor","impsan",
                                          "nhh","nrooms","hfoodsec"))
df <- droplevels(df)

 df$intervention_variable <- factor(df$intervention_variable, levels=
                                      c("exclfeed3","exclfeed36",
                                          "exclfeed6","hfoodsec",
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
        labs(x = "Exposure level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
        geom_text(aes(x=0.5, y=(min(df$RR.CI1))+.1, label=Nstudies), size=2,  hjust=0) +
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
  df2 <- df
  
  
df <- RMAest %>%
      filter(agecat=="6-24 months\ncumulative incidence") %>%
      filter(intervention_variable %in% c("anywast06","pers_wast"))
    
    df_monthly <- RMAest_monthky %>% filter(agecat=="6-24 months")
    df_monthly$Nstudies <- paste0("N studies ", as.character(df_monthly$Nstudies))
    df_monthly$intervention_level <- c("No", "Yes")
    df_monthly$intervention_variable <- "anywast06_monthly"
    
    df <- bind_rows(df, df_monthly)
df <- droplevels(df)
  
  

 df$intervention_variable <- factor(df$intervention_variable, levels=
                                      c("anywast06", "anywast06_monthly","pers_wast"))
df <- df %>% arrange(intervention_variable)
df$RFlabel[df$intervention_variable=="pers_wast"] <- "Persistent wasting\nbefore 6 months age"
df$RFlabel[df$intervention_variable=="anywast06_monthly"] <- "Any wasting\nbefore 6 months age\n(monthly studies)"
df$RFlabel[df$intervention_variable=="anywast06"] <- "Any wasting\nbefore 6 months age"
df$RFlabel <- factor(df$RFlabel, levels=unique(df$RFlabel))



yticks <- c(2/3, 1, 3/2,2,3,4)


    
  p <-  ggplot(df, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=intervention_variable, color=intervention_variable), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=intervention_variable),
                       alpha=0.5, size = 3) +
        facet_wrap(~RFlabel, scales="free_x", ncol=3, labeller = label_wrap) +
        labs(x = "Exposure level", y = "Relative risk") +
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
# Stunting by WHZ quartiles
#-------------------------------------------------------------------------------------------

  load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/webinar_results.rdata")
  
  table(results$intervention_variable)
  results <- results %>% filter(type=="RR" & intervention_variable=="lag_WHZ_quart")
  
  #Pooled estimate function
  poolRR <- function(d){
    #nstudies=length(unique(d$studyid))
    nstudies <- d %>% summarize(N=n())
    
    if(d$intervention_level[1] == d$baseline_level[1]){
      est <- data.frame(logRR.psi=1, logSE=0, RR=1, RR.CI1=1, RR.CI2=1, Nstudies= nstudies$N)
    }else{
      
      fit<-NULL
      try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="REML", measure="RR"))
      if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"))}
      
      est<-data.frame(fit$b, fit$se)
      colnames(est)<-c("logRR.psi","logSE")
      
      est$RR<-exp(est$logRR)
      est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
      est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
      
      est$Nstudies <- nstudies$N
    }
    
    return(est)
  }
  
  RMAest <- results %>% group_by(intervention_variable, agecat, intervention_level) %>% 
    do(try(poolRR(.))) %>% as.data.frame()
  
  
  

  
  
  
  label_wrap <- function(variable, value) {
    lapply(strwrap(as.character(value), width=10, simplify=FALSE), 
           paste, collapse="\n")
  }  

    yticks <- c(2/3, 1, 3/2,2,3,4)
  
  
  
df <- RMAest %>% filter(intervention_variable=="lag_WHZ_quart")
#df <- df[grepl("prev",df$agecat),]
df <- df[!grepl("-",df$agecat),]

#Temp fix agecat
df$agecat <- as.character(df$agecat)
df$agecat[df$agecat=="21 months"] <- "24 months"
df$agecat[df$agecat=="18 months"] <- "21 months"
df$agecat[df$agecat=="15 months"] <- "18 months"
df$agecat[df$agecat=="12 months"] <- "15 months"
df$agecat[df$agecat=="9 months"] <- "12 months"
df$agecat[df$agecat=="6 months"] <- "9 months"
df$agecat[df$agecat=="3 months"] <- "6 months"

#df$agecat <- factor(df$agecat, levels=c("3 months","6 months","9 months","12 months","15 months","18 months","21 months"))
df$agecat <- factor(df$agecat, levels=c("6 months","9 months","12 months","15 months","18 months","21 months", "24 months"))




df$intervention_level <- as.character(df$intervention_level)
df$intervention_level[df$intervention_level=="1"] <- "Q1"  
df$intervention_level[df$intervention_level=="2"] <- "Q2"  
df$intervention_level[df$intervention_level=="3"] <- "Q3"  
df$intervention_level[df$intervention_level=="4"] <- "Q4"  
df$intervention_level <- factor(df$intervention_level)

p <- ggplot(df, aes(x=intervention_level)) +
    geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
    geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                   alpha=0.5, size = 3) +
    facet_wrap(~agecat, scales="free_x", nrow=1, labeller = label_wrap) +
    labs(x = "Quartile of mean WLZ in the past three months",
         y = "Relative Risk of Stunting") +
    geom_hline(yintercept = 1) +
    geom_text(aes(x=0.5, y=(max(df$RR.CI2))-.1, label=paste0("Studies: ",Nstudies)), size=3,  hjust=0) +
    scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
    scale_fill_manual(values=rep(tableau10,4)) +
    scale_colour_manual(values=rep(tableau10,4)) +
    theme(plot.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          legend.position="none",
          axis.text.y = element_text(size=12),
          strip.text.x = element_text(size=10),
          axis.text.x = element_text(size=9, angle = 20, hjust = 1),
          panel.spacing = unit(0, "lines")) +
    ggtitle("Associations between prevalent stunting and\nquartile of mean WLZ in the past three months")
p

ggsave(p, file="Stunting_by_prior_WLZ_plots.png",  width=7, height=5)






df <- RMAest %>% filter(intervention_variable=="lag_WHZ_quart")
df <- df[grepl("-",df$agecat),]
df$agecat <- factor(df$agecat, levels=c("3-6 months","6-9 months","9-12 months","12-15 months","15-18 months"))

df$intervention_level <- as.character(df$intervention_level)
df$intervention_level[df$intervention_level=="1"] <- "Q1"  
df$intervention_level[df$intervention_level=="2"] <- "Q2"  
df$intervention_level[df$intervention_level=="3"] <- "Q3"  
df$intervention_level[df$intervention_level=="4"] <- "Q4"  
df$intervention_level <- factor(df$intervention_level)

p <- ggplot(df, aes(x=intervention_level)) +
  geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                 alpha=0.5, size = 3) +
  facet_wrap(~agecat, scales="free_x", nrow=1, labeller = label_wrap) +
  labs(x = "Quartile of mean WLZ in the prior three months to the CI period",
       y = "Relative Risk of Stunting") +
  geom_hline(yintercept = 1) +
  geom_text(aes(x=0.5, y=(max(df$RR.CI2))-.1, label=paste0("Studies: ",Nstudies)), size=3,  hjust=0) +
  scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
  scale_fill_manual(values=rep(tableau10,4)) +
  scale_colour_manual(values=rep(tableau10,4)) +
  theme(plot.title = element_text(hjust = 0.5),
        strip.background = element_blank(),
        legend.position="none",
        axis.text.y = element_text(size=12),
        strip.text.x = element_text(size=10),
        axis.text.x = element_text(size=9, angle = 20, hjust = 1),
        panel.spacing = unit(0, "lines")) +
  ggtitle("Associations between cumulative incidence of\nstunting over 3 months periods and\nquartile of mean WLZ in the prior 3 months periods")
p

ggsave(p, file="Stunting_CI_by_prior_WLZ_plots.png",  width=7, height=5)


#-------------------------------------------------------------------------------------------
# HAZ curves by WHZ quartiles
#-------------------------------------------------------------------------------------------
  
  load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/HAZ_by_WHZ.RData")


  
  p<-ggplot(plotdf, aes(x=agedays, y=haz, group=lag_WHZ_quart, color=lag_WHZ_quart)) +
    geom_smooth(method = 'loess') +
    #geom_smooth(method = 'gam', formula= y ~ s(x,  k=4, bs = "cs")) +
  facet_wrap(~agecat, scales="free_x", nrow=1) +
  scale_color_manual(values=tableau10, name = "Quartile of WHZ in\nthe prior 3 months")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 1)) +
  xlab("Child age in days") + ylab("LAZ") + 
  ggtitle("Spline curves of HAZ over 3-month age ranges\nstratified by quartile of WHZ in prior 3-month range.") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 0, hjust = 1)) 

ggsave(p, file="HAZcurves_by_WHZ.png", width=8.25, height=3.25)
  









#-------------------------------------------------------------------------------------------
# Recovery by 2 years
#-------------------------------------------------------------------------------------------


# sort by recovery %
rev_clean <- function(d){
    d$cohort=factor(d$cohort, levels = d$cohort[order(d$y)])
    d$y[d$cohort=="Pooled"]=d$y[d$cohort=="Pooled"]*100
    d$ci.lb[d$cohort=="Pooled"]=d$ci.lb[d$cohort=="Pooled"]*100
    d$ci.ub[d$cohort=="Pooled"]=d$ci.ub[d$cohort=="Pooled"]*100
    d$pooled=as.factor(ifelse(d$cohort=="Pooled",1,0))
  
    return(d)
}

plot.df <- rev_clean(plot.df)
plot.df36 <- rev_clean(plot.df36)
plot.df48 <- rev_clean(plot.df48)

plot.df$agecat <- plot.df36$agecat <- plot.df48$agecat <- "age"
plot.df$agecat[plot.df$cohort=="Pooled"] <- plot.df36$agecat[plot.df36$cohort=="Pooled"] <- plot.df48$agecat[plot.df48$cohort=="Pooled"] <- "pooled"

# plot recovery
prec <- ggplot(plot.df,aes(y=y,x=cohort), color=tableau10[3])+
  geom_point(aes(fill=agecat, color=agecat, shape=agecat), size = 4) +
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub, color=agecat), alpha=0.5, size = 3) +
  coord_flip()+
  scale_color_manual(values=c(tableau10[3],"black")) + scale_fill_manual(values=c(tableau10[3],"black"))+
  scale_shape_manual(values=c(21, 23)) +
  scale_y_continuous(limits=c(0,100))+
  xlab("Cohort")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who became stunted and were recovered at 36 months") +
  theme(legend.position = "none")

ggplot(plot.df36,aes(y=y,x=cohort), color=tableau10[3])+
  geom_point(aes(fill=agecat, color=agecat, shape=agecat), size = 4) +
  geom_linerange(aes(ymin=ci.lb, ymax=ci.ub, color=agecat), alpha=0.5, size = 3) +
  coord_flip()+
  scale_color_manual(values=c(tableau10[3],"black")) + scale_fill_manual(values=c(tableau10[3],"black"))+
  scale_shape_manual(values=c(21, 23)) +
  scale_y_continuous(limits=c(0,100))+
  xlab("Cohort")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who became stunted and were recovered at 36 months") +
  theme(legend.position = "none")


ggsave(prec, file="pooled_rec24.png", width=10, height=4)

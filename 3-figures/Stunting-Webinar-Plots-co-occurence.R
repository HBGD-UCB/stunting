
#---------------------------------------------------------------------
# Co-occurance webinar plots
#---------------------------------------------------------------------

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(metafor)
library(ggthemes)
library(scales)
library(gridExtra)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

# load base functions
source("C:/Users/andre/Documents/HBGDki/Stunting/1-outcomes/0_st_basefunctions.R")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/co_CI.Rdata")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/co_prev.RData")

setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/")


clean_agecat<-function(agecat){
  agecat <- as.character(agecat)
  agecat <- gsub("months","mo.", agecat)
  agecat <- factor(agecat, levels=unique(agecat))
  return(agecat)
}

ci.res$agecat <- clean_agecat(ci.res$agecat)
prev.res$agecat <- clean_agecat(prev.res$agecat)
dmn$agecat <- clean_agecat(dmn$agecat)
plotdf$agecat <- clean_agecat(plotdf$agecat)

prev.res$nmeas.f <- c("N=41K subj.", "N=53K subj.", "N=49K subj.", "N=39K subj.", "N=43K subj.", "N=18K subj.", "N=24K subj.")


# plot pooled prevalence
p_pool <- ggplot(prev.res, aes(y=est,x=agecat)) +
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[4],20))+ 
  xlab("Age category")+
  ylab("Point prevalence (95% CI)")+
  scale_y_continuous(limits=c(0,10))+
  annotate("text",x=prev.res$agecat,y=9.5,label=prev.res$nmeas.f,size=3)+
  annotate("text",x=prev.res$agecat,y=10,label=prev.res$nstudy.f,size=3)+
  annotate("text",label=round(prev.res$est, 1),x=prev.res$agecat,y=prev.res$est,hjust=-1.25,size=3)+
  ggtitle("Pooled point prevalence of stunting and wasting co-occurance") +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 

print(p_pool)

ggsave(p_pool, file="pooled_co_prev.png", width=6.5, height=5.5)


dmn <- dmn %>% rename(Status=status)

dmn <- dmn %>% arrange(agelevel)
dmn$agecat <- factor(as.character(dmn$agecat), levels=unique(as.character(dmn$agecat)))
                     
barplot_NH <- ggplot(dmn[dmn$Status!="Healthy",], aes(agecat, ..count..)) + geom_bar(aes(fill = Status), position = "fill") +
  scale_fill_manual(values=tableau10[-1]) +
  xlab("Age") + ylab("Proportion") +
  scale_y_continuous(labels = percent_format()) + 
  #ggtitle("Comparing distribution of co-occurrence, wasting only, and stunting only\namong children wasted or stunted at each age")+
  ggtitle("Co-occurrence status among children wasted or stunted at each age") +
  theme(strip.background = element_blank(),
        legend.position="bottom",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 
barplot_NH


barplot_H <- ggplot(dmn, aes(agecat, ..count..)) + geom_bar(aes(fill = Status), position = "fill") +
  scale_fill_manual(values=tableau10) +
  xlab("Age") + ylab("Proportion") +
  scale_y_continuous(labels = percent_format()) + 
  #ggtitle("Co-occurrence status among children\nwasted or stunted at each age") +
  ggtitle("Distribution of undernutrition\nstatus at each age") +
    theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle=25)) + 
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
barplot_H



#Make plot of the proportion stunted among wasted and not wasted, and vice-versa
d_wast <- dmn %>% filter(whz < -2) %>% mutate(Status = ifelse(Status=="Co-occurence", "Stunted", "Not stunted"))
d_stunt <- dmn %>% filter(haz < -2) %>% mutate(Status = ifelse(Status=="Co-occurence", "Wasted", "Not wasted"))

barplot_stunt <- ggplot(d_stunt, aes(agecat, ..count..)) + geom_bar(aes(fill = Status), position = "fill") +
  scale_fill_manual(values=tableau10[-2]) +
  xlab("Age") + ylab("Proportion") +
  scale_y_continuous(labels = percent_format()) + 
  ggtitle("Proportion of wasted children among children who are stunted")+
  theme(strip.background = element_blank(),
        legend.position="bottom",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 

barplot_wast <- ggplot(d_wast, aes(agecat, ..count..)) + geom_bar(aes(fill = Status), position = "fill") +
  scale_fill_manual(values=tableau10[c(3,4)]) +
  xlab("Age") + ylab("Proportion") +
  scale_y_continuous(labels = percent_format()) + 
  ggtitle("Proportion of stunted children\namong children who are wasted")+
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle=25))
        #plot.margin=unit(c(0.1,0.1,0.9,0.2),"cm"))

ggsave(barplot_wast, file="pers_stunting_among_wasted.png", width=5.5, height=4)


#---------------------------------------------------------------------
# Presentation sized plots.
#---------------------------------------------------------------------

#Combined pooled prev and bar chart
jpeg(file="pooled_prev.jpg", width=7, height=5.5, units = "in", res=400)
  #multiplot(p_pool, barplot_H, barplot_wast, layout=)
  grid.arrange(
    p_pool, barplot_H, barplot_wast, # widths = c(2, 1, 2),
    layout_matrix = rbind(c(1, 1),
                          c(2, 3)))
dev.off()




#---------------------------------------------------------------------
# CI Outcomes
#---------------------------------------------------------------------


p_pool <- ggplot(ci.res, aes(y=est,x=agecat)) +
  geom_point(aes(fill=agecat, color=agecat), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=agecat),
                 alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10[4],20))+ 
  xlab("Age category")+
  ylab("Cumulative incidence (95% CI)")+
  scale_y_continuous(limits=c(0,13))+
  annotate("text",x=ci.res$agecat,y=12.5,label=ci.res$nmeas.f,size=3)+
  annotate("text",x=ci.res$agecat,y=13,label=ci.res$nstudy.f,size=3)+
  annotate("text",label=round(ci.res$est, 1),x=ci.res$agecat,y=ci.res$est,hjust=-1.25,size=3)+
  ggtitle("Pooled cumulative incidence of stunting and wasting\nco-occurance among children in monthly-measured studies") + 
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12)) 

print(p_pool)



ggsave(p_pool, file="pooled_co_ci.png", width=5.5, height=4)




plotdf$which_first <- factor(plotdf$which_first, levels=c("Wasting", "Stunting", "Co-occurence" ))
plotdf <- plotdf %>% arrange(rev(which_first))
plotdf <- plotdf %>% rename(`Which occured\nfirst`=which_first)

barplot <- ggplot(plotdf, aes(agecat, ..count..)) + geom_bar(aes(fill = `Which occured\nfirst`), position = "fill") +
  scale_fill_manual(values=tableau10[c(3,2,4)]) +
  xlab("Age") + ylab("Proportion") +
  scale_y_continuous(labels = percent_format()) +
  theme(strip.background = element_blank(),
        legend.position="right",
        strip.text.x = element_text(size=12),
        #axis.text.x = element_text(size=12, angle = 15, hjust = 1), 
        axis.text.x = element_text(size=12), 
        strip.text.y = element_text(angle = 0)) +
  ggtitle("Proportion of stunting-first, wasting first, and co-occurence among\nchildren who became both wasted and stunted in the age range")
barplot





#Combined pooled prev and bar chart
jpeg(file="pooled_CI.jpg", width=6.5, height=5.5, units = "in", res=400)
multiplot(p_pool, barplot, cols=1)
dev.off()







#---------------------------------------------------------------------
# WHZ by HAZ curves
#---------------------------------------------------------------------

#Splines of HAZ by prior WHZ category
#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728", 
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

plotdf <- df %>% filter(measurefreq=="monthly")

p<-ggplot(plotdf, aes(x=agedays, y=haz, group=lag_WHZ_quart, color=lag_WHZ_quart)) + geom_smooth(method = 'loess') +
  facet_wrap(~agecat, scales="free_x", nrow=1) +
  scale_color_manual(values=tableau10, name = "Quartile of WHZ in\nthe prior 3 months")+
  xlab("Child age in days") + ylab("HAZ") + 
  ggtitle("Spline curves of HAZ over 3-month age ranges\nstratified by quartile of WHZ in prior 3-month range.") +
  theme(strip.background = element_blank(),
        legend.position="bottom",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=12, angle = 0, hjust = 1)) 


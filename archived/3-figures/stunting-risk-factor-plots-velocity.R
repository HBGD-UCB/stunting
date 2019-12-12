

rm(list=ls())
library(tidyverse)
library(metafor)

source("C:/Users/andre/Documents/HBGDki/GHAP-Data-Management/HBGDki_shared_functions.R")

#load("C:/Users/andre/Documents/HBGDki/Results/sprint_7D_longbow-master/sprint_7D_longbow-master/adjusted_binary/adjusted_binary_results.Rdata")
load("C:/Users/andre/Documents/HBGDki/Results/sprint_7D_longbow-master/sprint_7D_longbow-master/adjusted_velocity/adjusted_velocity_results.rdata")

d <- results
table(d$intervention_variable)

#Compare change in Z score
d <- d %>% filter(outcome_variable=="y_rate_haz")

#Subset to ATEs
table(d$type)
d <- d %>% filter(type=="ATE")

#Subset to primary outcomes
table(d$agecat)
table(is.na(d$agecat))

#Drop enrolled stunted as a RF for stunting
d <- d %>% filter(intervention_variable!="enstunt")

RMAest <- d %>% group_by(intervention_variable, agecat, intervention_level) %>% 
              do(try(poolRR(., family="gaussian"))) %>% as.data.frame()


#Clean dataframe
RMAest <- RMA_clean(RMAest, outcome="velocity", agecatlevels=c("0-3 months\nvelocity", "3-6 months\nvelocity", "6-12 months\nvelocity",  "12-24 months\nvelocity"))
RMAest$intervention_variable <- factor(RMAest$intervention_variable)

#Print plots across all intervention arms
yticks <- c((-4:5)/10)


#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Stunting/")
pdf("Velocity pooled/Risk Factor Plots Velocity.pdf", height=8, width=12)

j<-0
for(i in levels(RMAest$intervention_variable)){
  j<-j+1
  plotdf <- RMAest[RMAest$intervention_variable==i,]  
    
  ytick_range <- c(min((c(plotdf$CI1,plotdf$CI2)))-0.01, max((c(plotdf$CI1,plotdf$CI2)))+0.01)
  
  p <-  ggplot(plotdf, aes(x=intervention_level)) + 
        geom_point(aes(y=ATE, fill=agecat, color=agecat), size = 4) +
        geom_linerange(aes(ymin=CI1, ymax=CI2, color=agecat),
                       alpha=0.5, size = 3) +
        labs(x = "Risk factor level", y = "Change in Z-score") +
        geom_hline(yintercept = 0) +
        geom_text(aes(x=0.5, y=-0.05, label=Nstudies), size=3,  hjust=0) +
        coord_cartesian(ylim=ytick_range) +
        scale_y_continuous(breaks=yticks, labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        scale_size_continuous(range = c(0.5, 1))+
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=10), axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        facet_wrap(~agecat, nrow = 1, labeller = label_wrap) +
        ggtitle(plotdf$RFlabel[1])
  ggsave(p, file=paste0("Velocity pooled/",j,"_",i,"_RFplot.png"), width=5.7, height=4.6)
  print(p)
}

dev.off()

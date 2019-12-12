

rm(list=ls())
library(tidyverse)
library(metafor)

source("C:/Users/andre/Documents/HBGDki/GHAP-Data-Management/HBGDki_shared_functions.R")

load("C:/Users/andre/Documents/HBGDki/Results/sprint_7D_longbow-master/sprint_7D_longbow-master/adjusted_binary/adjusted_binary_results.Rdata")

d <- results
table(d$intervention_variable)

#Subset to ATEs
table(d$type)
d <- d %>% filter(type=="RR")



#Subset to primary outcomes
table(d$agecat)
table(is.na(d$agecat))
d <- d %>% filter(agecat=="6 months"|
                  agecat=="24 months"|
                  agecat=="0-6 months"|
                  agecat=="6-24 months"|
                  agecat=="0-24 months (no birth st.)"|
                  agecat=="0-6 months (no birth st.)"|
                  agecat=="0-24 months")


#Drop enrolled stunted as a RF for stunting
d <- d %>% filter(intervention_variable!="enstunt")



head(d)




RMAest <- d %>% group_by(intervention_variable, agecat, intervention_level) %>% 
              do(try(poolRR(., family="binomial"))) %>% as.data.frame()


#Clean dataframe
RMAest <- RMA_clean(RMAest)

#Split prevalence and incidence
RMAprev <- RMAest[!grepl("incidence",RMAest$agecat),]
RMAest <- RMAest[grepl("incidence",RMAest$agecat),]


#Print plots across all intervention arms
yticks <- c(0.125,0.25,0.5,1,2,4,8,16)


#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Stunting/")
pdf("CI only pooled/Risk Factor Plots Stunting.pdf", height=8, width=12)

j<-0
for(i in levels(RMAest$intervention_variable)){
  j<-j+1
  plotdf <- RMAest[RMAest$intervention_variable==i,]  
    
  p <-  ggplot(plotdf, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                       alpha=0.5, size = 3) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
          geom_text(aes(x=0.75, y=ceiling(max(plotdf$RR.CI2)), label=Nstudies), size=3,  hjust=0) +
        #coord_cartesian(ylim=range(yticks)) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
        scale_size_continuous(range = c(0.5, 1))+
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=10), axis.text.y = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        facet_wrap(~agecat, nrow = 1, labeller = label_wrap) +
        ggtitle(plotdf$RFlabel[1])
  ggsave(p, file=paste0("CI only pooled/",j,"_",i,"_RFplot.png"), width=5.7, height=4.6)
  #ggsave(p, file=paste0("CI only pooled/",j,"_",i,"_RFplot.png"), width=5.7, height=2.3)
  print(p)
}

dev.off()


label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=10, simplify=FALSE), 
        paste, collapse="\n")
}  



setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Stunting/")
pdf("Prev only pooled/Risk Factor Plots Stunting.pdf", height=8, width=12)

RMAprev <- droplevels(RMAprev)

j<-0
for(i in levels(RMAprev$intervention_variable)){
  j<-j+1
  plotdf <- RMAprev[RMAprev$intervention_variable==i,]  
    
  p <-  ggplot(plotdf, aes(x=intervention_level)) + 
        geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
        geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
                       alpha=0.5, size = 3) +
        labs(x = "Risk factor level", y = "Relative risk") +
        geom_hline(yintercept = 1) +
          geom_text(aes(x=0.75, y=ceiling(max(plotdf$RR.CI2)), label=Nstudies), size=3,  hjust=0) +
        #coord_cartesian(ylim=range(yticks)) +
        scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
        scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAprev$agecat)) +
        scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAprev$agecat)) +
        scale_size_continuous(range = c(0.5, 1))+
        theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
        facet_wrap(~agecat, nrow = 1, labeller = label_wrap) +
        ggtitle(plotdf$RFlabel[1])
  ggsave(p, file=paste0("Prev only pooled/",j,"_",i,"_RFplot.png"), width=5.7, height=4.6)
  print(p)
}

dev.off()



# 
# #Create 1 summary plot with the largest RR
# sumdf <- RMAest %>% ungroup() %>% 
#   group_by(intervention_variable) %>% 
#   mutate(maxRR=max(abs(1-RR))) %>% 
#   filter(maxRR==abs(1-RR)) %>% ungroup() %>%
#   arrange(RR) %>%
#   mutate(order=row_number())
# 
# sumdf$intervention_variable <- as.character(sumdf$intervention_variable)
# sumdf$intervention_variable <- factor(sumdf$intervention_variable, levels=unique(sumdf$intervention_variable))
# 
# yticks <- c(0.125,0.25,0.5,1,2,4,8,16,32,64)
# 
# 
# p <- ggplot(sumdf, aes(x=intervention_variable)) + 
#         geom_point(aes(y=RR, fill=agecat, color=agecat), size = 4) +
#         geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=agecat),
#                        alpha=0.5, size = 3) +
#         labs(x = "Risk factor", y = "Relative risk") +
#         geom_hline(yintercept = 1) +
#           scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
#         scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
#         scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(RMAest$agecat)) +
#         theme(strip.background = element_blank(),
#           legend.position="none",
#           strip.text.x = element_text(size=12),
#           axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
#         ggtitle("")
# 
# print(p) 
# 




  
  
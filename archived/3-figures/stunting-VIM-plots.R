

rm(list=ls())
library(tidyverse)
library(metafor)


source("C:/Users/andre/Documents/HBGDki/GHAP-Data-Management/HBGDki_shared_functions.R")

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")



load("C:/Users/andre/Documents/HBGDki/Results/opttx_vim_results.rdata")

d <- results

allvars<-unique(d$intervention_variable)

head(d)
table(d$type)

#Subset to relative risks
d <- d %>% filter(type=="RR")


#Subset to adjusted
d <- d %>% filter(adjustment_set!="unadjusted" | intervention_variable=="sex")

missing <- allvars[!(allvars %in% unique(d$intervention_variable))]
missing

#Subset to primary outcomes
table(d$agecat)

d <- d %>% filter( agecat=="Birth"|  agecat=="6 months"|  agecat=="24 months"| 
                     agecat=="0-6 months"|  agecat=="6-24 months"| agecat=="0-24 months"|  agecat=="0-24 months (no birth st.)"| agecat=="0-6 months (no birth st.)")


#Drop enrolled wasted as a RF for wasting
d <- d %>% filter(intervention_variable!="enstunt" & intervention_variable!="perdiar24")

#Look at prevalence and incidence
table(d$outcome_variable)
d <- d %>% filter(outcome_variable=="ever_stunted")

head(d)



RMAest <- d %>% group_by(intervention_variable, agecat, intervention_level,outcome_variable) %>%
  do(poolRR(.)) %>% as.data.frame()






#Clean dataframe
RMAest$agecat <- as.character(RMAest$agecat)
RMAest$agecat[RMAest$agecat=="0-24 months (no birth st.)"] <- "0-24 months"
RMAest$agecat[RMAest$agecat=="0-6 months (no birth st.)"] <- "0-6 months"

#Drop nonsensical exposure-outcome pairs
RMAest <- RMAest[!((RMAest$agecat=="0-6 months" | RMAest$agecat=="Birth") & RMAest$intervention_variable %in% c("exclfeed3","exclfeed36","exclfeed6","perdiar6","predexfd6","predfeed3","predfeed36","predfeed6")),]
RMAest <- RMAest[!(RMAest$agecat=="Birth" & (RMAest$intervention_variable=="birthwt"| RMAest$intervention_variable=="earlybf"| RMAest$intervention_variable=="birthlen"| RMAest$intervention_variable=="gagebrth")),] 


RMAest <- RMA_clean(RMAest, outcome="ever_wasted", 
                    agecatlevels=c("Birth prevalence","0-6 months\ncumulative incidence","6 months prevalence", "6-24 months\ncumulative incidence",  "0-24 months\ncumulative incidence","24 months prevalence"))
RMAest$RFlabel[RMAest$RFlabel=="Exclusive or Predominant breastfeeding under 6 months"] <- "Exclusive or Predominant\nbreastfeeding under 6 months" 

RMAest$intervention_variable <- factor(RMAest$intervention_variable)

#Add the reference level to the RFlabel
#RMAest$RFlabel <- paste0(RMAest$RFlabel," (Ref:",RMAest$baseline,")")


d <- RMAest



d$intervention_level <- factor(d$intervention_level, levels= c("0",  "No",                 
                                                               "Yes",">=50 cm","[48-50) cm","<48 cm",">= 2500 g", "< 2500 g",         
                                                               "4+",  "3", "2", "1",                             
                                                               "<32",  "[32-38)", ">=38",             
                                                               "High", "Medium","Low",
                                                               ">=167 cm", "[162-167) cm",  "<162 cm",            
                                                               "Full or late term", "Early term","Preterm",            
                                                               "Food Secure", "Mildly Food Insecure", "Food Insecure",      
                                                               "Q4","Q3", "Q2", "Q1",
                                                               "<25","[25-30)",">=30",               
                                                               "Overweight or Obese","Normal weight","Underweight",
                                                               ">=155 cm", "[151-155) cm",  "<151 cm",
                                                               ">=58 kg", "[52-58) kg","<52 kg", 
                                                               "2+","3+",  
                                                               "3 or less",  "4-5",    "6-7",    "8+",          
                                                               "0%", "(0%, 5%]", ">5%",                 
                                                               "Female", "Male"))  
d <- d %>% arrange(intervention_level)


#Set order of RF categories
unique(d$RFtype)
d$RFtype <- factor(d$RFtype , levels=c("Parent", "Parent\nanthro", "Birth", "Child", "Breast-\nfeeding", "SES", "House-\nhold", "WASH"))         
d <- d %>% arrange(RFtype)

#Subset to outcome and agecat of interest
groups024 <- c("Parent","Parent\nanthro","Birth","SES","House-\nhold","Child")  
groups624 <- c("Child","Breast-\nfeeding","WASH")


#Subset to outcome and agecat of interest
d <- d %>% filter((agecat=="0-24 months\ncumulative incidence" & RFtype %in% groups024) |
                                      (agecat=="6-24 months\ncumulative incidence" & RFtype %in% groups624)) %>%
  filter(!(intervention_variable=="sex" & agecat=="6-24 months\ncumulative incidence")) %>%
  filter(!(intervention_variable=="parity" & agecat=="6-24 months\ncumulative incidence")) %>%
  filter(!(intervention_variable=="perdiar6" & agecat=="0-24 months\ncumulative incidence"))

#Plot parameters
textcol <- "grey20"
plottitle="Cumulative incidence of stunting"



d$RFlabel <- as.character(d$RFlabel)
d <- d %>% arrange(rev(RR))
d$RFlabel <- factor(d$RFlabel, levels=unique(d$RFlabel))

yticks <- c( 0.5,0.6,0.7, 0.8,0.9, 1.00)
#hbgdki pallet
tableau10 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
scaleFUN <- function(x) sprintf("%.1f", x)

  #df$agecat <- "0-24 months cumulative incidence\n(no birth stunting)"
  Ylab <- "Relative Risk of EBF compared to no EBF"
  
  p <-  ggplot(d, aes(x=RFlabel)) + 
    geom_point(aes( y=RR,  color=RFtype), size = 4) +
    geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=RFtype)) +
    coord_flip(ylim=range(0.5,1)) +
    labs(x = "Exposure", y = "Percent reduction in cumulative incidence of stunting\nassociated with assigning each variable optimally") +
    geom_hline(yintercept = 1) +
    scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
    scale_shape_manual(values=c(21, 23)) +
    scale_colour_manual(values=tableau10, name = "Exposure\nCategory") +
    scale_size_continuous(range = c(0.5, 1))+
    theme(strip.background = element_blank(),
          legend.position="right",
          strip.text.x = element_text(size=12),
          axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
    ggtitle("Exposures ranked by\nvariable importance metric") +guides(shape=FALSE)
  print(p)


setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar")
ggsave(p, file="Stunting_VIM.png", width=6, height=5.6)
  

















rm(list=ls())
library(tidyverse)
library(metafor)

load("C:/Users/andre/Downloads/sprint_7D_longbow-master/sprint_7D_longbow-master/adjusted_binary/adjusted_binary_results.rdata")
#load("C:/Users/andre/Downloads/sprint_7D_longbow-master/sprint_7D_longbow-master/unadjusted_binary/unadjusted_binary_results.rdata")

d <- results
unique(d$intervention_variable)

#Grab mean WHZ RF dataset
whz <- d %>% filter(intervention_variable=="lag_WHZ_quart")


head(d)

#Subset to relative risks
d <- d %>% filter(type=="PAR")


#Subset to primary outcomes
table(d$agecat)
table(is.na(d$agecat))


# Drop at-birth prevalence of stunting
d <- d %>% filter(agecat!="Birth")


#Drop enrolled stunted as a RF for stunting
d <- d %>% filter(intervention_variable!="enstunt")



head(d)


#Pooled estimate function
poolPAR <- function(d){
  #nstudies=length(unique(d$studyid))
  nstudies <- d %>% summarize(N=n())
    
    fit<-NULL
    try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="REML", measure="GEN"))
    if(is.null(fit)){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="GEN"))}
    
    est<-data.frame(d$intervention_level[1], fit$b, fit$se)
    colnames(est)<-c("intervention_level", "logRR.psi", "logSE")
    
    est$RR<-exp(est$logRR)
    est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
    est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
    
    est$Nstudies <- nstudies$N
  
  return(est)
}

RMAest <- d %>% group_by(intervention_variable, agecat) %>% 
  do(try(poolPAR(.))) %>% as.data.frame()




#Make sure Nstudies is constant across RF levels
RMAest <- RMAest %>% group_by(agecat, intervention_variable) %>%
  mutate(Nstudies=paste0("N studies: ",max(Nstudies)),
         Nstudies=ifelse(intervention_level==first(intervention_level),Nstudies,"")) %>% ungroup()

head(RMAest)




#Order factors for plotting
table(RMAest$agecat)

RMAest <- droplevels(RMAest)
RMAest$agecat <- as.character(RMAest$agecat)

RMAest$agecat[grepl("-",RMAest$agecat)] <- paste0(RMAest$agecat[grepl("-",RMAest$agecat)],"\ncumulative incidence")
RMAest$agecat[!grepl("-",RMAest$agecat)] <- paste0(RMAest$agecat[!grepl("-",RMAest$agecat)]," prevalence")

#RMAest$agecat[grepl(" \\(no birth st\\.\\)\\\ncumulative incidence",RMAest$agecat)] <- "cumulative incidence\n(no birth stunting)"


#RMAest$agecat <- factor(RMAest$agecat, levels = c("0-6 month\ncumulative incidence","6 month prevalence", "6-24 month\ncumulative incidence", "24 month prevalence"))
RMAest$agecat <- factor(RMAest$agecat, levels=c("3 months prevalence", "3-6 months\ncumulative incidence", "0-6 months (no birth st.)\ncumulative incidence","0-6 months\ncumulative incidence",
                                                "6 months prevalence","6-9 months\ncumulative incidence","9 months prevalence","9-12 months\ncumulative incidence","12 months prevalence",
                                                "12-15 months\ncumulative incidence","15 months prevalence","15-18 months\ncumulative incidence","18 months prevalence",
                                                "0-24 months (no birth st.)","6-24 months\ncumulative incidence","0-24 months (no birth st.)\ncumulative incidence","0-24 months\ncumulative incidence","24 months prevalence"))


#Fix WHZ quartile RF levels
RMAest$RFlabel[RMAest$RFlabel=="1" & RMAest$intervention_variable=="lag_WHZ_quart"] <- "WHZ Q1"
RMAest$RFlabel[RMAest$RFlabel=="2" & RMAest$intervention_variable=="lag_WHZ_quart"] <- "WHZ Q2"
RMAest$RFlabel[RMAest$RFlabel=="3" & RMAest$intervention_variable=="lag_WHZ_quart"] <- "WHZ Q3"
RMAest$RFlabel[RMAest$RFlabel=="4" & RMAest$intervention_variable=="lag_WHZ_quart"] <- "WHZ Q4"

#Change binary variables into yes/no
binvars <- c("hdlvry","vagbrth", "enwast","anywast06","pers_wast", "earlybf","predexfd6",
             "predfeed3","predfeed36","predfeed6","exclfeed3","exclfeed36","exclfeed6",
             "perdiar6","perdiar24","impsan","safeh20","trth2o","impfloor","cleanck")
RMAest$intervention_level[RMAest$intervention_level=="0" & RMAest$intervention_variable %in% binvars] <- "No"
RMAest$intervention_level[RMAest$intervention_level=="1" & RMAest$intervention_variable %in% binvars] <- "Yes"


unique(RMAest$intervention_level)
RMAest$intervention_level <- factor(RMAest$intervention_level, 
                                    levels=c("0","1", "No", "Yes",
                                             "<48 cm" , "[48-50) cm",  ">=50 cm",                                  
                                             "Low birth weight","Normal or high birthweight", 
                                             "2","3","4","5","6","7","8","9",  "10" , "11","12" ,
                                             "<32" , "[32-38)", ">=38",
                                             "Low", "Medium", "High",                    
                                             "<162 cm", "[162-167) cm" , ">=167 cm",
                                             "Preterm", "Early term", "Full or late term",           
                                             "Food Insecure", "Mildly Food Insecure", "Food Secure",               
                                             "Wealth Q1", "Wealth Q2", "Wealth Q3", "Wealth Q4",
                                             "<25","[25-30)",">=30",                      
                                             "Underweight", "Normal weight", "Overweight or Obese",
                                             "<151 cm", "[151-155) cm", ">=155 cm",
                                             "<52 kg", "[52-58) kg", ">=58 kg",
                                             "2+","3 or less","4-5","6-7","8+","3+","4+",                                                 
                                             "0%","(0%, 5%]",">5%","Female","Male",
                                             "WHZ Q1", "WHZ Q2", "WHZ Q3", "WHZ Q4"))



unique(RMAest$intervention_variable)
RMAest$intervention_variable <- factor(RMAest$intervention_variable,
                                       levels=c("sex","birthlen","birthwt", "gagebrth",
                                                "hdlvry","vagbrth",
                                                "enwast","anywast06","pers_wast",
                                                "earlybf","predexfd6",
                                                "predfeed3","predfeed36","predfeed6",
                                                "exclfeed3","exclfeed36","exclfeed6",
                                                "perdiar6","perdiar24",
                                                "mage","fage","mhtcm","fhtcm",
                                                "mwtkg","mbmi","single",
                                                "meducyrs","feducyrs",
                                                "parity",
                                                "nchldlt5","nhh","nrooms",
                                                "hhwealth_quart","hfoodsec",
                                                "impsan","safeh20","trth2o",
                                                "impfloor","cleanck",
                                                "brthmon" ,"month",
                                                "lag_WHZ_quart"))   


#Add variable labels
unique(RMAest$intervention_variable)

RMAest$RFlabel <- NA
RMAest$RFlabel[RMAest$intervention_variable=="sex"] <-  "Gender"
RMAest$RFlabel[RMAest$intervention_variable=="enwast"] <-  "Enrolled wasted"
RMAest$RFlabel[RMAest$intervention_variable=="gagebrth"] <-  "Gestational age at birth"
RMAest$RFlabel[RMAest$intervention_variable=="predexfd6"] <-  "Exclusive or Predominant breastfeeding under 6 months"
RMAest$RFlabel[RMAest$intervention_variable=="mage"] <- "Mother's age" 
RMAest$RFlabel[RMAest$intervention_variable=="mhtcm"] <- "Mother's height" 
RMAest$RFlabel[RMAest$intervention_variable=="mwtkg"] <- "Mother's weight" 
RMAest$RFlabel[RMAest$intervention_variable=="mbmi"] <- "Mother's BMI" 
RMAest$RFlabel[RMAest$intervention_variable=="meducyrs"] <- "Mother's education" 
RMAest$RFlabel[RMAest$intervention_variable=="parity"] <-  "Birth order" 
RMAest$RFlabel[RMAest$intervention_variable=="hfoodsec"] <- "Household food security" 
RMAest$RFlabel[RMAest$intervention_variable=="nchldlt5"] <-   "Number of children <5 in household"
RMAest$RFlabel[RMAest$intervention_variable=="hhwealth_quart"] <-  "Household wealth" 
RMAest$RFlabel[RMAest$intervention_variable=="fage"] <- "Father's age" 
RMAest$RFlabel[RMAest$intervention_variable=="fhtcm"] <- "Father's height" 
RMAest$RFlabel[RMAest$intervention_variable=="birthwt"] <- "Birthweight (kg)" 
RMAest$RFlabel[RMAest$intervention_variable=="birthlen"] <- "Birth length (cm)" 
RMAest$RFlabel[RMAest$intervention_variable=="vagbrth"] <- "Vaginal birth" 
RMAest$RFlabel[RMAest$intervention_variable=="hdlvry"] <- "Child delivered at home" 
RMAest$RFlabel[RMAest$intervention_variable=="single"] <- "Single parent" 
RMAest$RFlabel[RMAest$intervention_variable=="nrooms"] <- "Number of rooms in household" 
RMAest$RFlabel[RMAest$intervention_variable=="nhh"] <- "Number of people in household" 
RMAest$RFlabel[RMAest$intervention_variable=="meducyrs"] <- "Maternal education quartile" 
RMAest$RFlabel[RMAest$intervention_variable=="feducyrs"] <- "Paternal education quartile" 
RMAest$RFlabel[RMAest$intervention_variable=="anywast06"] <- "Any wasting before 6 months age" 
RMAest$RFlabel[RMAest$intervention_variable=="pers_wast"] <- "Persistent wasting before 6 months age" 
RMAest$RFlabel[RMAest$intervention_variable=="trth2o"] <- "Treats drinking water" 
RMAest$RFlabel[RMAest$intervention_variable=="cleanck"] <- "Clean cooking fuel usage" 
RMAest$RFlabel[RMAest$intervention_variable=="impfloor"] <- "Improved floor" 
RMAest$RFlabel[RMAest$intervention_variable=="impsan"] <- "Improved sanitation" 
RMAest$RFlabel[RMAest$intervention_variable=="safeh20"] <- "Safe water source" 
RMAest$RFlabel[RMAest$intervention_variable=="perdiar6"] <- "Quartile of diarrhea longitudinal\nprevalence under 6 months" 
RMAest$RFlabel[RMAest$intervention_variable=="perdiar24"] <- "Quartile of diarrhea longitudinal\nprevalence under 24 months" 
RMAest$RFlabel[RMAest$intervention_variable=="earlybf"] <- "Breastfeed within an hour of birth" 
RMAest$RFlabel[RMAest$intervention_variable=="predfeed3"] <-  "Predominant breastfeeding under 3 months"
RMAest$RFlabel[RMAest$intervention_variable=="predfeed36"] <-  "Predominant breastfeeding from 3-6 months"
RMAest$RFlabel[RMAest$intervention_variable=="predfeed6"] <-  "Predominant breastfeeding under 6 months"
RMAest$RFlabel[RMAest$intervention_variable=="exclfeed3"] <-  "Exclusive breastfeeding under 3 months"
RMAest$RFlabel[RMAest$intervention_variable=="exclfeed36"] <-  "Exclusive breastfeeding from 3-6 months"
RMAest$RFlabel[RMAest$intervention_variable=="exclfeed6"] <-  "Exclusive breastfeeding under 6 months"
RMAest$RFlabel[RMAest$intervention_variable=="month"] <-  "Month of measurement"
RMAest$RFlabel[RMAest$intervention_variable=="brthmon"] <-  "Birth month"
RMAest$RFlabel[RMAest$intervention_variable=="lag_WHZ_quart"] <-  "Mean WHZ in the prior 3 months"




#Print plots across all intervention arms
yticks <- c(0.125,0.25,0.5,1,2,4,8,16)
scaleFUN <- function(x) sprintf("%.2f", x)

label_wrap <- function(variable, value) {
  lapply(strwrap(as.character(value), width=20, simplify=FALSE), 
         paste, collapse="\n")
}  


#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")




setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/PAR/")
pdf("Risk Factor Plots PAR.pdf", height=8, width=12)

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
    geom_text(aes(x=0.75, y=max(plotdf$RR.CI2)+0.05, label=Nstudies), size=3,  hjust=0) +
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
  ggsave(p, file=paste0("",j,"_",i,"_RFplot.png"), height=8, width=12)
  ggsave(p, file=paste0("Powerpoint size/",j,"_",i,"_RFplot.png"), width=5.7, height=4.6)
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



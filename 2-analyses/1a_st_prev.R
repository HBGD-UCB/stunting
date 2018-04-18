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
# library(tidyverse)
# library(caret)
# library(MASS)
# library(reshape2)
# library(zoo)
# library(epitools)
library(binom)
theme_set(theme_bw())


# source("Wast_incidence_functions.R") #need to write
setwd("U:/data/GHAP_data/")

# # pooling across studies
# x=data.frame(a=c(1,1),b=c(1,1))
# y=data.frame(a=c(1,1),b=c(1,1))
# 
# x=do.call(rbind(x,y))

# NO MLEX CHECK WITH ANDREW

study.list=c("akup","bfzn","cmc","cmin","cort","cntt",
             "dvds","ee","eu","gmsn","gbsc","irc","jvt3",
             "jvt4","knba","lcn5","mled","nrbt",
             "ncry","prbt","rspk","cmpf","fspp","tzc2",
             "vb12","vita","wsb","wsk","zvit","zmrt",
             "lnsz","ilnd","ildm")

# subset to control arm, but not all studies have arm

# import and prep data function
data.prep=function(dataname){
  # import data
  data<-readRDS(paste0(dataname,".rds"))

  # check if intervention arm column present
  # keep relevant variables
  if("ARM" %in% colnames(data)){
    data=select(data,c("SUBJID","STUDYID","ARM","SEX","AGEDAYS","HAZ"))
  }
  
  data=select(data,c("SUBJID","STUDYID","SEX","AGEDAYS","HAZ"))
  colnames(data)=tolower(colnames(data))
  
  print(paste(dataname)) 
  return(data)
}

data.set=lapply(study.list,data.prep)
all.data=do.call(rbind,data.set)
nrow(all.data)

#----------------------------------------------
# Mean and 95% CI function 
# IS THIS THE RIGHT FUNCTION?
#----------------------------------------------
mean95CI <- function(Y, id=rep(1:length(Y)), 
             persontime=NULL, proportion=F, percent=F, count=F){
  
  if(proportion==F){
    if(count==T){
      IR.CI <- pois.exact(Y, pt = persontime, conf.level = 0.95)[3:5] 
      mean_ci <- data.frame(N=Y, Mean=IR.CI[1], SD=NA, Robust.SE=NA ,  Lower.95.CI=IR.CI[2] ,  Upper.95.CI=IR.CI[3] )
      colnames(mean_ci) <- c("N","Mean","SD","Robust SE", "Lower 95%CI", "Upper 95%CI") 
    }else{
      if(!is.na(mean(Y[complete.cases(Y)]))){
        mudat <- data.frame(id = id, Y = Y)
        mudat <- mudat[complete.cases(mudat), ]
        n.sub <- dim(mudat)[1]
        fit <- glm(Y ~ 1, family = gaussian, data = mudat)
        vcovCL <- sandwichSE(mudat, fm = fit, cluster = mudat$id)
        rfit <- coeftest(fit, vcovCL)
        lb <- rfit[1, 1] - 1.96 * rfit[1, 2]
        ub <- rfit[1, 1] + 1.96 * rfit[1, 2]
        mean_ci <- matrix(c(n.sub, rfit[1, 1], sd(mudat$Y), rfit[1, 
                                                                 2], lb, ub), nrow = 1, ncol = 6)
        colnames(mean_ci) <- c("N", "Mean", "SD", "Robust SE", "Lower 95%CI", 
                               "Upper 95%CI")
        
      }else{
        mean_ci <- data.frame(N=NA, Mean=NA, SD=NA, `Robust SE`=NA, `Lower 95%CI`=NA, `Upper 95%CI`=NA)
        colnames(mean_ci) <- c("N", "Mean", "SD", "Robust SE", "Lower 95%CI", "Upper 95%CI")  
      }
    }
  }else{
    
    require(binom)
    # Find the number of obs
    n = length(Y[!is.na(Y)])
    if(percent==T){
      CR.res<-binom.confint(sum(Y/100, na.rm = T), n, method="exact")
    }else{
      CR.res<-binom.confint(sum(Y, na.rm = T), n, method="exact")
    }
    mean_ci <- data.frame(N=n, Mean=CR.res[4], SD=NA, `Robust SE`=NA, `Lower 95%CI`=CR.res[5], `Upper 95%CI`=CR.res[6])
    colnames(mean_ci) <- c("N", "Mean", "SD", "Robust SE", "Lower 95%CI", "Upper 95%CI")  
  }
  return(mean_ci)
}

#Function to calculate the robust SEs
sandwichSE <- function (dat, fm, cluster) 
{
  require(sandwich, quietly = TRUE)
  require(lmtest, quietly = TRUE)
  if (is.factor(cluster)) {
    cluster <- droplevels(cluster)
  }
  M <- length(unique(cluster))
  N <- length(cluster)
  K <- fm$rank
  dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
  uj <- apply(estfun(fm), 2, function(x) tapply(x, cluster, 
                                                sum))
  vcovCL <- dfc * sandwich(fm, meat = crossprod(uj)/N)
  return(vcovCL)
}

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
  

# calculate prevalence by age group
sprev.data = all.data %>%
  filter(!is.na(agecat)) %>%
  group_by(agecat) %>%
  summarise(nprev=sum(!is.na(stunted)),
            prev=mean(stunted),
            lb=mean95CI(stunted,proportion=T)[["Lower 95%CI"]],
            ub=mean95CI(stunted,proportion=T)[["Upper 95%CI"]],
            measure="Stunting") 

# calculate prevalence by age group
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

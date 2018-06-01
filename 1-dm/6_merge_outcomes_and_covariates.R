
rm(list=ls())
library(tidyverse)

#merge outcomes with covariates

# setwd("U:/UCB-SuperLearner/Stunting rallies/")
setwd("U:/ucb-superlearner/Stunting rallies/")

#load covariates
cov<-readRDS("FINAL_temp_clean_covariates.rds")

#load outcomes
load("st_prev.rdata")
load("st_cuminc.rdata")
load("st_rec.rdata")


dim(prev)
dim(cuminc)
dim(rev)


colnames(prev)
colnames(cuminc)
colnames(rev)


head(prev)
head(cuminc)
head(rev)

#convert subjid to character for the merge with covariate dataset
cov$subjid <- as.character(cov$subjid)
prev$subjid <- as.character(prev$subjid)
cuminc$subjid <- as.character(cuminc$subjid)
rev$subjid <- as.character(rev$subjid)


#------------------------------------
# Create cumulative incidence dataset
#------------------------------------

#merge in covariates
d <- left_join(cuminc, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("ever_stunted")

#Vector of risk factor names
A<-c( "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec")
#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_cuminc_rf.Rdata")


#------------------------------------
# Create prevalence dataset
#------------------------------------


#merge in covariates
d <- left_join(prev, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("stunted","sstunted")



save(d, Y, A,V, id, file="st_prev_rf.Rdata")


#------------------------------------
# Create recovery dataset
#------------------------------------

#merge in covariates
d <- left_join(rev, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("s03rec24")

#Vector of risk factor names
A<-c( "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec")
#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_rec_rf.Rdata")


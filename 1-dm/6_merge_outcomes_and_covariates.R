
rm(list=ls())
library(tidyverse)

#merge outcomes with covariates

setwd("U:/UCB-SuperLearner/Stunting rallies/")

#load covariates
cov<-readRDS("FINAL_temp_clean_covariates.rds")

#load outcomes
load("st_prev.rdata")
load("st_cuminc.rdata")


dim(prev)
dim(cuminc)


colnames(prev)
colnames(cuminc)


head(prev)
head(cuminc)


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
      "hhwealth_quart")

#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")


save(d, Y, A,V, file="st_cuminc_rf.Rdata")


#------------------------------------
# Create prevalence dataset
#------------------------------------


#merge in covariates
d <- left_join(prev, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("stunted","sstunted")



save(d, Y, A,V, file="st_prev_rf.Rdata")

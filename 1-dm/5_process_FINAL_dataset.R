

#Add instructions for downloadin FINAL dataset


rm(list=ls())
library(tidyverse)
library(data.table)

source("U:/GHAP-Data-Management/HBGDki_functions.R")


setwd("U:/data")
gc()


#Read rds file and drop unneeded columns
d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T,
         drop = c("AGEDAYS", "AGEIMPFL","MONTH",   "WTKG",    "HTCM",    "LENCM",   "WAZ",     "HAZ",  
                  "WHZ",     "BAZ",     "HCAZ",    "MUAZ",    "SITEID", 
                  "REGCTRY", "REGCTYP", "CITYTOWN","LATITUDE","LONGITUD", "HHID",    "ARM", 
                  "DEAD",    "AGEDTH",  "CAUSEDTH","FEEDING",
                  "DURBRST", "BRTHYR",  "BRTHWEEK","BRTHMON",
                  "PARITY",  "BRTHORDR",
                  "BRFEED", 
                  "SUMEP",   "SUMDIAR", "SUMDAYS",
                  "PCTDIAR", "IMPSAN",  "SOAP",    "SAFEH2O", "TRTH2O",  "CLEANCK",
                  "IMPFLOOR","H2OTIME",
                  "CHICKEN", "COW",     "CATTLE",  "SES",     "INCTOT", 
                  "INCTOTU", "BFEDFL",  "EXBFEDFL","WEANFL",  "ANMLKFL", "PWMLKFL",
                  "FORMLKFL","BOTTLEFL","H20FEDFL","OTHFEDFL","SLDFEDFL","NBFYES",  "EARLYBF", "CMFDINT", "DIARFL",  "LSSTLFL",
                  "NUMLS",   "BLDSTLFL","DIARFL_R","LSSTFL_R","NUMLS_R", "BLDSTL_R",
                  "DUR_R",   "HFOODSEC"))
colnames(d) <- tolower(colnames(d))
gc()

#Drop studies Vishak added to data product that don't meet inclusion criteria
#d <- d %>% filter(studyid!="ki1000301-DIVIDS" & studyid!="ki1055867-WomenFirst" & studyid!="ki1135782-INCAP")
d <- d[studyid!="ki1000301-DIVIDS" & studyid!="ki1055867-WomenFirst" & studyid!="ki1135782-INCAP"]

#mark measure frequencies
d$measurefreq <- NA

d$measurefreq[d$studyid %in% c(
  "ki0047075b-MAL-ED",   
  "ki1000108-CMC-V-BCS-2002",              
  "ki1000108-IRC",               
  "ki1000109-EE",           
  "ki1000109-ResPak",  
  "ki1017093b-PROVIDE",  
  "ki1066203-TanzaniaChild2",           
  "ki1101329-Keneba",  
  "ki1112895-Guatemala BSC",       
  "ki1113344-GMS-Nepal",             
  "ki1114097-CONTENT"
)] <- "monthly"

d$measurefreq[d$studyid %in% c(
  "ki1112895-iLiNS-Zinc",  
  "kiGH5241-JiVitA-3",          
  "kiGH5241-JiVitA-4", 
  "ki1148112-LCNI-5",          
  "ki1017093-NIH-Birth",
  "ki1017093c-NIH-Crypto",   
  "ki1119695-PROBIT",         
  "ki1000304b-SAS-CompFeed",   
  "ki1000304b-SAS-FoodSuppl",   
  "ki1126311-ZVITAMBO",   
  "ki1114097-CMIN",                 
  "ki1135781-COHORTS"
)] <- "quarterly"

d$measurefreq[d$studyid %in% c(
  "ki1000110-WASH-Bangladesh",       
  "ki1000111-WASH-Kenya",  
  "ki1148112-iLiNS-DOSE",     
  "ki1148112-iLiNS-DYAD-M", 
  "ki1033518-iLiNS-DYAD-G",
  "ki1000125-AgaKhanUniv",           
  "ki1112895-Burkina Faso Zn",    
  "ki1000304-VITAMIN-A",  
  "ki1000304-Vitamin-B12",
  "ki1000107-Serrinha-VitA",   
  "ki1000304-EU",        
  "ki1000304-ZnMort"
)] <- "yearly"


#Keep monthly and quarterly studies
d <- d %>% filter(measurefreq!="yearly")

#Subset to control arms for intervention studies
d <- filter(d, tr=="Control" | tr=="")


#Drop unneeded or unfinished variables
# d <- subset(d, select = -c("agedays", "ageimpfl","month",   "wtkg",    "htcm",    "lencm",   "waz",     "haz",  
#                            "whz",     "baz",     "hcaz",    "muaz",    "siteid", 
#                            "regctry", "regctyp", "citytown","latitude","longitud","clustid", "hhid",    "arm", 
#                            "tr", 
#                            "dead",    "agedth",  "causedth","feeding",
#                            "durbrst", "brthyr",  "brthweek","brthmon",
#                            "parity",  "brthordr",
#                            "brfeed", 
#                            "sumep",   "sumdiar", "sumdays",
#                            "pctdiar", "impsan",  "soap",    "safeh2o", "trth2o",  "cleanck",
#                            "impfloor","h2otime",
#                            "chicken", "cow",     "cattle",  "ses",     "inctot", 
#                            "inctotu", "bfedfl",  "exbfedfl","weanfl",  "anmlkfl", "pwmlkfl",
#                            "formlkfl","bottlefl","h20fedfl","othfedfl","sldfedfl","nbfyes",  "earlybf", "cmfdint", "diarfl",  "lsstlfl",
#                            "numls",   "bldstlfl","diarfl_r","lsstfl_r","numls_r", "bldstl_r",
#                            "dur_r",   "hfoodsec"))

#Keep one observation per child
d <- d %>% group_by(studyid, country, subjid) %>% slice(1) 


#Replace "" with NA for categorical variables




#Convert continious variables to quartiled categorical 
#drop risk factors without enough studies or unneeded variables
d <- subset(d, select = -c(fwtkg, fbmi, measurefreq, region, tr))


#quantiling functions
quantile_rf <- function(A, labs=NULL){
  A<-as.numeric(A)
  Acuts=c(0, as.numeric(quantile(A, probs = c(.25,.5,.75), na.rm=T)), max(A, na.rm=T))
  Alevels=c(paste0("<=",round(Acuts[2],3)), 
            paste0(round(Acuts[2],3),"-",round(Acuts[3],3)),
            paste0(round(Acuts[3],3),"-",round(Acuts[4],3)), 
            paste0(">",round(Acuts[4],3))) 
  if(!is.null(labs)){
    Alevels=labs
  }

    A <- cut(A,breaks=Acuts,labels=Alevels)
  
  return(A)
}

quantile_rf_bystudy <- function(df){
  
  df$meducyrs <- quantile_rf(df$meducyrs, labs=c("Q1","Q2","Q3","Q4"))
  df$feducyrs <- quantile_rf(df$feducyrs, labs=c("Q1","Q2","Q3","Q4"))
  
  return(df)
}

#Overall quantile
d$gagebrth <- quantile_rf(d$gagebrth)
d$birthwt <- quantile_rf(d$birthwt)
d$birthlen <- quantile_rf(d$birthlen)
d$mage <- quantile_rf(d$mage)
d$mhtcm <- quantile_rf(d$mhtcm)
d$mwtkg <- quantile_rf(d$mwtkg)
d$mbmi <- quantile_rf(d$mbmi)
d$fage <- quantile_rf(d$fage)
d$fhtcm <- quantile_rf(d$fhtcm)



#quantile by study
# d <- d %>% group_by(studyid, country) %>%
#   do(quantile_rf_bystudy(.))

#TEMP drop education until can figure out how to quartile within study without error in studies with sparse levels
d <- d %>% subset(., select= -c(meducyrs, feducyrs))


#Categorize nrooms, nhh, nchild5
nroom<-NA
nroom[d$nrooms<2] <- "1"
nroom[d$nrooms==2] <- "2"
nroom[d$nrooms==3] <- "3"
nroom[d$nrooms>3] <- "4+"
d$nrooms <- as.factor(nroom)
table(d$nrooms)


table(d$nhh)  
nhh<-NA
nhh[d$nhh<4] <- "3 or less"
nhh[d$nhh==4] <- "4"
nhh[d$nhh>4 & d$nhh<6] <- "5-6"
nhh[d$nhh>6] <- "6+"
d$nhh <- as.factor(nhh)
table(d$nhh)


table(d$nchldlt5)
nchild5<-NA
nchild5[d$nchldlt5==0] <- "0"
nchild5[d$nchldlt5==1] <- "1"
nchild5[d$nchldlt5>2] <- "2+"
nchldlt5 <- as.factor(nchild5)
table(d$nchldlt5)


#Create an ID variable
d$id <- NA
d$id[d$studyid %in% c("ki1112895-iLiNS-Zinc",
                      "kiGH5241-JiVitA-3",    
                      "kiGH5241-JiVitA-4",
                      "ki1119695-PROBIT",
                      "ki1000304b-SAS-CompFeed")] <-d$clustid[d$studyid %in% c("ki1112895-iLiNS-Zinc",
                                                                               "kiGH5241-JiVitA-3",    
                                                                               "kiGH5241-JiVitA-4",
                                                                               "ki1119695-PROBIT",
                                                                               "ki1000304b-SAS-CompFeed")]
d$id[!(d$studyid %in% c("ki1112895-iLiNS-Zinc",
                        "kiGH5241-JiVitA-3",    
                        "kiGH5241-JiVitA-4",
                        "ki1119695-PROBIT",
                        "ki1000304b-SAS-CompFeed"))] <-d$subjid[!(d$studyid %in% c("ki1112895-iLiNS-Zinc",
                                                                                   "kiGH5241-JiVitA-3",    
                                                                                   "kiGH5241-JiVitA-4",
                                                                                   "ki1119695-PROBIT",
                                                                                   "ki1000304b-SAS-CompFeed"))]
d$id[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"] <-d$clustid[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"]

table(is.na(d$id))

#Create BMI/height/weight variables for those studies that have 2 of 3 variables


#Merge in HHwealth
load("U:/results/allGHAPstudies-HHwealth.Rdata")
colnames(pca) <- tolower(colnames(pca))
d$subjid<-as.numeric(d$subjid)
d<-left_join(d, pca, by=c("studyid", "country" ,"subjid"))


#Harmonize parity/birthorder




#save dataset
saveRDS(d, file="FINAL_temp_clean_covariates.rds")

saveRDS(d, file="U:/UCB-SuperLearner/Stunting rallies/FINAL_temp_clean_covariates.rds")





rm(list=ls())
library(tidyverse)
library(reshape2)

#merge outcomes with covariates

# setwd("U:/UCB-SuperLearner/Stunting rallies/")
setwd("U:/ucb-superlearner/Stunting rallies/")

#load covariates
cov<-readRDS("FINAL_clean_covariates.rds")

#load outcomes
load("st_prev_rf_outcomes.rdata")
load("st_cuminc_rf_outcomes.rdata")
load("st_rec_rf_outcomes.rdata")
load("st_vel_rf_outcomes.rdata")


dim(prev)
dim(cuminc)
dim(rev)
dim(vel_haz)


colnames(prev)
colnames(cuminc)
colnames(rev)
colnames(vel_haz)


head(prev)
head(cuminc)
head(rev)
head(vel_haz)

#convert subjid to character for the merge with covariate dataset
cov$subjid <- as.character(cov$subjid)
prev$subjid <- as.character(prev$subjid)
cuminc$subjid <- as.character(cuminc$subjid)
rev$subjid <- as.character(rev$subjid)
vel_haz$subjid <- as.character(vel_haz$subjid)
vel_lencm$subjid <- as.character(vel_lencm$subjid)


#------------------------------------
# Create cumulative incidence dataset
#------------------------------------

#merge in covariates
cuminc <- cuminc %>% subset(., select = -c(tr))
d <- left_join(cuminc, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("ever_stunted")

#Vector of risk factor names
A<-c( "sex",              "gagebrth",      "birthwt",      
      "birthlen",      "enstunt",       "vagbrth",       "hdlvry",        "mage",          "mhtcm",         "mwtkg",        
      "mbmi",          "single",        "fage",          "fhtcm",         "nrooms",        "nhh",           "nchldlt5",     
      "hhwealth_quart", "month", "brthmon", "parity",   "meducyrs", 
      "feducyrs", "hfoodsec",  
      "enwast", "anywast06", "pers_wast", 
      "trth2o", "cleanck", "impfloor",  "impsan", "safeh20",
      "perdiar6", "perdiar24", "predexfd6", "earlybf")  

#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id,  file="st_cuminc_rf.Rdata")


#------------------------------------
# Create prevalence dataset
#------------------------------------


#merge in covariates
d <- left_join(prev, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("stunted","sstunted")



save(d, Y, A,V, id,  file="st_prev_rf.Rdata")


#------------------------------------
# Create recovery dataset
#------------------------------------

#merge in covariates
d <- left_join(rev, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("s03rec24")

#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_rec_rf.Rdata")




#------------------------------------
# Create growth velocity dataset
#------------------------------------

#HAZ

#merge in covariates
d <- left_join(vel_haz, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("y_rate")


#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_haz_vel_rf.Rdata")


# Height in cm

#merge in covariates
d <- left_join(vel_lencm, cov, by=c("studyid", "subjid", "country"))
head(d)


#Vector of outcome names
Y<-c("y_rate")


#Vector of covariate names
W<-c("")

#Subgroup variable
V <- c("agecat")

#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_len_vel_rf.Rdata")


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Create intervention effects datasets
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


load("st_int_prev.RData")
load("st_int_cuminc.RData")


#-------------------------------------
# Prevalence
#-------------------------------------


#merge in covariates
d <- left_join(int_prev, cov, by=c("studyid", "subjid", "country"))
head(d)

d <- droplevels(d)


#Vector of outcome names
# The age period over which cumulative incidence is calculated will depend on the timing of the delivery of the intervention. 
# Zinc, LNS, and other interventions:  6-12 months, 12-18 months, 18-24 months, and intervention delivery to 24 months.
# Pre-birth maternal interventions: Stunting incidence at birth, and post-birth stunting from 0-6 months in age.

Y<-c("stunted")

#Vector of risk factor names
A<-c("tr")

#Vector of covariate names
W<-c("")

#Subgroup variable
#Objective 3: infant sex, stunting status at enrollment, wasting status at enrollment, gestational age, 
#geographic region (Asia, Africa, Latin America), exclusive or predominant breastfeeding in the first 6 months, 
#maternal age, maternal education, maternal height, or maternal BMI, birth order, household food insecurity, 
#number children under 5 years of age in the household

#Create interactions to have single stratification variables
d$intXsex <- interaction( d$sex, d$agecat, drop = T, sep = "_")
d$intXenstunt <- interaction( d$enstunt, d$agecat, drop = T, sep = "_")
d$intXenwast <- interaction( d$enwast, d$agecat, drop = T, sep = "_")
d$intXgagebrth <- interaction( d$gagebrth, d$agecat, drop = T, sep = "_")
d$intXpredexfd6 <- interaction( d$predexfd6, d$agecat, drop = T, sep = "_")
d$intXmage <- interaction( d$mage, d$agecat, drop = T, sep = "_")
d$intXmhtcm <- interaction( d$mhtcm, d$agecat, drop = T, sep = "_")
d$intXmbmi <- interaction( d$mbmi, d$agecat, drop = T, sep = "_")
d$intXmeducyrs <- interaction( d$meducyrs, d$agecat, drop = T, sep = "_")
d$intXparity <- interaction( d$parity, d$agecat, drop = T, sep = "_")
d$intXhfoodsec <- interaction( d$hfoodsec, d$agecat, drop = T, sep = "_")
d$intXnchldlt5 <- interaction( d$nchldlt5, d$agecat, drop = T, sep = "_")
d$intXhhwealth_quart <- interaction( d$hhwealth_quart, d$agecat, drop = T, sep = "_")

V <- c("agecat", 
       "intXsex", 
       "intXenstunt", 
       "intXenwast", 
       "intXgagebrth", 
       "intXpredexfd6", 
       "intXmage", 
       "intXmhtcm", 
       "intXmbmi", 
       "intXmeducyrs", 
       "intXparity", 
       "intXhfoodsec", 
       "intXnchldlt5", 
       "intXhhwealth_quart")


#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_prev_int.Rdata")



#-------------------------------------
# Cumulative incidence
#-------------------------------------


int_cuminc <- int_cuminc %>% subset(., select = -c(tr))

#merge in covariates
d <- left_join(int_cuminc, cov, by=c("studyid", "subjid", "country"))
head(d)

d <- droplevels(d)


#Vector of outcome names
# The age period over which cumulative incidence is calculated will depend on the timing of the delivery of the intervention. 
# Zinc, LNS, and other interventions:  6-12 months, 12-18 months, 18-24 months, and intervention delivery to 24 months.
# Pre-birth maternal interventions: Stunting incidence at birth, and post-birth stunting from 0-6 months in age.

Y<-c("ever_stunted")

#Vector of risk factor names
A<-c("tr")

#Vector of covariate names
W<-c("")

#Subgroup variable
#Objective 3: infant sex, stunting status at enrollment, wasting status at enrollment, gestational age, 
#geographic region (Asia, Africa, Latin America), exclusive or predominant breastfeeding in the first 6 months, 
#maternal age, maternal education, maternal height, or maternal BMI, birth order, household food insecurity, 
#number children under 5 years of age in the household

#Create interactions to have single stratification variables
d$intXsex <- interaction( d$sex, d$agecat, drop = T, sep = "_")
d$intXenstunt <- interaction( d$enstunt, d$agecat, drop = T, sep = "_")
d$intXenwast <- interaction( d$enwast, d$agecat, drop = T, sep = "_")
d$intXgagebrth <- interaction( d$gagebrth, d$agecat, drop = T, sep = "_")
d$intXpredexfd6 <- interaction( d$predexfd6, d$agecat, drop = T, sep = "_")
d$intXmage <- interaction( d$mage, d$agecat, drop = T, sep = "_")
d$intXmhtcm <- interaction( d$mhtcm, d$agecat, drop = T, sep = "_")
d$intXmbmi <- interaction( d$mbmi, d$agecat, drop = T, sep = "_")
d$intXmeducyrs <- interaction( d$meducyrs, d$agecat, drop = T, sep = "_")
d$intXparity <- interaction( d$parity, d$agecat, drop = T, sep = "_")
d$intXhfoodsec <- interaction( d$hfoodsec, d$agecat, drop = T, sep = "_")
d$intXnchldlt5 <- interaction( d$nchldlt5, d$agecat, drop = T, sep = "_")
d$intXhhwealth_quart <- interaction( d$hhwealth_quart, d$agecat, drop = T, sep = "_")

V <- c("agecat", 
       "intXsex", 
       "intXenstunt", 
       "intXenwast", 
       "intXgagebrth", 
       "intXpredexfd6", 
       "intXmage", 
       "intXmhtcm", 
       "intXmbmi", 
       "intXmeducyrs", 
       "intXparity", 
       "intXhfoodsec", 
       "intXnchldlt5", 
       "intXhhwealth_quart")


#clusterid ID variable
id <- c("id")


save(d, Y, A,V, id, file="st_cuminc_int.Rdata")






#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
# Create list of adjustment variables
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


adjustment_sets <- list( 
  sex=c( "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
         "mhtcm","mwtkg","mbmi", "fhtcm",
         "vagbrth","hdlvry",
         "gagebrth","birthwt","birthlen",
         "single",
         "nrooms","nhh","nchldlt5",
         "month","brthmon","parity",
         "trth2o","cleanck","impfloor","impsan","safeh20"),   
  
  gagebrth=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             "mhtcm","mwtkg","mbmi", "fhtcm",
             "vagbrth","hdlvry",
             "single",
             "nrooms","nhh","nchldlt5",
             "month","brthmon","parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"),         
  
  birthwt=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "mhtcm","mwtkg","mbmi", "fhtcm",
            "vagbrth","hdlvry",
            "single",
            "nrooms","nhh","nchldlt5",
            "month","brthmon","parity",
            "trth2o","cleanck","impfloor","impsan","safeh20"),   
  
  birthlen=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             "mhtcm","mwtkg","mbmi", "fhtcm",
             "vagbrth","hdlvry",
             "single",
             "nrooms","nhh","nchldlt5",
             "month","brthmon","parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"),   
  
  enstunt=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "mhtcm","mwtkg","mbmi", "fhtcm",
            "vagbrth","hdlvry",
            "single",
            "nrooms","nhh","nchldlt5",
            "month","brthmon","parity",
            "trth2o","cleanck","impfloor","impsan","safeh20"),      
  
  vagbrth=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "mhtcm","mwtkg","mbmi", "fhtcm",
            "hdlvry",
            "single",
            "nrooms","nhh","nchldlt5",
            "month","brthmon","parity",
            "trth2o","cleanck","impfloor","impsan","safeh20"),     
  
  hdlvry=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "mhtcm","mwtkg","mbmi", "fhtcm",
           "single",
           "nrooms","nhh","nchldlt5",
           "month","brthmon","parity",
           "trth2o","cleanck","impfloor","impsan","safeh20"),      
  
  mage=c("sex", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
         "mhtcm","mwtkg","mbmi", "fhtcm",
         "gagebrth","birthwt","birthlen",
         "single",
         "nrooms","nhh","nchldlt5",
         "month","brthmon",
         "trth2o","cleanck","impfloor","impsan","safeh20"),     
  
  mhtcm=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
          "fhtcm",
          "vagbrth","hdlvry",
          "gagebrth",
          "single",
          "nrooms","nhh","nchldlt5",
          "month","brthmon","parity",
          "trth2o","cleanck","impfloor","impsan","safeh20"),    
  
  mwtkg=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
          "fhtcm",
          "vagbrth","hdlvry",
          "single",
          "nrooms","nhh","nchldlt5",
          "month","brthmon","parity",
          "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  mbmi=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
         "fhtcm",
         "vagbrth","hdlvry",
         "single",
         "nrooms","nhh","nchldlt5",
         "month","brthmon","parity",
         "trth2o","cleanck","impfloor","impsan","safeh20"),      
  
  single=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "mhtcm","mwtkg","mbmi", "fhtcm",
           "vagbrth","hdlvry",
           "gagebrth","birthwt","birthlen",
           "nrooms","nhh","nchldlt5",
           "month","brthmon","parity",
           "trth2o","cleanck","impfloor","impsan","safeh20"),    
  
  fage=c("sex", "mage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
         "mhtcm","mwtkg","mbmi", "fhtcm",
         "vagbrth","hdlvry",
         "gagebrth",
         "single",
         "nrooms","nhh","nchldlt5",
         "month","brthmon",
         "trth2o","cleanck","impfloor","impsan","safeh20"),   
  
  fhtcm=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
          "mhtcm","mwtkg","mbmi",
          "vagbrth","hdlvry",
          "gagebrth","birthwt","birthlen",
          "single",
          "nrooms","nhh","nchldlt5",
          "month","brthmon","parity",
          "trth2o","cleanck","impfloor","impsan","safeh20"),     
  
  nrooms=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "mhtcm","mwtkg","mbmi", "fhtcm",
           "vagbrth","hdlvry",
           "gagebrth","birthwt","birthlen",
           "single",
           "nhh","nchldlt5",
           "month","brthmon","parity",
           "trth2o","cleanck","impfloor","impsan","safeh20", "earlybf"),    
  
  nhh=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
        "mhtcm","mwtkg","mbmi", "fhtcm",
        "vagbrth","hdlvry",
        "gagebrth","birthwt","birthlen",
        "single",
        "nrooms",
        "month","brthmon","parity",
        "trth2o","cleanck","impfloor","impsan","safeh20", "earlybf"),    
  
  nchldlt5=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             "mhtcm","mwtkg","mbmi", "fhtcm",
             "vagbrth","hdlvry",
             "gagebrth","birthwt","birthlen",
             "single",
             "nrooms",
             "month","brthmon","parity",
             "trth2o","cleanck","impfloor","impsan","safeh20", "earlybf"),
  
  hhwealth_quart=c("sex", "mage", "fage", "meducyrs", "feducyrs",  "hfoodsec",
                   "vagbrth","hdlvry",
                   "gagebrth","birthwt","birthlen",
                   "single",
                   "nrooms","nhh","nchldlt5",
                   "month","brthmon","parity",
                   "trth2o","cleanck","impfloor","impsan","safeh20"), 
  
  month=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
          "mhtcm","mwtkg","mbmi", "fhtcm",
          "vagbrth","hdlvry",
          "gagebrth","birthwt","birthlen",
          "single",
          "nrooms","nhh","nchldlt5",
          "brthmon","parity",
          "trth2o","cleanck","impfloor","impsan","safeh20",
          "enstunt","enwast","anywast06","pers_wast",
          "perdiar6","perdiar24","predexfd6", "earlybf"), 
  
  brthmon=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "mhtcm","mwtkg","mbmi", "fhtcm",
            "vagbrth","hdlvry",
            "single",
            "nrooms","nhh","nchldlt5",
            "month","parity",
            "trth2o","cleanck","impfloor","impsan","safeh20"), 
  
  parity=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "mhtcm","mwtkg","mbmi", "fhtcm",
           "vagbrth","hdlvry",
           "gagebrth","birthwt","birthlen",
           "single",
           "nrooms",
           "month","brthmon",
           "trth2o","cleanck","impfloor","impsan","safeh20"),   
  
  meducyrs=c("sex", "mage", "fage", "feducyrs", "hhwealth_quart", "hfoodsec",
             "mhtcm","mwtkg","mbmi", "fhtcm",
             "vagbrth","hdlvry",
             "gagebrth","birthwt","birthlen",
             "single",
             "nrooms","nhh","nchldlt5",
             "month","brthmon","parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  feducyrs=c("sex", "mage", "fage", "meducyrs",  "hhwealth_quart", "hfoodsec",
             "mhtcm","mwtkg","mbmi", "fhtcm",
             "vagbrth","hdlvry",
             "gagebrth","birthwt","birthlen",
             "single",
             "nrooms","nhh","nchldlt5",
             "month","brthmon","parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"), 
  
  hfoodsec=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart",
             "vagbrth","hdlvry",
             "single",
             "nrooms","nhh","nchldlt5",
             "month","brthmon","parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  enwast=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "mhtcm","mwtkg","mbmi", "fhtcm",
           "vagbrth","hdlvry",
           "single",
           "nrooms","nhh","nchldlt5",
           "month","brthmon","parity",
           "trth2o","cleanck","impfloor","impsan","safeh20"), 
  
  anywast06=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
              "mhtcm","mwtkg","mbmi", "fhtcm",
              "vagbrth","hdlvry",
              "single",
              "nrooms","nhh","nchldlt5",
              "month","brthmon","parity",
              "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  pers_wast=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
              "mhtcm","mwtkg","mbmi", "fhtcm",
              "vagbrth","hdlvry",
              "single",
              "nrooms","nhh","nchldlt5",
              "month","brthmon","parity",
              "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  trth2o=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "mhtcm","mwtkg","mbmi", "fhtcm",
           "vagbrth","hdlvry",
           "gagebrth","birthwt","birthlen",
           "single",
           "nrooms","nhh","nchldlt5",
           "month","brthmon","parity",
           "cleanck","impfloor","impsan","safeh20", "earlybf"), 
  
  cleanck=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "mhtcm","mwtkg","mbmi", "fhtcm",
            "vagbrth","hdlvry",
            "gagebrth","birthwt","birthlen",
            "single",
            "nrooms","nhh","nchldlt5",
            "month","brthmon","parity",
            "trth2o","impfloor","impsan","safeh20", "earlybf"), 
  
  impfloor=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             "mhtcm","mwtkg","mbmi", "fhtcm",
             "vagbrth","hdlvry",
             "gagebrth","birthwt","birthlen",
             "single",
             "nrooms","nhh","nchldlt5",
             "month","brthmon","parity",
             "trth2o","cleanck","impsan","safeh20", "earlybf"),  
  
  impsan=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
           "mhtcm","mwtkg","mbmi", "fhtcm",
           "vagbrth","hdlvry",
           "gagebrth","birthwt","birthlen",
           "single",
           "nrooms","nhh","nchldlt5",
           "month","brthmon","parity",
           "trth2o","cleanck","impfloor","safeh20", "earlybf"), 
  
  safeh20=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "mhtcm","mwtkg","mbmi", "fhtcm",
            "vagbrth","hdlvry",
            "gagebrth","birthwt","birthlen",
            "single",
            "nrooms","nhh","nchldlt5",
            "month","brthmon","parity",
            "trth2o","cleanck","impfloor","impsan","earlybf"),
  
  perdiar6=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
             "mhtcm","mwtkg","mbmi", "fhtcm",
             "vagbrth","hdlvry",
             "gagebrth","birthwt","birthlen",
             "single",
             "nrooms","nhh","nchldlt5",
             "month","brthmon","parity",
             "trth2o","cleanck","impfloor","impsan","safeh20"), 
  
  perdiar24=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
              "mhtcm","mwtkg","mbmi", "fhtcm",
              "vagbrth","hdlvry",
              "gagebrth","birthwt","birthlen",
              "single",
              "nrooms","nhh","nchldlt5",
              "month","brthmon","parity",
              "trth2o","cleanck","impfloor","impsan","safeh20"), 
  
  predexfd6=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
              "mhtcm","mwtkg","mbmi", "fhtcm",
              "vagbrth","hdlvry",
              "gagebrth","birthwt","birthlen",
              "single",
              "nrooms","nhh","nchldlt5",
              "month","brthmon","parity",
              "trth2o","cleanck","impfloor","impsan","safeh20"),
  
  earlybf=c("sex", "mage", "fage", "meducyrs", "feducyrs", "hhwealth_quart", "hfoodsec",
            "mhtcm","mwtkg","mbmi", "fhtcm",
            "vagbrth","hdlvry",
            "gagebrth","birthwt","birthlen",
            "single",
            "nrooms","nhh","nchldlt5",
            "month","brthmon","parity",
            "trth2o","cleanck","impfloor","impsan","safeh20")
)
save(adjustment_sets, file="adjustment_sets_list.Rdata")




# Instructions for downloading FINAL dataset

# Go to https://git.ghap.io/stash/projects/HBGD/repos/adhoc/browse
# click clone button
# Copy link (mine is https://andrew.mertens@git.ghap.io/stash/scm/hbgd/adhoc.git)
# Open Sourcetree (Click window icon in bottom left, then search magnifying glass icon
# in the top right, and search Sourcetree to find)
# Click clone button in source tree 
# Paste link in source path box
# Add U:/data/FINAL/ to the destination path (make sure FINAL folder is empty)
# Click clone



rm(list=ls())
library(tidyverse)
library(data.table)
# options(repos = c(CRAN = "http://cran.rstudio.com/",
#  deltarho = "http://packages.deltarho.org"))
# install.packages("growthstandards")
library(growthstandards)

source("U:/GHAP-Data-Management/HBGDki_functions.R")


setwd("U:/data")
gc()



#Read rds file and drop unneeded columns
#Andrew's
d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T,
         
         #Jade's
         #d<-fread("U:/data/Stunting/Full-compiled-data/FINAL.csv", header = T,
         drop = c( "AGEIMPFL",  "WTKG",    "HTCM",    "LENCM",       
                   "WHZ",     "BAZ",     "HCAZ",    "MUAZ",    
                   "REGCTRY", "REGCTYP", "CITYTOWN","LATITUDE","LONGITUD", "HHID",    "ARM", 
                   "DEAD",    "AGEDTH",  "CAUSEDTH","FEEDING",
                   "DURBRST", "BRTHYR",  
                   "ENSTUNT",
                   "FWTKG", "FBMI",
                   "BRFEED", 
                   "SUMEP",   "SUMDIAR", "SUMDAYS",
                   "PCTDIAR", "IMPSAN",  "SOAP",    "SAFEH2O", "TRTH2O",  "CLEANCK",
                   "IMPFLOOR","H2OTIME",
                   "CHICKEN", "COW",     "CATTLE",  "INCTOT", 
                   "INCTOTU", "BFEDFL",  "EXBFEDFL","WEANFL",  "ANMLKFL", "PWMLKFL",
                   "FORMLKFL","BOTTLEFL","H20FEDFL","OTHFEDFL","SLDFEDFL","NBFYES",  "EARLYBF", "CMFDINT", "DIARFL",  "LSSTLFL",
                   "NUMLS",   "BLDSTLFL","DIARFL_R","LSSTFL_R","NUMLS_R", "BLDSTL_R",
                   "DUR_R"))


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


#--------------------------------------------------------
#Code to keep monthly and quarterly studies
#--------------------------------------------------------
#d <- d %>% filter(measurefreq!="yearly")
#NOTE: not run here because subsetting to correct studies is done
#in the outcome datasets
d <- d %>% subset(., select=-c(measurefreq))


#--------------------------------------------------------
#Subset to control arms for intervention studies
#--------------------------------------------------------
#table(d$studyid, d$tr)
#d <- filter(d, tr=="Control" | tr=="")

#--------------------------------------------
# drop trial arms with intervention impact on HAZ
# potentially subset cmin and cohorts to control too,
# but currently there is no tr variable for them
#--------------------------------------------
d=d[-which(d$studyid=="kiGH5241-JiVitA-4" & d$tr!="Control"),]
d=d[-which(d$studyid=="ki1119695-PROBIT" & d$tr!="Control"),]
d=d[-which(d$studyid=="ki1000304b-SAS-FoodSuppl" & d$tr!="Control"),]
d=d[-which(d$studyid=="ki1112895-iLiNS-Zinc" & d$tr!="Control"),]
d=d[-which(d$studyid=="ki1000304b-SAS-CompFeed" & d$tr!="Control"),]




#--------------------------------------------------------
#Calculate stunting at enrollment and keep one observation per child
#Also check if children without a recorded birthweight or birthlength have WAZ or HAZ in the first year of life
#--------------------------------------------------------
d <- d %>% group_by(studyid, subjid) %>% 
  arrange(agedays) %>% 
  mutate(enstunt= as.numeric(haz < -2),
         birthLAZ= haz,
         birthWAZ= waz) %>%
  slice(1) 

table(is.na(d$birthwt), d$agedays>1)

#keep where anthro is measured on first day, but birth anthro is not recorded
d$birthLAZ[d$agedays>1] <- NA 
d$birthWAZ[d$agedays>1] <- NA
d <- d %>% subset(., select=-c(agedays, haz, waz))


#--------------------------------------------------------
#merge in household assets PCA wealth
#--------------------------------------------------------

#convert subjid to character for the merge
d$subjid <- as.character(d$subjid)

#load in pca results
load("U:/results/allGHAPstudies-HHwealth.Rdata")
table(pca$STUDYID, pca$HHwealth_quart)
#Note, only the COHORTS study has SES but no HH wealth quartile
colnames(pca) <- tolower(colnames(pca))
pca <- as.data.frame(pca)
pca$subjid <-as.character(pca$subjid)
d <- left_join(d, pca, by=c("studyid", "country", "subjid"))

#merge in ses variable for COHORTS for all countries except INDIA. The other countries have wealth based on 
#an asset-based PCA index, but India is based on father's occupation.
d$hhwealth_quart <- as.character(d$hhwealth_quart)
chtses<- d$ses[is.na(d$hhwealth_quart) & d$studyid=="ki1135781-COHORTS" & d$country!="INDIA"]
chtses[chtses==""] <- NA
chtses[chtses=="Low"] <- "Wealth Q1"
chtses[chtses=="Lower-mi"] <- "Wealth Q2"
chtses[chtses=="Middle"] <- "Wealth Q3"
chtses[chtses=="Upper-mi"] <- "Wealth Q4"
chtses[chtses=="Upper"] <- "Wealth Q4"

d$hhwealth_quart[is.na(d$hhwealth_quart) & d$studyid=="ki1135781-COHORTS" & d$country!="INDIA"] <-chtses
d$hhwealth_quart <- factor(d$hhwealth_quart)

#Check and make sure all merged
#df <- d %>% filter(!is.na(hhwealth_quart)) %>% group_by(studyid, subjid) %>% slice(1)
#table(pca$studyid, pca$hhwealth_quart)
#table(df$studyid, df$hhwealth_quart)




#--------------------------------------------------------------------------
# Code Food security
#--------------------------------------------------------------------------

#Recode into 4 harmonized categories
unique(d$hfoodsec)

d$temp <- NA

d$temp[d$hfoodsec=="Mildly Food Insecure"] <- "Mildly Food Insecure"
d$temp[d$hfoodsec=="Food Insecure"] <- "Moderately Food Insecure"
d$temp[d$hfoodsec=="Food secure"] <- "Food Secure"

d$temp[d$hfoodsec=="Neither Deficit Nor Surplus"] <- "Mildly Food Insecure"
d$temp[d$hfoodsec=="Sometimes Deficit"] <- "Moderately Food Insecure"
d$temp[d$hfoodsec=="Deficit In Whole Year"] <- "Severely Food Insecure"
d$temp[d$hfoodsec=="Surplus"] <- "Food Secure"

d$temp[d$hfoodsec=="Neither deficit nor surplus"] <- "Mildly Food Insecure"
d$temp[d$hfoodsec=="Sometimes deficit"] <- "Moderately Food Insecure"
d$temp[d$hfoodsec=="Deficit in whole year"] <- "Severely Food Insecure"

d$temp[d$hfoodsec=="Mildly Food Insecure Access"] <- "Mildly Food Insecure"
d$temp[d$hfoodsec=="Moderately Food Insecure Access"] <- "Moderately Food Insecure"
d$temp[d$hfoodsec=="Severely Food Insecure Access"] <- "Severely Food Insecure"
d$temp[d$hfoodsec=="Food Secure"] <- "Food Secure"

d$hfoodsec <- d$temp
d <- d %>% subset(., select=-c(temp))

table(d$studyid, d$hfoodsec)





#--------------------------------------------------------------------------
# birth characteristics
#--------------------------------------------------------------------------



#parity
#Combine parity and birthorder
table(d$studyid, d$parity)
table(d$studyid, d$brthordr)

d$parity[is.na(d$parity)] <- d$brthordr[is.na(d$parity)]


#Fix 5 obs of 0 in ki1000304b-SAS-FoodSuppl
d$parity[d$studyid=="ki1000304b-SAS-FoodSuppl" & d$parity==0] <- NA
table(d$studyid, d$parity)



#Convert birthweight and birthlength to WAZ and LAZ
table(d$studyid, is.na(d$birthlen))
table(d$studyid, is.na(d$birthwt))

#sex must be "Male" or "Female"
table(d$sex)
#set blank sex to missing
d$sex[d$sex=="" | d$sex=="Unknown"]<-NA
#drop kids missing sex (investigate later)
d <- d %>% filter(!is.na(sex))
birthhaz <- who_htcm2zscore(rep(0,nrow(d)), d$birthlen, sex = d$sex)
birthwaz <- who_wtkg2zscore(rep(0,nrow(d)), d$birthwt/1000, sex = d$sex)

#Check if children without a recorded birthweight or birthlength have WAZ or HAZ in the first year of life
#and add into birthweight variable

table(is.na(birthhaz))
table(is.na(birthwaz))

table(is.na(birthhaz), is.na(d$birthLAZ))
table(is.na(birthwaz), is.na(d$birthWAZ))

birthhaz[is.na(birthhaz)] <- d$birthLAZ[is.na(birthhaz)]
birthwaz[is.na(birthwaz)] <- d$birthWAZ[is.na(birthwaz)]

table(is.na(birthhaz))
table(is.na(birthwaz))

d$birthlen <- birthhaz
d$birthwt <- birthwaz

#Drop outlier birth HAZ and WAZ
d$birthlen[d$birthlen < -6 | d$birthlen > 6] <- NA
d$birthwt[d$birthwt < -6 | d$birthwt > 6] <- NA

table(d$studyid, is.na(d$birthlen))
table(d$studyid, is.na(d$birthwt))

#--------------------------------------------------------------------------
# parent characteristics
#--------------------------------------------------------------------------

#single mom
table(d$studyid, d$single)

#Note Jivita-4 single mother seems way too high
#drop single mother in kiGH5241-JiVitA-4 until the raw data can be checked more thoroughly
d$gagebrth[d$studyid=="kiGH5241-JiVitA-4"] <-NA


#Calculate bmi from height and weight, and vice versa, for when 2 of 3 are measured
#bmi
flag <- is.na(d$mbmi) & !is.na(d$mhtcm) & !is.na(d$mwtkg)
d$mbmi[flag] <- d$mwtkg[flag] / (d$mhtcm[flag] / 100)^2

#weight
flag <- is.na(d$mwtkg) & !is.na(d$mhtcm) & !is.na(d$mbmi)
d$mwtkg[flag] <- d$mbmi[flag] * (d$mhtcm[flag] / 100)^2

#height
flag <- is.na(d$mhtcm) & !is.na(d$mwtkg) & !is.na(d$mbmi)
d$mhtcm[flag] <- sqrt(d$mwtkg[flag] / d$mbmi[flag]) * 100

#--------------------------------------------------------------------------
# house characteristics
#--------------------------------------------------------------------------

#set to missing any observations of 0 rooms
d$nrooms[d$nrooms==0] <- NA

#check the number of children < 5 and set to missing if 0
table(d$nchldlt5)
table(d$studyid, d$nchldlt5)

#Need to shift full distribution by 1 in studies with 0 marked- 
#  inconsistent marking of subject in the count across studies
d$nchldlt5[d$studyid=="ki1148112-LCNI-5" & d$nchldlt5==0] <- NA #LCNI has 3 children marked as 0
d$nchldlt5[d$studyid=="ki1000108-IRC"] <- d$nchldlt5[d$studyid=="ki1000108-IRC"] + 1
d$nchldlt5[d$studyid=="ki1017093b-PROVIDE"] <- d$nchldlt5[d$studyid=="ki1017093b-PROVIDE"] + 1
d$nchldlt5[d$studyid=="ki1017093c-NIH-Crypto"] <- d$nchldlt5[d$studyid=="ki1017093c-NIH-Crypto"] + 1
d$nchldlt5[d$studyid=="ki1066203-TanzaniaChild2"] <- d$nchldlt5[d$studyid=="ki1066203-TanzaniaChild2"] + 1
d$nchldlt5[d$studyid=="ki1148112-iLiNS-DYAD-M"] <- d$nchldlt5[d$studyid=="ki1148112-iLiNS-DYAD-M"] + 1
d$nchldlt5[d$studyid=="kiGH5241-JiVitA-3"] <- d$nchldlt5[d$studyid=="kiGH5241-JiVitA-3"] + 1


#--------------------------------------------------------------------------
# birthmonth
#--------------------------------------------------------------------------

#Calculate birthmonth from brthweek where brthmonth is missing
d$brthmon[is.na(d$brthmon)] <- ceiling(d$brthweek[is.na(d$brthmon)]/53 *12)


#--------------------------------------------------------
# create id variable for unit of independent observation
#--------------------------------------------------------

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

#No site id in the cohorts guatemala raw data
#d$id[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"] <-d$clustid[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"]

#use siteid from PROBIT
d$id[d$studyid %in% c("ki1119695-PROBIT")] <-d$siteid[d$studyid %in% c("ki1119695-PROBIT")]

table(is.na(d$id))


#--------------------------------------------------------
# Drop risk factors without enough studies or unneeded variables 
#--------------------------------------------------------

colnames(d)
d <- subset(d, select = -c(siteid, region,  clustid, tr, brthweek,   brthordr, ses))






#--------------------------------------------------------
#Convert continious variables to quartiled categorical 
#--------------------------------------------------------



#quantiling functions
quantile_rf <- function(A, labs=NULL, Acuts=NULL){
  A<-as.numeric(A)
  if(sum(is.na(A))!=length(A)){
    if(is.null(Acuts)){
      Acuts=c(0, as.numeric(quantile(A, probs = c(.25,.5,.75), na.rm=T)), max(A, na.rm=T))
    }
    
    if(length(Acuts)==4){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0("[",round(Acuts[2],2),"-",round(Acuts[3],2),")"),
                paste0(">=",round(Acuts[3],2))) 
    }
    if(length(Acuts)==5){
    Alevels=c(paste0("<",round(Acuts[2],2)), 
              paste0("[",round(Acuts[2],2),"-",round(Acuts[3],2),")"),
              paste0("[",round(Acuts[3],2),"-",round(Acuts[4],2),")"), 
              paste0(">=",round(Acuts[4],2))) 
    }
    if(length(Acuts)==6){
      Alevels=c(paste0("<",round(Acuts[2],2)), 
                paste0("[",round(Acuts[2],2),"-",round(Acuts[3],2),")"),
                paste0("[",round(Acuts[3],2),"-",round(Acuts[4],2),")"),
                paste0("[",round(Acuts[4],2),"-",round(Acuts[5],2),")"), 
                paste0(">=",round(Acuts[5],2))) 
    }    
    
    
    if(!is.null(labs)){
      Alevels=labs
    }
    
    if(length(unique(Acuts))==length((Acuts))){
      A <- cut(A, include.lowest = T, right = FALSE, breaks=Acuts,labels=Alevels)
    }else{
      A <- cut(A, include.lowest = T, right = FALSE, breaks=4,labels=c("Q1","Q2","Q3","Q4","Q5")[1:(length(Acuts)-1)])
    }
    A <- factor(A)
    return(A)
  }
}

quantile_rf_bystudy <- function(df){
  
  df$meducyrs <- quantile_rf(df$meducyrs, labs=c("Q1","Q2","Q3","Q4"))
  df$feducyrs <- quantile_rf(df$feducyrs, labs=c("Q1","Q2","Q3","Q4"))
  
  return(df)
}




#gestational age at birth
#<37 weeks = preterm
#37-38 weeks = early term
#39-40 weeks = full term (baseline)
#>=41 weeks = late/post term
#maternal BMI (is this measured when pregnant or not? if pregnant, then we may need to change these categories)
#<18.5 = underweight
#>=18.5 and <25 = normal weight (baseline)
#>=25 and <30 = overweight
#>=30 = obese
#maternal height (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3095774/)
#less than 145 cm
#145–149.9 cm
#150–154.9 cm
#155–159.9 cm
#160.0 cm or greater. (baseline)
#maternal weight?
#mother’s/father's education
#highest education level = baseline
#father age
#oldest = baseline
#father height?

# summary(d$fhtcm)
# test <- quantile_rf(d$fhtcm, Acuts=c(0,160,170,max(d$mhtcm, na.rm=T)))
# table(test)
# table(d$studyid, test)

#Overall a-priori quantiles
d$gagebrth <- quantile_rf(d$gagebrth, Acuts=c(0,37*7,39*7,41*7,max(d$gagebrth, na.rm=T)))
d$birthwt <- quantile_rf(d$birthwt, Acuts=c(-100,-3,-2,-1,0,max(d$birthwt, na.rm=T)))
d$birthlen <- quantile_rf(d$birthlen, Acuts=c(-100,-3,-2,-1,0,max(d$birthlen, na.rm=T)))
d$mage <- quantile_rf(d$mage, Acuts=c(0,20,25,30,max(d$mage, na.rm=T)))
d$mhtcm <- quantile_rf(d$mhtcm, Acuts=c(0,145,150,155,160,max(d$mhtcm, na.rm=T)))
d$mwtkg <- quantile_rf(d$mwtkg, Acuts=c(0,42.5,50,57.5,max(d$mwtkg, na.rm=T)))
d$mbmi <- quantile_rf(d$mbmi, Acuts=c(0,18.5,25,30,max(d$mbmi, na.rm=T)))
d$fage <- quantile_rf(d$fage, Acuts=c(0,25,30,35,max(d$fage, na.rm=T)))
d$fhtcm <- quantile_rf(d$fhtcm, Acuts=c(0,160,170,max(d$fhtcm, na.rm=T)))

# res <- quantile_rf_bystudy(d)
# res2 <- d %>% group_by(studyid, country) %>%
#   do(quantile_rf_bystudy(.))
# table(res$studyid, res$meducyrs)
# table(res2$studyid, res2$meducyrs)

#quantile by study
d <- d %>% group_by(studyid, country) %>%
  do(quantile_rf_bystudy(.))


#Categorize nrooms, nhh, nchild5
table(d$nrooms)
table(d$studyid, d$nrooms)
nroom<-NA
nroom[d$nrooms<2] <- "1"
nroom[d$nrooms==2] <- "2"
nroom[d$nrooms==3] <- "3"
nroom[d$nrooms>3] <- "4+"
d$nrooms <- as.factor(nroom)
table(d$nrooms)


table(d$nhh)  
table(d$studyid, d$nhh)

nhh<-NA
nhh[d$nhh<4] <- "3 or less"
nhh[d$nhh>3 & d$nhh<6] <- "4-5"
nhh[d$nhh>5 & d$nhh<8] <- "6-7"
nhh[d$nhh>7] <- "8+"
d$nhh <- as.factor(nhh)
table(d$nhh)
table(d$studyid, d$nhh)


table(d$nchldlt5)
table(d$studyid, d$nchldlt5)

nchild5<-NA
nchild5[d$nchldlt5==1] <- "1"
nchild5[d$nchldlt5==2] <- "2"
nchild5[d$nchldlt5>2] <- "3+"
d$nchldlt5 <- as.factor(nchild5)
table(d$nchldlt5)
table(d$studyid, d$nchldlt5)

d$nchldlt5 <- relevel(d$nchldlt5, ref="1")

table(d$parity)
table(d$studyid, d$parity)

parity<-NA
parity[d$parity==0] <- NA 
parity[d$parity==1] <- "1"
parity[d$parity==2] <- "2"
parity[d$parity>2] <- "3+"
d$parity <- as.factor(parity)
table(d$parity)
table(d$studyid, d$parity)

d$parity <- relevel(d$parity, ref="1")




#---------------------------------------
# Set reference levels
#---------------------------------------

#birthweight
#WAZ <-3
#-3 < WAZ<=-2
#-2 <= WAZ <-1
#-1 <= WAZ < 0
#WAZ >=0 (baseline)

d$birthwt <- relevel(d$birthwt, ref=">=0")

#birth length: 
#LAZ <-3
#-3 < LAZ<=-2
#-2 <= LAZ <-1
#-1 <= LAZ < 0
#LAZ >=0 (baseline)

d$birthlen <- relevel(d$birthlen, ref=">=0")

#wealth index: 
#wealthiest quartile - Q4 is baseline

d$hhwealth_quart <- relevel(d$hhwealth_quart, ref="Wealth Q4")

# children < 5 in HH
#not sure how this could be zero — can you double check this? 
#baseline should be smallest number

d$nchldlt5 <- relevel(d$nchldlt5, ref="1")

#gestational age at birth
#<37 weeks = preterm
#37-38 weeks = early term
#39-40 weeks = full term (baseline)
#>=41 weeks = late/post term

d$gagebrth <- relevel(d$gagebrth, ref="[273-287)")

#maternal BMI (is this measured when pregnant or not? if pregnant, then we may need to change these categories)
#<18.5 = underweight
#>=18.5 and <25 = normal weight (baseline)
#>=25 and <30 = overweight
#>=30 = obese

d$mbmi <- relevel(d$mbmi, ref="[18.5-25)")

#maternal height (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3095774/)
#less than 145 cm
#145–149.9 cm
#150–154.9 cm
#155–159.9 cm
#160.0 cm or greater. (baseline)

d$mhtcm <- relevel(d$mhtcm, ref=">=160")

#maternal weight?
d$mwtkg <- relevel(d$mwtkg, ref=">=57.5")

#mother’s/father's education
#highest education level = baseline
d$meducyrs <- relevel(factor(d$meducyrs), ref="Q4")
d$feducyrs <- relevel(factor(d$feducyrs), ref="Q4")

#father age
#oldest = baseline
d$fage <- relevel(d$fage, ref=">=35")

#father height?
d$fhtcm <- relevel(d$fhtcm, ref="[160-170)")




#---------------------------------------
#Create an ID variable
#---------------------------------------

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


#Set remaining risk factors to factors
d$brthmon <- factor(d$brthmon)
d$month <- factor(d$month)
d$single <- factor(d$single)
d$vagbrth <- factor(d$vagbrth)
d$hdlvry <- factor(d$hdlvry)
d$hfoodsec <- factor(d$hfoodsec)
d$enstunt <- factor(d$enstunt)
d$sex <- factor(d$sex)
d$meducyrs <- factor(d$meducyrs)

#Check that all risk factor variables are set as factors
d<-as.data.frame(d)
for(i in 1:ncol(d)){
  cat(colnames(d)[i], ": ", class(d[,i]), "\n")
}



#Tabulate missingness
for(i in 1:ncol(d)){
  print(colnames(d)[i])
  print(table(is.na(d[,i])))
  print(levels(d[,i]))
}



#drop remaining unneeded columns
d <- subset(d, select=-c(birthLAZ, birthWAZ))



#save dataset
saveRDS(d, file="FINAL_temp_clean_covariates.rds")

saveRDS(d, file="U:/UCB-SuperLearner/Stunting rallies/FINAL_temp_clean_covariates.rds")




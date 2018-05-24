

#Add instructions for downloadin FINAL dataset



rm(list=ls())
library(tidyverse)
library(data.table)

source("U:/GHAP-Data-Management/HBGDki_functions.R")


setwd("U:/data")
gc()



#Read rds file and drop unneeded columns
d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T,
         drop = c( "AGEIMPFL",  "WTKG",    "HTCM",    "LENCM",   "WAZ",      
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
d <- filter(d, tr=="Control" | tr=="")


#--------------------------------------------------------
#Calculate stunting at enrollment and keep one observation per child
#--------------------------------------------------------
d <- d %>% group_by(studyid, subjid) %>% 
  arrange(agedays) %>% 
  slice(1) %>% 
  mutate(enstunt= as.numeric(haz < -2))
d <- d %>% subset(., select=-c(agedays, haz))


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

#TEMP
#drop gestational age in ki1017093c-NIH-Crypto until it is fixed (all too low: ~ 1month)
d$gagebrth[d$studyid=="ki1017093c-NIH-Crypto"] <-NA




#parity
#Combine parity and birthorder
table(d$studyid, d$parity)
table(d$studyid, d$brthordr)

d$parity[is.na(d$parity)] <- d$brthordr[is.na(d$parity)]


#shift observations of the studies that start at 0 rightward by one.
d$parity[!is.na(d$parity) & d$studyid %in% c("ki1066203-TanzaniaChild2", "ki1066203-TanzaniaChild2", "ki1101329-Keneba",
                                             "ki1113344-GMS-Nepal", "kiGH5241-JiVitA-3")] <-d$parity[!is.na(d$parity) & d$studyid %in% c("ki1066203-TanzaniaChild2", "ki1066203-TanzaniaChild2", "ki1101329-Keneba",
                                                                                                                                         "ki1113344-GMS-Nepal", "kiGH5241-JiVitA-3")] + 1

#Fix 14 obs of 0 in ki1000304b-SAS-FoodSuppl
d$parity[d$studyid=="ki1000304b-SAS-FoodSuppl" & d$parity==0] <- NA
table(d$studyid, d$parity)



#--------------------------------------------------------------------------
# parent characteristics
#--------------------------------------------------------------------------

#single mom
table(d$studyid, d$single)

#Note Jivita-4 single mother seems way too high
#TEMP
#drop single mother in kiGH5241-JiVitA-4 until it is fixed
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
d$id[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"] <-d$clustid[d$studyid=="ki1135781-COHORTS" & d$country=="GUATEMALA"]

#use siteid from PROBIT
d$id[d$studyid %in% c("ki1119695-PROBIT")] <-d$siteid[d$studyid %in% c("ki1119695-PROBIT")]

#--------------------------------------------------------
# Drop risk factors without enough studies or unneeded variables 
#--------------------------------------------------------

colnames(d)
d <- subset(d, select = -c(siteid, region,  clustid, tr, brthweek,   brthordr, ses))






#--------------------------------------------------------
#Convert continious variables to quartiled categorical 
#--------------------------------------------------------



#quantiling functions
quantile_rf <- function(A, labs=NULL){
  A<-as.numeric(A)
  if(sum(is.na(A))!=length(A)){
    Acuts=c(0, as.numeric(quantile(A, probs = c(.25,.5,.75), na.rm=T)), max(A, na.rm=T))
    Alevels=c(paste0("<=",round(Acuts[2],2)), 
              paste0("(",round(Acuts[2],2),"-",round(Acuts[3],2),"]"),
              paste0("(",round(Acuts[3],2),"-",round(Acuts[4],2),"]"), 
              paste0(">",round(Acuts[4],2))) 
    if(!is.null(labs)){
      Alevels=labs
    }
    
    if(length(unique(Acuts))==length((Acuts))){
      A <- cut(A, include.lowest = T, breaks=Acuts,labels=Alevels)
    }else{
      A <- cut(A, include.lowest = T, breaks=4,labels=c("Q1","Q2","Q3","Q4"))
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

res <- quantile_rf_bystudy(d)
res2 <- d %>% group_by(studyid, country) %>%
  do(quantile_rf_bystudy(.))
table(res$studyid, res$nchldlt5)
table(res2$studyid, res2$nchldlt5)

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
nchild5[d$nchldlt5==0] <- "0"
nchild5[d$nchldlt5==1] <- "1"
nchild5[d$nchldlt5>1] <- "2+"
d$nchldlt5 <- as.factor(nchild5)
table(d$nchldlt5)
table(d$studyid, d$nchldlt5)

d$nchldlt5 <- relevel(d$nchldlt5, ref="1")

table(d$parity)
table(d$studyid, d$parity)

parity<-NA
parity[d$parity==0] <- NA #TEMP: set parity of 0 to NA until Vishak responds
parity[d$parity==1] <- "1"
parity[d$parity==2] <- "2"
parity[d$parity>2] <- "3+"
d$parity <- as.factor(parity)
table(d$parity)
table(d$studyid, d$parity)

d$nchldlt5 <- relevel(d$nchldlt5, ref="1")



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
}







#save dataset
saveRDS(d, file="FINAL_temp_clean_covariates.rds")

saveRDS(d, file="U:/UCB-SuperLearner/Stunting rallies/FINAL_temp_clean_covariates.rds")




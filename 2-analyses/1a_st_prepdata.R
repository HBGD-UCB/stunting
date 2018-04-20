#-----------------------------------
# Stunting analysis
# Objective 1a
# Import data, subset to relevant variables
#-----------------------------------
rm(list=ls())
library(dplyr)
library(data.table)

setwd("U:/data/GHAP_data/")
d<-fread("U:/data/Stunting/Full-compiled-data/FINAL.csv", header = T)


#--------------------------------------------
# Subset to relevant variables
#--------------------------------------------
colnames(d)=tolower(colnames(d))
d <- d %>% select(studyid, subjid, country, tr, sex, agedays, haz)

nrow(d)

#--------------------------------------------
# drop unrealistic HAZ
#--------------------------------------------
nrow(d)
d = filter(d,haz >= -6 & haz <=6)
nrow(d)

#--------------------------------------------
# order data, create measurement id
#--------------------------------------------
d <- d %>% 
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  # create id for measurement within person
  mutate(measid=seq_along(subjid)) 

#--------------------------------------------
# calculate average months between measurements
#--------------------------------------------
d <- d %>%
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  mutate(agedayslag=lag(agedays),
         deltat=agedays-agedayslag,
         deltam=deltat/30.4167) %>%
  mutate(agedayslag=ifelse(is.na(agedayslag),0,agedayslag),
         deltam=ifelse(is.na(deltam)|measid==1,0,deltam))

avgmths <- d %>%
  group_by(studyid) %>%
  summarise(meantime=mean(deltam)) %>%
  arrange(meantime)

# subset to studies with data collection at least every 3 months
drops=c("ki1148112-iLiNS-DYAD-M","ki1148112-iLiNS-DOSE",
        "ki1000111-WASH-Kenya","ki1000110-WASH-Bangladesh")

d<- d %>%
  filter(!studyid %in% drops) 
rm(drops)

drops=which(d$studyid=="ki1135781-COHORTS" & (
  d$country=="BRAZIL"|d$country=="INDIA"|
    d$country=="SOUTH AFRICA"))

d=d[-drops,]
rm(drops)

#--------------------------------------------
# drop trial arms with intervention impact on HAZ
# potentially subset cmin and cohorts to control too,
# but currently there is no tr variable for them
#--------------------------------------------
d=d[-which(d$studyid=="ki1000304-EU" & d$tr=="Zinc"),]
d=d[-which(d$studyid=="kiGH5241-JiVitA-4" & d$tr!="Control"),]
d=d[-which(d$studyid=="ki1119695-PROBIT" & d$tr!="Control"),]
d=d[-which(d$studyid=="ki1000304b-SAS-FoodSuppl" & d$tr!="Control"),]
d=d[-which(d$studyid=="ki1000304-VITAMIN-A" & d$tr!="Control"),]

# count number of studies
length(names(table(d$studyid)))

# table of studies
table(d$studyid)

save(d,file="U:/Data/Stunting/stunting_data.RData")


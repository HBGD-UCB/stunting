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
# plot HAZ by agedays for all studies
#--------------------------------------------
pdf("U:/Figures/haz-scatter-all.pdf",width=15,height=15,onefile=TRUE)
ggplot(d[d$agedays<=365*2,],aes(x=agedays,y=haz))+geom_point(alpha=0.3)+geom_smooth()+
  facet_wrap(~studyid+country)+geom_hline(yintercept=-2,linetype="dashed",col="red")
dev.off()

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
# assess frequency of measurement
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

# calculate average months between measurements
avgmths <- d %>%
  group_by(studyid,country) %>%
  summarise(meantime=mean(deltam)) %>%
  arrange(meantime)

medmths <- d %>%
  group_by(studyid,country) %>%
  summarise(medtime=median(deltam)) %>%
  arrange(medtime)


# calculate # measurements per child under 24 months
nmeas <- d %>%
  group_by(studyid,country,subjid) %>%
  filter(agedays<=365*2) %>%
  summarise(nmeas=n()) %>%
  group_by(studyid,country) %>%
  summarise(mnmeas=mean(nmeas)) %>%
  arrange(mnmeas)

# combine 
comb=full_join(avgmths,nmeas,by=c("studyid","country")) 
comb=full_join(comb,medmths,by=c("studyid","country")) 

# candidates for dropping
comb[comb$meantime>3 | comb$mnmeas<5,]

# subset to studies with data collection at least every 3 months
drops=c("ki1148112-iLiNS-DYAD-M","ki1148112-iLiNS-DOSE",
        "ki1000111-WASH-Kenya","ki1000110-WASH-Bangladesh",
        "ki1000107-Serrinha-VitA","ki1000304-VITAMIN-A",
        "ki1000304-Vitamin-B12","ki1000301-DIVIDS",
        "ki1112895-Guatemala BSC","ki1000304-ZnMort",
        "ki1000304-EU")

d<- d %>%
  filter(!studyid %in% drops) 
rm(drops)

drops=which(d$studyid=="ki1135781-COHORTS" & (
  d$country=="BRAZIL"|d$country=="INDIA"|
    d$country=="SOUTH AFRICA"))

# drop cmin guinnea and cohorts guatemala trials since no arm data

d=d[-drops,]
rm(drops)

#--------------------------------------------
# drop trial arms with intervention impact on HAZ
# potentially subset cmin and cohorts to control too,
# but currently there is no tr variable for them
#--------------------------------------------
d=d[-which(d$studyid=="kiGH5241-JiVitA-4" & d$tr!="Control"),]
d=d[-which(d$studyid=="ki1119695-PROBIT" & d$tr!="Control"),]
d=d[-which(d$studyid=="ki1000304b-SAS-FoodSuppl" & d$tr!="Control"),]

# count number of studies
length(names(table(d$studyid)))

# table of studies
table(d$studyid)

#--------------------------------------------
# plot HAZ by agedays for included studies
#--------------------------------------------
pdf("U:/Figures/haz-scatter-1a.pdf",width=15,height=15,onefile=TRUE)
ggplot(d[d$agedays<=365*2,],aes(x=agedays,y=haz))+geom_point(alpha=0.3)+geom_smooth()+
  facet_wrap(~studyid+country)+geom_hline(yintercept=-2,linetype="dashed",col="red")
dev.off()


save(d,file="U:/Data/Stunting/stunting_data.RData")


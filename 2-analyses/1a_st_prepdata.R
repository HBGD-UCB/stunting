#-----------------------------------
# Stunting analysis
# Objective 1a
# Import data, subset to relevant variables
#-----------------------------------
rm(list=ls())
library(dplyr)

setwd("U:/data/GHAP_data/")


# NO MLEX CHECK WITH ANDREW
study.list=c("akup","bfzn","cmc","cmin","cort","cntt",
             "dvds","ee","eu","gmsn","gbsc","irc","jvt3",
             "jvt4","knba","lcn5","mled","nrbt",
             "ncry","prbt","rspk","cmpf","fspp","tzc2",
             "vb12","vita","wsb","wsk","zvit","zmrt",
             "lnsz","ilnd","ildm")
length(study.list)

# import and prep data function
data.prep=function(dataname){
  # import data
  data<-readRDS(paste0(dataname,".rds"))
  
  # check if intervention arm column present
  # keep relevant variables
  if("ARM" %in% colnames(data)){
    data=select(data,c("SUBJID","STUDYID","COUNTRY","ARM","SEX","AGEDAYS","HAZ"))
  }
  
  data=select(data,c("SUBJID","STUDYID","COUNTRY","SEX","AGEDAYS","HAZ"))
  colnames(data)=tolower(colnames(data))
  
  print(paste(dataname)) 
  print(data$studyid[1])
  return(data)
}

data.set=lapply(study.list,data.prep)
all.data=do.call(rbind,data.set)
nrow(all.data)

# drop unrealistic HAZ
nrow(all.data)
all.data = filter(all.data,haz >= -6 & haz <=6)
nrow(all.data)

all.data <- all.data %>% 
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  # create id for measurement within person
  mutate(measid=seq_along(subjid)) 

# calculate average months between measurements
all.data <- all.data %>%
  arrange(studyid,subjid,agedays) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  mutate(agedayslag=lag(agedays),
         deltat=agedays-agedayslag,
         deltam=deltat/30.4167) %>%
  mutate(agedayslag=ifelse(is.na(agedayslag),0,agedayslag),
         deltam=ifelse(is.na(deltam)|measid==1,0,deltam))

avgmths <- all.data %>%
  group_by(studyid) %>%
  summarise(meantime=mean(deltam)) %>%
  arrange(meantime)

# subset to studies with data collection at least every 3 months
drops=c("ki1148112-iLiNS-DYAD-M","ki1148112-iLiNS-DOSE",
        "ki1000111-WASH-Kenya","ki1000110-WASH-Bangladesh")
  
all.data<- all.data %>%
  filter(!studyid %in% drops) 

drops2=which(all.data$studyid=="ki1135781-COHORTS" & (
  all.data$country=="BRAZIL"|all.data$country=="INDIA"|
    all.data$country=="SOUTH AFRICA"))

all.data=all.data[-drops2,]

# count 

save(all.data,file="U:/Data/Stunting/stunting_data.RData")


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

save(all.data,file="U:/Data/Stunting/stunting_data.RData")
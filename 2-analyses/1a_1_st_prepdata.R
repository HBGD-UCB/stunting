#-----------------------------------
# Stunting analysis
# Objective 1a
# Import data, subset to relevant variables
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(data.table)

#--------------------------------------------
# Read in .csv file and save as an .rds file
#--------------------------------------------
d<-fread("U:/data/FINAL/UCB Rally7/Main/adam/FINAL.csv", header = T)
dim(d)
head(d)
colnames(d)
saveRDS(d, "FINAL.rds")

#Read rds file
#d<- readRDS("FINAL.rds")

#--------------------------------------------
#Check for duplicate agedays
#--------------------------------------------
dup_age <- d %>% group_by(STUDYID, SUBJID, AGEDAYS) %>% summarize(N=n())
mean(dup_age$N)

#--------------------------------------------
# Save just identifying and haz data
#--------------------------------------------
df<-d %>% subset(., select=c(STUDYID, SUBJID, COUNTRY, TR, AGEDAYS, HAZ))
colnames(df) <- tolower(colnames(df))
save(df, file="U:/data/Stunting/Full-compiled-data/compiled_HAZ_dataset.RData")




#--------------------------------------------
# Load only the anthropometry data
#--------------------------------------------

setwd("U:/data/GHAP_data/")
load("U:/data/Stunting/Full-compiled-data/compiled_HAZ_dataset.RData")

#--------------------------------------------
# Subset to relevant variables
#--------------------------------------------
colnames(d)=tolower(colnames(d))
d <- d %>% select(studyid, subjid, country, tr, agedays, haz)

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

# count number of studies
length(names(table(d$studyid)))

# table of studies
table(d$studyid)
table(d$studyid,d$country)

#--------------------------------------------
# plot HAZ by agedays for included studies
#--------------------------------------------
pdf("U:/Figures/haz-scatter-1a.pdf",width=15,height=15,onefile=TRUE)
ggplot(d[d$agedays<=365*2,],aes(x=agedays,y=haz))+geom_point(alpha=0.3)+geom_smooth()+
  facet_wrap(~studyid+country)+geom_hline(yintercept=-2,linetype="dashed",col="red")
dev.off()



save(d,file="U:/Data/Stunting/stunting_data.RData")


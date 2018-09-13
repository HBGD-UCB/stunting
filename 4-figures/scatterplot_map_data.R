

rm(list=ls())
library(tidyverse)
library(scatterpie)
library(data.table)

#--------------------------------------------
# Read in .csv file and save as an .rds file
#--------------------------------------------

d<-fread("U:/data/Stunting/Full-compiled-data/FINAL.csv", header = T)

#--------------------------------------------
# Subset to  just identifying and haz data
#--------------------------------------------

#change names to lower case
colnames(d) <- tolower(colnames(d))
d<-d %>% subset(., select=c(studyid, subjid, country,  agedays, haz, latitude, longitud))
gc()
d <- d %>% filter(agedays <= 24*30.4167)



#Drop studies Vishak added to data product that don't meet inclusion criteria
d <- d %>% filter(studyid!="ki1000301-DIVIDS" & studyid!="ki1055867-WomenFirst" & studyid!="ki1135782-INCAP")




d$cohort <- paste0(d$studyid,"-",d$country)
df <- d %>% filter(!is.na(haz)) %>% group_by(studyid,cohort) %>% 
  summarise(n=n(),
            `Not stunted`= mean(haz >=-2),
            `Stunted`= mean(haz >=-3 & haz < (-2)),
            `Severe stunted`= mean(haz < (-3)),
            lat=mean(latitude),
            long=mean(longitud),
            country=first(country)) 

getwd()
save(df, file="U:/results/stunt_scattermap_data.Rdata")





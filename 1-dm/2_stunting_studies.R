########################################
# HBGD Stunting analysis

# Subset HBGD studies for stunting analysis
# Objective 1: descriptive statistics

# Subsetting based on inclusion criteria
# in analysis plan 

# Input: meta data from GHAP platform
# Output: data frame of studies with 
# indicators for inclusion criteria 

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
########################################

rm(list=ls())
library(dplyr)

d=read.csv("U:/GHAP_metadata.csv")
nrow(d)

#---------------------------------------
# Criterion 1: Were conducted in low- or middle-income countries
#---------------------------------------
hic=c("CHN","USA","DNK","NLD","SGP","USA, GTM")

d$ic1=ifelse(d$country %in% hic,0,1)

table(as.character(d$country[d$ic1==1]))

d1 <- d %>% filter(d$ic1==1)
nrow(d1)

#---------------------------------------
# Criterion 2: Did not restrict enrollment to acutely ill 
# children (e.g., only children presenting to hospital with 
# acute diarrhea) and did not restrict by LAZ
#---------------------------------------
# per email from Andrew:
acute=c("grip","ppd","zinf","zsga","zlbw")
laz=c("eczn","pzn")

d1$ic2=ifelse(d1$short_id %in% acute | d1$short_id %in% laz,0,1)

d2 <- d1 %>% filter(d1$ic2==1)
nrow(d2)

#---------------------------------------
# Criterion 3: Longitudinal study
#---------------------------------------
d2$ic3=ifelse(d2$study_type=="Longitudinal",1,0)

d3 <- d2 %>% filter(d2$ic3==1)
nrow(d3)

#---------------------------------------
# Criterion 4: Enrolled at least 200 children
#---------------------------------------
d3$ic4=ifelse(d3$numsubj>=200,1,0)
d3$ic4[d3$numsubj=="NA"]=9

# manual correction for studies with missing data
d3$ic4[d3$short_id=="mlex"]=1
d3$ic4[d3$short_id=="vita"]=1
d3$ic4[d3$short_id=="vb12"]=1

d4 <- d3 %>% filter(d3$ic4==1)
nrow(d4)


#---------------------------------------
# Criterion 5: QC complete
#---------------------------------------
d4$ic5=ifelse(d4$status=="QC completed",1,0)

# manual correction
add=c("mlex","vita","vb12","prvd")
d4$ic5[d4$short_id %in% add]=1

d5 <- d4 %>% filter(d4$ic5==1)
nrow(d5)

#---------------------------------------
# Criterion 6: Measured length 
#---------------------------------------
ht=c("height","Height"," ht","Ht","HT","HAZ","LAZ","len","Len","LT","LEN")
htrows=grep(paste(ht,collapse="|"),d5$anthropometric_data)

# display rows without ht strings
drops=seq(1,nrow(d5))[!seq(1,nrow(d5)) %in% htrows]
d5[drops,"anthropometric_data"]

# drop rows without length measurement
d5$ic6=1
d5[d5$anthropometric_data=="Ultrasound at 24-28 weeks gestation, then birth anthropometry","ic6"]=0
d5[d5$anthropometric_data=="None","ic6"]=0
d5[d5$anthropometric_data=="WT, APGAR Scores","ic6"]=0
d5[d5$anthropometric_data=="TBD","ic6"]=0
d5[d5$anthropometric_data=="Birth weight (22273 children), enrollment (1750 children) , endstudy (1127 children)","ic6"]=0
d5[d5$anthropometric_data=="Ultrasound at 24-28 weeks gestation, then birth anthropometry","ic6"]=0

# manual correction
d5$ic6[d5$short_id=="zmrt"]=1

# display rows with ht measurement
as.data.frame(table(as.character(d5$anthropometric_data[d5$ic6==1])))[,1]
# display rows without ht measurement
as.data.frame(table(as.character(d5$anthropometric_data[d5$ic6==0])))[,1]

d6 <- d5 %>% filter(d5$ic6==1)
nrow(d6)

#---------------------------------------
# Criterion 7: Measured length between birth and age 24 months
#---------------------------------------
# studies based on Andrew and Esther's CONSORT that are out of age range
wrong.age=c("npre", "bts", "hemy", "ib21", "ig21", "igu", "gual")
d6$ic7=1
d6$ic7[d6$short_id %in% wrong.age]=0

d7 <- d6 %>% filter(d6$ic7==1)
nrow(d7)

#---------------------------------------
# Collected anthropometry measurements at 
# least every 3 months (descriptive analyses)

# Creating indicators rather than subsetting
#---------------------------------------
d7$every3mo=ifelse(d7$median_length_between_measures<30.5*3,1,0)
d7$every6mo=ifelse(d7$median_length_between_measures<30.5*6,1,0)
d7$every12mo=ifelse(d7$median_length_between_measures<30.5*12,1,0)

# display if missing every3mo
as.data.frame(table(as.character(d7$anthropometric_data[is.na(d7$every3mo)])))[,1]

d7$every3mo[d7$anthropometric_data=="Monthly measures of height and weight"]=1
d7$every6mo[d7$anthropometric_data=="Monthly measures of height and weight"]=1
d7$every12mo[d7$anthropometric_data=="Monthly measures of height and weight"]=1

d7$every3mo[d7$anthropometric_data=="Weight and length at enrollment and endstudy ( 4 months after enrollment)"]=0
d7$every6mo[d7$anthropometric_data=="Weight and length at enrollment and endstudy ( 4 months after enrollment)"]=1
d7$every12mo[d7$anthropometric_data=="Weight and length at enrollment and endstudy ( 4 months after enrollment)"]=1
 

# print study ids with unknown frequency of length measurement
unknown=c("HAZ, LAZ, WAZ only","Weight, Height, MUAC")
table(as.character(d4$short_id[d4$anthropometric_data %in% unknown]))

# cleaning
# based on Andrew Esther CONSORT spreadsheet
d7$every3mo[d7$short_id=="imnc"]=0
d7$every6mo[d7$short_id=="imnc"]=0
d7$every12mo[d7$short_id=="imnc"]=1

d7$every3mo[d7$anthropometric_data=="Weight and length at enrollment and endstudy ( 4 months after enrollment)"]=0
d7$every6mo[d7$anthropometric_data=="Weight and length at enrollment and endstudy ( 4 months after enrollment)"]=1
d7$every12mo[d7$anthropometric_data=="Weight and length at enrollment and endstudy ( 4 months after enrollment)"]=1

d7$every3mo[d7$anthropometric_data=="Weight and length at enrollment and  endstudy ( 4 months after enrollment)"]=0
d7$every6mo[d7$anthropometric_data=="Weight and length at enrollment and  endstudy ( 4 months after enrollment)"]=1
d7$every12mo[d7$anthropometric_data=="Weight and length at enrollment and  endstudy ( 4 months after enrollment)"]=1

d7$every6mo[d7$anthropometric_data=="WT, LEN, HCIR and MUAC every 6 months"]=1
d7$every12mo[d7$anthropometric_data=="WT, LEN, HCIR and MUAC every 6 months"]=1

d7$every12mo[d7$anthropometric_data=="Len, Wt, Hcir at enrollment, 15 months and 27 months after enrollment"]=1

d7$short_id=droplevels(d7$short_id)

# manually drop imnc
d7=d7[d7$short_id!="imnc",]

# list all ids
table(as.character(d7$short_id))
# number of studies
length(table(d7$short_id))
# list all ids for studies with measurement at least every 3 m
table(as.character(d7$short_id[d7$every3mo==1]))


ss=d7

save(ss,file="U:/Data/Stunting/stunting_studies.RData")

#################################
# HBGD Stunting analysis

# Script to download/load select datasets into R using
# the ghap() package, save as raw rds file

# by Jade Benjamin-Chung (jadebc@berkeley.edu)
#################################
# git clone <grant_url> <directory>
rm(list=ls())
library(dplyr)
library(tidyr)
library(caret)
library(ghap)

set_git_base_path("U:/scripts")
get_git_base_path()

source("U:/Scripts/Stunting/1-dm/0_impute_static_vars.R")
setwd("U:/data/GHAP_data/")

# -------------------------------------
# studies eligible for stunting analysis
# -------------------------------------
d<-use_study("agakhanuniv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="akup_raw.rds") 
d<-impute_static_vars(d)
saveRDS(d, file="akup.rds") 
rm(d)

d<-use_study("burkina_faso_zn")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="bfzn_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="bfzn.rds") 
rm(d)

d<-use_study("cmc_v_bcs_2002") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cmc_raw.rds")  
d<-impute_static_vars(d)
saveRDS(d, file="cmc.rds")  
rm(d)

d<-use_study("cmin")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cmin_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="cmin.rds")
rm(d)

d<-use_study("cohorts")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cort_raw.rds") 
d<-impute_static_vars(d)
saveRDS(d, file="cort.rds") 
rm(d)

d<-use_study("content")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cntt_raw.rds") 
d<-impute_static_vars(d)
saveRDS(d, file="cntt.rds") 
rm(d)

# PROBLEM IMPORTING
d<-use_study("divids")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="dvds_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="dvds.rds") 
rm(d)

d<-use_study("ee")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ee_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="ee.rds")  
rm(d)

d<-use_study("eu")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="eu_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="eu.rds")
rm(d)

d<-use_study("gms_nepal")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gmsn_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="gmsn.rds")
rm(d)

# manual entry due to git problem
# d<-use_study("guatemala_bsc")
d<-read.csv("U:/Scripts/hbgd/ki1112895/PMID17299460/adam/ANTHA.csv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gbsc_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="gbsc.rds")
rm(d)

# manual entry due to git problem
# d<-use_study("irc")
d<-read.csv("U:/Scripts/hbgd/ki1000108/PMC3894229/adam/KI1000108_PMC3894229.csv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="irc_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="irc.rds")  
rm(d)

# manual entry due to git problem
# d<-use_study("jivita_3")
d<-read.csv("U:/Scripts/hbgd/kiGH5241/JiVitA-3/adam/ads_full_KIGH5241_JIVITA.csv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="jvt3_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="jvt3.rds")
rm(d)

# manual entry due to git problem
# d<-use_study("jivita_4")
d<-read.csv("U:/Scripts/hbgd/kiGH5241/JiVitA-4/adam/ads_full_KIGH5241_JIVITA.csv")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="jvt4_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="jvt4.rds")
rm(d)

d<-use_study("keneba")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="knba_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="knba.rds") 
rm(d)

d<-use_study("lcni_5")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="lcn5_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="lcn5.rds") 
rm(d)

d<-use_study("mal_ed")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="mled_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="mled.rds") 
rm(d)

d<-use_study("mal_ed_ext")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="mlex_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="mlex.rds")
rm(d)

d<-use_study("nih_birth")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="nbrt_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="nrbt.rds")
rm(d)

d<-use_study("nih_crypto")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ncry_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="ncry.rds")
rm(d)

d<-use_study("probit")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="prbt_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="prbt.rds")
rm(d)

d<-use_study("prvd")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="prvd_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="nrbt.rds")
rm(d)

d<-use_study("respak")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="rspk_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="rspk.rds")
rm(d)

d<-use_study("sas_compfeed")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="cmpf_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="cmpf.rds")
rm(d)

d<-use_study("sas_foodsuppl")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="fspp_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="fspp.rds")
rm(d)

d<-use_study("tanzaniachild2")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="tzc2_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="tzc2.rds")
rm(d)

d<-use_study("vb12")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="vb12_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="vb12.rds")
rm(d)

d<-use_study("vita")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="vita_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="vita.rds")
rm(d)

d<-use_study("wash_bangladesh")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="wsb_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="wsb.rds")
rm(d)

d<-use_study("wash_kenya")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="wsk_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="wsk.rds")
rm(d)

d<-use_study("zvitambo")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="zvit_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="zvit.rds")
rm(d)

d<-use_study("znmort")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="zmrt_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="zmrt.rds")
rm(d)

d<-use_study("ilins_zinc")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="lnsz_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="lnsz.rds")
rm(d)

d<-use_study("ilins_dose")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ilnd_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="ilnd.rds")
rm(d)

d<-use_study("ilins_dyad_m")
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ildm_raw.rds")
d<-impute_static_vars(d)
saveRDS(d, file="ildm.rds")
rm(d)






# -------------------------------------
# other studies
# -------------------------------------

d<-use_study("zinf") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="zinf_raw.rds")


d<-use_study("gual") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gual_raw.rds")


d<-use_study("ppd") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="ppd_raw.rds")


d<-use_study("mahn") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="mahn_raw.rds")


d<-use_study("incp") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="incp_raw.rds")


d<-use_study("gsto") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gsto_raw.rds")


d<-use_study("gusto")          
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gsto_raw.rds") 


d<-use_study("grip") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="grip_raw.rds")


d<-use_study("gtwn") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="gtwn_raw.rds")


d<-use_study("eczn") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="eczn_raw.rds")


d<-use_study("peru_huascar")   
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="phua_raw.rds") 



d<-use_study("mmam") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="mmam_raw.rds")


d<-use_study("dvds") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="dvds_raw.rds")


d<-use_study("bigu") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="bigu_raw.rds")

d<-use_study("tdc")            
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="tdc_raw.rds")  

d<-use_study("bigcs_ultrasound") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="bigu_raw.rds")


d<-use_study("imnci") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="imnc_raw.rds")

d<-use_study("amanhi") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="amni_raw.rds")

d<-use_study("peru_zn") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="pzn_raw.rds")

d<-use_study("Ecuador Egg") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="eegg_raw.rds")

d<-use_study("Bangladesh Diarrhea") 
colnames(d)<- toupper(colnames(d))
saveRDS(d, file="bngd_raw.rds")












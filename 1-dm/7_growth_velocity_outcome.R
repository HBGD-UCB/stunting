

# FINAL dataset of all studies
library(tidyverse)
library(data.table)
# install.packages("bit64")
library(bit64)

# options(repos = c(CRAN = "http://cran.rstudio.com/",
#  deltarho = "http://packages.deltarho.org"))
# install.packages("growthstandards")
library(growthstandards)

# source("U:/GHAP-Data-Management/HBGDki_functions.R")

# setwd("U:/data")
setwd("/data/ucb-superlearner/Stunting rallies")
gc()

#Read rds file and drop unneeded columns
d<-fread("/data/adhoc/UCB Rally7/Main/adam/FINAL.csv", header = T,
         colClasses = c(SUBJID = "integer64"),
         drop = c( "AGEIMPFL",  
                   # "WTKG",    "HTCM",    "LENCM",       
                   "WHZ",     "BAZ",     "HCAZ",    "MUAZ",    
                   "REGCTRY", "REGCTYP", "CITYTOWN","LATITUDE","LONGITUD", "HHID",    "ARM", 
                   "DEAD",    "AGEDTH",  "CAUSEDTH","FEEDING",
                   "DURBRST", "BRTHYR",  
                   "ENSTUNT",
                   "FWTKG",    "FBMI",
                   "BRFEED", 
                   "SUMEP",   "SUMDIAR", "SUMDAYS",
                   "PCTDIAR", "IMPSAN",  "SOAP",    "SAFEH2O", "TRTH2O",  "CLEANCK",
                   "IMPFLOOR","H2OTIME",
                   "CHICKEN", "COW",     "CATTLE",  "INCTOT", 
                   "INCTOTU", "BFEDFL",  "EXBFEDFL","WEANFL",  "ANMLKFL", "PWMLKFL",
                   "FORMLKFL","BOTTLEFL","H20FEDFL","OTHFEDFL","SLDFEDFL","NBFYES",  "EARLYBF", "CMFDINT", "DIARFL",  "LSSTLFL",
                   "NUMLS",   "BLDSTLFL","DIARFL_R","LSSTFL_R","NUMLS_R", "BLDSTL_R",
                   "DUR_R", 
                   "BRTHWEEK", "BRTHMON", "PARITY", "VAGBRTH", "HDLVRY",
                   "BRTHORDR", "MAGE", "MHTCM", "MWTKG", "MBMI",
                   "MEDUCYRS", "SINGLE", "FAGE", "FHTCM", "FEDUCYRS", "NROOMS", "NHH", "NCHLDLT5", "SES", "HFOODSEC"
                   )
         )

colnames(d) <- tolower(colnames(d))
setkeyv(d, cols = c("country","studyid","subjid"))
gc()

#Drop studies Vishak added to data product that don't meet inclusion criteria
d <- d[d$studyid!="ki1000301-DIVIDS" & d$studyid!="ki1055867-WomenFirst" & d$studyid!="ki1135782-INCAP"]

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

#Mark COHORTS and CMIN cohorts with different measurement frequency than quarterly
d$measurefreq[d$studyid=="ki1114097-CMIN" & d$country=="BANGLADESH"] <- "monthly"
d$measurefreq[d$studyid=="ki1114097-CMIN" & d$country=="PERU"] <- "monthly"
d<- d[!(d$studyid=="ki1135781-COHORTS" & d$country=="BRAZIL"),] #Drop because yearly but not an RCT
d<- d[!(d$studyid=="ki1135781-COHORTS" & d$country=="SOUTH AFRICA"),] #Drop because yearly but not an RCT


#--------------------------------------------------------
# filter out obs with missing sex
# filter out person-time obs with missing both haz & waz
#--------------------------------------------------------
#sex must be "Male" or "Female"
table(d$sex)
#set blank sex to missing
d$sex[d$sex=="" | d$sex=="Unknown"]<-NA
#drop kids missing sex
d <- d[!is.na(sex), ]
# d <- d %>% filter(!is.na(sex)) %>% data.table
#drop if both haz and waz are missing
d <- d[!(is.na(haz) & is.na(waz)), ]
# d[is.na(haz), ]
# d[is.na(waz), ]

#--------------------------------------------------------------------------
# birth characteristics
# separate birthweight and birthlength
# convert to waz / haz and add to main data set as a new row with "agedays=0"
#--------------------------------------------------------------------------
table(d$studyid, is.na(d$birthlen))
table(d$studyid, is.na(d$birthwt))
dblenwt <- d[,list(birthwt=first(birthwt), birthlen=first(birthlen), sex=first(sex)), by = list(studyid, subjid)]
dblenwt <- dblenwt[!(is.na(birthwt) & is.na(birthlen)), ]
dblenwt[is.na(birthlen), ]
dblenwt[is.na(birthwt), ]
dblenwt[, agedays := 0]
dblenwt[, waz := round(who_wtkg2zscore(agedays, birthwt/1000, sex = sex),2)]
dblenwt[, haz := round(who_htcm2zscore(agedays, birthlen, sex = sex),2)]
setkeyv(dblenwt, cols = c("country","studyid", "subjid", "agedays"))

## check things are matching with main haz/waz when agedays=1 was observed
d[subjid==5444, ]
dblenwt[subjid==5444, ]
dblenwt[, birthwt := NULL][, birthlen := NULL]

## merge birth haz / waz into main dataset
d <- merge(d, dblenwt, all=TRUE, by = c("country","studyid", "subjid","sex","agedays"))
setnames(d,c("waz.x","haz.x"),c("waz","haz"))
d[agedays==0, waz := waz.y][, waz.y := NULL]
d[agedays==0, haz := haz.y][, haz.y := NULL]
setkeyv(d, cols = c("country","studyid","subjid","agedays"))

#Drop outlier birth HAZ and WAZ
d[waz < -6 | waz > 6, waz := NA]
d[haz < -6 | haz > 6, haz := NA]


# convert waz / haz back to wtkg / lencm (that way everything is standardized and always matching)
# save actual wtkg / lencm as back-up for comparison
setnames(d, c("wtkg", "lencm"), c("wtkg.orig", "lencm.orig"))
d[agedays==0, wtkg := round(who_zscore2wtkg(agedays, waz, sex = sex),3)]
d[agedays>0, wtkg := round(who_zscore2wtkg(agedays-1, waz, sex = sex),3)]
d[agedays==0, lencm := round(who_zscore2htcm(agedays, haz, sex = sex),1)]
d[agedays>0, lencm := round(who_zscore2htcm(agedays-1, haz, sex = sex),1)]


#--------------------------------------------------------------------------
# calculate velocity between two observational time-points (t1,t2) (e.g., diff in haz divided by months lapsed)
#--------------------------------------------------------------------------
# when exact t is not available, impute:
# take the closest available observations within (t1-/+tgap,t2-/+tgap)
# where tgap is a preset window in days (14)
t1vec = c(0,3,6,12)  ## 1st time-point in months
t2vec = c(3,6,12,24) ## 2nd time-point in months
outvec = c("haz","waz","lencm","wtkg")

# t1mths   ## 1st time-point in months
# t2mths   ## 2nd time-point in months
# tgap     ## number of days around the time-point of interest (measurement time)
# yname    ## outcome
growth_velocity = function(d, t1mths, t2mths, yname = "haz", tgap = 14) {
  daysmth = 30.4167 ## average number of months in a year
  setkeyv(d, cols = c("country","studyid", "subjid", "agedays"))
  t1 <- as.integer(round((daysmth)*t1mths,0))
  t1_int <- c(t1-tgap,t1+tgap)
  t2 <- as.integer(round((daysmth)*t2mths,0))
  t2_int <- c(t2-tgap,t2+tgap)

  d_yt1 <- d[(agedays >= t1_int[1]) & (agedays <= t1_int[2]) & (!is.na(eval(as.name(yname)))), ]
  d_yt2 <- d[(agedays >= t2_int[1]) & (agedays <= t2_int[2]) & (!is.na(eval(as.name(yname)))), ]

  dd_yt1 <- d_yt1[, list(t1agedays = agedays[which.min(abs(t1-agedays))], t1y = eval(as.name(yname))[which.min(abs(t1-agedays))]), by = list(country,studyid,subjid)]
  dd_yt2 <- d_yt2[, list(t2agedays = agedays[which.min(abs(t2-agedays))], t2y = eval(as.name(yname))[which.min(abs(t2-agedays))]), by = list(country,studyid,subjid)]

  ## merge both time-points and auto drop when one of the measurements is missing 
  ## obtain a single dataset where both measurements must be present)
  setkeyv(dd_yt1, cols = c("country","studyid","subjid"))
  setkeyv(dd_yt2, cols = c("country","studyid","subjid"))
  dd_diff <- merge(dd_yt1, dd_yt2, all=FALSE, by=c("country","studyid","subjid"))
  dd_diff[is.na(t1y),]
  dd_diff[is.na(t2y),]
  ## calculate diff in agedays between (t1,t2), convert to months
  ## calculate linear growth velocity (diff in growth divided by lapsed months)
  dd_diff[, 
    "diffdays" := t2agedays - t1agedays][, 
    "diffmths" := diffdays/daysmth][,
    "y_diff" := t2y-t1y][,
    "y_rate" := y_diff/diffmths][,
    "diffcat" := paste0(t1mths,"-",t2mths," months")][,
    "ycat" := yname]

  cat("(t1 - t2) in months: ", t1mths, "-",t2mths, "\n")
  cat("y outcome: ", yname, "\n")
  cat("tgap value: ", tgap, "\n")
  cat("No. of subjects with y avail in window of t1: ", nrow(dd_yt1), "\n") 
  cat("No. of subjects with y avail in window of t2: ", nrow(dd_yt2), "\n") 
  cat("No. of subjects with both y avail (t1,t2): ", nrow(dd_diff), "\n") 

  return(dd_diff)
}

dd_diff = NULL
for (j in 1:length(outvec)) {
  for (i in 1:length(t1vec)) {
    dd_diff_tmp = growth_velocity(d, t1mths = t1vec[i], t2mths = t2vec[i], yname = outvec[j])
    # print(dd_diff_tmp[])
    dd_diff = c(dd_diff, list(dd_diff_tmp))
  }
}
dd_out = rbindlist(dd_diff)
diffcatlevs = paste0(t1vec, "-", t2vec, " months")
dd_out[, "diffcat" := factor(diffcat, levels = diffcatlevs)]
head(dd_out[["diffcat"]])

saveRDS(dd_out, file="velocity_longfmt.rds")

#--------------------------------------------------------------------------
# resave in wide format (diff and rate for each outcome type are separate columns)
#--------------------------------------------------------------------------
dd_out <- readRDS(file="velocity_longfmt.rds")
## cast to wide format:
dd_out_wide <- dcast(dd_out,country+studyid+subjid+diffcat~ycat,value.var=c("y_diff","y_rate"))
saveRDS(dd_out_wide, file="velocity_widefmt.rds")

#--------------------------------------------------------------------------
# add the baseline characteristics to the data (becomes "_rf")
#--------------------------------------------------------------------------
cov<-readRDS("FINAL_temp_clean_covariates.rds")
dd_out <- readRDS(file="velocity_longfmt.rds")
cov$subjid = as.integer64(cov$subjid)
setDT(cov)

dd_out_RF <- left_join(dd_out, cov, by=c("studyid", "subjid", "country"))
dd_out_RF <- data.table(dd_out_RF)
saveRDS(dd_out_RF, file="velocity_longfmt_rf.rds")

dd_out_wide_RF <- left_join(dd_out_wide, cov, by=c("studyid", "subjid", "country"))
dd_out_wide_RF <- data.table(dd_out_wide_RF)
saveRDS(dd_out_wide_RF, file="velocity_widefmt_rf.rds")

dd_out_RF[, list(Mean_rate = mean(y_rate), N = .N), by = list(sex, ycat, diffcat)]
#        sex  ycat      diffcat    Mean_rate     N
#  1: Female   haz   0-3 months -0.086065739 28472
#  2:   Male   haz   0-3 months -0.146099363 29119
#  3: Female   haz   3-6 months  0.004065326 25907
#  4:   Male   haz   3-6 months -0.014515824 26319
#  5: Female   haz  6-12 months -0.047025325 26695
#  6:   Male   haz  6-12 months -0.034587049 27037
#  7: Female   haz 12-24 months -0.032788457 12658
#  8:   Male   haz 12-24 months -0.024167496 13267
#  9: Female   waz   0-3 months  0.025372963 30198
# 10:   Male   waz   0-3 months -0.036246121 31036
# 11: Female   waz   3-6 months  0.052576208 26813
# 12:   Male   waz   3-6 months  0.046247653 27532
# 13: Female   waz  6-12 months -0.012407575 27324
# 14:   Male   waz  6-12 months -0.008473672 27703
# 15: Female   waz 12-24 months -0.026173729 12863
# 16:   Male   waz 12-24 months -0.019970716 13466
#        sex  ycat      diffcat    Mean_rate     N

dat_countr = dd_out_RF[, list(Mean_rate = mean(y_rate), N = .N), by = list(sex, ycat, diffcat, country)]
data.frame(dat_countr[N>500, ])
#        sex  ycat      diffcat                      country    Mean_rate     N
# 1   Female   haz   0-3 months                   BANGLADESH  0.071951729 11903
# 2     Male   haz   0-3 months                   BANGLADESH  0.040419171 12156
# 3   Female   haz   0-3 months                      BELARUS -0.347679271  6534
# 4     Male   haz   0-3 months                      BELARUS -0.532176395  6211
# 5     Male   haz   0-3 months                       GAMBIA -0.297327932   670
# 6   Female   haz   0-3 months                       GAMBIA -0.271483442   606
# 7   Female   haz   0-3 months                        INDIA -0.049612677  3948
# 8     Male   haz   0-3 months                        INDIA -0.069188085  4350
# 9   Female   haz   0-3 months                     ZIMBABWE -0.129356581  3833
# 10    Male   haz   0-3 months                     ZIMBABWE -0.155617218  4031
# 11  Female   haz   3-6 months                   BANGLADESH -0.003307988  8224
# 12    Male   haz   3-6 months                   BANGLADESH -0.007536632  8404
# 13  Female   haz   3-6 months                      BELARUS  0.068584128  6859
# 14    Male   haz   3-6 months                      BELARUS  0.024208236  6450
# 15    Male   haz   3-6 months                       GAMBIA -0.016001826   727
# 16  Female   haz   3-6 months                       GAMBIA -0.017664970   670
# 17  Female   haz   3-6 months                        INDIA -0.051696180  4119
# 18    Male   haz   3-6 months                        INDIA -0.051789266  4459
# 19  Female   haz   3-6 months TANZANIA, UNITED REPUBLIC OF -0.061568316   934
# 20    Male   haz   3-6 months TANZANIA, UNITED REPUBLIC OF -0.103181188   965
# 21  Female   haz   3-6 months                     ZIMBABWE  0.002652603  2894
# 22    Male   haz   3-6 months                     ZIMBABWE -0.004465638  2995
# 23  Female   haz  6-12 months                   BANGLADESH -0.067004770  7434
# 24    Male   haz  6-12 months                   BANGLADESH -0.061532274  7652
# 25  Female   haz  6-12 months                      BELARUS  0.014480308  6680
# 26    Male   haz  6-12 months                      BELARUS  0.072169903  6216
# 27    Male   haz  6-12 months                       GAMBIA -0.063362035   674
# 28  Female   haz  6-12 months                       GAMBIA -0.059322112   632
# 29    Male   haz  6-12 months                    GUATEMALA -0.104078217   544
# 30  Female   haz  6-12 months                        INDIA -0.086574935  4660
# 31    Male   haz  6-12 months                        INDIA -0.083188797  5095
# 32    Male   haz  6-12 months                       MALAWI -0.033157460   995
# 33  Female   haz  6-12 months                       MALAWI -0.031523664  1006
# 34    Male   haz  6-12 months                  PHILIPPINES -0.096729256  1269
# 35  Female   haz  6-12 months                  PHILIPPINES -0.103481135  1150
# 36  Female   haz  6-12 months TANZANIA, UNITED REPUBLIC OF -0.074459576   636
# 37    Male   haz  6-12 months TANZANIA, UNITED REPUBLIC OF -0.067352108   670
# 38  Female   haz  6-12 months                     ZIMBABWE -0.043408664  2363
# 39    Male   haz  6-12 months                     ZIMBABWE -0.055530816  2208
# 40  Female   haz 12-24 months                   BANGLADESH -0.020896823  4888
# 41    Male   haz 12-24 months                   BANGLADESH -0.013587152  5158
# 42  Female   haz 12-24 months                      BELARUS -0.055884013  1303
# 43    Male   haz 12-24 months                      BELARUS -0.029453687  1230
# 44    Male   haz 12-24 months                       GAMBIA -0.018612885   614
# 45  Female   haz 12-24 months                       GAMBIA -0.022658068   561
# 46  Female   haz 12-24 months                        INDIA -0.036612764  2568
# 47    Male   haz 12-24 months                        INDIA -0.031875619  2820
# 48    Male   haz 12-24 months                       MALAWI -0.019612275   640
# 49  Female   haz 12-24 months                       MALAWI -0.026604164   663
# 50  Female   haz 12-24 months                  PHILIPPINES -0.064141887  1080
# 51    Male   haz 12-24 months                  PHILIPPINES -0.051712122  1180

# 52  Female   waz   0-3 months                   BANGLADESH  0.096768628 12084
# 53    Male   waz   0-3 months                   BANGLADESH  0.082978026 12395
# 54  Female   waz   0-3 months                      BELARUS -0.045636304  6543
# 55    Male   waz   0-3 months                      BELARUS -0.271800259  6209
# 56    Male   waz   0-3 months                       GAMBIA -0.676257514   672
# 57  Female   waz   0-3 months                       GAMBIA -0.601020518   609
# 58  Female   waz   0-3 months                        INDIA -0.065960193  4190
# 59    Male   waz   0-3 months                        INDIA -0.073142961  4686
# 60  Female   waz   0-3 months TANZANIA, UNITED REPUBLIC OF  0.022251786  1122
# 61    Male   waz   0-3 months TANZANIA, UNITED REPUBLIC OF -0.008147379  1133
# 62  Female   waz   0-3 months                     ZIMBABWE  0.115024378  3944
# 63    Male   waz   0-3 months                     ZIMBABWE  0.076532152  4148
# 64  Female   waz   3-6 months                   BANGLADESH  0.013053978  8298
# 65    Male   waz   3-6 months                   BANGLADESH  0.001802759  8485
# 66  Female   waz   3-6 months                      BELARUS  0.189813827  6865
# 67    Male   waz   3-6 months                      BELARUS  0.202514025  6454
# 68    Male   waz   3-6 months                       GAMBIA -0.067496728   728
# 69  Female   waz   3-6 months                       GAMBIA -0.045612223   671
# 70  Female   waz   3-6 months                        INDIA  0.028924684  4844
# 71    Male   waz   3-6 months                        INDIA  0.031772329  5458
# 72  Female   waz   3-6 months TANZANIA, UNITED REPUBLIC OF -0.075237119   934
# 73    Male   waz   3-6 months TANZANIA, UNITED REPUBLIC OF -0.090622591   967
# 74  Female   waz   3-6 months                     ZIMBABWE -0.005997897  2914
# 75    Male   waz   3-6 months                     ZIMBABWE -0.015117097  3026
# 76  Female   waz  6-12 months                   BANGLADESH -0.058193198  7579
# 77    Male   waz  6-12 months                   BANGLADESH -0.059207514  7792
# 78  Female   waz  6-12 months                      BELARUS  0.082574430  6685
# 79    Male   waz  6-12 months                      BELARUS  0.130930444  6223
# 80    Male   waz  6-12 months                       GAMBIA -0.088203680   674
# 81  Female   waz  6-12 months                       GAMBIA -0.073601351   632
# 82  Female   waz  6-12 months                    GUATEMALA -0.064375394   503
# 83    Male   waz  6-12 months                    GUATEMALA -0.065675184   548
# 84  Female   waz  6-12 months                        INDIA -0.014317484  5020
# 85    Male   waz  6-12 months                        INDIA -0.019439252  5500
# 86    Male   waz  6-12 months                       MALAWI -0.037532338  1095
# 87  Female   waz  6-12 months                       MALAWI -0.020580310  1112
# 88    Male   waz  6-12 months                  PHILIPPINES -0.081464487  1272
# 89  Female   waz  6-12 months                  PHILIPPINES -0.084640128  1147
# 90  Female   waz  6-12 months TANZANIA, UNITED REPUBLIC OF -0.033968940   638
# 91    Male   waz  6-12 months TANZANIA, UNITED REPUBLIC OF -0.056832475   669
# 92  Female   waz  6-12 months                     ZIMBABWE -0.036057933  2381
# 93    Male   waz  6-12 months                     ZIMBABWE -0.056384095  2220
# 94  Female   waz 12-24 months                   BANGLADESH -0.024816563  5059
# 95    Male   waz 12-24 months                   BANGLADESH -0.019027208  5309
# 96  Female   waz 12-24 months                      BELARUS -0.049765198  1300
# 97    Male   waz 12-24 months                      BELARUS -0.037390738  1235
# 98    Male   waz 12-24 months                       GAMBIA -0.002939979   616
# 99  Female   waz 12-24 months                       GAMBIA -0.012951562   563
# 100 Female   waz 12-24 months                        INDIA -0.020860321  2554
# 101   Male   waz 12-24 months                        INDIA -0.021613525  2807
# 102   Male   waz 12-24 months                       MALAWI -0.017644574   680
# 103 Female   waz 12-24 months                       MALAWI -0.026440094   705
# 104 Female   waz 12-24 months                  PHILIPPINES -0.028203970  1078
# 105   Male   waz 12-24 months                  PHILIPPINES -0.017488925  1184



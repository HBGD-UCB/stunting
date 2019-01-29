



rm(list = ls())
library(tidyverse)
library(metafor)
library(data.table)

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")


load("U:/Data/Wasting/Stunting_inc_data.RData")


dstunt$whz <- dstunt$haz
dstunt_noBW$whz <- dstunt_noBW$haz

dstunt$wast_inc <- dstunt$stunt_inc
dstunt_noBW$wast_inc <- dstunt_noBW$stunt_inc

dstunt$sevwast_inc <- dstunt$sevstunt_inc
dstunt_noBW$sevwast_inc <- dstunt_noBW$sevstunt_inc


#Subset to monthly 
d <- dstunt #%>% filter(measurefreq == "monthly")
d_noBW <- dstunt_noBW #%>% filter(measurefreq == "monthly")



#Prevalence and WHZ  - including yearly studies
d <- calc.prev.agecat(d)
prev.data <- summary.prev(d)
prev.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.prev(.)$prev.res)
prev.Pakistan <- summary.prev(d[d$country=="PAKISTAN",])$prev.res
prev.cohort <-
  prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
  rename(est = prev,  lb = ci.lb,  ub = ci.ub)

prev_year <- bind_rows(
  data.frame(cohort = "pooled", region = "Pakistan", prev.Pakistan),
  data.frame(cohort = "pooled", region = "Overall", prev.data$prev.res),
  data.frame(cohort = "pooled", prev.region),
  prev.cohort
)

#Severe wasting prevalence
sev.prev.data <- summary.prev(d, severe.wasted = T)
sev.prev.region <-
  d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.prev(., severe.wasted = T)$prev.res)
sev.prev.Pakistan <- summary.prev(d[d$country=="PAKISTAN",], severe.wasted = T)$prev.res
sev.prev.cohort <-
  sev.prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
  rename(est = prev,  lb = ci.lb,  ub = ci.ub)

sev.prev_year <- bind_rows(
  data.frame(cohort = "pooled", region = "Pakistan", sev.prev.Pakistan),
  data.frame(cohort = "pooled", region = "Overall", sev.prev.data$prev.res),
  data.frame(cohort = "pooled", sev.prev.region),
  sev.prev.cohort
)

#mean whz
whz.data <- summary.whz(d)
whz.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.whz(.)$whz.res)
whz.Pakistan <- summary.whz(d[d$country=="PAKISTAN",])$whz.res
whz.cohort <-
  whz.data$whz.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  meanwhz,  ci.lb,  ci.ub)) %>%
  rename(est = meanwhz,  lb = ci.lb,  ub = ci.ub)

whz_year <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", whz.data$whz.res),
  data.frame(cohort = "pooled", region = "Pakistan", whz.Pakistan),
  data.frame(cohort = "pooled", whz.region),
  whz.cohort
)





#Drop yearly studies
d <- dstunt %>% filter(measurefreq != "yearly")
d_noBW <- dstunt_noBW %>% filter(measurefreq != "yearly")






#Prevalence and WHZ  - not including yearly studies
d <- calc.prev.agecat(d)
prev.data <- summary.prev(d)
prev.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.prev(.)$prev.res)
prev.Pakistan <- summary.prev(d[d$country=="PAKISTAN",])$prev.res
prev.cohort <-
  prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
  rename(est = prev,  lb = ci.lb,  ub = ci.ub)

prev <- bind_rows(
  data.frame(cohort = "pooled", region = "Pakistan", prev.Pakistan),
  data.frame(cohort = "pooled", region = "Overall", prev.data$prev.res),
  data.frame(cohort = "pooled", prev.region),
  prev.cohort
)

#Severe wasting prevalence
sev.prev.data <- summary.prev(d, severe.wasted = T)
sev.prev.region <-
  d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.prev(., severe.wasted = T)$prev.res)
sev.prev.Pakistan <- summary.prev(d[d$country=="PAKISTAN",], severe.wasted = T)$prev.res
sev.prev.cohort <-
  sev.prev.data$prev.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  prev,  ci.lb,  ci.ub)) %>%
  rename(est = prev,  lb = ci.lb,  ub = ci.ub)

sev.prev <- bind_rows(
  data.frame(cohort = "pooled", region = "Pakistan", sev.prev.Pakistan),
  data.frame(cohort = "pooled", region = "Overall", sev.prev.data$prev.res),
  data.frame(cohort = "pooled", sev.prev.region),
  sev.prev.cohort
)

#mean whz
whz.data <- summary.whz(d)
whz.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.whz(.)$whz.res)
whz.Pakistan <- summary.whz(d[d$country=="PAKISTAN",])$whz.res
whz.cohort <-
  whz.data$whz.cohort %>% subset(., select = c(cohort, region, agecat, nmeas,  meanwhz,  ci.lb,  ci.ub)) %>%
  rename(est = meanwhz,  lb = ci.lb,  ub = ci.ub)

whz <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", whz.data$whz.res),
  data.frame(cohort = "pooled", region = "Pakistan", whz.Pakistan),
  data.frame(cohort = "pooled", whz.region),
  whz.cohort
)







#Cumulative inc
d <- calc.ci.agecat(d, range = 6)
agelst = list("0-6 months", "6-12 months", "12-18 months", "18-24 months")
ci.data <- summary.ci(d, agelist = agelst)
ci.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst)$ci.res)
ci.Pakistan <- summary.ci(d[d$country=="PAKISTAN",], agelist = agelst)$ci.res
ci.cohort <-
  ci.data$ci.cohort %>% subset(., select = c(cohort, region, agecat, nchild,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)


ci <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data$ci.res),
  data.frame(cohort = "pooled", region = "Pakistan", ci.Pakistan),
  data.frame(cohort = "pooled", ci.region),
  ci.cohort
)

#Cumulative inc 3 month intervals
d3 <- calc.ci.agecat(d, range = 3)
agelst3 = list(
  "0-3 months",
  "3-6 months",
  "6-9 months",
  "9-12 months",
  "12-15 months",
  "15-18 months",
  "18-21 months",
  "21-24 months"
)
ci.data3 <- summary.ci(d3, agelist = agelst3)
ci.region3 <- d3 %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst3)$ci.res)
ci.Pakistan3 <- summary.ci(d3[d3$country=="PAKISTAN",], agelist = agelst3)$ci.res
ci.cohort3 <-
  ci.data3$ci.cohort %>% subset(., select = c(cohort, region, agecat, nchild,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)


ci_3 <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data3$ci.res),
  data.frame(cohort = "pooled", region = "Pakistan", ci.Pakistan3),
  data.frame(cohort = "pooled", ci.region3),
  ci.cohort3
)



#Cumulative inc, no birth
d_noBW <- calc.ci.agecat(d_noBW, range = 6)
ci.data.nobirth <- summary.ci(d_noBW, agelist = agelst)
ci.region.nobirth <-
  d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst)$ci.res)
ci.Pakistan.nobirth <- summary.ci(d_noBW[d_noBW$country=="PAKISTAN",], agelist = agelst)$ci.res
ci.cohort.nobirth <-
  ci.data.nobirth$ci.cohort  %>% subset(., select = c(cohort, region, agecat, nchild,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)


ci_nobw <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", ci.data.nobirth$ci.res),
  data.frame(cohort = "pooled", region = "Pakistan", ci.Pakistan.nobirth),
  data.frame(cohort = "pooled", ci.region.nobirth),
  ci.cohort.nobirth
)


#Cumulative inc 3 month intervals
d3 <- calc.ci.agecat(d_noBW, range = 3)
agelst3 = list(
  "0-3 months",
  "3-6 months",
  "6-9 months",
  "9-12 months",
  "12-15 months",
  "15-18 months",
  "18-21 months",
  "21-24 months"
)
ci.data.nobirth3 <- summary.ci(d3, agelist = agelst3)
ci.region.nobirth3 <-
  d3 %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst3)$ci.res)
ci.Pakistan.nobirth3 <- summary.ci(d3[d3$country=="PAKISTAN",], agelist = agelst3)$ci.res
ci.cohort.nobirth3 <-
  ci.data.nobirth3$ci.cohort %>% subset(., select = c(cohort, region, agecat, nchild,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)


ci_nobw3 <- bind_rows(
  data.frame(
    cohort = "pooled",
    region = "Overall",
    ci.data.nobirth3$ci.res
  ),
  data.frame(cohort = "pooled", region = "Pakistan", ci.Pakistan.nobirth3),
  data.frame(cohort = "pooled", ci.region.nobirth3),
  ci.cohort.nobirth3
)


#Cumulative inc of severe wasting
d <- calc.ci.agecat(d, range = 6)
agelst = list("0-6 months", "6-12 months", "12-18 months", "18-24 months")
sev.ci.data <- summary.ci(d, agelist = agelst, severe.wasted = T)
sev.ci.Pakistan <- summary.ci(d[d$country=="PAKISTAN",], agelist = agelst, severe.wasted = T)$ci.res
sev.ci.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ci(., agelist = agelst, severe.wasted = T)$ci.res)
sev.ci.cohort <-
  sev.ci.data$ci.cohort %>% subset(., select = c(cohort, region, agecat, nchild,  yi,  ci.lb,  ci.ub)) %>%
  rename(est = yi,  lb = ci.lb,  ub = ci.ub, nmeas=nchild)


sev.ci <- bind_rows(
  data.frame(cohort = "pooled", region = "Overall", sev.ci.data$ci.res),
  data.frame(cohort = "pooled", region = "Pakistan", sev.ci.Pakistan),
  data.frame(cohort = "pooled", sev.ci.region),
  sev.ci.cohort
)


# 
# #Recovery cumulative inc
# #NOTE: need to make sure to only include those wasted in the denominator
# #rec.data <- summary.ci(d, recovery = T)
# 
# #Incidence rate
# ir.data <- summary.ir(d, agelist = agelst)
# ir.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ir(., agelist = agelst)$ir.res)
# ir.Pakistan <- summary.ir(d[d$country=="PAKISTAN",], agelist = agelst)$ir.res
# ir.cohort <-
#   ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
#   rename(est = yi,  lb = ci.lb,  ub = ci.ub)
# 
# ir <- bind_rows(
#   data.frame(cohort = "pooled", region = "Overall", ir.data$ir.res),
#   data.frame(cohort = "pooled", region = "Pakistan", ir.Pakistan),
#   data.frame(cohort = "pooled", ir.region),
#   ir.cohort
# )
# 
# #Incidence rate - no birth wasting
# ir.data.nobirth <- summary.ir(d_noBW, agelist = agelst)
# ir.region.nobirth <- d_noBW %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ir(., agelist = agelst)$ir.res)
# ir.Pakistan.nobirth <- summary.ir(d_noBW[d_noBW$country=="PAKISTAN",], agelist = agelst)$ir.res
# ir.cohort.nobirth <-
#   ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
#   rename(est = yi,  lb = ci.lb,  ub = ci.ub)
# 
# ir_noBW <- bind_rows(
#   data.frame(cohort = "pooled", region = "Overall", ir.data$ir.res),
#   data.frame(cohort = "pooled", region = "Pakistan", ir.Pakistan.nobirth),
#   data.frame(cohort = "pooled", ir.region),
#   ci.cohort
# )
# 
# #Incidence rate - severe wasting
# sev.ir.data <- summary.ir(d, sev.wasting = T, agelist = agelst)
# sev.ir.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.ir(., agelist = agelst, sev.wasting = T)$ir.res)
# sev.ir.Pakistan <- summary.ir(d[d$country=="PAKISTAN",], agelist = agelst, sev.wasting = T)$ir.res
# sev.ir.cohort <-
#   sev.ir.data$ir.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
#   rename(est = yi,  lb = ci.lb,  ub = ci.ub)
# 
# sev.ir <- bind_rows(
#   data.frame(cohort = "pooled", region = "Overall", sev.ci.data$ci.res),
#   data.frame(cohort = "pooled", region = "Pakistan", ir.Pakistan.nobirth),
#   data.frame(cohort = "pooled", sev.ci.region),
#   sev.ci.cohort
# )
# 
# 
# #Recovery incidence rate
# #rec.ir.data <- summary.ir(d, recovery = T)
# 
# #Recovery incidence rate - no birth wasting
# #rec.ir.data.d_noBW <- summary.ir(d_noBW, recovery = T)
# 
# #Duration
# #dur.data <- summary.dur(d)
# 
# #Recovery within 30, 60, 90 days
# d <- calc.ci.agecat(d, range = 6)
# rec.data30 <- summary.rec60( d, length = 30, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))
# rec.data60 <- summary.rec60( d, length = 60, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))
# rec.data90 <- summary.rec60( d, length = 90, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))
# rec30.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.rec60( d, length = 30, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$ci.res)
# rec60.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.rec60( d, length = 60, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$ci.res)
# rec90.region <- d %>% filter(country!="PAKISTAN") %>% group_by(region) %>% do(summary.rec60( d, length = 90, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$ci.res)
# rec.data30.Pakistan <- summary.rec60( d[d$country=="PAKISTAN",], length = 30, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$ci.res
# rec.data60.Pakistan <- summary.rec60( d[d$country=="PAKISTAN",], length = 60, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$ci.res
# rec.data90.Pakistan <- summary.rec60( d[d$country=="PAKISTAN",], length = 90, agelist = c("0-6 months", "6-12 months", "12-18 months", "18-24 months"))$ci.res
# 
# rec30.cohort <-
#   rec.data30$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
#   rename(est = yi,  lb = ci.lb,  ub = ci.ub)
# rec60.cohort <-
#   rec.data60$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
#   rename(est = yi,  lb = ci.lb,  ub = ci.ub)
# rec90.cohort <-
#   rec.data90$ci.cohort %>% subset(., select = c(cohort, region, agecat,  yi,  ci.lb,  ci.ub)) %>%
#   rename(est = yi,  lb = ci.lb,  ub = ci.ub)
# 
# rec30<- bind_rows(
#   data.frame(cohort = "pooled", region = "Overall", rec.data30$ci.res),
#   data.frame(cohort = "pooled", region = "Pakistan", rec.data30.Pakistan),
#   data.frame(cohort = "pooled", rec30.region),
#   rec30.cohort
# )
# rec60<- bind_rows(
#   data.frame(cohort = "pooled", region = "Overall", rec.data60$ci.res),
#   data.frame(cohort = "pooled", region = "Pakistan", rec.data60.Pakistan),
#   data.frame(cohort = "pooled", rec60.region),
#   rec60.cohort
# )
# rec90<- bind_rows(
#   data.frame(cohort = "pooled", region = "Overall", rec.data90$ci.res),
#   data.frame(cohort = "pooled", region = "Pakistan", rec.data90.Pakistan),
#   data.frame(cohort = "pooled", rec90.region),
#   rec90.cohort
# )
# 





shiny_desc_data <- bind_rows(
  data.frame(outcome = "Stunting prevalence", prev),
  data.frame(outcome = "Severe stunting prevalence", sev.prev),
  data.frame(outcome = "Mean LAZ",  whz),
  data.frame(outcome = "Stunting prevalence - including yearly studies", prev_year),
  data.frame(outcome = "Severe stunting prevalence - including yearly studies", sev.prev_year),
  data.frame(outcome = "Mean LAZ - including yearly studies",  whz_year),
  data.frame(outcome = "Cumulative incidence", ci),
  data.frame(outcome = "Cumulative incidence - 3 month range", ci_3),
  data.frame(outcome = "Cumulative incidence - no birth stunting",  ci_nobw),
  data.frame(outcome = "Cumulative incidence - no birth stunting, 3 month range",  ci_nobw3),
  data.frame(outcome = "Cumulative incidence - severe stunting",  sev.ci))



shiny_desc_data <- shiny_desc_data %>% subset(., select = -c(se, nmeas.f,  ptest.f))

unique(shiny_desc_data$agecat)
shiny_desc_data$agecat <- factor(shiny_desc_data$agecat, levels=unique(shiny_desc_data$agecat))

unique(shiny_desc_data$region)
shiny_desc_data$region <- factor(shiny_desc_data$region, levels=c("Overall", "Africa", "Latin America", "Asia", "Pakistan"))


#Fix where CI has been coverted to % but the point estimate hasn't
#shiny_desc_data$est[abs(shiny_desc_data$est) < abs(shiny_desc_data$lb)] <- shiny_desc_data$est[abs(shiny_desc_data$est) < abs(shiny_desc_data$lb)] * 100
shiny_desc_data$est[(shiny_desc_data$est < shiny_desc_data$lb & shiny_desc_data$est < shiny_desc_data$ub) | (shiny_desc_data$est > shiny_desc_data$lb & shiny_desc_data$est > shiny_desc_data$ub)] <- 
  shiny_desc_data$est[(shiny_desc_data$est < shiny_desc_data$lb & shiny_desc_data$est < shiny_desc_data$ub) | (shiny_desc_data$est > shiny_desc_data$lb & shiny_desc_data$est > shiny_desc_data$ub)] * 100

save(shiny_desc_data, file = "U:/Data/Wasting/shiny_desc_data_stunting.Rdata")




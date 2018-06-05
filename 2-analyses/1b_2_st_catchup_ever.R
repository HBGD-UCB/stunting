#-----------------------------------
# Stunting analysis
# Objective 1b
# Catch up growth 

# Cohort specific estimates & 
# Pooled estimates using random effects

# What is the proportion of 
# children who were ever stunted within the first 2 years of life, 
# who are stunted (or not) by 24, 36, and 48 mos. of age?
#-----------------------------------
rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load base functions
source("U:/Scripts/Stunting/2-analyses/0_st_basefunctions.R")

load("U:/Data/Stunting/stunting_data.RData")

# subset to monthly data
d <- d %>% filter(measurefreq=="monthly")

# create age in months
d <- d %>% mutate(agem=agedays/30.4167)

# sort data
d <- d %>% arrange(studyid, country, subjid, agedays)

# make subjid a character
d <- d %>% 
  ungroup() %>%
  mutate(subjid=as.character(subjid))

# define age windows incidence
d = d %>%
  mutate(agem=agedays/30.4167) %>%
  mutate(agecat=ifelse(agedays==1,"Birth",
                       ifelse(agedays>1 & agedays<=3*30.4167,"3 months",
                              ifelse(agedays>3*30.4167 & agedays<=6*30.4167,"6 months",
                                     ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                                            ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                                                   ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months",""))))))) %>%
  mutate(agecat=factor(agecat,levels=c("Birth","3 months","6 months",
        "12 months","18 months","24 months",""))) %>%
  mutate(age24=ifelse(agem<=25,1,0),
         age36=ifelse(agem<=37,1,0),
         age48=ifelse(agem<=49,1,0),
         age60=ifelse(agem<=61,1,0))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167,na.rm=TRUE),
            max=max(agedays/30.4167))

# indicator for stunted
rev <- d %>%
  group_by(studyid,country,subjid) %>%
  mutate(measid=seq_along(subjid))  %>%
  mutate(stunted=ifelse(haz< -2,1,0),
         lagstunted=lag(stunted),
         leadstunted=lead(stunted))  %>%
  # unique stunting episode
  mutate(sepisode=ifelse(lagstunted==0 & stunted==1 & leadstunted==1 |
                           stunted==1 & measid==1,1,0)) 

# stunted by 24 months
stunt.24 <- rev %>%
  filter(agem<=25) %>%
  group_by(studyid,country,subjid) %>%
  summarise(maxst=max(sepisode)) %>%
  mutate(stunted24=ifelse(maxst==1,1,0))

# recovery by x months
rec <- rev %>%
  # mutate(agecat=droplevels(agecat)) %>%
  arrange(studyid,country,subjid,agecat,agedays) %>%
  # indicator for whether haz in time t> haz in time t-1
  mutate(hazinc=ifelse(haz>lag(haz),1,0)) %>%
  # create recovery indicator
  # NA means that it was the age cat of first measurement 
  mutate(recrow=ifelse(stunted==0 & lagstunted==1 & lag(lagstunted)==1,
                       1,0)) %>%
  # cumulative sum of recovery indicator
  group_by(studyid,country,subjid,agecat) %>%
  mutate(notst=ifelse(stunted==0,1,0)) %>%
  mutate(recsum=cumsum(notst)) %>%
  # assess whether recsum is for contiguous rec indicators
  mutate(contig = ifelse(lag(recrow)==1,1,0)) %>%
  # count as recovery if at least two meas have haz>=-2
  mutate(rec=ifelse(recsum>=2 & contig==1 &
        notst==1,1,0)) 

# count observations in each age bin
# because of the small n's for older ages, just focusing
# on 24 months
rec %>% ungroup() %>% filter(agem<=24) %>% summarise(n24=n())
rec %>% ungroup() %>% filter(agem>24 & agem<=36) %>% summarise(n36=n())
rec %>% ungroup() %>% filter(agem>36 & agem<=48) %>% summarise(n48=n())
rec %>% ungroup() %>% filter(agem>48 & agem<=60) %>% summarise(n60=n())

rec.24<- rec %>%
  ungroup() %>%
  filter(age24==1) %>%
  select(studyid,country,subjid,haz,agem,rec)

rs.24<- full_join(stunt.24,rec.24,by=c("studyid","country","subjid")) %>%
  group_by(studyid,country,subjid) %>%
  summarise(st=max(stunted24),rec=max(rec)) %>%
  mutate(str=ifelse(st==1 & rec==1,1,0))

# cohort specific means
rec.data=rs.24 %>%
  group_by(studyid,country) %>%
  summarise(n=length(subjid[str==1]),
            N=n()) %>%
  mutate(agecat=as.factor("24 months"))

rec.cohort=fit.escalc(data=rec.data,ni="N", xi="n",age="24 months",meas="PR")
rec.cohort=cohort.format(rec.cohort, y=rec.cohort$yi,
    lab="24 months")



# estimate random effects, format results
fit=rma(ni = rec.data$N, xi = rec.data$n, method = "REML", 
            measure = "PR")
rev.res = rec.data %>%
  ungroup() %>%
  summarise(nstudies=length(unique(studyid)),
          nmeas=sum(rec.data$N)) %>%
  mutate(agecat="24 months",est=fit$beta, se=fit$se, lb=fit$ci.lb, ub=fit$ci.ub,
         nmeas.f=paste0("N=",format(sum(rec.data$N),big.mark=",",scientific=FALSE),
                        " children"),
         nstudy.f=paste0("N=",nstudies," studies"))


# add the pooled result to the cohort plot
pooled= rev.res %>% select(est,lb,ub) %>%
  rename(y=est, ci.lb=lb,ci.ub=ub) %>%
  mutate(cohort="Pooled") %>%
  select(cohort,y,ci.lb,ci.ub)
cohort=rec.cohort %>%
  select(cohort,y,ci.lb,ci.ub) %>%
  mutate(cohort=as.character(cohort))

plot.df=bind_rows(as.data.frame(pooled),cohort)

# sort by recovery %
plot.df$cohort=factor(plot.df$cohort, 
      levels = plot.df$cohort[order(plot.df$y)])
plot.df$y[plot.df$cohort=="Pooled"]=plot.df$y[plot.df$cohort=="Pooled"]*100
plot.df$ci.lb[plot.df$cohort=="Pooled"]=plot.df$ci.lb[plot.df$cohort=="Pooled"]*100
plot.df$ci.ub[plot.df$cohort=="Pooled"]=plot.df$ci.ub[plot.df$cohort=="Pooled"]*100
plot.df$pooled=as.factor(ifelse(plot.df$cohort=="Pooled",1,0))

# plot recovery
pdf("U:/Figures/stunting-rec24.pdf",width=8,height=4,onefile=TRUE)
ggplot(plot.df,aes(y=y,x=cohort))+
  geom_point(aes(shape=pooled),size=2)+coord_flip()+
  geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(0,100))+
  scale_shape_manual("",values=c(16,15),guide=FALSE)+
  xlab("Cohort")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who became stunted and\nrecovered within 24 months")
dev.off()




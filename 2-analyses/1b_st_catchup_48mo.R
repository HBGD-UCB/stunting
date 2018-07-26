#What is the proportion of children who were ever stunted within the first 2 years of life, 
#who are stunted (or not) at 24, 36, and 48 mos. of age?



#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth

# Cohort specific estimates & 
# Pooled estimates using random effects

# create dataset for risk factor analyses: 
# stunting from birth to 3 months, recover by 24 months

# Not used for descriptive analyses
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

#Examine the number of obs > 24months by study

d %>% filter(agedays > 30.4167 * 24) %>% group_by(studyid, country, subjid) %>%
  summarize(N=n()) %>% group_by(studyid, country) %>% summarize(Nobs=sum(N), Nchild=n())

# convert subjid to character
d <- d %>% 
  ungroup() %>%
  mutate(subjid=as.character(subjid))

# subset to monthly data
d <- d %>% filter(measurefreq=="monthly")

# create age in months
d <- d %>% mutate(agem=agedays/30.4167)

# sort data
d <- d %>% arrange(studyid, country, subjid, agedays)

d <- d %>% group_by(studyid,country,subjid)

# identify whether child had stunting episode by 24 months 
stunt.24 <- d %>%
  filter(agem<=24) %>%
  # create stunting indicator
  mutate(measid=seq_along(subjid))  %>%
  mutate(stunted=ifelse(haz< -2,1,0),
         lagstunted=lag(stunted),
         leadstunted=lead(stunted))  %>%
  # unique stunting episode
  mutate(sepisode=ifelse(lagstunted==0 & stunted==1 & leadstunted==1 |
                           stunted==1 & measid==1,1,0)) %>%
  summarise(stunted24=max(sepisode,na.rm=TRUE))

#Recovery by 30 months
rec.30 <- d %>%
  filter(agem>25 & agem<=31) %>%
  # identify last two measurements prior to 48 months
  group_by(studyid,country,subjid) %>%
  mutate(rank=min_rank(-agedays)) %>%
  # keep last two measurements 
  filter(rank<= 2) %>%
  # flag kids with 2 measurements not stunted
  mutate(rec=ifelse(haz>= -2,1,0)) %>%
  mutate(recsum=cumsum(rec)) %>%
  # one row for each kid, indicator for recovered
  summarise(maxrec=max(recsum)) %>%
  mutate(rec30=ifelse(maxrec==2,1,0)) %>%
  select(-c(maxrec))

#Recovery by 36 months
rec.36 <- d %>%
  filter(agem>31 & agem<=37) %>%
  # identify last two measurements prior to 48 months
  group_by(studyid,country,subjid) %>%
  mutate(rank=min_rank(-agedays)) %>%
  # keep last two measurements 
  filter(rank<= 2) %>%
  # flag kids with 2 measurements not stunted
  mutate(rec=ifelse(haz>= -2,1,0)) %>%
  mutate(recsum=cumsum(rec)) %>%
  # one row for each kid, indicator for recovered
  summarise(maxrec=max(recsum)) %>%
  mutate(rec36=ifelse(maxrec==2,1,0)) %>%
  select(-c(maxrec))

#Recovery by 42 months
rec.42 <- d %>%
  filter(agem>37 & agem<=43) %>%
  # identify last two measurements prior to 48 months
  group_by(studyid,country,subjid) %>%
  mutate(rank=min_rank(-agedays)) %>%
  # keep last two measurements 
  filter(rank<= 2) %>%
  # flag kids with 2 measurements not stunted
  mutate(rec=ifelse(haz>= -2,1,0)) %>%
  mutate(recsum=cumsum(rec)) %>%
  # one row for each kid, indicator for recovered
  summarise(maxrec=max(recsum)) %>%
  mutate(rec42=ifelse(maxrec==2,1,0)) %>%
  select(-c(maxrec))

#Recovery by 48 months
rec.48 <- d %>%
  filter(agem>43 & agem<=49) %>%
  # identify last two measurements prior to 48 months
  group_by(studyid,country,subjid) %>%
  mutate(rank=min_rank(-agedays)) %>%
  # keep last two measurements 
  filter(rank<= 2) %>%
  # flag kids with 2 measurements not stunted
  mutate(rec=ifelse(haz>= -2,1,0)) %>%
  mutate(recsum=cumsum(rec)) %>%
  # one row for each kid, indicator for recovered
  summarise(maxrec=max(recsum)) %>%
  mutate(rec48=ifelse(maxrec==2,1,0)) %>%
  select(-c(maxrec))

rev <- full_join(stunt.24, rec.30,by=c("studyid","country","subjid"))
rev <- full_join(rev, rec.36,by=c("studyid","country","subjid"))
rev <- full_join(rev, rec.42,by=c("studyid","country","subjid"))
rev <- full_join(rev, rec.48,by=c("studyid","country","subjid"))

rev <- rev %>% filter(!is.na(rec30) | !is.na(rec36) | !is.na(rec42) | !is.na(rec48))

head(rev)

table(rev$studyid)


rev <- rev %>%
  # subset to kids who were stunted
  filter(stunted24==1) %>%
  mutate(srec30=ifelse(stunted24==1 & rec30==1,1,0),
         snorec30=ifelse(stunted24==1 & rec30==0,1,0),
         srec36=ifelse(stunted24==1 & rec36==1,1,0),
         snorec36=ifelse(stunted24==1 & rec36==0,1,0),
         srec42=ifelse(stunted24==1 & rec42==1,1,0),
         snorec42=ifelse(stunted24==1 & rec42==0,1,0),
         srec48=ifelse(stunted24==1 & rec48==1,1,0),
         snorec48=ifelse(stunted24==1 & rec48==0,1,0))

#rev = rev %>% mutate(stunted24s=ifelse(stunted24==1,"Stunted","Not stunted"))
#rev = rev %>% mutate(rec24s=ifelse(rec24==1,"Recover","Not recover"))
#prop.table(table(rev$stunted24s,rev$rec24s))

# prepare data for pooling 
rev.data30 <- rev %>%
  group_by(studyid,country) %>%
  summarise(mn.r=mean(srec30,na.rm=TRUE),
            n.r=sum(srec30,na.rm=TRUE),
            N.r=sum(!is.na(srec30)),
            mn.s=mean(snorec30,na.rm=TRUE),
            n.s=sum(snorec30,na.rm=TRUE),
            N.s=sum(!is.na(snorec30))) %>%
  mutate(agecat="30 months")


rev.data36 <- rev %>%
  group_by(studyid,country) %>%
  summarise(mn.r=mean(srec36,na.rm=TRUE),
            n.r=sum(srec36,na.rm=TRUE),
            N.r=sum(!is.na(srec36)),
            mn.s=mean(snorec36,na.rm=TRUE),
            n.s=sum(snorec36,na.rm=TRUE),
            N.s=sum(!is.na(snorec36))) %>%
  mutate(agecat="36 months")


rev.data42 <- rev %>%
  group_by(studyid,country) %>%
  summarise(mn.r=mean(srec42,na.rm=TRUE),
            n.r=sum(srec42,na.rm=TRUE),
            N.r=sum(!is.na(srec42)),
            mn.s=mean(snorec42,na.rm=TRUE),
            n.s=sum(snorec42,na.rm=TRUE),
            N.s=sum(!is.na(snorec42))) %>%
  mutate(agecat="42 months")


rev.data48 <- rev %>%
  group_by(studyid,country) %>%
  summarise(mn.r=mean(srec48,na.rm=TRUE),
            n.r=sum(srec48,na.rm=TRUE),
            N.r=sum(!is.na(srec48)),
            mn.s=mean(snorec48,na.rm=TRUE),
            n.s=sum(snorec48,na.rm=TRUE),
            N.s=sum(!is.na(snorec48))) %>%
  mutate(agecat="48 months")

revlist<-list(rev.data30, rev.data36, rev.data42, rev.data48)

agecats <- c("30 months", "36 months", "42 months", "48 months")



# estimate random effects, format results
cohort_list <- list()
rev.res.df <- NULL
s.res.df <- NULL
for(i in 1:4){
  
  fit.r=rma(ni=revlist[[i]]$N.r, xi=revlist[[i]]$n.r, 
            method="REML", measure="PR")
  rev.res = revlist[[i]] %>%
    ungroup() %>%
    summarise(nstudies=length(unique(studyid)),
              nmeas=sum(rev.data30$N.r)) %>%
    mutate(agecat=agecats[i],est=fit.r$beta, se=fit.r$se, lb=fit.r$ci.lb, ub=fit.r$ci.ub,
           nmeas.f=paste0("N=",format(sum(revlist[[i]]$N.r),big.mark=",",scientific=FALSE),
                          " children"),
           nstudy.f=paste0("N=",nstudies," studies"))
  
  fit.s=rma(ni=revlist[[i]]$N.s, xi=revlist[[i]]$n.s, 
            method="REML", measure="PR")
  s.res = revlist[[i]] %>%
    ungroup() %>%
    summarise(nstudies=length(unique(studyid)),
              nmeas=sum(revlist[[i]]$N.s)) %>%
    mutate(agecat=agecats[i],est=fit.s$beta, se=fit.s$se, lb=fit.s$ci.lb, ub=fit.s$ci.ub,
           nmeas.f=paste0("N=",format(sum(revlist[[i]]$N.s),big.mark=",",scientific=FALSE),
                          " children"),
           nstudy.f=paste0("N=",nstudies," studies"))

  # rec.cohort=fit.escalc(data=revlist[[i]],ni="N", xi="n",age="24 months",meas="PR")
  # rec.cohort=cohort.format(rec.cohort, y=rec.cohort$yi,
  #                          lab=agecats[i])
  # cohort_list[[i]] <- rec.cohort
  rev.res.df <- rbind(rev.res.df, rev.res)
  s.res.df <- rbind(s.res.df, s.res)
}







# # add the pooled result to the cohort plot
# pooled= rev.res %>% select(est,lb,ub) %>%
#   rename(y=est, ci.lb=lb,ci.ub=ub) %>%
#   mutate(cohort="Pooled") %>%
#   select(cohort,y,ci.lb,ci.ub)
# cohort=rec.cohort %>%
#   select(cohort,y,ci.lb,ci.ub) %>%
#   mutate(cohort=as.character(cohort))
# 
# plot.df=bind_rows(as.data.frame(pooled),cohort)
# 
# # sort by recovery %
# plot.df$cohort=factor(plot.df$cohort, 
#                       levels = plot.df$cohort[order(plot.df$y)])
# plot.df$y[plot.df$cohort=="Pooled"]=plot.df$y[plot.df$cohort=="Pooled"]*100
# plot.df$ci.lb[plot.df$cohort=="Pooled"]=plot.df$ci.lb[plot.df$cohort=="Pooled"]*100
# plot.df$ci.ub[plot.df$cohort=="Pooled"]=plot.df$ci.ub[plot.df$cohort=="Pooled"]*100
# plot.df$pooled=as.factor(ifelse(plot.df$cohort=="Pooled",1,0))
# 
# # plot recovery
# #pdf("U:/Figures/stunting-rec30.pdf",width=8,height=4,onefile=TRUE)
# ggplot(plot.df,aes(y=y,x=cohort))+
#   geom_point(aes(shape=pooled),size=2)+coord_flip()+
#   geom_linerange(aes(ymin=ci.lb,ymax=ci.ub),
#                  size=2,alpha=0.3) +
#   scale_y_continuous(limits=c(0,25))+
#   scale_shape_manual("",values=c(16,15),guide=FALSE)+
#   xlab("Cohort")+
#   ylab("Percentage (95% CI)")+
#   ggtitle("Percentage of children who became stunted and\nrecovered within 24 months")
# #dev.off()



pdf("U:/Figures/stunting-rec48.pdf",width=8,height=4,onefile=TRUE)
ggplot(rev.res.df,aes(y=est*100,x=agecat))+
  geom_point(size=2)+#coord_flip()+
  geom_linerange(aes(ymin=lb*100,ymax=ub*100),
                 size=2,alpha=0.3) +
  geom_text(aes(x=agecat, y=48, label=nmeas.f)) +
  scale_y_continuous(limits=c(-1,50))+
  scale_shape_manual("",values=c(16,15),guide=FALSE)+
  xlab("Child age")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who became stunted within 24 months who recover by different ages")
dev.off()


pdf("U:/Figures/stunting-no-rec48.pdf",width=8,height=4,onefile=TRUE)
ggplot(s.res.df,aes(y=est*100,x=agecat))+
  geom_point(size=2)+#coord_flip()+
  geom_linerange(aes(ymin=lb*100,ymax=ub*100),
                 size=2,alpha=0.3) +
  geom_text(aes(x=agecat, y=48, label=nmeas.f)) +
  scale_y_continuous(limits=c(40,101))+
  scale_shape_manual("",values=c(16,15),guide=FALSE)+
  xlab("Child age")+
  ylab("Percentage (95% CI)")+
  ggtitle("Percentage of children who became stunted within\n24 months who remain stunted by different ages")
dev.off()
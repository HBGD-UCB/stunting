#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate incidence at
# 3, 6, 12, 18, and 24 mo of age

# Incidence rate pooled using random effects
#-----------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load random effects function
source("U:/Scripts/Stunting/2-analyses/0_st_basefunctions.R")

load("U:/Data/Stunting/stunting_data.RData")


# define age windows
d = d %>% 
  mutate(agecat=ifelse(agedays<=3*30.4167,"3 months",
                       ifelse(agedays>3*30.4167 & agedays<=6*30.4167,"6 months",
                              ifelse(agedays>6*30.4167 & agedays<=12*30.4167,"12 months",
                                     ifelse(agedays>12*30.4167 & agedays<=18*30.4167,"18 months",
                                            ifelse(agedays>12*30.4167& agedays<=24*30.4167,"24 months","")))))) %>%
  mutate(agecat=factor(agecat,levels=c("3 months","6 months","12 months","18 months","24 months")))

# check age categories
d %>%
  group_by(agecat) %>%
  summarise(n=sum(!is.na(agedays)),
            min=min(agedays/30.4167),
            mean=mean(agedays/30.4167),
            max=max(agedays/30.4167))

# ---------------------------------------
# flag incident cases and define risk set
# ---------------------------------------
inc.prep = d %>%
  filter(!is.na(agecat) & agedays>1) %>%
  group_by(studyid,subjid) %>%
  arrange(studyid,subjid,agedays) %>%
  
  # create id for measurement within person
  mutate(measid=seq_along(subjid)) %>%
  # duration between measurements 
  mutate(agedayslag=lag(agedays)) %>%
  mutate(agedayslag=ifelse(is.na(agedayslag),0,agedayslag)) %>%
  mutate(deltat=ifelse(measid==1 & agecat=="Birth",0,agedays-agedayslag)) %>%

  # create indicator for whether haz at t < haz at t-1
  mutate(hazlag=lag(haz)) %>%
  mutate(newcase=ifelse(measid==1,ifelse(haz< -2, 1,0),
                    ifelse(hazlag>= -2 & haz< -2,1,0))) %>%
  mutate(newcaselag=lag(newcase))%>%
  mutate(newcaselag=ifelse(measid==1,0,newcaselag))%>%
  mutate(cnewcaselag=cumsum(newcaselag)) %>%
  
  # create at risk variable
  mutate(atrisk=ifelse(cnewcaselag>=1,0,1)) %>%
  # create inc case variable
  mutate(inccase=ifelse(cnewcaselag>=1,0,newcase)) %>%
  
  # create delta t with half interval for row
  # with incident case assuming it occurred halfway through
  # the follow-up period
  mutate(deltat_half=deltat/2) %>%
  mutate(deltat2=ifelse(inccase==0,deltat,deltat_half)) %>%

  # create person days
  mutate(pdays=atrisk*deltat2) %>%
    
  # clean up
  select(-c(hazlag,newcase,newcaselag, cnewcaselag,agedayslag,
            deltat,deltat_half))

# manually calculate incident cases, person-time at risk at each time point
inc.prep %>%
  group_by(agecat) %>%
  summarise(inc.case=sum(inccase),ptar=sum(pdays)) %>%
  mutate(cruderate=inc.case/ptar)


# count incident cases and sum person time at risk per study by age
# exclude time points if number of children per age
# in a study is <50  
inc.data = inc.prep %>%
  group_by(studyid,country,agecat) %>%
  summarise(ptar=sum(pdays),
            ncase=sum(inccase),
            nchild=length(unique(subjid)),
            nstudy=length(unique(studyid))) %>%
  filter(nchild>=50)


# cohort specific results
inc.cohort=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x) 
  fit.escalc(data=inc.data,ni="ptar", xi="ncase",age=x,meas="IR"))
inc.cohort=as.data.frame(do.call(rbind, inc.cohort))
inc.cohort$agecat=factor(inc.cohort$agecat,levels=
                           c("3 months","6 months","12 months","18 months","24 months"))
inc.cohort$yi.f=sprintf("%0.0f",inc.cohort$yi)
inc.cohort$cohort=paste0(inc.cohort$studyid,"-",inc.cohort$country)
inc.cohort = inc.cohort %>% mutate(region = ifelse(country=="BANGLADESH" | country=="INDIA"|
                                                     country=="NEPAL" | country=="PAKISTAN"|
                                                     country=="PHILIPPINES" ,"Asia",
                                                   ifelse(country=="BURKINA FASO"|
                                                            country=="GUINEA-BISSAU"|
                                                            country=="MALAWI"|
                                                            country=="SOUTH AFRICA"|
                                                            country=="TANZANIA, UNITED REPUBLIC OF"|
                                                            country=="ZIMBABWE"|
                                                            country=="GAMBIA","Africa",
                                                          ifelse(country=="BELARUS","Europe",
                                                                 "Latin America"))))
inc.cohort <- inc.cohort %>% 
  mutate(age.f=ifelse(agecat=="3 months","0-3m",
                      ifelse(agecat=="6 months","4-6m",
                             ifelse(agecat=="12 months","7-12m",
                                    ifelse(agecat=="18 months","13-18m","19-24m")))))
inc.cohort$age.f=factor(inc.cohort$age.f,levels=
                          c("0-3m","4-6m","7-12m","13-18m","19-24m"))

    
# estimate random effects, format results
ir.res=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x)
  fit.rma(data=inc.data,ni="ptar", xi="ncase",age=x,measure="IR",nlab=" person-days"))
ir.res=as.data.frame(do.call(rbind, ir.res))
ir.res[,4]=as.numeric(ir.res[,4])
ir.res$agecat.f=as.factor(ifelse(ir.res$agecat=="3 months","0-3 months",
                                 ifelse(ir.res$agecat=="6 months","4-6 months",
                                        ifelse(ir.res$agecat=="12 months","7-12 months",
                                               ifelse(ir.res$agecat=="18 months","13-18 months","19-24 months")))))
ir.res$agecat.f=factor(ir.res$agecat.f,levels=c("0-3 months","4-6 months",
                                                "7-12 months","13-18 months","19-24 months"))


ir.res
ir.res$pt.f=paste0("N=",format(ir.res$nmeas,big.mark=",",scientific=FALSE),
                  " person-days")
ir.res$ptest.f=sprintf("%0.02f",ir.res$est*1000)



# plot cohort incidence
lab.af=inc.cohort[inc.cohort$region=="Africa",] %>% group_by(cohort) %>% summarise(N=sum(nchild))
lab.af.f=paste0("N=",lab.af$N)

pdf("U:/Figures/stunting-inc-africa.pdf",width=12,height=6,onefile=TRUE)
ggplot(inc.cohort[inc.cohort$region=="Africa",],
       aes(y=yi*1000,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb*1000,ymax=ci.ub*1000),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(-0.1,22))+
  xlab("Age category")+
  ylab("Cumulative incidence (95% CI)")+
  ggtitle("Cohort-specific stunting rate - Africa")+
  annotate("text", x=4.5,y=20,label=lab.af.f,size=4)
dev.off()

lab.lae=inc.cohort[inc.cohort$region=="Latin America"|  inc.cohort$region=="Europe",] %>% 
  group_by(cohort) %>% summarise(N=sum(nchild))
lab.lae.f=paste0("N=",lab.lae$N)

pdf("U:/Figures/stunting-inc-latamer-eur.pdf",width=12,height=6,onefile=TRUE)
ggplot(inc.cohort[inc.cohort$region=="Latin America"|
                    inc.cohort$region=="Europe",],
       aes(y=yi*1000,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb*1000,ymax=ci.ub*1000),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(-0.1,22))+
  xlab("Age category")+
  ylab("Cumulative incidence (95% CI)")+
  ggtitle("Cohort-specific stunting rate - Latin America & Europe")+
  annotate("text", x=4.5,y=20,label=lab.lae.f,size=4)
dev.off()

lab.asia=inc.cohort[inc.cohort$region=="Asia",] %>% group_by(cohort) %>% summarise(N=sum(nchild))
lab.asia.f=paste0("N=",lab$N)

pdf("U:/Figures/stunting-inc-asia.pdf",width=13,height=8,onefile=TRUE)
ggplot(inc.cohort[inc.cohort$region=="Asia",],
       aes(y=yi*1000,x=age.f))+
  geom_point(size=2)+facet_wrap(~cohort)+
  geom_linerange(aes(ymin=ci.lb*1000,ymax=ci.ub*1000),
                 size=2,alpha=0.3) +
  scale_y_continuous(limits=c(-0.1,22))+
  xlab("Age category")+
  ylab("Point prevalence (95% CI)")+
  ggtitle("Cohort-specific stunting rate - Asia")+
  annotate("text", x=4.5,y=20,label=lab.asia.f,size=4)
dev.off()


pdf("U:/Figures/stunting-inc-pool.pdf",width=9,height=4,onefile=TRUE)
ggplot(ir.res,aes(y=est*1000,x=agecat.f))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb*1000,ymax=ub*1000),width=0.05) +
  scale_color_manual(values=tableau10)+xlab("Age category")+
  ylab("Incidence rate per 1,000 child-days (95% CI)")+
  scale_y_continuous(limits=c(0,8))+
  annotate("text",x=ir.res$agecat.f,y=0.4,label=ir.res$pt.f,size=3)+
  annotate("text",x=ir.res$agecat.f,y=0.01,label=ir.res$nstudy.f,size=3)+
  annotate("text",label=ir.res$ptest.f,x=ir.res$agecat.f,
           y=ir.res$est*1000,hjust=-0.3,size=3)+
  ggtitle("Pooled stunting incidence rate")
dev.off()


# export data
inc = inc.prep %>%
  select(studyid,subjid,country,tr,agedays,haz,agecat,
         atrisk,inccase,pdays)

save(inc,file="U:/Data/Stunting/st_inc.RData")
save(inc,file="U:/ucb-superlearner/Stunting rallies/st_inc.RData")


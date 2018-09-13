








#-----------------------------------
# Stunting analysis
# Objective 1a
# Calculate cumulative incidence (ever stunted) at
# 3, 6, 12, 18, and 24 mo of age

# Cumulative incidence pooled using random effects
#-----------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load base functions
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

# identify ever stunted children
evs = d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,subjid) %>%
  #create variable with minhaz by age category, cumulatively
  mutate(minhaz=ifelse(agecat=="3 months",min(haz[agecat=="3 months"]),
                       ifelse(agecat=="6 months",min(haz[agecat=="3 months" | agecat=="6 months"]),
                              ifelse(agecat=="12 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"]),
                                     ifelse(agecat=="18 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"|agecat=="18 months"]),
                                            min(haz)))))) %>%
  # create indicator for whether the child was ever stunted
  # by age category
  group_by(studyid,country,agecat,subjid) %>%
  summarise(minhaz=min(minhaz)) %>%
  mutate(ever_stunted=ifelse(minhaz< -2,1,0))



# count incident cases per study by age
# exclude time points if number of measurements per age
# in a study is <50  
cuminc.data= evs%>%
  group_by(studyid,country,agecat) %>%
  summarise(
    nchild=length(unique(subjid)),
    nstudy=length(unique(studyid)),
    ncases=sum(ever_stunted),
    N=sum(length(ever_stunted))) %>%
  filter(N>=50)

cuminc.data

# estimate random effects, format results
ci.res=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x)
  fit.rma(data=cuminc.data,ni="N", xi="ncases",age=x,measure="PR",nlab=" children"))
ci.res=as.data.frame(do.call(rbind, ci.res))
ci.res[,4]=as.numeric(ci.res[,4])
ci.res = ci.res %>%
  mutate(est=est*100, lb=lb*100, ub=ub*100)
ci.res$agecat.f=as.factor(ifelse(ci.res$agecat=="3 months","0-3 months",
                                 ifelse(ci.res$agecat=="6 months","0-6 months",
                                        ifelse(ci.res$agecat=="12 months","0-12 months",
                                               ifelse(ci.res$agecat=="18 months","0-18 months","0-24 months")))))
ci.res$agecat.f=factor(ci.res$agecat.f,levels=unique(ci.res$agecat.f))
ci.res$ptest.f=sprintf("%0.0f",ci.res$est)

ci.res


# plot cohort incidence




#------------------------------
# identify ever stunted children in age ranges
#------------------------------


stunt_ci = d %>% ungroup() %>%
  filter(!is.na(agecat) ) %>%
  #filter(!is.na(agecat) & agedays>1) %>%
  group_by(studyid,country,subjid) %>%
  arrange(studyid,country,subjid,agedays) %>%
  mutate(stunt = as.numeric(haz < -2), stunt_nmeas=cumsum(stunt), stunt_onset= as.numeric(stunt==1 & stunt_nmeas==1)) %>%
  filter(stunt_nmeas<2) %>%
  ungroup() %>% group_by(studyid,country, agecat) %>% mutate(N=n()) %>%
  ungroup() %>% group_by(studyid,country,subjid, agecat) %>% arrange(desc(stunt_onset)) %>% slice(1) %>% 
  ungroup() 


# count incident cases per study by age
# exclude time points if number of measurements per age
# in a study is <50  
cuminc.data.agerange <- stunt_ci%>%
  group_by(studyid,country,agecat) %>%
  summarise(
    nchild=length(unique(subjid)),
    nstudy=length(unique(studyid)),
    ncases=sum(stunt_onset),
    N=sum(length(stunt_onset))) %>%
  filter(N>=50)



# manually calculate incident cases, person-time at risk at each time point
stunt_ci %>% group_by(studyid,country,agecat) %>% filter(N>=50) %>%
  group_by(agecat) %>%
  summarise(inc.case=sum(stunt_onset),N= n())


#cuminc.data %>% group_by(agecat) %>% summarise(mean(ncases/nchild))
cuminc.data.agerange %>% group_by(agecat) %>% summarize(sum(ncases), sum(N), one=mean(ncases/N), two=sum(ncases)/sum(N))


# estimate random effects, format results
gc()
ci.res.agerange=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x)
  fit.rma(data=cuminc.data.agerange,ni="N", xi="ncases",age=x,measure="PR",nlab=" at-risk"))
ci.res.agerange=as.data.frame(do.call(rbind, ci.res.agerange))
ci.res.agerange[,4]=as.numeric(ci.res.agerange[,4])
ci.res.agerange = ci.res.agerange %>%
  mutate(est=est*100, lb=lb*100, ub=ub*100)
ci.res.agerange$agecat.f=as.factor(ifelse(ci.res.agerange$agecat=="3 months","0-3 months",
                                          ifelse(ci.res.agerange$agecat=="6 months","3-6 months",
                                                 ifelse(ci.res.agerange$agecat=="12 months","6-12 months",
                                                        ifelse(ci.res.agerange$agecat=="18 months","12-18 months","18-24 months")))))
ci.res.agerange$agecat.f=factor(ci.res.agerange$agecat.f,levels=c("0-3 months","3-6 months",
                                                                  "6-12 months","12-18 months","18-24 months"))
ci.res.agerange$ptest.f=sprintf("%0.0f",ci.res.agerange$est)

ci.res.agerange

# plot pooled cumulative incidence
p1<-ggplot(ci.res,aes(y=est,x=agecat.f))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_y_continuous(limits=c(0,80))+
  xlab("Age category")+
  ylab("Percent stunte\n(95% CI)")+
  annotate("text",x=ci.res$agecat.f,y=5,label=ci.res$nmeas.f,size=3)+
  annotate("text",x=ci.res$agecat.f,y=1,label=ci.res$nstudy.f,size=3)+
  annotate("text",label=ci.res$ptest.f,x=ci.res$agecat.f,
           y=ci.res$est,hjust=-0.75,size=3)+
  ggtitle("Pooled cumulative incidence of stunting since birth")


p2<-ggplot(ci.res.agerange,aes(y=est,x=agecat.f))+
  geom_point(size=3)+
  geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
  scale_y_continuous(limits=c(0,40))+
  xlab("Age category")+
  ylab("Percent stunted\n(95% CI)")+
  annotate("text",x=ci.res.agerange$agecat.f,y=5,label=ci.res.agerange$nmeas.f,size=3)+
  annotate("text",x=ci.res.agerange$agecat.f,y=1,label=ci.res.agerange$nstudy.f,size=3)+
  annotate("text",label=ci.res.agerange$ptest.f,x=ci.res.agerange$agecat.f,
           y=ci.res.agerange$est,hjust=-0.75,size=3)+
  ggtitle("Pooled incidence proportion of stunting within age ranges")


library(cowplot)
p <- plot_grid(p1, p2,  labels = c("A", "B"), ncol = 1)
p

pdf("U:/Figures/stunting-cuminc_2panel.pdf",width=9,height=7,onefile=TRUE)
p
dev.off()


df <- rbind(data.frame(ci.res, Measure="Cumulative incidence\nfrom birth"), data.frame(ci.res.agerange, Measure="Incidence proportion\nwithin age ranges"))
            


df$agecat.f2 <- paste0(c("0-","3-","6-","12-","18-"), df$agecat)
df$agecat.f2 <- factor(df$agecat.f2, levels=unique(df$agecat.f2))

save(df, file="U:/Data/Stunting/st_incprop.RData")

          p_comb <- ggplot(df, aes(y=est,x=agecat.f2, color=Measure))+
              geom_point(size=3, position=position_dodge(width=0.25))+
              geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05, position=position_dodge(width=0.25)) +
              scale_y_continuous(limits=c(0,80))+
              xlab("Age category")+
              ylab("Percent stunted (95% CI)") +
              annotate("text",x=df$agecat.f2[1:5],y=74,label=df$nmeas.f[1:5],size=3) +
              annotate("text",x=df$agecat.f2[6:10],y=4,label=df$nmeas.f[6:10],size=3) +
              annotate("text",x=df$agecat.f2[1:5],y=71,label=df$nstudy.f[1:5],size=3) +
              annotate("text",x=df$agecat.f2[6:10],y=1,label=df$nstudy.f[6:10],size=3) +
              annotate("text",x=df$agecat.f2[1],y=78,label="N's for cumulative incidence from birth",size=4, hjust = 0) +
              annotate("text",x=df$agecat.f2[1],y=8,label="N's for incidence proportion within age ranges",size=4, hjust = 0) +
              annotate("text",label=df$ptest.f[1:5],x=df$agecat.f2[1:5], y=df$est[1:5],hjust=2.5,size=3)+
              annotate("text",label=df$ptest.f[6:10],x=df$agecat.f2[6:10], y=df$est[6:10],hjust=-2,size=3)+
              theme(strip.background = element_blank(), strip.text.x = element_text(size=12)) +
            scale_fill_manual(name = "Measure", values=c("#56B4E9","#D55E00")) +
            scale_colour_manual(name = "Measure",values=c("#56B4E9","#D55E00")) +
              ggtitle("Pooled cumulative incidence of stunting")
          p_comb  
              
              
            pdf("U:/Figures/stunting-cuminc.pdf",width=9,height=7,onefile=TRUE)
            p_comb
            dev.off()
            
            #png version          
            png("U:/Figures/stunting-cuminc.png",width=9*72,height=7*72)
            p_comb
            dev.off()
            
            
            
            
            
            
            #-----------------------------------
            # Stunting analysis
            # Objective 1a
            # Calculate cumulative incidence (ever stunted) at
            # 3, 6, 12, 18, and 24 mo of age
            # Exclude birth measurements
            # Cumulative incidence pooled using random effects
            #-----------------------------------
            library(dplyr)
            library(ggplot2)
            library(tidyr)
            library(metafor)
            theme_set(theme_bw())
            
            # load base functions
            source("U:/Scripts/Stunting/2-analyses/0_st_basefunctions.R")
            
            load("U:/Data/Stunting/stunting_data.RData")
            
            # define age windows
            d = d %>% 
              filter(agedays>1) %>%
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
            
            # identify ever stunted children
            evs = d %>%
              filter(!is.na(agecat)) %>%
              group_by(studyid,country,subjid) %>%
              arrange(studyid,subjid) %>%
              #create variable with minhaz by age category, cumulatively
              mutate(minhaz=ifelse(agecat=="3 months",min(haz[agecat=="3 months"]),
                                   ifelse(agecat=="6 months",min(haz[agecat=="3 months" | agecat=="6 months"]),
                                          ifelse(agecat=="12 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"]),
                                                 ifelse(agecat=="18 months",min(haz[agecat=="3 months" | agecat=="6 months"|agecat=="12 months"|agecat=="18 months"]),
                                                        min(haz)))))) %>%
              # create indicator for whether the child was ever stunted
              # by age category
              group_by(studyid,country,agecat,subjid) %>%
              summarise(minhaz=min(minhaz)) %>%
              mutate(ever_stunted=ifelse(minhaz< -2,1,0))
            
            
            
            # count incident cases per study by age
            # exclude time points if number of measurements per age
            # in a study is <50  
            cuminc.data= evs%>%
              group_by(studyid,country,agecat) %>%
              summarise(
                nchild=length(unique(subjid)),
                nstudy=length(unique(studyid)),
                ncases=sum(ever_stunted),
                N=sum(length(ever_stunted))) %>%
              filter(N>=50)
            
            cuminc.data
            
            # estimate random effects, format results
            ci.res=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x)
              fit.rma(data=cuminc.data,ni="N", xi="ncases",age=x,measure="PR",nlab=" children"))
            ci.res=as.data.frame(do.call(rbind, ci.res))
            ci.res[,4]=as.numeric(ci.res[,4])
            ci.res = ci.res %>%
              mutate(est=est*100, lb=lb*100, ub=ub*100)
            ci.res$agecat.f=as.factor(ifelse(ci.res$agecat=="3 months","0-3 months",
                                             ifelse(ci.res$agecat=="6 months","0-6 months",
                                                    ifelse(ci.res$agecat=="12 months","0-12 months",
                                                           ifelse(ci.res$agecat=="18 months","0-18 months","0-24 months")))))
            ci.res$agecat.f=factor(ci.res$agecat.f,levels=unique(ci.res$agecat.f))
            ci.res$ptest.f=sprintf("%0.0f",ci.res$est)
            
            ci.res
            
            
            # plot cohort incidence
            
            
            
            
            #------------------------------
            # identify ever stunted children in age ranges
            #------------------------------
            
            
            stunt_ci = d %>% ungroup() %>%
              filter(!is.na(agecat) & agedays>1) %>%
              group_by(studyid,country,subjid) %>%
              arrange(studyid,country,subjid,agedays) %>%
              mutate(stunt = as.numeric(haz < -2), stunt_nmeas=cumsum(stunt), stunt_onset= as.numeric(stunt==1 & stunt_nmeas==1)) %>%
              filter(stunt_nmeas<2) %>%
              ungroup() %>% group_by(studyid,country, agecat) %>% mutate(N=n()) %>%
              ungroup() %>% group_by(studyid,country,subjid, agecat) %>% arrange(desc(stunt_onset)) %>% slice(1) %>% 
              ungroup() 
            
            
            # count incident cases per study by age
            # exclude time points if number of measurements per age
            # in a study is <50  
            cuminc.data.agerange <- stunt_ci%>%
              group_by(studyid,country,agecat) %>%
              summarise(
                nchild=length(unique(subjid)),
                nstudy=length(unique(studyid)),
                ncases=sum(stunt_onset),
                N=sum(length(stunt_onset))) %>%
              filter(N>=50)
            
            
            
            # manually calculate incident cases, person-time at risk at each time point
            stunt_ci %>% group_by(studyid,country,agecat) %>% filter(N>=50) %>%
              group_by(agecat) %>%
              summarise(inc.case=sum(stunt_onset),N= n())
            
            
            #cuminc.data %>% group_by(agecat) %>% summarise(mean(ncases/nchild))
            cuminc.data.agerange %>% group_by(agecat) %>% summarize(sum(ncases), sum(N), one=mean(ncases/N), two=sum(ncases)/sum(N))
            
            
            # estimate random effects, format results
            gc()
            ci.res.agerange=lapply(list("3 months","6 months","12 months","18 months","24 months"),function(x)
              fit.rma(data=cuminc.data.agerange,ni="N", xi="ncases",age=x,measure="PR",nlab=" at-risk"))
            ci.res.agerange=as.data.frame(do.call(rbind, ci.res.agerange))
            ci.res.agerange[,4]=as.numeric(ci.res.agerange[,4])
            ci.res.agerange = ci.res.agerange %>%
              mutate(est=est*100, lb=lb*100, ub=ub*100)
            ci.res.agerange$agecat.f=as.factor(ifelse(ci.res.agerange$agecat=="3 months","2 days-3 months",
                                                      ifelse(ci.res.agerange$agecat=="6 months","3-6 months",
                                                             ifelse(ci.res.agerange$agecat=="12 months","6-12 months",
                                                                    ifelse(ci.res.agerange$agecat=="18 months","12-18 months","18-24 months")))))
            ci.res.agerange$agecat.f=factor(ci.res.agerange$agecat.f,levels=c("2 days-3 months","3-6 months",
                                                                              "6-12 months","12-18 months","18-24 months"))
            ci.res.agerange$ptest.f=sprintf("%0.0f",ci.res.agerange$est)
            
            ci.res.agerange
            
            # plot pooled cumulative incidence
            p1<-ggplot(ci.res,aes(y=est,x=agecat.f))+
              geom_point(size=3)+
              geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
              scale_y_continuous(limits=c(0,80))+
              xlab("Age category")+
              ylab("Percent stunte\n(95% CI)")+
              annotate("text",x=ci.res$agecat.f,y=5,label=ci.res$nmeas.f,size=3)+
              annotate("text",x=ci.res$agecat.f,y=1,label=ci.res$nstudy.f,size=3)+
              annotate("text",label=ci.res$ptest.f,x=ci.res$agecat.f,
                       y=ci.res$est,hjust=-0.75,size=3)+
              ggtitle("Pooled cumulative incidence of stunting since birth")
            
            
            p2<-ggplot(ci.res.agerange,aes(y=est,x=agecat.f))+
              geom_point(size=3)+
              geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05) +
              scale_y_continuous(limits=c(0,40))+
              xlab("Age category")+
              ylab("Percent stunted\n(95% CI)")+
              annotate("text",x=ci.res.agerange$agecat.f,y=5,label=ci.res.agerange$nmeas.f,size=3)+
              annotate("text",x=ci.res.agerange$agecat.f,y=1,label=ci.res.agerange$nstudy.f,size=3)+
              annotate("text",label=ci.res.agerange$ptest.f,x=ci.res.agerange$agecat.f,
                       y=ci.res.agerange$est,hjust=-0.75,size=3)+
              ggtitle("Pooled incidence proportion of stunting within age ranges")
            
            library(cowplot)
            p <- plot_grid(p1, p2,  labels = c("A", "B"), ncol = 1)
            p
            
            pdf("U:/Figures/stunting-cuminc_2panel_nobirth.pdf",width=9,height=7,onefile=TRUE)
            p
            dev.off()
            
            
            df <- rbind(data.frame(ci.res, Measure="Cumulative incidence\nfrom birth"), data.frame(ci.res.agerange, Measure="Incidence proportion\nwithin age ranges"))
            
            
            
            df$agecat.f2 <- paste0(c("2 days-","3-","6-","12-","18-"), df$agecat)
            df$agecat.f2 <- factor(df$agecat.f2, levels=unique(df$agecat.f2))
            
            p_comb <- ggplot(df, aes(y=est,x=agecat.f2, color=Measure))+
              geom_point(size=3, position=position_dodge(width=0.25))+
              geom_errorbar(aes(ymin=lb,ymax=ub),width=0.05, position=position_dodge(width=0.25)) +
              scale_y_continuous(limits=c(0,80))+
              xlab("Age category")+
              ylab("Percent stunted (95% CI)") +
              annotate("text",x=df$agecat.f2[1:5],y=74,label=df$nmeas.f[1:5],size=3) +
              annotate("text",x=df$agecat.f2[6:10],y=4,label=df$nmeas.f[6:10],size=3) +
              annotate("text",x=df$agecat.f2[1:5],y=71,label=df$nstudy.f[1:5],size=3) +
              annotate("text",x=df$agecat.f2[6:10],y=1,label=df$nstudy.f[6:10],size=3) +
              annotate("text",x=df$agecat.f2[1],y=78,label="N's for cumulative incidence from birth",size=4, hjust = 0) +
              annotate("text",x=df$agecat.f2[1],y=8,label="N's for incidence proportion within age ranges",size=4, hjust = 0) +
              annotate("text",label=df$ptest.f[1:5],x=df$agecat.f2[1:5], y=df$est[1:5],hjust=2.5,size=3)+
              annotate("text",label=df$ptest.f[6:10],x=df$agecat.f2[6:10], y=df$est[6:10],hjust=-2,size=3)+
              theme(strip.background = element_blank(), strip.text.x = element_text(size=12)) +
              scale_fill_manual(name = "Measure", values=c("#56B4E9","#D55E00")) +
              scale_colour_manual(name = "Measure",values=c("#56B4E9","#D55E00")) +
              ggtitle("Pooled cumulative incidence of stunting")
            p_comb  
            
            
            pdf("U:/Figures/stunting-cuminc_nobirth.pdf",width=9,height=7,onefile=TRUE)
            p_comb
            dev.off()            
            
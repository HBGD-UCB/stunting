


#-----------------------------------
# Stunting webinar plots
#-----------------------------------

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(binom)
library(metafor)
library(ggthemes)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

# load base functions
source("C:/Users/andre/Documents/HBGDki/Stunting/2-analyses/0_st_basefunctions.R")

load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_cuminc.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_incprop.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_prev.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_rec_interim.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/pool_vel.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_rf_res.RData")



setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/")


ls()

prev.cohort
ci.cohort
ip.cohort
ip.cohort_nobirth
rec.cohort
poolhaz <- poolhaz %>% filter(pooled==0) %>% rename(agecat=strata, studyid=country_cohort) %>% subset(., select=-c(nmeas.f, nstudy.f, statistic, stratacol))
poolhaz



fit.cont.rma=function(data,age,yi,vi,ni,nlab){
  data=filter(data,agecat==age)
  
  fit <- rma(yi=data[[yi]], vi=data[[vi]], method="REML")
  
  out = data %>%
    ungroup() %>%
    summarise(nstudies=length(unique(studyid)),
              nmeas=sum(data[[ni]][agecat==age])) %>%
    mutate(agecat=age,est=fit$beta, se=fit$se, lb=fit$ci.lb, ub=fit$ci.ub,
           nmeas.f=paste0("N=",format(sum(data[[ni]]),big.mark=",",scientific=FALSE),
                          " ",nlab),
           nstudy.f=paste0("N=",nstudies," studies"))
  return(out)
}

# estimate random effects, format results
region.rma <- function(d, region="Asia", measure="prev", agecat.list=list("Birth","3 months","6 months","9 months","12 months","18 months","24 months"), resdf=NULL){
  
  
  if(measure=="prev"){
    res=lapply(agecat.list,function(x) 
      fit.rma(data=d[d$region==region,],ni="nmeas", xi="nxprev",age=x,measure="PR",nlab="children"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    res = res %>%
      mutate(est=est*100,lb=lb*100,ub=ub*100)
    res$agecat=factor(res$agecat,levels=c("Birth","3 months","6 months","9 months","12 months","18 months","24 months"))
    res$ptest.f=sprintf("%0.0f",res$est)
  }
  if(measure=="cuminc"){
    res=lapply(agecat.list,function(x)
      fit.rma(data=d[d$region==region,],ni="N", xi="ncases",age=x,measure="PR",nlab=" measurements"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    res = res %>%
      mutate(est=est*100, lb=lb*100, ub=ub*100)
    res$agecat.f=as.factor(ifelse(res$agecat=="3 months","0-3 months",
                                     ifelse(res$agecat=="6 months","4-6 months",
                                            ifelse(res$agecat=="12 months","7-12 months",
                                                   ifelse(res$agecat=="18 months","13-18 months","19-24 months")))))
    res$agecat.f=factor(res$agecat.f,levels=c("0-3 months","4-6 months",
                                                    "7-12 months","13-18 months","19-24 months"))
    res$ptest.f=sprintf("%0.0f",res$est)
    
  }
  if(measure=="incprop" | measure=="incprop_nobirth"){
    res=lapply(agecat.list,function(x)
      fit.rma(data=d[d$region==region,],ni="N", xi="ncases",age=x,measure="PR",nlab=" at-risk"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    res = res %>%
      mutate(est=est*100, lb=lb*100, ub=ub*100)
    res$agecat.f=as.factor(ifelse(res$agecat=="3 months","0-3 months",
                           ifelse(res$agecat=="6 months","3-6 months",
                           ifelse(res$agecat=="9 months","6-9 months",
                           ifelse(res$agecat=="12 months","9-12 months",
                           ifelse(res$agecat=="15 months","12-15 months",
                           ifelse(res$agecat=="18 months","15-18 months",
                           ifelse(res$agecat=="21 months","19-21 months","21-24 months"))))))))
    res$agecat.f=factor(res$agecat.f,levels=c("0-3 months","3-6 months","6-9 months",
                                              "9-12 months","12-15 months","15-18 months","18-21 months","18-24 months"))
    res$ptest.f=sprintf("%0.0f",res$est)
  }
  if(measure=="rec"){
    res=lapply(list("0-3 months","3-6 months","6-9 months",
                        "9-12 months","12-15 months","15-18 months",
                        "18-21 months", "21-24 months"),function(x) 
                          fit.rma(data=d[d$region==region,],ni="N", xi="n",age=x,measure="PR",
                                  nlab="children"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    res = res %>%
      mutate(est=est*100,lb=lb*100,ub=ub*100)
    res$agecat=factor(res$agecat,levels=c("0-3 months","3-6 months","6-9 months",
                                                  "9-12 months","12-15 months","15-18 months",
                                                  "18-21 months", "21-24 months"))
    
    res$ptest.f=sprintf("%0.1f",res$est)
  }
  if(measure=="vel"){
    d$var<-((d$Mean - d$Lower.95.CI)/1.96)^2
    res=lapply(agecat.list,function(x) 
      fit.cont.rma(data=d[d$region==region,],yi="Mean", vi="var", ni="N",age=x, nlab="children"))
    res=as.data.frame(do.call(rbind, res))
  }
  
  res <- data.frame(measure, region, res)
  resdf <- bind_rows(resdf, res)
  return(resdf)
}

res <- region.rma(prev.cohort)
res <- region.rma(prev.cohort, region="Africa", resdf=res)
res <- region.rma(prev.cohort, region="Latin America", resdf=res)

res <- region.rma(ci.cohort, measure="cuminc", agecat.list=list("3 months","6 months","12 months","18 months","24 months"), resdf=res)
res <- region.rma(ci.cohort, region="Africa", measure="cuminc", agecat.list=list("3 months","6 months","12 months","18 months","24 months"), resdf=res)
res <- region.rma(ci.cohort, region="Latin America", measure="cuminc", agecat.list=list("3 months","6 months","12 months","18 months","24 months"), resdf=res)

res <- region.rma(ip.cohort, measure="incprop", agecat.list=list("3 months","6 months","9 months","12 months","15 months","18 months","21 months", "24 months"), resdf=res)
res <- region.rma(ip.cohort, region="Africa", measure="incprop", agecat.list=list("3 months","6 months","9 months","12 months","15 months","18 months","21 months", "24 months"), resdf=res)
res <- region.rma(ip.cohort, region="Latin America", measure="incprop", agecat.list=list("3 months","6 months","9 months","12 months","15 months","18 months","21 months", "24 months"), resdf=res)

res <- region.rma(ip.cohort_nobirth, measure="incprop_nobirth", agecat.list=list("3 months","6 months","9 months","12 months","15 months","18 months","21 months", "24 months"), resdf=res)
res <- region.rma(ip.cohort_nobirth, region="Africa", measure="incprop_nobirth", agecat.list=list("3 months","6 months","9 months","12 months","15 months","18 months","21 months", "24 months"), resdf=res)
res <- region.rma(ip.cohort_nobirth, region="Latin America", measure="incprop_nobirth", agecat.list=list("3 months","6 months","9 months","12 months","15 months","18 months","21 months", "24 months"), resdf=res)

res <- region.rma(rec.cohort, measure="rec", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
res <- region.rma(rec.cohort, region="Africa", measure="rec", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)
res <- region.rma(rec.cohort, region="Latin America", measure="rec", agecat.list=list("0-3 months","3-6 months","6-9 months","9-12 months","12-15 months","15-18 months","18-21 months", "21-24 months"), resdf=res)

res <- region.rma(poolhaz, measure="vel", agecat.list=list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"), resdf=res)
res <- region.rma(poolhaz, region="Africa", measure="vel", agecat.list=list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"), resdf=res)
res <- region.rma(poolhaz, region="Latin America", measure="vel", agecat.list=list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"), resdf=res)



head(res)

res$region <- factor(res$region, levels=c("Asia","Africa","Latin America"))

res$measure_name <- NA
res$measure_name[res$measure=="prev"] <- "Prevalence"
res$measure_name[res$measure=="cuminc"] <- "Cumulative incidence"
res$measure_name[res$measure=="incprop"] <- "Incidence proportion"
res$measure_name[res$measure=="rec"] <- "Recovery"
res$measure_name[res$measure=="vel"] <- "LAZ change"
res$measure_name <- factor(res$measure_name, levels=c("Prevalence", "Cumulative incidence", "Incidence proportion", "Recovery",  "LAZ change"))

res$agecat <- gsub("months", "mo.", res$agecat)
res$agecat <- factor(res$agecat, levels=unique(res$agecat))


#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")



res <- res %>% rename(Region=region)


#NOTE!

#Make seperate plot per outcome, faceted by agecat
#Then combine with multiplot
#Make legend seperately

#Add incidence proportion, excluding birth to the region stratified plots?

#Better in each plot to use position dodge, placing ages next to each other and coloring by region
# ggplot(res,aes(y=est,x=agecat))+
#   geom_point(aes(fill=region, color=region), position=position_dodge(width=0.6), size = 4) +
#   geom_linerange(aes(ymin=lb, ymax=ub, color=region), position=position_dodge(width=0.6),
#                  alpha=0.5, size = 3) +
#   facet_wrap(~measure_name, scales="free", ncol=2) +
#   scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) 



legend<-ggplot(res[res$measure=="prev",],aes(y=est,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = 3) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("%")+ ggtitle("Prevalence") +
  theme(legend.position="right", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
  
ggsave(legend, file="region_legend.png", width=10, height=4)


unique(res$measure)

a <- 3
b <- 2.5
  
p1 <- ggplot(res[res$measure=="prev",],aes(y=est,x=agecat))+
    geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
    geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
    scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
    xlab("Age category")+ ylab("%")+ ggtitle("Prevalence") +
    theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p2 <- ggplot(res[res$measure=="cuminc",],aes(y=est,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("%")+ ggtitle("Cumulative incidence") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p3 <- ggplot(res[res$measure=="incprop",],aes(y=est,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
  coord_cartesian(ylim=c(0, 35)) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("%")+ ggtitle("A) Incidence proportion") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p4 <- ggplot(res[res$measure=="incprop_nobirth",],aes(y=est,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
  coord_cartesian(ylim=c(0, 35)) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("%")+ ggtitle("B) Incidence proportion-no birth stunting") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p5 <- ggplot(res[res$measure=="rec",],aes(y=est,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("%")+ ggtitle("C) Recovery") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p6 <- ggplot(res[res$measure=="vel",],aes(y=est,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = a) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = b) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("Delta-Z")+ ggtitle("D) Change in LAZ") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 



multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL, title="", 
                      fontsize = 12, fontfamily = "Helvetica") {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (nchar(title)>0){
    layout <- rbind(rep(0, ncol(layout)), layout)
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), 
                                               ncol(layout), 
                                               heights = if (nchar(title)>0) {unit(c(0.5, rep(5,nrow(layout)-1)), "null")}
                                               else {unit(c(rep(5, nrow(layout))), "null")})))
    
    # Make each plot, in the correct location
    if (nchar(title)>0) {
      grid.text(title, 
                vp = viewport(layout.pos.row = 1, layout.pos.col = 1:ncol(layout)),
                gp = gpar(fontsize = fontsize, fontfamily = fontfamily))
    }
    
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#Add in no birth stunting plot

jpeg("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/region_strat_descriptive_epi.jpeg", width = 10, height = 4, units = 'in', res = 400)
  multiplot( p3,  p5, p4, p6, cols=2, title="Region-stratified descriptive statistics of stunting")
dev.off()





#-------------------------------------------------
# Stratified co-occurance plots
#-------------------------------------------------




# estimate random effects, format results
region.rma.co <- function(d, region="Asia", measure="prev", agecat.list=list("Birth","3 months","6 months","9 months","12 months","18 months","24 months"), resdf=NULL){
  
  if(measure=="prev"){
    res=lapply(agecat.list,function(x) 
      fit.rma(data=d[d$region==region,],ni="nmeas", xi="nxprev",age=x,measure="PR",nlab="children"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    res = res %>%
      mutate(est=est*100,lb=lb*100,ub=ub*100)
    res$agecat=factor(res$agecat,levels=c("Birth","3 months","6 months","9 months","12 months","18 months","24 months"))
    res$ptest.f=sprintf("%0.0f",res$est)
  }
  if(measure=="cuminc"){
    res=lapply(agecat.list,function(x)
      fit.rma(data=d[d$region==region,], ni="Nchildren", xi="Ncases_anystunt_wast",age=x,measure="PR",nlab=" measurements"))
    res=as.data.frame(do.call(rbind, res))
    res[,4]=as.numeric(res[,4])
    res = res %>%
      mutate(est=est*100, lb=lb*100, ub=ub*100)
    res$agecat.f=as.factor(res$agecat)
    # res$agecat.f=as.factor(ifelse(res$agecat=="3 months","0-3 months",
    #                               ifelse(res$agecat=="6 months","4-6 months",
    #                                      ifelse(res$agecat=="12 months","7-12 months",
    #                                             ifelse(res$agecat=="18 months","13-18 months","19-24 months")))))
    # res$agecat.f=factor(res$agecat.f,levels=c("0-3 months","4-6 months",
    #                                           "7-12 months","13-18 months","19-24 months"))
    res$ptest.f=sprintf("%0.0f",res$est)
    
  }
  
  res <- data.frame(measure, region, res)
  resdf <- bind_rows(resdf, res)
  return(resdf)
}

load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/co_CI.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/co_prev.RData")
load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/pooled_CI_res.RData")

res<-NULL
res <- region.rma.co(prev.cohort)
res <- region.rma.co(prev.cohort, region="Africa", resdf=res)
res <- region.rma.co(prev.cohort, region="Latin America", resdf=res)

res <- res %>% rename(Region=region) %>% mutate(Region=factor(Region, levels=c("Asia","Africa","Latin America")))

p1 <- ggplot(res[res$measure=="prev",],aes(y=est,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = 3) +
  coord_cartesian(ylim=c(0.75,18)) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("Proportion (%)")+ ggtitle("Prevalence") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p1



res_co_ci <- co_ci %>% group_by(studyid, country, agecat) %>%
  summarize(Ncases_co=sum(ever_co), 
            Ncases_anystunt_wast=sum(anystunt_wast), 
            Ncases_anystuntorwast=sum(anystunt==1 | anywast==1), 
            Ncases_nostunt_nowast=sum(anystunt==0 & anywast==0), 
            Ncases_stunt_first=sum(stuntfirst==1), 
            Ncases_wast_first=sum(wastfirst==1), 
            Ncases_stunt_only=sum(anystunt==1 & anywast==0), 
            Ncases_wast_only=sum(anystunt==0 & anywast==1), 
            Nchildren=sum(N)) %>% 
  filter(Nchildren>1) %>% 
  as.data.frame()  

# region variable
df <- res_co_ci %>% mutate(region = case_when(
  country=="BANGLADESH" | country=="INDIA"|
    country=="NEPAL" | country=="PAKISTAN"|
    country=="PHILIPPINES"                   ~ "Asia", 
  country=="KENYA"|
    country=="GHANA"|
    country=="BURKINA FASO"|
    country=="GUINEA-BISSAU"|
    country=="MALAWI"|
    country=="SOUTH AFRICA"|
    country=="TANZANIA, UNITED REPUBLIC OF"|
    country=="ZIMBABWE"|
    country=="GAMBIA"                       ~ "Africa",
  country=="BELARUS"                      ~ "Europe",
  country=="BRAZIL" | country=="GUATEMALA" |
    country=="PERU"                         ~ "Latin America",
  TRUE                                    ~ "Other"
)) %>% filter(region!="Europe")

res<-NULL
res <- region.rma.co(df, measure="cuminc", agecat.list=list("0-6 months","6-12 months","12-18 months","18-24 months"), resdf=res)
res <- region.rma.co(df, region="Africa", measure="cuminc", agecat.list=list("0-6 months","6-12 months","12-18 months","18-24 months"), resdf=res)
res <- region.rma.co(df, region="Latin America", measure="cuminc", agecat.list=list("0-6 months","6-12 months","12-18 months","18-24 months"), resdf=res)

res <- res %>% rename(Region=region) %>% mutate(Region=factor(Region, levels=c("Asia","Africa","Latin America")))


p2 <- ggplot(res[res$measure=="cuminc",],aes(y=est,x=agecat))+
  geom_point(aes(fill=Region, color=Region), position=position_dodge(width=0.6), size = 4) +
  geom_linerange(aes(ymin=lb, ymax=ub, color=Region), position=position_dodge(width=0.6),alpha=0.5, size = 3) +
  coord_cartesian(ylim=c(0.75,18)) +
  scale_color_manual(values=rep(tableau10,20))+  scale_fill_manual(values=rep(tableau10,20)) +
  xlab("Age category")+ ylab("Proportion (%)")+ ggtitle("Cumulative incidence") +
  theme(legend.position="none", strip.text.x = element_text(size=12), axis.text.x = element_text(size=12, angle=20, hjust = 1)) 
p2


jpeg("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/region_strat_descriptive_epi_co.jpeg", width = 10, height = 4, units = 'in', res = 400)
multiplot( p1, p2, cols=2, title="Region-stratified descriptive statistics of co-occurent wasting and stunting")
dev.off()


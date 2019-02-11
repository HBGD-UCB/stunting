

# load packages
rm(list=ls())
library(tidyverse)
library(metafor)
library(data.table)

source("U:/Scripts/Stunting/2-analyses/0_st_basefunctions.R")


# random effects function, save results nicely
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



d <- readRDS(file="U:/UCB-SuperLearner/Stunting rallies/velocity_longfmt.rds")
head(d)

#Merge in sex
cov<-readRDS("U:/UCB-SuperLearner/Stunting rallies/FINAL_temp_clean_covariates.rds")
cov <- subset(cov, select = c(studyid,subjid,country,sex))
setDT(cov)

dim(d)
d <- left_join(d, cov, by=c("studyid", "subjid", "country"))
dim(d)


table(d$diffcat)

d <- d %>% rename(agecat = diffcat) %>%
            group_by(studyid, country, agecat, ycat, sex) %>%
            summarize(mean=mean(y_rate, na.rm=T), var=var(y_rate, na.rm=T), n=n()) %>%
            mutate(se=sqrt(var), ci.lb=mean - 1.96 * se, ci.ub=mean + 1.96 * se) %>% 
  mutate(region = case_when(
                country=="BANGLADESH" | country=="INDIA"|
                country=="NEPAL" | country=="PAKISTAN"|
                country=="PHILIPPINES"                   ~ "Asia", 
                country=="BURKINA FASO"|
                country=="GUINEA-BISSAU"|
                country=="MALAWI"|
                country=="KENYA"|
                country=="GHANA"|
                country=="SOUTH AFRICA"|
                country=="TANZANIA, UNITED REPUBLIC OF"|
                country=="ZIMBABWE"|
                country=="GAMBIA"                       ~ "Africa",
                country=="BELARUS"                      ~ "Europe",
                country=="BRAZIL" | country=="GUATEMALA" |
                country=="PERU"                         ~ "Latin America",
                TRUE ~ "Other"
            ),
         country_cohort=paste0(studyid," ", country))


# Drop yearly studies and non-control arms for the descriptive analysis
#Drop studies Vishak added to data product that don't meet inclusion criteria
d <- d[d$studyid!="ki1000301-DIVIDS" & d$studyid!="ki1055867-WomenFirst" & d$studyid!="ki1135782-INCAP",]

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

#Drop yearly
d <- d %>% filter(measurefreq!="yearly")



# age specific pooled results
RE_pool <- function(df, ycategory, gender){
  
  df <- df %>% filter(ycat==ycategory)
  df <- df %>% filter(sex==gender)
  
  pooled.vel=lapply(list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"),function(x) 
    fit.cont.rma(data=df,yi="mean", vi="var", ni="n",age=x, nlab="children"))
  pooled.vel=as.data.frame(do.call(rbind, pooled.vel))
  
  # age and region specific pooled results
  asia.vel=lapply(list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"),function(x) 
    fit.cont.rma(data=df[df$region=="Asia",],yi="mean", vi="var", ni="n",age=x, nlab="children"))
  LA.vel=lapply(list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"),function(x) 
    fit.cont.rma(data=df[df$region=="Latin America",],yi="mean", vi="var", ni="n",age=x, nlab="children"))
  africa.vel=lapply(list("0-3 months", "3-6 months",  "6-9 months","9-12 months","12-15 months","15-18 months","18-21 months","21-24 months"),function(x) 
    fit.cont.rma(data=df[df$region=="Africa",],yi="mean", vi="var", ni="n",age=x, nlab="children"))
  
  asia.vel=as.data.frame(do.call(rbind, asia.vel))
  LA.vel=as.data.frame(do.call(rbind, LA.vel))
  africa.vel=as.data.frame(do.call(rbind, africa.vel))
  
  
  #Bind together pooled and cohort specific estimates
  
  pooled.df <- rbind(
    data.frame(country_cohort="Pooled - All", pooled=1, region="Overall", pooled.vel),
    data.frame(country_cohort="Pooled - Asia", pooled=1, region="Asia",asia.vel),
    data.frame(country_cohort="Pooled - Africa", pooled=1, region="Africa",africa.vel),
    data.frame(country_cohort="Pooled - Amer.", pooled=1, region="Latin America",LA.vel)
  ) %>% subset(., select = -c(se)) %>%
    rename(strata=agecat, Mean=est, N=nmeas, Lower.95.CI=lb, Upper.95.CI=ub)
  
  cohort.df <- df %>% subset(., select = c(country_cohort, agecat, n, mean, ci.lb, ci.ub, region)) %>%
                      rename(N=n, Mean=mean, Lower.95.CI=ci.lb, Upper.95.CI=ci.ub,
                             strata=agecat) %>%
                      mutate(pooled=0, nstudies=1)
  
  plotdf <- bind_rows(pooled.df, cohort.df)
  
  
  #Format variables for plotting
  
  #remove grant identifier
  plotdf$country_cohort<- gsub("^k.*?-" , "",plotdf$country_cohort)
  
  simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
          sep="", collapse=" ")
  }
  
  for(i in 1:length(plotdf$country_cohort)){
    plotdf$country_cohort[i] <- simpleCap(plotdf$country_cohort[i])
  }

  plotdf$statistic="Velocity"
  plotdf$country_cohort <- factor(plotdf$country_cohort, levels=unique(plotdf$country_cohort))
  plotdf$strata <- factor(plotdf$strata, levels=unique(plotdf$strata))
  
  plotdf$stratacol <- "strata"
  plotdf$stratacol[plotdf$strata=="Overall"] <- "overall"
  plotdf$stratacol[plotdf$pooled==1] <- "pooled"
  plotdf$stratacol[plotdf$strata=="Overall" & plotdf$pooled==1] <- "pooled_unstrat"
  
  plotdf$sex <- gender
  plotdf$ycat <- ycategory
  
return(plotdf)
}



poolhaz_boys <- RE_pool(d, ycategory="haz", gender="Male")
poolhaz_girls <- RE_pool(d, ycategory="haz", gender="Female")
poollencm_boys <- RE_pool(d, ycategory="lencm", gender="Male")
poollencm_girls <- RE_pool(d, ycategory="lencm", gender="Female")

pooled_vel <- rbind(
  poolhaz_boys, poolhaz_girls, poollencm_boys, poollencm_girls
)

save(pooled_vel, file="U:/data/Stunting/pool_vel.RData")



#----------------------
# Plot results
#----------------------


#Set theme and colors
theme_set(theme_bw())

cbPalette <- c( overall="#56B4E9", strata="#999999" , pooled="#f7a809", pooled_unstrat="#009E73")

#Set plot width and height
w <- 10
h <- 7

#----------------------
# Plot function
#----------------------

desc_epi_metaplot <- function(d, 
                              stat="Wasting\nincidence\nrate",
                              ylabel="Wasting incidence rate per 1000 days",
                              title="Wasting incidence rate",
                              xlabel="Child age stratification",
                              text_adj=0.05,
                              text_adj2=0){
  if(!is.null(stat)){
    p <- ggplot(d[d$statistic==stat,]) +
      geom_point(aes(x=strata, y=Mean, fill=stratacol, color=stratacol), size = 4) +
      geom_linerange(aes(x=strata, ymin = Lower.95.CI, ymax = Upper.95.CI, color=stratacol), 
                     alpha=0.5, size = 3) 
  }else{
    p <- ggplot(d) + 
      geom_point(aes(x=statistic, y=Mean, fill=stratacol, color=stratacol), size = 4) +
      geom_linerange(aes(x=statistic, ymin = Lower.95.CI, ymax = Upper.95.CI, color=stratacol), 
                     alpha=0.5, size = 3) 
  }
  
  p <- p + 
    facet_wrap(~country_cohort, nrow = 2)  + 
    scale_fill_manual(values=cbPalette) +
    scale_colour_manual(values=cbPalette) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(strip.background = element_blank(),
          legend.position="none",
          strip.text.x = element_text(size=8),
          axis.text.x = element_text(size=8)) +
    ylab(ylabel)+
    ggtitle(title) + 
    xlab(xlabel)
  
  if(d$pooled[1]==1){
    p <- p +  
      geom_text(aes(x=d$strata,y=min(d$Lower.95.CI)-text_adj2+text_adj,label=d$nmeas.f), size=3) +
      geom_text(aes(x=d$strata,y=min(d$Lower.95.CI)-text_adj2,label=d$nstudy.f), size=3)
  }
  
  return(p)
}



#----------------------
# Primary plots
#----------------------


#Set plot directory
setwd("U:/Figures")

#Pooled haz velocity
p1 <- desc_epi_metaplot(poolhaz[poolhaz$stratacol=="pooled",], stat="Velocity",
                        ylabel="Z-score change per month",
                        title="HAZ velocity per month")
#p1
ggsave("HAZvel_pooled_metaplot.pdf", p1, width = w, height = h, units = "in")

#Cohort specific haz velocity
p2 <- desc_epi_metaplot(poolhaz[poolhaz$stratacol!="pooled",], stat="Velocity",
                        ylabel="Z-score change per month",
                        title="HAZ velocity per month")
p2
ggsave("HAZvel_metaplot.pdf", p2, width = w, height = h, units = "in")


#Pooled haz velocity
p3 <- desc_epi_metaplot(poolwaz[poolwaz$stratacol=="pooled",], stat="Velocity",
                        ylabel="Z-score change per month",
                        title="WAZ velocity per month")
p3
ggsave("WAZvel_pooled_metaplot.pdf", p3, width = w, height = h, units = "in")

#Cohort specific haz velocity
p4 <- desc_epi_metaplot(poolwaz[poolwaz$stratacol!="pooled",], stat="Velocity",
                        ylabel="Z-score change per month",
                        title="WAZ velocity per month")
p4
ggsave("WAZvel_metaplot.pdf", p4, width = w, height = h, units = "in")



#Pooled height velocity
p5 <- desc_epi_metaplot(poollencm[poollencm$stratacol=="pooled",], stat="Velocity",
                        ylabel="Height change in cm per month",
                        title="Height velocity per month", text_adj=0.5, text_adj2=1)
#p5
ggsave("Heightvel_pooled_metaplot.pdf", p5, width = w, height = h, units = "in")

#Cohort specific haz velocity
p6 <- desc_epi_metaplot(poollencm[poollencm$stratacol!="pooled",], stat="Velocity",
                        ylabel="Height change in cm per month",
                        title="Height velocity per month")
p6
ggsave("Heightvel_metaplot.pdf", p6, width = w, height = h, units = "in")

#Cohort sp
#Pooled haz velocity
p7 <- desc_epi_metaplot(poolwtkg[poolwtkg$stratacol=="pooled",], stat="Velocity",
                        ylabel="Weight change in kg per month",
                        title="Weight velocity per month")
p7
ggsave("Weightvel_pooled_metaplot.pdf", p7, width = w, height = h, units = "in")

#Cohort specific haz velocity
p8 <- desc_epi_metaplot(poolwtkg[poolwtkg$stratacol!="pooled",], stat="Velocity",
                      ylabel="Weight change in kg per month",
                        title="Weight velocity per month")
p8
ggsave("Weightvel_metaplot.pdf", p8, width = w, height = h, units = "in")





#PNG versions

ggsave("HAZvel_pooled_metaplot.png", p1, width = w, height = h)

ggsave("HAZvel_metaplot.png", p2, width = w, height = h)

ggsave("WAZvel_pooled_metaplot.png", p3, width = w, height = h)

ggsave("WAZvel_metaplot.png", p4, width = w, height = h)

ggsave("Heightvel_pooled_metaplot.png", p5, width = w, height = h)

ggsave("Heightvel_metaplot.png", p6, width = w, height = h)

ggsave("Weightvel_pooled_metaplot.png", p7, width = w, height = h)

ggsave("Weightvel_metaplot.png", p8, width = w, height = h)




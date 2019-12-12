

# load packages
rm(list=ls())
library(tidyverse)

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")


#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

pcols <-  tableau10

# Load data
load("U:/Data/Stunting/stunting_data.RData")




#set age categories
d <- calc.prev.agecat(d)
d <- d %>% filter(!is.na(agecat))



p <- ggplot(data=d,aes(x=haz,group=agecat,color=agecat,fill=agecat)) +
  facet_wrap(~agecat,ncol=1) +
  geom_density(aes(y=..density..),color=NA,alpha=0.7)+
  geom_vline(aes(xintercept= -2)) +
  scale_x_continuous(limits = c(-5,5), breaks = c(-4:4)) +
  scale_fill_manual(values=rep(pcols[4],10)) +
  scale_color_manual(values=rep(pcols[4],10)) +
  labs(x="LAZ",y="Density") +
  theme_minimal(base_size=16) +
  theme(legend.position = "none")
p


# Facet by region
d <- d %>% filter(region!="Europe")
d$region <- factor(d$region, levels = c("Asia","Africa","Latin America"))
d <- d %>% arrange(region)
p_strat <- ggplot(data=d,aes(x=haz,group=region,color=region,fill=region)) +
  facet_grid(region~agecat) +
  geom_density(aes(y=..density..),color=NA,alpha=0.7)+
  geom_vline(aes(xintercept= -2)) +
  scale_x_continuous(limits = c(-5,5), breaks = c(-4,-2,0,2,4)) +
  scale_fill_manual(values=pcols) +
  scale_color_manual(values=pcols) +
  labs(x="LAZ",y="Density") +
  theme_minimal(base_size=16) +
  theme(legend.position = "none")
p_strat


#Subset to one cohort
d <- d %>% filter(studyid=="ki0047075b-MAL-ED" & country=="BANGLADESH")
p <- ggplot(data=d,aes(x=haz,group=agecat,color=agecat,fill=agecat)) +
  facet_wrap(~agecat,nrow=3,ncol=3) +
  geom_density(aes(y=..density..),color=NA,alpha=0.7)+
  geom_vline(aes(xintercept= -2)) +
  scale_x_continuous(limits = c(-5,5), breaks = c(-4:4)) +
  scale_fill_manual(values=pcols) +
  scale_color_manual(values=pcols) +
  labs(x="LAZ",y="Density") +
  theme_minimal(base_size=16) +
  theme(legend.position = "none")
p











#-----------------------------------
# Stunting analysis
# Objective 1b
# Calculate catch up growth 

# Cohort specific estimates & 
# Pooled estimates using random effects

# What is the mean duration of time for 
# reversing stunting (measured in cohorts 
# with at least monthly measurement)? 
#-----------------------------------

rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyr)
library(metafor)
theme_set(theme_bw())

# load base functions
source("U:/Scripts/Stunting/1-outcomes/0_st_basefunctions.R")

load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/recovery_data.Rdata")



# max measureid for each child
maxid = d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  summarise(maxid=max(measid))

#create indicators for stunting
rev <- d %>%
  filter(!is.na(agecat)) %>%
  group_by(studyid,country,subjid) %>%
  #
  mutate(stunted=ifelse(haz< -2,1,0),
         lagstunted=lag(stunted),
         lag2stunted=lag(stunted,2),
         leadstunted=lead(stunted))  %>%
  # unique stunting episode
  mutate(sepisode=ifelse(lagstunted==0 & stunted==1 & leadstunted==1 |
                           stunted==1 & measid==1,1,0))

rev$lagstunted[is.na(rev$lagstunted)] <- 0
rev$lag2stunted[is.na(rev$lag2stunted)] <- 0
rev$leadstunted[is.na(rev$leadstunted)] <- rev$stunted[is.na(rev$leadstunted)] 

# make indicator for max measurement
rev2 <- full_join(rev, maxid, by=c("studyid","country","subjid"))  %>%
  arrange(studyid, country, subjid, agedays) %>%
  # determine the start and end of each stunting episode
  mutate(start=ifelse(stunted==1 & lagstunted==0 & lag2stunted==0,1,0),
         end=case_when(
           stunted==1 & maxid==measid   ~ 0,
           leadstunted==0 & stunted==0 & lag(stunted)==1 ~ 1,
           TRUE                         ~ 0
         ),
         cumlaz=cumsum(haz),
         numstart=cumsum(start),
         numend=cumsum(end)) 


head(as.data.frame(rev2),30)

rev3 <- rev2 %>% filter(numstart==numend & numend>0) #& stunted!=1)
head(as.data.frame(rev3),30)

mean(rev3$haz)
mean(rev3$haz[rev3$numend==1])
mean(rev3$haz[rev3$numend==2])
mean(rev3$haz[rev3$numend==3])

table(rev3$numend)

#Mean of child means
rev4 <- rev3 %>% summarize(meanLAZ=mean(haz)) 
mn <- mean(rev4$meanLAZ)
md <- median(rev4$meanLAZ)
summary(rev4$meanLAZ)

labs<-data.frame(points_pos=rep(NA,2))
labs$points_pos <-c(md, mn)
labs$points_posy <-c(1.15, 0.87)
labs$points_pos_labels <- c("Median: -1.6","Mean: -1.4")

#Plot themes
theme_set(theme_bw())
scaleFUN <- function(x) sprintf("%.2f", x)

#hbgdki pallet
tableau10 <- c("black","#1F77B4","#FF7F0E","#2CA02C","#D62728", 
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
yticks <- c(-2,-1.5,-1,-0.5,0,0.5,1,1.5,2)
ylabs <- as.character(yticks)

p <- ggplot(data=rev4) +  geom_density(aes(x=meanLAZ, color=T, fill=T), size=2, alpha=0.5) +
  geom_point(data=labs, aes(x=points_pos, y=points_posy), size=4, shape=25, fill="black") +
  geom_text(data=labs, aes(x=points_pos, y=points_posy, label=points_pos_labels ), hjust=-0.3) +
  scale_x_continuous(breaks=yticks, labels=ylabs) + theme_bw() + xlab("Mean LAZ after recovery") + ylab("Density") + 
  ggtitle("Distribution of mean LAZ after recovery from stunting\n(Recovery: two consecutive non-stunted measures)") +
  scale_fill_manual(values=tableau10[4]) + 
  scale_color_manual(values=tableau10[4]) +
  coord_cartesian(xlim=c(-2,1), ylim=c(0,1.5), expand=c(0,0)) +
  #scale_x_continuous( trans='log10', labels=scaleFUN) +
  theme(strip.background = element_blank(),
        legend.position="none",
        strip.text.x = element_text(size=12),
        axis.text.x = element_text(size=14))
p

ggsave(p, file="C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/recoveryLAZ_density.png", width=6, height=5)

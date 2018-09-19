



#-------------------------------------------------------------------------------------------
# Heatmaps
#-------------------------------------------------------------------------------------------
library('dplyr')
library('tidyr')
library('stringr')
library('scales')
library('RColorBrewer')
library('ggplot2')
library('gridExtra')

#define a color for fonts
textcol <- "grey20"

load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/st_heatmap.RData")


dp <- dp %>% filter(measure_freq!="Yearly")
dd <- dd %>% filter(measure_freq!="Yearly")

dp$age <- dp$age - 1
dd$age <- dd$age - 1

# heat map plot scheme
hm <- ggplot(dp,aes(x=age,y=studycountry)) +
    facet_grid(measure_freq~.,scales='free_y',space='free_y') +
  #facet_wrap(~measure_freq, scales = "free_y", ncol=1) +
  geom_tile(colour="white",size=0.25)+
  scale_y_discrete(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0),
                     breaks = scales::pretty_breaks(n = 8))+
  #coord_equal()+
  theme_grey(base_size=12)+
  theme(
    strip.background = element_blank(),
    legend.title=element_text(color=textcol,size=12),
    legend.margin = margin(grid::unit(0.1,"cm")),
    legend.text=element_text(colour=textcol,size=8),
    legend.key.height=grid::unit(0.2,"cm"),
    legend.key.width=grid::unit(1,"cm"),
    legend.position = "bottom",
    axis.text.x=element_text(size=12,colour=textcol,angle=0,vjust=0.5),
    axis.text.y=element_text(size=9.5,vjust = 0.2,colour=textcol),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(angle=270,size=12),
    plot.background=element_blank(),
    panel.border=element_blank())


# side bar plot scheme
# sidebar <- ggplot(data = dd, aes(x = studycountry)) + 
#   geom_bar(stat = "identity") +
#   coord_flip() + 
#   facet_grid(measure_freq~.,scales='free_y',space='free_y') +
#   scale_x_discrete(expand=c(0,0)) +
#   scale_fill_manual(values=rep('gray70',6),na.value="grey90",
#                     guide=guide_legend(title="",title.hjust = 0.5,
#                                        label.position="bottom",label.hjust=0.5,nrow=1,
#                                        override.aes = list(color = "white", fill="white"))) +
#   theme_grey(base_size=12) +
#   theme(
#     legend.title=element_text(color=textcol,size=8),
#     legend.margin = margin(grid::unit(0.1,"cm")),
#     legend.text=element_text(colour=NA,size=7,face="bold"),
#     legend.key.height=grid::unit(0.2,"cm"),
#     legend.key.width=grid::unit(0.2,"cm"),
#     legend.position = "bottom",
#     axis.title.y = element_blank(), 
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     strip.text.x = element_blank(),
#     strip.text.y = element_blank(),
#     axis.title.x = element_text(size=12),
#     plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
#     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#     panel.background = element_blank())



#-----------------------------------
# STUNTING PREVALENCE HEAT MAP
#-----------------------------------

setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/")


# heat map
wphm <- hm +
  aes(fill=stpcat) +
  labs(x="Age in months",y="",title="Stunting prevalence by month of age") + theme(legend.position="none") +
  scale_fill_viridis_d(na.value="grey90",
                    guide=guide_legend(title="Stunting (%)",title.vjust = 1,
                                       label.position="bottom",label.hjust=0.5,nrow=2))
ggsave(wphm, file="stuntprev_heatmap.png", width=8, height=6)


# # stunting prevalence side bar plot
# wpbar <- sidebar +
#   aes(y=stuntprev) +
#   labs(x = "",y="Overall Prevalence (%)",title="Stunting (%)") +
#   scale_y_continuous(expand=c(0,0),limits=c(0,70),
#                      breaks=seq(0,80,by=20),labels=seq(0,80,by=20)) +
#   geom_hline(yintercept = seq(0,80,by=20),color='white',size=0.3)
#   
#   
# # combined plot
# wpgrid <- grid.arrange(wphm, wpbar, nrow = 1, ncol = 2, widths=c(100,20))



#-----------------------------------
# measurement heat map
#-----------------------------------

nhm <- hm +
  aes(fill=ncat) +
  labs(x="Age in months",y="",title="N children measured by month of age") + theme(legend.position="none") +
  scale_fill_viridis_d(na.value="grey90",
                    guide=guide_legend(title="Number of\nMeasurements",title.vjust = 1,
                                       label.position="bottom",nrow=2))
ggsave(nhm, file="N_heatmap.png", width=9, height=6)

# nbar <- sidebar +
#   aes(y=nmeas/1000) +
#   labs(x = "",y="Child-Months (x1000)",title="Sample size") +
#   scale_y_continuous(expand=c(0,0),limits=c(0,125),
#                      breaks=seq(0,120,by=20),labels=seq(0,120,by=20)) +
#   geom_hline(yintercept = seq(0,120,by=20),color='white',size=0.3) 
# 
# 
# ngrid <- grid.arrange(nhm, nbar, nrow = 1, ncol = 2, widths=c(100,20))
# 
# ggsave(filename="stunting-study-N-heatmap.pdf",plot = ngrid,device='png', width=7, height=6)












#------------------------------------------
# Covariate presence heatmap
#------------------------------------------

load("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/Plot data/covariate_presence.Rdata")

#Clean up study names
unique(plotdf$studyid)
plotdf$studyid[plotdf$studyid=="CMC-V-BCS-2002"] <- "CMC-V-BCS"




#Categorize N's
summary(plotdf$N)
plotdf$Ncat <- cut(plotdf$N,breaks=c(0,250,500,1000,5000,10000,100000),
                   labels=c("<250","250-500","500-1000","1000-5000","5000-10000",">10000"))
plotdf$Ncat <- factor(plotdf$Ncat)


plotdf$variable <- toupper(plotdf$variable)

varfreq <- plotdf %>% group_by(variable) %>% 
  summarize(numstudies= sum(value)) %>%
  arrange(numstudies)
varfreq

studyfreq <- plotdf %>% group_by(studyid) %>% 
  summarize(numstudies= sum(value)) %>%
  arrange(-numstudies)
studyfreq


#order variables and studies by frequency for the plot
plotdf$variable <- factor(plotdf$variable , levels=varfreq$variable)
levels(plotdf$variable)

plotdf$studyid <- factor(plotdf$studyid , levels=studyfreq$studyid)

#rename variables
plotdf<-mutate(plotdf,variable=fct_recode(variable,
                                          "Gender"="SEX",          "Enrolled stunted"="ENSTUNT",      "Enrolled wasted"="ENWAST",       "Birth month"="BRTHMON",      "Month"="MONTH",        "Mom age"="MAGE",         
                                          "Mom educ."="MEDUCYRS",     "Birthweight"="BIRTHWT",      "Mom height"="MHTCM",        "Birth length"="BIRTHLEN",     "Dad educ."="FEDUCYRS",     "Improved sanitation"="IMPSAN",       
                                          "HH wealth"="HHWEALTH_QUART", "Home delivery"="HDLVRY",       "Mom BMI"="MBMI",         "Mom weight"="MWTKG",        "# rooms"="NROOMS",       "Pred./ex. breastfeeding"="PREDEXFD6",    
                                          "Impr. H20"="SAFEH20",      "Impr. floor"="IMPFLOOR",     "Birth order"="PARITY",       "% diarrhea <24mo."="PERDIAR24",    "Any wasting <6mo."="ANYWAST06",    "Gestational age"="GAGEBRTH",     
                                          "# people in HH"="NHH",          "Persistent wasting"="PERS_WAST",    "Clean cook"="CLEANCK",      "Number child <5 in HH"="NCHLDLT5",     "Vaginal birth"="VAGBRTH",      "Early initiation of BF"="EARLYBF",      
                                          "Dad age"="FAGE",         "Food security"="HFOODSEC",     "Single parent"="SINGLE",       "% diar <6mo."="PERDIAR6",     "Treats H20"="TRTH2O",       "Dad height"="FHTCM",
                                          "EBF <6mo"="EXCLFEED6", "EBF <3mo"="EXCLFEED3", "EBF 3-6mo"="EXCLFEED36", "Pred BF 3-6mo"="PREDFEED36", "Pred BF <3mo"="PREDFEED3", "Pred BF <6mo"="PREDFEED6"))


#Drop enrolled stunted- only a RF in the wasting analysis
plotdf <- plotdf %>% filter(variable!="Enrolled stunted")

#define a color for fonts
textcol <- "grey20"


covhm <- ggplot(plotdf,aes(x=studyid, y=variable)) +
  geom_tile(aes(fill=Ncat), colour="white",size=0.25) +
  theme_grey(base_size=10) +
  scale_x_discrete(position = "top") +
  ylab("Risk factor variable") + xlab("Study") +
  theme(
    # legend.title=element_text(color=textcol,size=8),
    # legend.margin = margin(grid::unit(0.1,"cm")),
    # legend.text=element_text(colour=textcol,size=7,face="bold"),
    # legend.key.height=grid::unit(0.2,"cm"),
    # legend.key.width=grid::unit(1,"cm"),
    axis.text.x=element_text(angle = 35, hjust = 0, size=11,colour=textcol,vjust=0),
    axis.text.y=element_text(size=9.5,vjust = 0.2,colour=textcol),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    plot.background=element_blank(),
    panel.border=element_blank(),
    legend.position="bottom",
    plot.margin = margin(0, 1, 0, 0, "cm")
  )  + theme(legend.position="none") +
  scale_fill_viridis_d(na.value="grey90", guide=guide_legend(title="Number of children with covariate measured",title.vjust = 1, label.position="bottom",nrow=2)) #+
  #ggtitle("Covariate presence and measurement frequency across\nstudies used in the HBGDki risk factor analysis.")


covhm

ggsave(covhm, file="Cov_heatmap.png", width=7, height=5.5)



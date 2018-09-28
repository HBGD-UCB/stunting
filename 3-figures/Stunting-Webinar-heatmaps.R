



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

#Switch erroneously classified studies from the heatmap data
# dd$measure_freq[dd$short_id=="jvt3"] <- "Quarterly Measurements"
# dp$measure_freq[dp$study_id=="jvt3"] <- "Quarterly Measurements"
# dd$measure_freq[dd$short_id=="akup" | dd$short_id=="bfzn"] <- "Yearly"
# dp$measure_freq[dp$study_id=="AgaKhanUniv" | dp$study_id=="Burkina Faso Zn"] <- "Yearly"
# table(dd$short_id,dd$measure_freq)
# 
# #Drop Mal-ED Pakistan
# dd <- dd %>% filter(!(short_id=="mled" & country=="Pakistan"))
# dp <- dp %>% filter(!(study_id=="MAL-ED" & country=="Pakistan"))

#Drop yearly
dd <- dd %>% filter(measure_freq!="Yearly")

#dd$age <- dd$age - 1



# make a study-country label, and make the monthly variable into a factor
# including an anonymous label (temporary) for sharing with WHO
dd <- mutate(dd,
             country=str_to_title(str_to_lower(countrycohort)), 
             studycountry=paste0(short_description,', ',country))

unique(dd$studycountry)
unique(dd$study_id)

dd$studycountry[dd$studycountry=="Tanzania Child 2, Tanzania"] <- "Tanzania Child 2" 

dd <- mutate(dd,
             monthly=factor(monthly,levels=c(1,0),
                            labels=c('Monthly Measurements','Quarterly Measurements')),
             studycountry = factor(studycountry,
                                   levels=unique(studycountry[order(monthly,stuntprev)]), 
                                   ordered=TRUE),
             anonym = factor(paste("Cohort",1:nrow(dd),dd$country)),
             anonym = factor(anonym,levels=unique(anonym[order(monthly,stuntprev)]),
                             ordered=TRUE)
)

# categorize stunting prevalence
dd$stpcat <- cut(dd$stuntprev,breaks=c(0,5,10,20,30,40,50,60,100),labels=c("<5","5-10","10-20","20-30","30-40","40-50","50-60",">60"))
dd$stpcat <- factor(dd$stpcat)


#Create dataset of studies we are using
colnames(dd)

studylist <- dd[,c(1:10,29:38)] %>% distinct()
library(xlsx)
write.xlsx(studylist, "U:/Data/Stunting/UCBerkeley_stunting_studylist.xlsx")
saveRDS(studylist, "U:/Data/Stunting/UCBerkeley_stunting_studylist.rds")

#-----------------------------------
# Create a long format dataset
# for ggplot2
#-----------------------------------

# gather N measurements by month data into long format
dnsubj <- select(dd,study_id,anonym,country,studycountry,region,measure_freq,stuntprev,starts_with('n')) %>%
  select(-neurocog_data,-nutrition,-notes,-num_countries,-numcountry,-numsubj,-numobs,-nmeas) %>%
  gather(age,nobs,-study_id,-anonym,-country,-studycountry,-region,-measure_freq,-stuntprev) %>%
  mutate(age=as.integer(str_sub(age,2,-1)),nobs=as.integer(nobs)) %>%
  select(study_id,anonym,country,studycountry,measure_freq,stuntprev,region,age,nobs) %>%
  filter(age>=1 & age <=24 ) %>%
  arrange(measure_freq,stuntprev)

# gather stunting prev by month data into long format
dstuntp <- select(dd,study_id,anonym,country,studycountry,region,starts_with('stuntprev_m')) %>%
  gather(age,stp,-study_id,-anonym,-country,-studycountry,-region) %>%
  mutate(age=as.integer(str_sub(age,12,-1))) %>%
  select(study_id,anonym,country,studycountry, region,age,stp) %>%
  filter(age>=1 & age <=24 )

# join the long tables together and sort countries by measure_freq and stunting prev
dp <- left_join(dnsubj,dstuntp,by=c('study_id','anonym','country','studycountry','region','age'))


# categorize stunting prevalence, set stunting prevalence category estimates to missing if n<50
dp$stpcat <- cut(dp$stp,breaks=c(0,5,10,20,30,40,50,60,100),labels=c("<5","5-10","10-20","20-30","30-40","40-50","50-60",">60"))

dp$stpcat <- factor(dp$stpcat)
dp$stpcat[dp$nobs<50 | is.nan(dp$stp)] <- NA

# categorize number of observations

N_breaks <- c(1,50, 100, 250, 500, 750, 1000, 1500, 2000, 100000)
dp$ncat <- cut(dp$nobs,
               breaks=N_breaks,
               labels=c('<50','50-100','100-250','250-500','500-750','750-1000','1000-1500','1500-2000','>2000'))
dp$ncat <- factor(dp$ncat)






#Arrange plot data by region
dp$region <- factor(dp$region, levels=c("Europe","Latin America", "Africa", "Asia"))
dp <- dp %>% arrange(measure_freq, region, country)
dp$studycountry <- factor(as.character(dp$studycountry), levels=unique(dp$studycountry))







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



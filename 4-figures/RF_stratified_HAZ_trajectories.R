
rm(list=ls())
library(tidyverse)

#Set theme
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728", 
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


load("U:/Data/Stunting/rf_stunting_data.RData")
d <- d %>% subset(., select=-c(tr))


#merge HAZ outcomes with covariates

setwd("U:/ucb-superlearner/Stunting rallies/")

#load covariates
cov<-readRDS("FINAL_clean_covariates.rds")


cov <- cov %>% subset(., select=-c( W_gagebrth,    W_birthwt,     W_birthlen,   
                                    W_mage,        W_mhtcm,       W_mwtkg,       W_mbmi,        W_fage,        W_fhtcm,       W_meducyrs,    W_feducyrs,   
                                    W_nrooms,      W_nhh,         W_nchldlt5,    W_parity,         
                                    W_perdiar6,    W_perdiar24))

d <- left_join(d, cov, by=c("studyid","country","subjid"))

d <- d %>% filter(agedays < 24 * 30.4167)



#Add RF labels
d <- d %>% rename(
  `Gender`=sex ,
  `Enrolled wasted`= enwast,
  `Gestational age at birth`= gagebrth,
  `Exclusive or Predominant breastfeeding under 6 months`= predexfd6,
  `Mother's age`= mage,
  `Mother's height`= mhtcm,
  `Mother's weight`= mwtkg,
  `Mother's BMI`= mbmi,
  `Mother's education`= meducyrs,
  `Birth order`= parity,
  `Household food security`= hfoodsec,
  `Number of children <5 in household`= nchldlt5,
  `Household wealth`= hhwealth_quart,
  `Father's age`= fage,
  `Father's height`= fhtcm,
  `Birthweight (kg)`= birthwt,
  `Birth length (cm)`= birthlen,
  `Vaginal birth`= vagbrth,
  `Child delivered at home`= hdlvry,
  `Single parent`= single,
  `Number of rooms in household`= nrooms,
  `Number of people in household`= nhh,
  `Maternal education quartile`= meducyrs,
  `Paternal education quartile`= feducyrs,
  `Any wasting before 6 months age`= anywast06,
  `Persistent wasting before 6 months age`= pers_wast,
  `Treats drinking water`= trth2o,
  `Clean cooking fuel usage`= cleanck,
  `Improved floor`= impfloor,
  `Improved sanitation`= impsan,
  `Safe water source`= safeh20,
  `Quartile of diarrhea longitudinal\nprevalence under 6 months`= perdiar6,
  `Quartile of diarrhea longitudinal\nprevalence under 24 months`= perdiar24,
  `Breastfeed within an hour of birth`= earlybf,
  `Predominant breastfeeding under 3 months`= predfeed3,
  `Predominant breastfeeding from 3-6 months`= predfeed36,
  `Predominant breastfeeding under 6 months`= predfeed6,
  `Exclusive breastfeeding under 3 months`= exclfeed3,
  `Exclusive breastfeeding from 3-6 months`= exclfeed36,
  `Exclusive breastfeeding under 6 months`= exclfeed6,
  `Month of measurement`= month,
  `Birth month`= brthmon)

d <- subset(d, select = -c(id))


pdf("U:/Figures/Risk Factor HAZ curves.pdf", height=8, width=12)
for(i in 11:ncol(d)){
  df <- d[!is.na(d[,i]),]
  Aname <- colnames(df)[i]
  colnames(df)[i] <- "Avar"
  p<-ggplot(df, aes(x=agedays, y=haz, group=Avar, color=Avar)) + geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
    scale_color_manual(values=rep(tableau10,2), name = paste0("Levels of ", Aname))+
    xlab(Aname) + ylab("HAZ") + 
    ggtitle(paste0("Spline curves of HAZ, stratified by levels of ", Aname))
  
  print(p)

}
dev.off()


#Cohort stratified
#Strip grant identifier and add country
d$studyid <- gsub("^k.*?-" , "", d$studyid)
d$studyid <- paste0(d$studyid, ", ", paste0(substring(as.character(d$country),1,1), tolower(substring(as.character(d$country),2))))
d$studyid <- gsub("Tanzania, united republic of", "Tanzania", d$studyid)
d$studyid <- gsub("africa", "Africa", d$studyid)





pdf("U:/Figures/Risk Factor HAZ curves-cohort stratified.pdf", height=12, width=12)
for(i in 11:ncol(d)){
  df <- d[!is.na(d[,i]),]
  Aname <- colnames(df)[i]
  colnames(df)[i] <- "Avar"
  p<-ggplot(df, aes(x=agedays, y=haz, group=Avar, color=Avar)) + geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
    scale_color_manual(values=rep(tableau10,2), name = paste0("Levels of ", Aname))+
    facet_wrap(~studyid) +
    xlab(Aname) + ylab("HAZ") + 
    ggtitle(paste0("Spline curves of HAZ, stratified by levels of ", Aname))
  
  print(p)
  
}
dev.off()



df <- d %>% ungroup() %>% select(studyid, agedays, haz, 
                   `Mother's height`,
                   `Mother's weight`,
                   `Mother's BMI`,
                   `Father's height`)

df2<-melt(df, id=c("studyid","agedays", "haz"))
head(df2)

p<-ggplot(df, aes(x=agedays, y=haz, group=value, color=value)) + geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
  scale_color_manual(values=rep(tableau10,2), name = paste0("Levels of ", Aname))+
  facet_wrap(~variable ) +
  xlab(Aname) + ylab("HAZ") + 
  ggtitle(paste0("Spline curves of HAZ, stratified by levels of "))

print(p)



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
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


levels(df$`Mother's BMI`)
levels(df$`Mother's height`)
levels(df$`Mother's weight`)
levels(df$`Father's height`)

df$`Mother's BMI`<-factor(as.character(df$`Mother's BMI`), levels=c("Overweight or Obese", "Normal weight","Underweight"))
df$`Mother's height`<-factor(as.character(df$`Mother's height`), levels=c(">=155 cm", "[151-155) cm",  "<151 cm"))
df$`Mother's weight`<-factor(as.character(df$`Mother's weight`), levels=c(">=58 kg", "[52-58) kg", "<52 kg"))
df$`Father's height`<-factor(as.character(df$`Father's height`), levels=c(">=167 cm", "[162-167) cm", "<162 cm"))

colnames(df)
p1<-ggplot(df[!is.na(df$`Mother's height`),], aes(x=agedays, y=haz, group=`Mother's height`, color=`Mother's height`)) + geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
  scale_color_manual(values=rep(tableau10,2), name = paste0("Levels of\nMother's height"))+
  xlab("") + ylab("HAZ") + 
  ggtitle("Mother's height") + theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12))
p2<-ggplot(df[!is.na(df$`Mother's weight`),], aes(x=agedays, y=haz, group=`Mother's weight`, color=`Mother's weight`)) + geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
  scale_color_manual(values=rep(tableau10,2), name = paste0("Levels of\nMother's weight"))+
  xlab("Age in days") + ylab("HAZ") + 
  ggtitle("Mother's weight") + theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12))
p3<-ggplot(df[!is.na(df$`Mother's BMI`),], aes(x=agedays, y=haz, group=`Mother's BMI`, color=`Mother's BMI`)) + geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
  scale_color_manual(values=rep(tableau10,2), name = paste0("Levels of\nMother's BMI"))+
  xlab("") + ylab("HAZ") + 
  ggtitle("Mother's BMI") + theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12))
p4<-ggplot(df[!is.na(df$`Father's height`),], aes(x=agedays, y=haz, group=`Father's height`, color=`Father's height`)) + geom_smooth(method = 'gam', formula= y ~ s(x, bs = "cs")) +
  scale_color_manual(values=rep(tableau10,2), name = paste0("Levels of\nFather's height"))+
  xlab("Age in days") + ylab("HAZ") + 
  ggtitle("Father's height") + theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12))

jpeg("U:/Figures/Stunting Webinar/HAZ_trajectories.jpeg", width = 9, height = 4, units = 'in', res = 400)
#png("U:/Figures/Stunting Webinar/HAZ_trajectories.png", width=11*72, height=6*72)
multiplot(p1,p2,p3,p4, cols=2, title="Spline curves of HAZ stratified by levels of parental anthropometry")
dev.off()





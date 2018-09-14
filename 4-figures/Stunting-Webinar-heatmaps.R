



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
    axis.text.y=element_text(size=10,vjust = 0.2,colour=textcol),
    axis.ticks=element_line(size=0.4),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    strip.text.x = element_text(size=12),
    strip.text.y = element_text(angle=270,size=12),
    plot.background=element_blank(),
    panel.border=element_blank())


# side bar plot scheme
sidebar <- ggplot(data = dd, aes(x = studycountry)) + 
  geom_bar(stat = "identity") +
  coord_flip() + 
  facet_grid(measure_freq~.,scales='free_y',space='free_y') +
  scale_x_discrete(expand=c(0,0)) +
  scale_fill_manual(values=rep('gray70',6),na.value="grey90",
                    guide=guide_legend(title="",title.hjust = 0.5,
                                       label.position="bottom",label.hjust=0.5,nrow=1,
                                       override.aes = list(color = "white", fill="white"))) +
  theme_grey(base_size=12) +
  theme(
    legend.title=element_text(color=textcol,size=8),
    legend.margin = margin(grid::unit(0.1,"cm")),
    legend.text=element_text(colour=NA,size=7,face="bold"),
    legend.key.height=grid::unit(0.2,"cm"),
    legend.key.width=grid::unit(0.2,"cm"),
    legend.position = "bottom",
    axis.title.y = element_blank(), 
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text.x = element_blank(),
    strip.text.y = element_blank(),
    axis.title.x = element_text(size=12),
    plot.title=element_text(colour=textcol,hjust=0,size=12,face="bold"),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank())



#-----------------------------------
# STUNTING PREVALENCE HEAT MAP
#-----------------------------------

setwd("C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/")


# heat map
wphm <- hm +
  aes(fill=stpcat) +
  labs(x="Age in months",y="",title="Stunting prevalence\nby month of age") +
  scale_fill_viridis_d(na.value="grey90",
                    guide=guide_legend(title="Stunting (%)",title.vjust = 1,
                                       label.position="bottom",label.hjust=0.5,nrow=2))
ggsave(wphm, file="stuntprev_heatmap.png", width=7, height=6)


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
  labs(x="Age in months",y="",title="N children measured by month of age") +
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

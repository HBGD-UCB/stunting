






rm(list = ls())
library(tidyverse)
library(metafor)
library(data.table)

source("U:/Wasting/1-outcomes/0_wast_incfunctions.R")


load("U:/Data/Wasting/Stunting_inc_data.RData")


#Drop yearly studies
d <- dstunt %>% filter(measurefreq != "yearly") %>%
  filter(agedays< 24 * 30.4167)


# Rename region
asia_region <- which(levels(d$region) == 'Asia')
levels(d$region)[asia_region] = 'South Asia'

#Plot themes
theme_ki <- function() {
  theme_bw() %+replace%
    theme(
      strip.background = element_blank(),
      legend.position="none",
      plot.title = element_text(size = 30, face = "bold"),
      strip.text = element_text(size=22, vjust= 1.5),
      axis.title = element_text(size=18),
      axis.text.y = element_text(size=16),
      axis.text.x = element_text(size=15, angle = 25, hjust = 1, vjust= -0.2)
    )
}

theme_set(theme_ki())

#hbgdki pallets
tableau10 <- c("#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")
tableau11 <- c("Black","#1F77B4","#FF7F0E","#2CA02C","#D62728",
               "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

d$region <- factor(d$region, levels=c("Africa","Latin America", "Asia", "Europe" ))

p_laz_spline <- ggplot(d, aes(x=agedays, y=haz)) + 
  geom_smooth(se=F, color= "Black")  + 
  geom_smooth(data=d[d$region!="Europe",], aes(group=region, color=region), se=F)  + 
  scale_color_manual(values = tableau10) + theme(legend.position="right")
p_laz_spline

p_wlz_spline <- ggplot(d, aes(x=agedays, y=whz)) + 
  geom_smooth(se=F, color= "Black")  + 
  geom_smooth(data=d[d$region!="Europe",], aes(group=region, color=region), se=F)  + 
  scale_color_manual(values = tableau10) + theme(legend.position="right")



save(p_laz_spline, p_wlz_spline, file="U:/Data/Wasting/ki_spline_plot_data.Rdata")









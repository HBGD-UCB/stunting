




rm(list=ls())
library(tidyverse)
library(metafor)
library(EValue)


load("C:/Users/andre/Downloads/sprint_7D_longbow-master/sprint_7D_longbow-master/adjusted_binary/adjusted_binary_results.rdata")
#load("C:/Users/andre/Downloads/sprint_7D_longbow-master/sprint_7D_longbow-master/unadjusted_binary/unadjusted_binary_results.rdata")

d <- results
unique(d$intervention_variable)

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

#Grab mean WHZ RF dataset
whz <- d %>% filter(intervention_variable=="lag_WHZ_quart")


head(d)

#Subset to relative risks
d <- d %>% filter(type=="RR")


#Subset to primary outcomes
table(d$agecat)
table(is.na(d$agecat))

#d <- d %>% filter(agecat=="0-6 months"| agecat=="6 months"| agecat=="6-24 months"| agecat=="24 months")
d <- d %>% filter(!is.na(agecat) & agecat!="Birth")

#Drop enrolled stunted as a RF for stunting
d <- d %>% filter(intervention_variable!="enstunt")


head(d)

evalues.RRvec <- function(d){
  vec<-NULL
  for(i in 1:nrow(d)){
    suppressMessages(res <- evalues.RR( d$estimate[i], d$ci_lower[i], d$ci_upper[i]))
    vec<-c(vec,res[2,1])
  }
  return(vec)
}

d$EVals<-evalues.RRvec(d)

#Flip RR
d$estimate <- ifelse(d$estimate>1, d$estimate, 1/d$estimate )


p <- ggplot(d, aes(x=estimate, y=EVals)) + geom_point(alpha=0.1) + #geom_smooth(color="red") +
  #geom_abline(slope=1, intercept=0) +
  geom_vline(aes(xintercept=1)) + geom_hline(aes(yintercept=1)) + 
  #coord_equal(xlim = c(0.25,15), ylim = c(0.25,15)) +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10') +
  xlab("Adjusted RR") + ylab("E value") +
  theme(
    strip.text.x = element_text(size=12),
    axis.text.x = element_text(size=12)) 
p

ggplot(data=d) +  geom_density(aes(x=EVals - estimate)) + coord_equal(xlim = c(0, 5))





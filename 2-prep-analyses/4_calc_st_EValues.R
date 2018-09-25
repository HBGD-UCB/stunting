




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

evalues.RRvec <- function(d, pointest=T){
  vec<-NULL
  
  if(pointest){
    for(i in 1:nrow(d)){
      if(d$intervention_level[i]== d$baseline_level[i]){
        res<-NA
      }else{
      suppressMessages(res <- evalues.RR( d$estimate[i], d$ci_lower[i], d$ci_upper[i]))
      res<-res[2,1]
      }
    vec<-c(vec,res)
    }
  }else{
    for(i in 1:nrow(d)){
      if(d$ci_lower[i] <= 1  & d$ci_upper[i] >= 1){
        res<-NA
      }else{
        ci <- d$ci_lower[i]
        if(1/d$ci_lower[i]>1/d$ci_upper[i]){ ci <- d$ci_upper[i]}
        suppressMessages(res <- evalues.RR(ci))
        res<-res[2,1]
      }
      vec<-c(vec,res)}
  }
  return(vec)
}

d$EVals<-evalues.RRvec(d)
d$EVals_lb<-evalues.RRvec(d, pointest=F)

mean(d$EVals, na.rm=T)
mean(d$EVals_lb, na.rm=T)

summary(d$EVals, na.rm=T)
summary(d$EVals_lb, na.rm=T)

mean(d$estimate[!(d$ci_lower <= 1  & d$ci_upper >= 1)])

2.2/1.52 #Need on average 50% stronger unmeasured confounding than point estimate to move estimates to null
3.997/1.92 #Need on average 160% stronger unmeasured confounding than point estimate to make significant estimates insignificant

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

ggplot(data=d) +  geom_density(aes(x=EVals - estimate)) + coord_equal(xlim = c(0, 3))



fivenum(d$EVals)

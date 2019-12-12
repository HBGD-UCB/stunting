


rm(list=ls())
library(tidyverse)
library(metafor)
theme_set(theme_bw())

load("C:/Users/andre/Downloads/sprint_7D_longbow-master/sprint_7D_longbow-master/adjusted_binary/adjusted_binary_results.rdata")
adj <- results

load("C:/Users/andre/Downloads/sprint_7D_longbow-master/sprint_7D_longbow-master/unadjusted_binary/unadjusted_binary_results.rdata")
unadj <- results

d <- merge(adj, unadj, by=c("studyid", "country", "agecat", "intervention_variable",  "outcome_variable", "type" , "intervention_level", "baseline_level" ))

d <- d %>% rename(adj_est = estimate.x, unadj_est = estimate.y) %>% filter(intervention_level!=baseline_level)

d <- d %>% filter(type=="RR")

d <- d %>% filter(outcome_variable!="sstunted" & outcome_variable!="s03rec24")


#Mean ratio between adj and unadj
mean(d$adj_est/d$unadj_est)

mean(d$adj_est)/d$unadj_est)


mean((1-d$adj_est)-(1-d$unadj_est))


min(d$adj_est)
min(d$unadj_est)

max(d$adj_est)
max(d$unadj_est)

summary(d$adj_est)
summary(d$unadj_est)

p <- ggplot(d, aes(x=unadj_est, y=adj_est)) + geom_point(alpha=0.1) + geom_smooth(color="red") +
            geom_abline(slope=1, intercept=0) +
            geom_vline(aes(xintercept=1)) + geom_hline(aes(yintercept=1)) + 
            coord_equal(xlim = c(0.25,15), ylim = c(0.25,15)) +
            scale_x_continuous(trans='log10') +
            scale_y_continuous(trans='log10') +
            xlab("Unadjusted RR") + ylab("Adjusted RR") +
            theme(
            strip.text.x = element_text(size=12),
            axis.text.x = element_text(size=12,)) 

p



ggsave(p, file="C:/Users/andre/Dropbox/HBGDki figures/Stunting Webinar/adj_unadj_compare.png", width=5.7, height=4.6)



#On average, slight attenuation of the estimates compared to ratio of 1 when the RR are extreme. Hard to compare around 1.
#- How to calculate this analytically?




#Examine the outlier RR's
d[d$unadj_est>10, c(1:9,12,20)]
d[d$adj_est>10, c(1:9,12,20)]


#Large changes in estimates
d[(d$adj_est > 4 | d$adj_est < 1/4) & (d$unadj_est < 2 & d$unadj_est > 1/2), c(1:9,12,20)]
d[(d$unadj_est > 4 | d$unadj_est < 1/4) & (d$adj_est < 2 & d$adj_est > 1/2), c(1:9,12,20)]


mean(d$adj_est/d$unadj_est > 1.25 | d$unadj_est/d$adj_est > 1.25)


#Check for change in significance
d$unadj_sig <- d$ci_lower.y > 1 | d$ci_upper.x < 1 
mean(d$unadj_sig)

d$adj_sig <- d$ci_lower.x > 1 | d$ci_upper.y < 1 
mean(d$adj_sig)

mean(d$adj_sig==d$unadj_sig)

d[d$adj_est/d$unadj_est > 1.25 | d$unadj_est/d$adj_est > 1.25, c(1:9,12,20)]




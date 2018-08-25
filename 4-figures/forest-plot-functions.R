

rm(list=ls())
library(tidyverse)
library(metafor)

load("C:/Users/andre/Downloads/sprint_7D_longbow-master (2)/sprint_7D_longbow-master/adjusted_binary/adjusted_binary_results.rdata")
load("C:/Users/andre/Downloads/RiskFactor_Ns.rdata")

d <- results
rm(results)
#Merge in N's
ls()

head(prevN_birth)
head(d)

d<-left_join(d, cumincCase_024, by=c("studyid","country","agecat","intervention_variable","intervention_level"))   
d<-left_join(d, cumincCase_06, by=c("studyid","country","agecat","intervention_variable","intervention_level"))    
d<-left_join(d, cumincCase_624, by=c("studyid","country","agecat","intervention_variable","intervention_level"))   
d<-left_join(d, cumincN_024, by=c("studyid","country","agecat","intervention_variable","intervention_level"))      
d<-left_join(d, cumincN_06, by=c("studyid","country","agecat","intervention_variable","intervention_level"))       
d<-left_join(d, cumincN_624, by=c("studyid","country","agecat","intervention_variable","intervention_level"))     
d<-left_join(d, prevCase_24, by=c("studyid","country","agecat","intervention_variable","intervention_level"))
d<-left_join(d, prevCase_6, by=c("studyid","country","agecat","intervention_variable","intervention_level"))
d<-left_join(d, prevCase_birth, by=c("studyid","country","agecat","intervention_variable","intervention_level"))
d<-left_join(d, prevN_24, by=c("studyid","country","agecat","intervention_variable","intervention_level"))
d<-left_join(d, prevN_6, by=c("studyid","country","agecat","intervention_variable","intervention_level"))         
d<-left_join(d, prevN_birth, by=c("studyid","country","agecat","intervention_variable","intervention_level"))
d<-left_join(d, vel_hazMean_03, by=c("studyid","country","agecat","intervention_variable","intervention_level"))
d<-left_join(d, vel_hazMean_1224, by=c("studyid","country","agecat","intervention_variable","intervention_level"))
d<-left_join(d, vel_hazMean_36, by=c("studyid","country","agecat","intervention_variable","intervention_level")) 
d<-left_join(d, vel_hazMean_612, by=c("studyid","country","agecat","intervention_variable","intervention_level")) 
d<-left_join(d, vel_hazN_03, by=c("studyid","country","agecat","intervention_variable","intervention_level"))     
d<-left_join(d, vel_hazN_1224, by=c("studyid","country","agecat","intervention_variable","intervention_level"))  
d<-left_join(d, vel_hazN_36, by=c("studyid","country","agecat","intervention_variable","intervention_level"))   
d<-left_join(d, vel_hazN_612, by=c("studyid","country","agecat","intervention_variable","intervention_level")) 

head(d)
colnames(d)

index <-which(grepl("N_cases[.]",colnames(d) ))
d$N_cases <- rowSums(d[,index], na.rm=T)
d <- d[,-index]

index <-which(grepl("N[.]",colnames(d) ))
d$N <- rowSums(d[,index], na.rm=T)
d <- d[,-index]



head(d)


#------------------------------------------------
#Plot themes
#------------------------------------------------
scaleFUN <- function(x) sprintf("%.2f", x)

#Plot themes
theme_set(theme_bw())

#hbgdki pallet
tableau10 <- c("black","#1F77B4","#FF7F0E","#2CA02C","#D62728", 
  "#9467BD","#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")


#------------------------------------------------
#Pooled estimate function
#------------------------------------------------

poolRR <- function(d, method="REML"){
    #nstudies=length(unique(d$studyid))
  nstudies <- d %>% summarize(Nstudies=n(), N_cases=sum(N_cases, na.rm=T), N=sum(N, na.rm=T))
  
  if(d$intervention_level[1] == d$baseline_level[1]){
    est <- data.frame(logRR.psi=1, logSE=0, RR=1, RR.CI1=1, RR.CI2=1, Nstudies= nstudies$Nstudies, N= nstudies$N, N_cases= nstudies$N_cases)
  }else{
    
  #cat(d$intervention_variable[1]," ", levels(d$agecat)[1]," ", d$intervention_level[1],"\n")
  fit<-NULL
  try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method=method, measure="RR"),silent = TRUE)
  if(is.null(fit) & method=="REML"){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="ML", measure="RR"),silent = TRUE)}
  if(is.null(fit) & method=="REML"){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="HE", measure="RR"),silent = TRUE)}
  if(is.null(fit) & method=="REML"){try(fit<-rma(yi=untransformed_estimate, sei=untransformed_se, data=d, method="EB", measure="RR"),silent = TRUE)}

  if(is.null(fit)){
        est <- data.frame(logRR.psi=NA, logSE=NA, RR=NA, RR.CI1=NA, RR.CI2=NA, Nstudies= nstudies$Nstudies, N= nstudies$N, N_cases= nstudies$N_cases)
  }else{
  est<-data.frame(fit$b, fit$se)
  colnames(est)<-c("logRR.psi","logSE")
  
  est$RR<-exp(est$logRR)
  est$RR.CI1<-exp(est$logRR - 1.96 * est$logSE)
  est$RR.CI2<-exp(est$logRR + 1.96 * est$logSE)
  
  est$Nstudies <- nstudies$Nstudies
    est$N <- nstudies$N
  est$N_cases <- nstudies$N_cases

  }
  }
  
  return(est)
}

#------------------------------------------------
#Study capitalization function
#------------------------------------------------
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), tolower(substring(s, 2)),
      sep="", collapse=" ")
}


#------------------------------------------------
#Forest plot function
#------------------------------------------------

# dfull<-d
# d<-dfull
# parameter="RR"
# agerange="6 months"
# measure="RR"
# RF_to_drop=c("enstunt", "perdiar6", "trth2o")
# yticks = c(0.12, 0.25,0.5,1,2,4,8,16,32,64, 128)

KI_forest_plot <- function(d, parameter="RR", agerange="6 months", measure="RR",
                           RF_to_drop=c("enstunt", "perdiar6", "trth2o"),
                           yticks = c(0.12, 0.25,0.5,1,2,4,8,16,32,64,128),
                           pdfname="Risk Factor Forest Plots 6mo prevalence.pdf"){
  

#Subset to relative risks
d <- d[d$type==parameter,]


#Subset to age of outcome
d <- d[d$agecat==agerange,]


#Drop unwanted risk factors
d <- d[!(d$intervention_variable %in% RF_to_drop),]


#Drop Mal-ED Tanzania HHwealth 6-24mo (only has 2 levels)
d <- d %>% filter(!(intervention_variable=="hhwealth_quart" & agecat=="6-24 months" & studyid=="ki0047075b-MAL-ED" & country=="TANZANIA, UNITED REPUBLIC OF"))



#Strip grant identifier and add country
  d$studyid <- gsub("^k.*?-" , "", d$studyid)
  d$studyid <- paste0(d$studyid, ", ", paste0(substring(as.character(d$country),1,1), tolower(substring(as.character(d$country),2))))
  d$studyid <- gsub("Tanzania, united republic of", "Tanzania", d$studyid)
  d$studyid <- gsub("africa", "Africa", d$studyid)

#Add region
  d <- d %>% mutate(region = case_when(
    country=="BANGLADESH" | country=="INDIA"|
      country=="NEPAL" | country=="PAKISTAN"|
      country=="PHILIPPINES"                   ~ "Asia", 
      country=="BURKINA FASO"|
      country=="GUINEA-BISSAU"|
      country=="MALAWI"|
      country=="SOUTH AFRICA"|
      country=="TANZANIA, UNITED REPUBLIC OF"|
      country=="ZIMBABWE"|
      country=="GAMBIA"                       ~ "Africa",
      country=="BELARUS"                      ~ "Europe",
      country=="BRAZIL" | country=="GUATEMALA" |
      country=="PERU"                         ~ "Latin America",
      TRUE                                    ~ "Other"
  ))



#Pooled effects
  if(measure=="RR"){
    
    RMAest_RE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
                  do(try(poolRR(.))) %>% mutate(studyid="Pooled - Random", region="Pooled", pooled=1) %>% as.data.frame()
    RMAest_FE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolRR(., method = "FE")) %>% mutate(studyid="Pooled - Fixed", region="Pooled", pooled=1) %>% as.data.frame()
    
    
    #Add regional estimates
    RMAest_RE_africa <- d %>% filter(region=="Africa") %>% 
                  group_by(intervention_variable, agecat, intervention_level) %>%
                  do(try(poolRR(.))) %>% mutate(studyid="Pooled - Africa", region="Africa", pooled=1) %>% as.data.frame()
    RMAest_RE_asia <- d %>% ungroup() %>% filter(region=="Asia") %>% do(droplevels(.)) %>% 
                  group_by(intervention_variable, agecat, intervention_level) %>%
                  do(try(poolRR(.))) %>% mutate(studyid="Pooled - Asia", region="Asia", pooled=1) %>% as.data.frame()
    RMAest_RE_latamer <- d %>% filter(region=="Latin America") %>% 
                  group_by(intervention_variable, agecat, intervention_level) %>%
                  do(try(poolRR(.))) %>% mutate(studyid="Pooled - Latin America", region="Latin America", pooled=1) %>% as.data.frame()
    
    #merge in pooled effects
    d <- d %>% rename(RR=estimate, RR.CI1=ci_lower, RR.CI2=ci_upper) %>% 
      mutate(Nstudies=1, pooled=0, region=as.character(region)) %>% 
      subset(., select=c(studyid, country, region, intervention_variable,agecat,intervention_level, baseline_level,
                        RR, RR.CI1, RR.CI2, pooled, Nstudies, N, N_cases, adjustment_set))
    d <- bind_rows(d, RMAest_RE_africa, RMAest_RE_asia, RMAest_RE_latamer, RMAest_RE, RMAest_FE)
  }
  
  
  if(measure=="ATE"){
    RMAest_RE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(.)) %>% mutate(studyid="Pooled - Random", region="Pooled", pooled=1) %>% as.data.frame()
    RMAest_FE <- d %>% group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(., method = "FE")) %>% mutate(studyid="Pooled - Fixed", region="Pooled", pooled=1) %>% as.data.frame()
    
    
    #Add regional estimates
    RMAest_RE_africa <- d %>% filter(region=="Africa") %>% 
                  group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(.)) %>% mutate(studyid="Pooled - Africa", region="Africa", pooled=1) %>% as.data.frame()
    RMAest_RE_asia <- d %>% ungroup() %>% filter(region=="Asia") %>% do(droplevels(.)) %>% 
                  group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(.)) %>% mutate(studyid="Pooled - Asia", region="Asia", pooled=1) %>% as.data.frame()
    RMAest_RE_latamer <- d %>% filter(region=="Latin America") %>% 
                  group_by(intervention_variable, agecat, intervention_level) %>%
                  do(poolATE(.)) %>% mutate(studyid="Pooled - Latin America", region="Latin America", pooled=1) %>% as.data.frame()
    
    #merge in pooled effects
    d <- d %>% rename(ATE=estimate, ATE.CI1=ci_lower, ATE.CI2=ci_upper) %>% 
      mutate(Nstudies=1, pooled=0, region=as.character(region)) %>% 
      subset(., select=c(studyid, country, region, intervention_variable,agecat,intervention_level, baseline_level,
                        ATE, ATE.CI1, ATE.CI2, pooled, Nstudies, N, N_cases, adjustment_set))
    d <- bind_rows(d, RMAest_RE_africa, RMAest_RE_asia, RMAest_RE_latamer, RMAest_RE, RMAest_FE)

  }





#Order factors for plotting
d <- droplevels(d)
d$agecat <- as.character(d$agecat)
d$agecat[d$agecat=="Birth"] <- "At-birth prevalence"
d$agecat[d$agecat=="0-24 months"] <- "0-24 month\ncumulative incidence"
d$agecat[d$agecat=="0-6 months"] <- "0-6 month\ncumulative incidence"
d$agecat[d$agecat=="6 months"] <- "6 month prevalence"
d$agecat[d$agecat=="6-24 months"] <- "6-24 month\ncumulative incidence"
d$agecat[d$agecat=="24 months"] <- "24 month prevalence"
d$agecat <- factor(d$agecat, levels = c("At-birth prevalence","0-6 month\ncumulative incidence","6 month prevalence", "6-24 month\ncumulative incidence", "24 month prevalence","0-24 month\ncumulative incidence"))

unique(d$intervention_level)
d$intervention_level <- factor(d$intervention_level, 
  levels=c("0","1",
  "<259","[259-273)","[273-287)",">=287",
  "<-3","[-3--2)","[-2--1)","[-1-0)",">=0",
  "<18.5", "[18.5-25)",
  "<20","<25","[20-25)","[25-30)",">=30","[30-35)",">=35",
  "<145","[145-150)","[150-155)","[155-160)",">=160",
  "<42.5","[42.5-50)","[50-57.5)",">=57.5",
  "<160","[160-170)",">=170",
  "3 or less", "4-5","6-7", "8+",
  "2","3","4+",
  "3+",
  "Wealth Q1","Wealth Q2","Wealth Q3","Wealth Q4",
  "Q1","Q2","Q3","Q4",
  "Food Secure","Moderately Food Insecure","Mildly Food Insecure","Severely Food Insecure"))

unique(d$intervention_variable)
d$intervention_variable <- factor(d$intervention_variable,
                                       levels=c("birthlen","birthwt", "gagebrth",
                                                "hdlvry","vagbrth",
                                                "enwast","anywast06","pers_wast",
                                                "earlybf","predexfd6","perdiar24",
                                                "mage","fage","mhtcm","fhtcm",
                                                "mwtkg","mbmi","single",
                                                "meducyrs","feducyrs",
                                                "parity",
                                                "nchldlt5","nhh","nrooms",
                                                "hhwealth_quart","hfoodsec",
                                                "impsan","safeh20",#"trth2o",
                                                "impfloor","cleanck"))


#Add variable labels
unique(d$intervention_variable)

d$RFlabel <- NA
d$RFlabel[d$intervention_variable=="enwast"] <-  "Enrolled wasted"
d$RFlabel[d$intervention_variable=="gagebrth"] <-  "Gestational age at birth"
d$RFlabel[d$intervention_variable=="predexfd6"] <-  "Exclusive or Predominant breastfeed under 6 months"
d$RFlabel[d$intervention_variable=="mage"] <- "Mother's age" 
d$RFlabel[d$intervention_variable=="mhtcm"] <- "Mother's height" 
d$RFlabel[d$intervention_variable=="mwtkg"] <- "Mother's weight" 
d$RFlabel[d$intervention_variable=="mbmi"] <- "Mother's BMI" 
d$RFlabel[d$intervention_variable=="meducyrs"] <- "Mother's education" 
d$RFlabel[d$intervention_variable=="parity"] <-  "Birth order" 
d$RFlabel[d$intervention_variable=="hfoodsec"] <- "Household food security" 
d$RFlabel[d$intervention_variable=="nchldlt5"] <-   "Number of children <5 in household"
d$RFlabel[d$intervention_variable=="hhwealth_quart"] <-  "Household wealth" 
d$RFlabel[d$intervention_variable=="fage"] <- "Father's age" 
d$RFlabel[d$intervention_variable=="fhtcm"] <- "Father's height" 
d$RFlabel[d$intervention_variable=="birthwt"] <- "Birthweight (Z-scored)" 
d$RFlabel[d$intervention_variable=="birthlen"] <- "Birth length (Z-scored)" 
d$RFlabel[d$intervention_variable=="vagbrth"] <- "Vaginal birth" 
d$RFlabel[d$intervention_variable=="hdlvry"] <- "Child delivered at home" 
d$RFlabel[d$intervention_variable=="single"] <- "Single parent" 
d$RFlabel[d$intervention_variable=="nrooms"] <- "Number of rooms in household" 
d$RFlabel[d$intervention_variable=="nhh"] <- "Number of people in household" 
d$RFlabel[d$intervention_variable=="meducyrs"] <- "Maternal education quartile" 
d$RFlabel[d$intervention_variable=="feducyrs"] <- "Paternal education quartile" 
d$RFlabel[d$intervention_variable=="anywast06"] <- "Any wasting before 6 months age" 
d$RFlabel[d$intervention_variable=="pers_wast"] <- "Persistent wasting before 6 months age" 
d$RFlabel[d$intervention_variable=="trth2o"] <- "Treats drinking water" 
d$RFlabel[d$intervention_variable=="cleanck"] <- "Clean cooking fuel usage" 
d$RFlabel[d$intervention_variable=="impfloor"] <- "Improved floor" 
d$RFlabel[d$intervention_variable=="impsan"] <- "Improved sanitation" 
d$RFlabel[d$intervention_variable=="safeh20"] <- "Safe water source" 
d$RFlabel[d$intervention_variable=="perdiar6"] <- "Quartile of diarrhea longitudinal\nprevalence under 6 months" 
d$RFlabel[d$intervention_variable=="perdiar24"] <- "Quartile of diarrhea longitudinal\nprevalence under 24 months" 
d$RFlabel[d$intervention_variable=="earlybf"] <- "Breastfeed within an hour of birth" 


#order by region
d$region <- as.character(d$region)
d$region <- factor(d$region, levels=c("Pooled","Asia", "Latin America","Africa","Europe"))
d <- d[with(d, order(desc(pooled), region)),]
unique(d$studyid)

d$studyid <- as.character(d$studyid)
d$studyid <- factor(d$studyid, levels=unique(d$studyid))
d$order <- as.numeric(d$studyid)

#Clean up adjustment sets (drop W_ and the missingness indicators)
d$adjustment_set <- gsub("W_","",d$adjustment_set)
#d$adjustment_set <- gsub("delta_","m_",d$adjustment_set)
for(i in 1:30){d$adjustment_set <- gsub("(delta_).*?,", "", d$adjustment_set)}
d$adjustment_set <- gsub("(delta_).*?", "", d$adjustment_set)
d$adjustment_set <- gsub("  ", " ", d$adjustment_set)
d$adjustment_set <- gsub("  ", " ", d$adjustment_set)
d$adjustment_set <- gsub("  ", " ", d$adjustment_set)

for(i in 1:nrow(d)){
  if(nchar(d$adjustment_set[i])>50 & !is.na(d$adjustment_set[i])){
   d$adjustment_set[i] <- paste0(substr(d$adjustment_set[i],1,50),"\n",substr(d$adjustment_set[i],50, nchar(d$adjustment_set[i])))
  }
}

  if(measure=="RR"){Ylab="Relative Risk\nwith N's and number of cases printed on the left and\nthe set of adjustment variables printed on the right"}
  if(measure=="ATE"){Ylab="Average Treatment Effect"}

d <- droplevels(d)

#Print plots across all intervention arms

pdf(pdfname, height=12, width=12)

for(i in levels(d$intervention_variable)){
  
  df <- d[d$intervention_variable==i,]  
  df$studyid <- as.character(df$studyid)
  
  #Add reference N's to the studyid label
  df <- df %>% fill(baseline_level) %>%  fill(baseline_level, .direction="up") 
  
    if(measure=="RR"){
          df$studyid2 <- paste0(df$studyid, ": ",df$N_cases," cases / ",df$N)
          df$studyid2[df$pooled==1] <- paste0(df$studyid[df$pooled==1], ": ",df$Nstudies[df$pooled==1]," studies, ",df$N_cases[df$pooled==1]," cases / ",df$N[df$pooled==1])
          df$studyid2[df$intervention_level!=df$baseline_level] <- NA
    }else{
      df$Ns <- paste0("N= ",df$N," Mean=",df$mean)
          df$studyid2 <- paste0(df$studyid, ": ","N= ",df$N," Mean=",df$mean)
          df$studyid2[df$intervention_level!=df$baseline_level] <- NA
          df$studyid2[df$pooled==1] <- df$studyid[df$pooled==1]
    }
  
  df <- df %>% 
  group_by(studyid) %>% 
  fill(studyid2) %>%  fill(studyid2, .direction="up") %>% 
  ungroup()
  
  table(df$studyid2)
  
  df <- df[with(df, order(desc(pooled), region)),]
  df$studyid <- factor(df$studyid, levels=unique(df$studyid))
  df$studyid2 <- factor(df$studyid2, levels=unique(df$studyid2))

  df <- df %>% filter(RR.CI1!=RR.CI2) #drop reference level
  df <- droplevels(df)
  Npooled <- sum(df$pooled)/length(unique(df$intervention_level))
  df$pooled <- factor(df$pooled)
  
  stdyID <- levels(df$studyid)
  Nstud <- addNA(factor(df$adjustment_set))
  
    if(measure=="RR"){
  df$Ns <- paste0(df$N_cases," cases / ",df$N)
  df$Ns[df$pooled==1] <- paste0(df$Nstudies[df$pooled==1]," studies, ",df$N_cases[df$pooled==1]," cases / ",df$N[df$pooled==1])
  df$Ns[is.na(df$N)] <- ""
    }else{
      df$Ns <- paste0("N= ",df$N," Mean=",df$mean)
  df$Ns[is.na(df$N)] <- ""
    }
  # df$N[!is.na(df$N)] <- paste0("N= ",df$N[!is.na(df$N)])
  # df$N[is.na(df$N)] <- ""
  
    p <-  ggplot(df, aes(x=studyid2)) + 
          #geom_point(aes(y=RR, fill=region, color=region), size = 4, shape= ifelse(df$pooled==1,5,6)) +
          geom_point(aes(shape=pooled, y=RR, fill=region, color=region), size = 4) +
          geom_linerange(aes(ymin=RR.CI1, ymax=RR.CI2, color=region)) +
          coord_flip(ylim=range(yticks)) +
          labs(x = "Study-specific results stratified by risk factor level\nwith reference category N's and cases printed", y = Ylab) +
          geom_hline(yintercept = 1) +
          geom_vline(xintercept = 2.5, linetype=2) +
          geom_vline(xintercept = Npooled+0.5) +
          geom_text(aes(y=0.12, label=Ns), size=3,  hjust=0) +
          geom_text(aes(y=16, label=adjustment_set), size=3,  hjust=0) +
          scale_y_continuous(breaks=yticks, trans='log10', labels=scaleFUN) +
          #scale_x_discrete(labels= df$studyid2) +
          scale_shape_manual(values=c(21, 23)) +
          scale_fill_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(df$region)) +
          scale_colour_manual(values=rep(tableau10,4), drop=TRUE, limits = levels(df$region)) +
          scale_size_continuous(range = c(0.5, 1))+
          theme(strip.background = element_blank(),
            legend.position="bottom",
            strip.text.x = element_text(size=12),
            axis.text.x = element_text(size=12, angle = 45, hjust = 1)) +
          facet_wrap(~intervention_level, ncol=1) +
          ggtitle(paste0("Risk factor: ", df$RFlabel[1], "\n",
                         "Ref. level: ", df$baseline_level[!is.na(df$baseline_level)], "\n", 
                         "Outcome: ", df$agecat))
    print(p)
  }
dev.off()
}


setwd("C:/Users/andre/Dropbox/HBGDki figures/Risk Factor Analysis/Stunting/Forest Plots")


head(d)
table(d$type)
table(d$agecat)

KI_forest_plot(d)
dev.off()
KI_forest_plot(d, agerange="Birth", pdfname="Risk Factor Forest Plots birth prevalence.pdf")
dev.off()
KI_forest_plot(d, agerange="24 months", pdfname="Risk Factor Forest Plots 24mo prevalence.pdf")
dev.off()


KI_forest_plot(d, agerange="0-24 months", pdfname="Risk Factor Forest Plots 0-24mo CI.pdf")
dev.off()
KI_forest_plot(d, agerange="0-6 months", pdfname="Risk Factor Forest Plots 0-6mo CI.pdf")
dev.off()
KI_forest_plot(d, agerange="6-24 months", pdfname="Risk Factor Forest Plots 6-24mo CI.pdf")
dev.off()


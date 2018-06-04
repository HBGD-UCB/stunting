
#---------------------------------------
# fit.rma function
#---------------------------------------

# random effects function, save results nicely
fit.rma=function(data,age,ni,xi,measure,nlab){
  data=filter(data,agecat==age)
  if(measure!="IR"){
    fit<-rma(ni=data[[ni]], xi=data[[xi]], 
           method="REML", measure=measure)
  }else{
    fit<-rma(ti=data[[ni]], xi=data[[xi]], 
             method="REML", measure=measure)
  }
  out=data %>%
    ungroup() %>%
    summarise(nstudies=length(unique(studyid)),
              nmeas=sum(data[[ni]][agecat==age])) %>%
    mutate(agecat=age,est=fit$beta, se=fit$se, lb=fit$ci.lb, ub=fit$ci.ub,
           nmeas.f=paste0("N=",format(sum(data[[ni]]),big.mark=",",scientific=FALSE),
                          " ",nlab),
           nstudy.f=paste0("N=",nstudies," studies"))
  return(out)
}


sem<-function(x){
  sd(x)/sqrt(length(x))
}



#---------------------------------------
# fit.escalc function
#---------------------------------------

# calc individual cohort PR variances, standard errors, and 95% CI from the rma() arguements, and append to dataset
# Input:
# meas: PR for prevalence, CI for cumulative incidence, and IR for incidence rate

#Returns:
# Inputted dataframe with appended columns
# yi = outcome of interest
# vi = variance of outcome
# se = standard error
# ci.lb = lower bound of 95% confidence interval
# ci.ub = upper bound of 95% confidence interval

fit.escalc <- function(data,age,ni,xi, meas){
  data=filter(data,agecat==age)
  
  if(meas=="PR"){
  data<-escalc(data=data, ni=data[[ni]], xi=data[[xi]], method="REML", measure="PR", append=T)
  }

  if(meas=="IR"){
  data<-escalc(data=data, ti=data[[ni]], xi=data[[xi]], method="REML", measure="IR", append=T)
  }
  
data$se <- sqrt(data$vi)
data$ci.lb <- data$yi - 1.96 * data$se 
data$ci.ub <- data$yi + 1.96 * data$se 

  return(data)
}


#---------------------------------------
# cohort-specific output formatting
#---------------------------------------
# if percentage, multiply est and ci by 100
# create cohort name for plotting
# create region variable 
# add age labels for x-axis

# Input:
# data frame with fit.escalc output 
# vector of labels for plotting

#Returns:
# data frame formatted for plotting cohort specific results
cohort.format=function(df, lab, y, est="percent"){
  y = as.numeric(y)
  
  # rescale percentages
  if(est=="percent"){
    df = df %>% mutate(y=y*100,ci.lb=ci.lb*100,ci.ub=ci.ub*100)
  }
  if(est=="rate"){
    df = df %>% mutate(y=y*1000,ci.lb=ci.lb*1000,ci.ub=ci.ub*1000)
  }

  # cohort name
  df = df %>% mutate(cohort=paste0(studyid,"-",country)) %>%
              mutate(cohort=gsub("ki[^-]*-","",cohort))
  
  # region variable
  df <- df %>% mutate(region = case_when(
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
  
   # create formatted age categories for plotting 
  df <- df %>%  mutate(agecat=droplevels(agecat))
  df <- df %>%  mutate(age.f = factor(agecat,levels=levels(df$agecat),
                           labels=lab))

  return(df)
}





#---------------------------------------
# fit.rma function
#---------------------------------------

# random effects function, save results nicely
fit.rma=function(data,age,ni,xi,measure,nlab){
  data=filter(data,agecat==age)
  fit<-rma(ni=data[[ni]], xi=data[[xi]], 
           method="REML", measure=measure)
  out=data %>%
    ungroup() %>%
    summarise(nstudies=length(unique(studyid)),
              nmeas=sum(data[[ni]][agecat==age])) %>%
    mutate(agecat=age,est=fit$beta, se=fit$se, lb=fit$ci.lb, ub=fit$ci.ub,
           nmeas.f=paste0("N=",format(sum(data[[ni]]),big.mark=",",scientific=FALSE),
                          nlab),
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

fit.escalc <- function(data,age,ni,xi, meas="PR"){
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




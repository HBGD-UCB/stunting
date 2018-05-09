# random effects function, save results nicely
fit.rma=function(data,age,ni,xi){
  data=filter(data,agecat==age)
  fit<-rma(ni=data[[ni]], xi=data[[xi]], 
           method="REML", measure="PR")
  out=data %>%
    ungroup() %>%
    summarise(nstudies=length(unique(studyid)),
              nmeas=sum(data[[ni]][agecat==age])) %>%
    mutate(agecat=age,est=fit$beta, se=fit$se, lb=fit$ci.lb, ub=fit$ci.ub,
           nmeas.f=paste0("N=",format(sum(data[[ni]]),big.mark=",",scientific=FALSE),
                          " measurements"),
           nstudy.f=paste0("N=",nstudies," studies"))
  return(out)
}


sem<-function(x){
  sd(x)/sqrt(length(x))
}

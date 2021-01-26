########### Compute Probability of being under the LOQ, for all observations

#' Compute P(y_ij<LOQ) for all observations
#' 
#' For each observation in the dataset, computes the probability to be below LOQ
#' using the simulations under the model. This is generally performed automatically.
#' 
#' @usage compute.ploq(npdeObject)
#' 
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @return an object of class NpdeObject with a component ploq in the ["results"] slot
#' 
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords internal

compute.ploq<-function(npdeObject) {
  # For each observation, computes the probability of being LOQ under the model
  ### when the data has different LOQ values, the LOQ is taken to be the smallest LOQ
  ### p_LOQ is computed as the % of simulated values under the LOQ
  if(length(npdeObject["data"]["icens"])==0)
    return(npdeObject)
  ploq<-c()
  ploqfull<-rep(NA,dim(npdeObject["data"]["data"])[1])
  tab<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"],] # non-missing data
  tabsim<-npdeObject["sim.data"]["datsim"][npdeObject["data"]["not.miss"],] # corresponding simulated data    
  yobs<-tab[, npdeObject["data"]["name.response"]]
  nrep<-npdeObject["sim.data"]["nrep"]
  ysim<-tabsim$ysim
  idsim<-tabsim$idsim
  
  if(length(npdeObject["data"]["loq"])>0) {
    loq<-npdeObject["data"]["loq"]
    if(npdeObject@options$verbose) cat("Computing p(y<LOQ) using LOQ=",loq,"\n")
  } else {
    yloq<-npdeObject["data"]["data"][npdeObject["data"]["icens"], npdeObject["data"]["name.response"]]
    if(length(unique(yloq))==1) {
      if(npdeObject@options$verbose) cat("Same LOQ for all missing data, loq=",loq,"\n")
      loq<-unique(yloq)
    } else {
      loq<-min(unique(yloq))
      if(npdeObject@options$verbose) cat("Computing p(y<LOQ) for the lowest LOQ, loq=",loq,"\n")
    }
    npdeObject["data"]["loq"]<-loq
  }
  for(isuj in unique(npdeObject["data"]["data"][,"index"])) {
    matsim<-matrix(ysim[idsim==isuj],ncol=nrep)
    tcomp<-apply(matsim,2,"<",loq)
    if(!is.matrix(tcomp)) tcomp<-t(as.matrix(tcomp))
    ycal<-rowMeans(tcomp)
    ploq<-c(ploq,ycal)
  }
  ploqfull[npdeObject["data"]["not.miss"]]<-ploq
  npdeObject["results"]["ploq"]<-ploqfull
  #saving pd
  invisible(npdeObject)
}

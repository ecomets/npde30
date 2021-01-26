
########### Compute distribution of pd/npde under the model, using simulations

#' Compute distribution of pd/npde using simulations
#'
#' This function is used to build the distribution of pd/npde using the simulations under the model. The default is to build only the distribution of pd, and to sample from N(0,1) when building the distribution of npde under the null hypothesis.
#'
#' @aliases dist.pred.sim calcnpde.sim
#' @usage dist.pred.sim(npdeObject,nsamp, ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param nsamp number of datasets (defaults to 100 or to the number of replications if it is smaller)
#' @param \dots additional arguments. Currently only the value of calc.pd and calc.npde may be passed on, and will override their corresponding value in the "options" slot of npdeObject
#' @return an object of class NpdeObject; the ["results"] slot will contain pd and/or npde for a sample of the simulated datasets (depending on whether calc.pd/calc.npde are ), stored in pd.sim and/or npde.sim
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @export
#' @examples
#'\dontrun{
#' data(theopp)
#' data(simtheopp)
#' x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
#' # Use random samples from N(0,1) to obtain a prediction interval on the empirical cdf of the npde
#' plot(x,plot.type="ecdf",bands=TRUE,approx.pi=TRUE)
#' # defaults to computing the pd and npde for 100 simulated datasets
#' # (in the theophylline example, this uses all the simulated datasets)
#' x<-dist.pred.sim(x)
#' # Use the npde from the simulated datasets to obtain a prediction interval on the empirical cdf
#' plot(x,plot.type="ecdf",bands=TRUE,approx.pi=FALSE)
#'}
#'
dist.pred.sim<-function(npdeObject,nsamp, ...) {
  args1<-match.call(expand.dots=TRUE)
  i1<-match("calc.pd",names(args1))
  calc.pd<-NA
  if(!is.na(i1)) calc.pd<-as.logical(as.character(args1[[i1]]))
  if(is.na(calc.pd)) calc.pd<- npdeObject["options"]$calc.pd
  i1<-match("calc.npde",names(args1))
  calc.npde<-NA
  if(!is.na(i1)) calc.npde<-as.logical(as.character(args1[[i1]]))
  if(is.na(calc.npde)) calc.npde<- npdeObject["options"]$calc.npde
  if(!calc.pd & !calc.npde) {
    cat("At least one of calc.pd or calc.npde must be TRUE.\n")
    return(npdeObject)
  }



  # ECO not necessary since we're not using the imputed data... why not ??? maybe should ?
  # 	if(npdeObject["options"]$cens.method=="cdf" && length(npdeObject["sim.data"]["icens"])>0 && length(npdeObject["sim.data"]["datsim"]$ysim.imp)==0) {
  # 		cat("With the cdf method, the imputation step needs to be performed before a call to dist.pred.sim() is made. Please use computepd(x) first.\n")
  # 		return(npdeObject)
  # 	}
  nrep<-npdeObject["sim.data"]["nrep"]
  if(missing(nsamp)) nsamp<-min(100,nrep)
  # Extracting non-missing data
  keep<-npdeObject["data"]["not.miss"]
  tabsim<-npdeObject["sim.data"]["datsim"][keep,] # simulated data
  # Use original simulations to compute the PI under the model
  idsim<-tabsim$idsim
  ysim<-yobs<-matrix(tabsim[,"ysim"],ncol=nrep)
  colnames(yobs)<-paste("sim",1:nrep,sep="")
  if(nsamp<nrep) {
    isample<-sort(sample(1:nrep,nsamp))
    yobs<-yobs[,isample]
  }
  idobs<-npdeObject["data"]["data"][keep,npdeObject["data"]["name.group"]]

  pd<-npde<-matrix(nrow=0,ncol=nsamp,dimnames=list(NULL,colnames(yobs)))
  # Loop on isuj
  for(isuj in unique(idobs)) {
    matsim<-matrix(ysim[idsim==isuj],ncol=nrep)
    ysuj<-yobs[idobs==isuj,]
    if(calc.pd) {
      pdsuj<-c()
      # compute pdsim_ij
      for(i in 1:nsamp) {
        tcomp<-apply(matsim,2,"<",ysuj[,i])
        if(!is.matrix(tcomp)) tcomp<-t(as.matrix(tcomp))
        ycal<-rowMeans(tcomp)
        # pdsim_ij will be within the sequence seq(0,1-1/nrep,1/nrep) by construction
        pdsuj<-c(pdsuj,ycal)
      }
      # If ties: add 1/(2*nrep) to center the distribution to 1/2*nrep ; 1-1/2*nrep
      # else add U(0,1/nrep)
      if(npdeObject["options"]$ties)
        pdsuj<-pdsuj+1/(2*nrep)
      else
        pdsuj<-pdsuj+runif(length(pdsuj),0,1/nrep)
      pd<-rbind(pd,matrix(pdsuj,ncol=nsamp))
    }
    if(calc.npde) {
      y<-calcnpde.sim(ysuj,matsim,nrep,npdeObject["options"]$decorr.method)
      if(y$xerr==0) {
        pde<-y$pde
        if(npdeObject["options"]$ties)
          pde<-pde+1/(2*nrep)
        else
          pde<-pde+runif(length(pde),0,1/nrep)
        npde<-rbind(npde,qnorm(pde))
      } else {
        cat("Problem computing npde for subject",isuj,"\n")
        return(npdeObject)
      }
    }
  }

  # Saving results
  if(calc.pd) npdeObject["results"]["pd.sim"]<-pd
  if(calc.npde) npdeObject["results"]["npde.sim"]<-npde
  return(npdeObject)
}

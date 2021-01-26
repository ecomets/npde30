########### Compute pd

#' Internal functions used to compute prediction discrepancies
#' 
#' Functions used by \code{npde} and \code{autonpde} to compute prediction
#' discrepancies (not intended for the end-user).
#' 
#' These functions are normally not called by the end-user.
#' 
#' @usage computepd(npdeObject,...)
#' 
#' @param npdeObject an object of class NpdeObject
#' @param \ldots additional options to modify
#' @return an object of class NpdeObject; the results slot will now include prediction discrepancies (pd) as well as model predictions (ypred) obtained as the mean of the simulated data for each observation
#' 
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords internal

computepd<-function(npdeObject,...) {
  npdeObject@options<-replace.control.options(npdeObject@options,...)
  if(npdeObject["options"]$cens.method=="omit" | length(npdeObject["data"]["icens"])==0)
    object<-computepd.omit(npdeObject) else {
      if(npdeObject["options"]$cens.method=="cdf") {
        if(length(npdeObject["results"]["ploq"])==0)
          npdeObject<-compute.ploq(npdeObject)
        object<-computepd.cdf(npdeObject)
      }
      if(npdeObject["options"]$cens.method %in% c("ipred","ppred"))
        object<-computepd.replace(npdeObject)
      if(npdeObject["options"]$cens.method=="fix")
        object<-computepd.replace(npdeObject,...)
      if(npdeObject["options"]$cens.method=="loq") {
        if(length(npdeObject["data"]["loq"])==0)
          npdeObject<-compute.ploq(npdeObject)
        object<-computepd.replace(npdeObject)
      }
    }
  return(object)
}

# Functions to sort the lines/columns of a matrix
linesort<-function(mat) {
  for(i in 1:dim(mat)[1])
    mat[i,]<-sort(mat[i,])
  return(mat)
}
colsort<-function(mat) {
  for(i in 1:dim(mat)[2])
    mat[,i]<-sort(mat[,i])
  return(mat)
}

computepd.omit<-function(npdeObject) {
  # Preparing vectors for the different results
  ypredfull<-pdfull<-rep(NaN,dim(npdeObject["data"]["data"])[1])
  ycomp<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
  # Extracting only non-missing and non-censored data
  if(length(npdeObject["data"]["icens"])==0) keep<-npdeObject["data"]["not.miss"] else keep<-(npdeObject["data"]["not.miss"] & npdeObject["data"]["data"][,npdeObject["data"]["name.cens"]]==0)
  # missing and censored data = NaN in complete data
  ycomp[!keep]<-NaN
  tab<-npdeObject["data"]["data"][keep,] # non-missing data
  tabsim<-npdeObject["sim.data"]["datsim"][keep,] # corresponding simulated data    
  yobs<-tab[,npdeObject["data"]["name.response"]]
  idobs<-tab[,npdeObject["data"]["name.group"]]
  nrep<-npdeObject["sim.data"]["nrep"]
  ysim<-tabsim$ysim
  idsim<-tabsim$idsim
  
  ypredall<-pd<-c()
  # Loop on isuj
  for(isuj in unique(idobs)) {
    matsim<-matrix(ysim[idsim==isuj],ncol=nrep)
    ysuj<-yobs[idobs==isuj]
    # Compute ypred
    ypred<-rowMeans(matsim)
    ypredall<-c(ypredall,ypred)
    # compute pd_ij
    tcomp<-apply(matsim,2,"<",ysuj)
    if(!is.matrix(tcomp)) tcomp<-t(as.matrix(tcomp))
    ycal<-rowMeans(tcomp)
    if(npdeObject["options"]$ties) {
      ycal[!is.na(ycal) & ycal==0]<-1/(2*nrep)
      ycal[!is.na(ycal) & ycal==1]<-1-1/(2*nrep)
    } else {
      idx<-which(!is.na(ycal) & ycal>0 & ycal<1)
      ycal[!is.na(ycal) & ycal==0]<-runif(sum(ycal==0),0,1/nrep)
      ycal[!is.na(ycal) & ycal==1]<-runif(sum(ycal==0),1-1/nrep,1)
      ycal[idx]<-ycal[idx]+runif(length(idx),0,1/nrep)
    }
    pd<-c(pd,ycal)
  }
  pdfull[keep]<-pd
  ypredfull[keep]<-ypredall
  
  # Saving results
  if(length(npdeObject["results"]["res"])==0) { # create data.frame res
    res<-data.frame(ypred=ypredfull,ycomp=ycomp,pd=pdfull)
    npdeObject["results"]["res"]<-res
  } else { # append to data.frame res
    npdeObject["results"]["res"]$ypred<-ypredfull
    npdeObject["results"]["res"]$ycomp<-ycomp
    npdeObject["results"]["res"]$pd<-pdfull
  }
  
  return(npdeObject)
}

computepd.replace<-function(npdeObject,fix=NULL) {
  # Preparing vectors for the different results
  ypredfull<-pdfull<-rep(NaN,dim(npdeObject["data"]["data"])[1])
  ycomp<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
  # Extracting only non-missing data
  keep<-npdeObject["data"]["not.miss"]
  # missing data = NaN in complete data
  ycomp[!keep]<-NaN
  tab<-npdeObject["data"]["data"][keep,] # non-missing data
  tabsim<-npdeObject["sim.data"]["datsim"][keep,] # corresponding simulated data    
  yobs<-tab[,npdeObject["data"]["name.response"]]
  icens<-tab[,npdeObject["data"]["name.cens"]]
  idobs<-tab[,npdeObject["data"]["name.group"]]
  if(npdeObject["options"]$cens.method=="fix" & !is.null(fix)) 
    yobs[icens==1]<-fix
  if(npdeObject["options"]$cens.method=="ipred") 
    yobs[icens==1]<-tab[icens==1,npdeObject["data"]["name.ipred"]]
  if(npdeObject["options"]$cens.method=="loq") 
    yobs[icens==1]<-npdeObject["data"]["loq"]
  nrep<-npdeObject["sim.data"]["nrep"]
  ysim<-tabsim$ysim
  idsim<-tabsim$idsim
  
  ypredall<-pd<-c()
  # Loop on isuj
  for(isuj in unique(idobs)) {
    matsim<-matrix(ysim[idsim==isuj],ncol=nrep)
    ysuj<-yobs[idobs==isuj]
    # Compute ypred
    ypred<-rowMeans(matsim)
    ypredall<-c(ypredall,ypred)
    if(npdeObject["options"]$cens.method=="ppred") ysuj[icens[idobs==isuj]==1]<-ypred[icens[idobs==isuj]==1]
    # compute pd_ij
    tcomp<-apply(matsim,2,"<",ysuj)
    if(!is.matrix(tcomp)) tcomp<-t(as.matrix(tcomp))
    ycal<-rowMeans(tcomp)
    if(npdeObject["options"]$ties) {
      ycal[!is.na(ycal) & ycal==0]<-1/(2*nrep)
      ycal[!is.na(ycal) & ycal==1]<-1-1/(2*nrep)
    } else {
      idx<-which(!is.na(ycal) & ycal>0 & ycal<1)
      ycal[!is.na(ycal) & ycal==0]<-runif(sum(ycal==0),0,1/nrep)
      ycal[!is.na(ycal) & ycal==1]<-runif(sum(ycal==0),1-1/nrep,1)
      ycal[idx]<-ycal[idx]+runif(length(idx),0,1/nrep)
    }
    pd<-c(pd,ycal)
  }
  pdfull[keep]<-pd
  ypredfull[keep]<-ypredall
  if(npdeObject["options"]$cens.method=="ppred") 
    ycomp[npdeObject["data"]["icens"]]<-ypredfull[npdeObject["data"]["icens"]]
  if(npdeObject["options"]$cens.method %in% c("ipred","fix","loq")) { 
    ycomp[npdeObject["data"]["icens"]]<-yobs[icens==1]
  }
  # Saving results
  if(length(npdeObject["results"]["res"])==0) { # create data.frame res
    res<-data.frame(ypred=ypredfull,ycomp=ycomp,pd=pdfull)
    npdeObject["results"]["res"]<-res
  } else { # append to data.frame res
    npdeObject["results"]["res"]$ypred<-ypredfull
    npdeObject["results"]["res"]$ycomp<-ycomp
    npdeObject["results"]["res"]$pd<-pdfull
  }
  return(npdeObject)
}

computepd.cdf<-function(npdeObject) {
  # Censored observation
  ### cdf: replace pd_ij with random sample from a uniform distribution [0-p_LOQ_ij], where p_LOQ_ij is the probability of being under the LOQ (==previous computation of pd_ij because y_ij=censoring value)
  
  # Preparing vectors for the different results
  ypredfull<-pdfull<-rep(NaN,dim(npdeObject["data"]["data"])[1])
  ycomp<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
  # Indicators for censored data to be imputed
  not.miss<-npdeObject["data"]["not.miss"]
  # missing data = NaN in complete data
  ycomp[!not.miss]<-NaN
  tab<-npdeObject["data"]["data"][not.miss,] # non-missing data
  tabsim<-npdeObject["sim.data"]["datsim"][not.miss,] # corresponding simulated data    
  yobs<-tab[,npdeObject["data"]["name.response"]]
  ycens<-tab[,npdeObject["data"]["name.cens"]]
  ploq<-npdeObject["results"]["ploq"][not.miss]
  loq<-npdeObject["data"]["loq"]
  has.cens<-(length(loq)>0)
  idobs<-tab[,npdeObject["data"]["name.group"]]
  nrep<-npdeObject["sim.data"]["nrep"]
  ysim<-tabsim$ysim
  idsim<-tabsim$idsim
  ypredall<-pd<-c()
  
  # ECO ici comment gerer les indices proprement ?
  # Compute prediction discrepancies	
  # Loop on isuj
  for(isuj in unique(idobs)) {
    matsim<-matrix(ysim[idsim==isuj],ncol=nrep)
    # compute pd_ij
    tcomp<-apply(matsim,2,"<",yobs[idobs==isuj])
    if(!is.matrix(tcomp)) tcomp<-t(as.matrix(tcomp))
    pdsuj<-ycal<-rowMeans(tcomp)
    ploq.suj<-ploq[idobs==isuj]
    # impute pd_ij for censored data
    iobs.loq<-which(ycens[idobs==isuj]==1)
    if(length(iobs.loq)>0)
      pdsuj[iobs.loq]<-runif(length(iobs.loq),0,ploq.suj[iobs.loq])
    # Compute ypred
    ypred<-rowMeans(matsim)
    ypredall<-c(ypredall,ypred)
    # If we plan to compute npde afterwards, we need to impute observed & simulated data under the LOQ; for simulated data, we first need to impute pd in the same way
    isim.loq<-which(matsim<loq,arr.ind=TRUE)
    if(npdeObject["options"]$calc.npde & (dim(isim.loq)[1]+length(iobs.loq))>0) {
      # when pd=k/K, jitter by imputing an observation to a value between ysim(k) (kth simulated value) and ysim(k+1)
      matsort<-linesort(matsim)
      if(!npdeObject["options"]$ties) {
        # Pb of jittering when the imputed pd is 1 (k=K) because no ysim(K+1) 
        ncol<-dim(matsort)[2]
        matsort<-cbind(matsort,matsort[,ncol]*2-matsort[,(ncol-1)])
      }
      if(length(iobs.loq)>0){
        ids1<-trunc(pdsuj[iobs.loq]*nrep)+1
        if(!npdeObject["options"]$ties) {
          yobs.imp<-yobs[idobs==isuj]
          for(j in 1:length(iobs.loq)) yobs.imp[iobs.loq[j]]<-runif(1, matsort[iobs.loq[j],ids1[j]],matsort[iobs.loq[j],(ids1[j]+1)])
          yobs[idobs==isuj]<-yobs.imp
        } else {
          yobs.imp<-yobs[idobs==isuj]
          yobs.imp[iobs.loq]<-diag(matsort[iobs.loq,ids1,drop=F])
          yobs[idobs==isuj]<-yobs.imp
        }
      }
      # saving the imputed data for simulations in the simulated dataset
      if(dim(isim.loq)[1]>0) {
        pdsim.imp<-runif(dim(isim.loq)[1],0,ploq.suj[isim.loq[,1]])
        ids2<-trunc(pdsim.imp*nrep)+1
        isim.repl<-isim.loq
        if(!npdeObject["options"]$ties) {
          ids2[ids2>=nrep]<-nrep-1
          isim.repl[,2]<-ids2
          isim.repl2<-isim.repl
          isim.repl2[,2]<-isim.repl2[,2]+1
          matsim[isim.loq]<-runif(dim(isim.loq)[1], matsort[isim.repl], matsort[isim.repl2])
        } else {
          isim.repl[,2]<-ids2
          matsim[isim.loq]<-matsort[isim.repl]
        }
        ysim[idsim==isuj & ysim<loq]<-matsim[isim.loq]
      }
    }
    pd<-c(pd,pdsuj)
  }
  
  # Dealing with extreme values of pd & smoothing if !(npdeObject["options"]$ties)
  if(npdeObject["options"]$ties) {
    pd[pd==0]<-1/(2*nrep)
    pd[pd==1]<-1-1/(2*nrep)
  } else {
    idx<-which(pd>0 & pd<1)
    pd[pd==0]<-runif(sum(pd==0),0,1/nrep)
    pd[pd==1]<-runif(sum(pd==1),1-1/nrep,1)
    pd[idx]<-pd[idx]+runif(length(idx),0,1/nrep)
  }
  
  if(npdeObject["options"]$calc.npde) {
    ycomp[not.miss]<-yobs
    npdeObject["sim.data"]["datsim"][not.miss,"ysim.imp"]<-ysim
  }
  ypredfull[not.miss]<-ypredall
  pdfull[not.miss]<-pd
  # Saving results
  if(length(npdeObject["results"]["res"])==0) { # create data.frame res
    res<-data.frame(ypred=ypredfull,ycomp=ycomp,pd=pdfull)
    npdeObject["results"]["res"]<-res
  } else { # append to data.frame res
    npdeObject["results"]["res"]$ypred<-ypredfull
    npdeObject["results"]["res"]$ycomp<-ycomp
    npdeObject["results"]["res"]$pd<-pdfull
  }
  return(npdeObject)
}

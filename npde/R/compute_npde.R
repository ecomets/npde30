########### Compute npde

#' Internal functions used to compute normalised prediction distribution errors (npde)
#'
#' Functions used by \code{npde} and \code{autonpde} to compute prediction
#' discrepancies (not intended for the end-user).
#'
#' These functions are normally not called by the end-user.
#'
#' @usage computenpde(npdeObject,...)
#' @param npdeObject an object of class NpdeObject
#' @param \ldots additional options to modify
#' @return an object of class NpdeObject; the results slot will now include prediction discrepancies (npde) as well as decorrelated observed data, while the sim.data slot will now include decorrelated simulated data
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}
#'
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords internal
#' @importFrom stats cov dnorm rnorm spline qqline qqnorm qqplot

computenpde<-function(npdeObject,...) {
	npdeObject@options<-replace.control.options(npdeObject@options,...)
	if(npdeObject["options"]$cens.method=="cdf" & length(npdeObject["data"]["icens"])>0 & length(npdeObject["results"]["res"]$ycomp)==0) {
		cat("With method",npdeObject["options"]$cens.method,"prediction discrepancies need to be computed first \n")
		npdeObject<-computepd(npdeObject)
	}
	if(npdeObject["options"]$cens.method=="omit" | length(npdeObject["data"]["icens"])==0)
		object<-computenpde.omit(npdeObject) else
			object<-computenpde.loq(npdeObject)
	return(object)
}

computenpde.omit<-function(npdeObject) {
	# Compute (normalised) prediction distribution errors omitting BQL
	# Preparing vectors for the different results
	# Indicators for censored data to be imputed
	if(npdeObject["options"]$cens.method=="omit" & length(npdeObject["data"]["icens"])>0) {
		tab<-npdeObject["data"]["data"]
		not.miss<-(tab[,npdeObject["data"]["name.miss"]]==0 & tab[,npdeObject["data"]["name.cens"]]==0)
	} else not.miss<-npdeObject["data"]["not.miss"]
	tab<-npdeObject["data"]["data"][not.miss,] # non-missing data
	tabsim<-npdeObject["sim.data"]["datsim"][not.miss,] # corresponding simulated data
	ypred<-pde<-rep(NA,dim(tab)[1])
	yobs.all<-ydobs<-tab[,npdeObject["data"]["name.response"]]
	npdefull<-ydobsfull<-rep(NA,dim(npdeObject["data"]["data"])[1])
	ydsimfull<-rep(NA,dim(npdeObject["sim.data"]["datsim"])[1])
	id<-tab[,npdeObject["data"]["name.group"]]
	nrep<-npdeObject["sim.data"]["nrep"]
	xerr<-0
	ysim<-ydsim<-tabsim$ysim
	idsim<-tabsim$idsim

	# computing pde
	for(isuj in unique(id)) {
		yobs<-yobs.all[id==isuj]
		matsim<-matrix(ydsim[idsim==isuj],ncol=nrep)
		x<-calcnpde(isuj,yobs,matsim,nrep,npdeObject["options"]$decorr.method, npdeObject["options"]$verbose)
		xerr<-x$xerr
		if(x$xerr==0) {
		  pde[id==isuj]<-x$pde
		  ydsim[idsim==isuj]<-x$ydsim
		  ydobs[id==isuj]<-x$ydobs
		  ypred[id==isuj]<-x$ypred
		}
		if (xerr>0) break
	}
	if(xerr>0) {
		cat("The computation of the pde has failed for subject",isuj,"because \n")
		if(xerr==1) {
			if(npdeObject["options"]$decorr.method=="cholesky") cat("the Cholesky decomposition of the covariance matrix of the simulated data could not be obtained.\n")
			if(npdeObject["options"]$decorr.method=="inverse") cat("the covariance matrix of the simulated data could not be diagonalised through eigen().\n")
			if(npdeObject["options"]$decorr.method=="polar") cat("the Cholesky decomposition of the covariance matrix of the simulated data could not be obtained.\n")
		}
		if(xerr==2) cat("the covariance matrix of the simulated data could not be inverted.\n")
		cat("This usually means that the covariance matrix is not positive-definite, or that is is poorly conditioned.\n")
		cat("This can be caused by simulations widely different from observations (in \n")
		cat("other words, a poor model).\n")
		cat("We suggest to plot a prediction interval from the simulated data to check\n")
		cat("whether the simulations are reasonable, and to consider prediction\n")
		cat("discrepancies (obtained without the decorrelation step).\n")
		cat("Prediction discrepancies will now be computed.\n")
#		break
	}

	# saving pde
	if(xerr==0) {
		if(npdeObject["options"]$ties) {
			pde[pde==0]<-1/(2*nrep)
			pde[pde==1]<-1-1/(2*nrep)
		} else {
			idx<-which(pde>0 & pde<1)
			pde[pde==0]<-runif(sum(pde==0),0,1/nrep)
			pde[pde==1]<-runif(sum(pde==1),1-1/nrep,1)
			pde[idx]<-pde[idx]+runif(length(idx),0,1/nrep)
		}
		npde<-qnorm(pde)
		npdeObject["results"]["xerr"]<-xerr
		npdefull[not.miss]<-npde
		ydobsfull[not.miss]<-ydobs
		ydsimfull[not.miss]<-ydsim
		if(length(npdeObject["results"]["res"])==0) {
			res<-data.frame(npde=npdefull,ydobs=ydobsfull)
			npdeObject["results"]["res"]<-res
		} else {
			npdeObject["results"]["res"]$ydobs<-ydobsfull
			npdeObject["results"]["res"]$npde<-npdefull
		}
		npdeObject["sim.data"]["datsim"]$ydsim<-ydsimfull
		if(!npdeObject["options"]$calc.pd) {
			ypredfull<-npdefull
			ypredfull[not.miss]<-ypred
			npdeObject["results"]["res"]$ypred<-ypredfull
		}
	} else {
		npde<-rep(NA,length(pde))
		npdeObject["options"]$calc.pd<-TRUE
	}
	return(npdeObject)
}

computenpde.loq<-function(npdeObject) {


	# Compute (normalised) prediction distribution errors in the presence of BQL data

	# ECO TODO: securiser ici, faire test
	if(length(npdeObject["data"]["icens"])>0 & length(npdeObject["results"]["res"]$pd)==0) {
		cat("Please compute prediction discrepancies first \n")
		return(npdeObject)
	}
	# Preparing vectors for the different results
	# Indicators for censored data to be imputed
	not.miss<-npdeObject["data"]["not.miss"]
	tab<-npdeObject["data"]["data"][not.miss,] # non-missing data
	tabsim<-npdeObject["sim.data"]["datsim"][not.miss,] # corresponding simulated data
	ycens<-tab[,npdeObject["data"]["name.cens"]]
	ypred<-pde<-rep(NA,dim(tab)[1])
	if(npdeObject["options"]$cens.method%in% c("ipred","ppred","cdf"))
		yobs.all<-ydobs<-npdeObject["results"]["res"][not.miss,"ycomp"] else
			yobs.all<-ydobs<-tab[,npdeObject["data"]["name.response"]]
	npdefull<-ydobsfull<-rep(NA,dim(npdeObject["data"]["data"])[1])
	ydsimfull<-rep(NA,dim(npdeObject["sim.data"]["datsim"])[1])

	loq<-npdeObject["data"]["loq"]

	has.cens<-(length(loq)>0)
	id<-tab[,npdeObject["data"]["name.group"]]
	nrep<-npdeObject["sim.data"]["nrep"]
	xerr<-0
	ysim<-tabsim$ysim
	idsim<-tabsim$idsim
	ypredall<-pd<-c()
	if(npdeObject["options"]$cens.method=="cdf") ydsim<-tabsim$ysim.imp else ydsim<-ysim

	# computing pde
	for(isuj in unique(id)) {
		yobs<-yobs.all[id==isuj]
		matsim<-matrix(ydsim[idsim==isuj],ncol=nrep)
		x<-calcnpde(isuj,yobs,matsim,nrep,npdeObject["options"]$decorr.method, npdeObject["options"]$verbose)
		xerr<-x$xerr
		if(x$xerr==0) {
		  pde[id==isuj]<-x$pde
		  ydsim[idsim==isuj]<-x$ydsim
		  ydobs[id==isuj]<-x$ydobs
		  ypred[id==isuj]<-x$ypred
		}
		if (xerr>0) break
	}
	if(xerr>0) {
		cat("The computation of the pde has failed for subject",isuj,"because \n")
		if(xerr==1) {
			if(npdeObject["options"]$decorr.method=="cholesky") cat("the Cholesky decomposition of the covariance matrix of the simulated data could not be obtained.\n")
			if(npdeObject["options"]$decorr.method=="inverse") cat("the covariance matrix of the simulated data could not be diagonalised through eigen().\n")
			if(npdeObject["options"]$decorr.method=="polar") cat("the Cholesky decomposition of the covariance matrix of the simulated data could not be obtained.\n")
		}
		if(xerr==2) cat("the covariance matrix of the simulated data could not be inverted.\n")
		cat("This usually means that the covariance matrix is not positive-definite, or that is is poorly conditioned.\n")
		cat("This can be caused by simulations widely different from observations (in \n")
		cat("other words, a poor model).\n")
		cat("We suggest to plot a prediction interval from the simulated data to check\n")
		cat("whether the simulations are reasonable, and to consider prediction\n")
		cat("discrepancies.\n")
		cat("Prediction discrepancies will now be computed.\n")
#		break
	}

	# saving pde
	if(xerr==0) {
		# Dealing with extreme values of npde & smoothing if !(npdeObject["options"]$ties)
		if(npdeObject["options"]$ties) {
			pde[pde==0]<-1/(2*nrep)
			pde[pde==1]<-1-1/(2*nrep)
		} else {
			idx<-which(pde>0 & pde<1)
			pde[pde==0]<-runif(sum(pde==0),0,1/nrep)
			pde[pde==1]<-runif(sum(pde==1),1-1/nrep,1)
			pde[idx]<-pde[idx]+runif(length(idx),0,1/nrep)
		}
		npde<-qnorm(pde)
		npdeObject["results"]["xerr"]<-xerr
		npdefull[not.miss]<-npde
		npdeObject["results"]["res"]$npde<-npdefull
		ydobsfull[not.miss]<-ydobs
		ydsimfull[not.miss]<-ydsim
		npdeObject["results"]["res"]$ydobs<-ydobsfull
		npdeObject["sim.data"]["datsim"]$ydsim<-ydsimfull
	} else {
		npde<-rep(NA,length(pde))
		npdeObject["options"]$calc.pd<-TRUE
	}
	return(npdeObject)
}


########### Compute npde given observed and simulated data

#' @keywords internal

calcnpde<-function(isuj,yobs,matsim,nrep,decorr.method,verbose=FALSE) {
	# matsim: simulated data, with BQL data replaced (depending on method)
	# matsim: simulated data, raw
	if (verbose) cat("Computing the npde for subject ",isuj,"\n")
	#Computing decorrelated ysim* and yobs* for subject isuj
	#variance-covariance matrix computed using the cov function
	#Computing ypred
	varsim<-cov(t(matsim))
	moysim<-rowMeans(matsim)
	#computing V-1/2
	xerr<-0
	if(length(moysim)>1) {
		y<-switch(decorr.method,
							cholesky=decorr.chol(varsim),
							polar=decorr.polar(varsim),
							inverse=decorr.inverse(varsim))
		if(y$xerr==0) ymat<-y$y else xerr<-y$xerr
	} else ymat<-1/sqrt(varsim)
	if(xerr==0) {
		#decorrelation of the simulations
		decsim<-t(ymat)%*%(matsim-moysim)
		decobs<-t(ymat)%*%(yobs-moysim)
		ydsim<-c(decsim)
		#decorrelation of the observations
		ydobs<-decobs
		#Computing the pde
		tcomp<-apply(decsim,2,"<",decobs)
		if(!is.matrix(tcomp)) tcomp<-t(as.matrix(tcomp))
		ycal<-rowMeans(tcomp)
		pde<-ycal
	}
	return(list(xerr=xerr,pde=pde,ydsim=ydsim,ydobs=ydobs,ypred=moysim))
}

calcnpde.sim<-function(yobs,matsim,nrep,decorr.method) {
  # yobs: matrix with data treated as observed (1 column=1 set of data=1 'subject')
  # matsim: simulated data to be used to decorrelate
  # returns decorrelated yobs* for each 'subject'
  #variance-covariance matrix computed depending on "method"
  varsim<-cov(t(matsim))
  moysim<-rowMeans(matsim)
  #computing V-1/2
  xerr<-0
  if(length(moysim)>1) {
    y<-switch(decorr.method,
              cholesky=decorr.chol(varsim),
              polar=decorr.polar(varsim),
              inverse=decorr.inverse(varsim))
    if(y$xerr==0) ymat<-y$y else xerr<-y$xerr
  } else ymat<-1/sqrt(varsim)
  pde<-yobs
  if(xerr==0) {
    #decorrelation of the simulations & the observations
    decsim<-t(ymat)%*%(matsim-moysim)
    decobs<-t(ymat)%*%(yobs-moysim)
    #Computing the pde
    for(i in 1:dim(yobs)[2]) {
      tcomp<-apply(decsim,2,"<",decobs[,i])
      if(!is.matrix(tcomp)) tcomp<-t(as.matrix(tcomp))
      ycal<-rowMeans(tcomp)
      pde[,i]<-ycal
    }
  }
  return(list(xerr=xerr,pde=pde))
}

########### Decorrelation methods

#' @name npde.decorr.method
#' @aliases decorr.chol decorr.inverse decorr.polar
#' @title Decorrelation methods in npde
#'
#' @description Specifies the method used to decorrelate observed and simulated data
#'
#' @param x a square matrix
#' \describe{
#'  \item{cholesky}{decorrelation is performed through the Cholesky decomposition (default)}
#'  \item{inverse}{decorrelation is performed by inverting Vi through the \code{eigen} function}
#'  \item{polar}{the singular-value decomposition (\code{svd}) is used}
#'  }
#'
#' @details More details can be found in the PDF documentation.
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords methods
NULL

## #' @keywords internal

decorr.chol<-function(x) {
	xerr<-0
	xmat<-try(chol(x))
	if(is.numeric(xmat)) {
		ymat<-try(solve(xmat))
		if(!is.numeric(ymat))
			xerr<-2
	} else
		xerr<-1
	return(list(y=ymat,xerr=xerr))
}

decorr.inverse<-function(x) {
	xerr<-0
	var.eig<-eigen(x)
	xmat<-try(var.eig$vectors %*% diag(sqrt(var.eig$values)) %*% solve(var.eig$vectors))
	if(is.numeric(xmat)) {
		ymat<-try(solve(xmat))
		if(!is.numeric(ymat))
			xerr<-2
	} else
		xerr<-1
	return(list(y=ymat,xerr=xerr))
}

decorr.polar<-function(x) {
	xerr<-0
	xmat<-try(chol(x))
	if(is.numeric(xmat)) {
		svdec<-svd(xmat)
		umat<-svdec$u %*% t(svdec$v)
		vmat<-t(umat) %*% xmat
		ymat<-try(solve(vmat))
		if(!is.numeric(ymat))
			xerr<-2
	} else
		xerr<-1
	return(list(y=ymat,xerr=xerr))
}

####################################################################################

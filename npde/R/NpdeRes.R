##################################################################################

##' Class "NpdeRes"
##'
##' The results component of a NpdeObject object
##'
##' @name NpdeRes-class
##' @aliases NpdeRes NpdeRes-class, print,NpdeRes-method
##' showall,NpdeRes-method summary,NpdeRes-method test,NpdeRes-method
##' @docType class
##' @section Objects from the Class: NpdeRes objects are created during a call to \code{\link{npde}} or \code{\link{autonpde}} as the "results" slot in a NpdeObject object. An NpdeRes object contains the following slots:
##'
##' \describe{
##' \item{res}{a dataframe containing the results. Columns include id (group), xobs (observed X), yobs (observed Y), cens (indicator for censored data), as well as the actual results: ypred (model population predictions), pd (prediction discrepancies), npde (normalised prediction distribution errors), ycomp (completed data), ydobs (decorrelated observed data).}
##' \item{N}{number of subjects}
##' \item{ntot.obs}{total number of non-missing observations}
##' \item{ploq}{a vector giving the probability that a given observation is LOQ, according to the model}
##' \item{icens}{index of (non-missing) censored observations}
##' \item{not.miss}{a vector of boolean indicating for each observation whether it is missing (FALSE) or available (TRUE)}
##' \item{pd.sim}{pd computed for a number of simulated datasets (optional, used to obtain prediction intervals on the distribution of pd)}
##' \item{npde.sim}{npde computed for a number of simulated datasets (optional, used to obtain prediction intervals on the distribution of npde)}
##' }
##' @section Methods:
##' \describe{
##'   \item{print(npde.res):}{Prints a summary of object npde.res}
##'   \item{show(npde.res):}{Prints a short summary of object npde.res}
##'   \item{showall(npde.res):}{Prints a detailed summary of object npde.res}
##'   \item{plot(npde.res):}{Plots the data in npde.res. More details can be found in \code{\link{plot.NpdeRes}}}
##'   \item{summary(npde.res):}{Returns a summary of object npde.res in list format}
##'   \item{gof.test(x, parametric=TRUE, ...):}{Returns goodness-of-fit tests}
##' }
##' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{plot.NpdeRes}}, \code{\link{NpdeObject}}
##' @keywords classes internal
##' @examples
##' \dontrun{
##' data(theopp)
##'
##' methods(class="NpdeRes")
##'
##' showClass("NpdeRes")
##' }
##' @exportClass NpdeRes

setClass(
  Class="NpdeRes",
  representation=representation(
  	res="data.frame",          # a data frame containing the results:id (group), xobs (observed X), yobs (observed Y), cens (indicator for censored data), as well as the actual results: ypred (model population predictions), pd (prediction discrepancies), npde (normalised prediction distribution errors), ycomp (completed data), ydobs (decorrelated observed data)
  	icens="numeric",		# index of the censored observations (non-missing)
    not.miss="logical",		# vector of logical, TRUE if present (=not missing), FALSE for missing data
    ploq="numeric",		# probability to be below LOQ
    xerr="numeric",		# error code
  	pd.sim="matrix",          # a matrix with pd for a sample of the simulated datasets (as many lines as non-missing observations)
  	npde.sim="matrix",          # a matrix with npde for a sample of the simulated datasets (as many lines as non-missing observations)
  	ntot.obs="numeric"		# total number of observations
  ),
  validity=function(object){
#    cat ("--- Checking NpdeRes object ---\n")
  	if(length(object@res)>0) {
  		if(length(object@not.miss)>0 && length(object@not.miss)!=dim(object@res)[1]) {
  			cat("Size mismatch between indicator variable not.miss and the data.\n")
  			return(FALSE)
  		}
  		if(length(object@ntot.obs)>0 && length(object@not.miss)>0 && object@ntot.obs!=dim(object@res[object@not.miss,])[1]) {
  			cat("Size mismatch between the total number of observations and the data.\n")
  			return(FALSE)
  		}
  	}
    return(TRUE)
  }
)

setMethod(
  f="initialize",
  signature="NpdeRes",
  definition= function (.Object){
#    cat ("--- initialising NpdeRes Object --- \n")
    return (.Object )
  }
)

##################################################################################

#' @rdname extract-methods
#' @aliases [,NpdeRes-method [<-,NpdeRes-method
#' @exportMethod [
#' @exportMethod [<-

#### NpdeRes
# Getteur
setMethod(
  f ="[",
  signature = "NpdeRes" ,
  definition = function (x,i,j,drop ){
  switch (EXPR=i,
    "res"={return(x@res)},
    "icens"={return(x@icens)},
    "not.miss"={return(x@not.miss)},
    "ploq"={return(x@ploq)},
    "xerr"={return(x@xerr)},
  	"pd.sim"={return(x@pd.sim)},
  	"npde.sim"={return(x@npde.sim)},
    "ntot.obs"={return(x@ntot.obs)},
    stop("No such attribute\n")
   )
  }
)

# Setteur
setReplaceMethod(
  f ="[",
  signature = "NpdeRes" ,
  definition = function (x,i,j,value){
  switch (EXPR=i,
    "res"={x@res<-value},
    "icens"={x@icens<-value},
    "not.miss"={x@not.miss<-value},
    "ploq"={x@ploq<-value},
    "xerr"={x@xerr<-value},
  	"pd.sim"={x@pd.sim<-value},
  	"npde.sim"={x@npde.sim<-value},
    "ntot.obs"={x@ntot.obs<-value},
    stop("No such attribute\n")
   )
   validObject(x)
   return(x)
  }
)

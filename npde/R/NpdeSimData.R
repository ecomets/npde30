#' Class "NpdeSimData" representing the structure of the longitudinal data
#'
#' A longitudinal data structure, with simulated data
#'
#' @name NpdeSimData-class
#' @aliases NpdeSimData NpdeSimData-class
#' @docType class
#' @section Objects from the Class: NpdeSimData objects are created by associating an NpdeData object with matching simulated data, and they contain the following slots.
#'
#' \describe{
#' \item{nrep}{number of replications)}
#' \item{datsim}{a dataframe containing the simulated data,  with columns: idsim (subject id), irsim (replication index), xsim (simulated x), ysim (simulated response). After a call to \code{\link{npde}} or \code{\link{autonpde}}, an additional column ydsim (decorrelated replicated data) will be added.}
#' }
#' @section Methods:
#' \describe{
#'   \item{print(npde.simdata):}{Prints a summary of object npde.simdata}
#'   \item{show(npde.simdata):}{Prints a short summary of object npde.simdata}
#'   \item{showall(npde.simdata):}{Prints a detailed summary of object npde.simdata}
#' }
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}
#' @keywords classes
#' @examples
#' \dontrun{
#' showClass("NpdeSimData")
#' }
#' @exportClass NpdeSimData

setClass(
  Class="NpdeSimData",
  representation=representation(
    nrep="numeric",		# number of replications
    datsim="data.frame"	# data (a dataframe with columns: idsim (subject id), irsim (replication index), xsim (simulated x), ysim (simulated response), ydsim (decorrelated replicated data))
  ),
  validity=function(object){
    #    cat ("--- Checking NpdeData object ---\n")
    return(TRUE)
  }
)

setMethod(
  f="initialize",
  signature="NpdeSimData",
  definition= function (.Object){
    #    cat ("--- initialising NpdeSimData Object --- \n")
    return (.Object )
  }
)

#' @rdname extract-methods
#' @aliases [,NpdeSimData-method [<-,NpdeSimData-method [,NpdeSimData [<-,NpdeSimData
#' @exportMethod [
#' @exportMethod [<-

# Getteur
setMethod(
  f ="[",
  signature = "NpdeSimData" ,
  definition = function (x,i,j,drop ){
    switch (EXPR=i,
            "nrep"={return(x@nrep)},
            "datsim"={return(x@datsim)},
            stop("No such attribute\n")
    )
  }
)

# Setteur
setReplaceMethod(
  f ="[",
  signature = "NpdeSimData" ,
  definition = function (x,i,j,value){
    switch (EXPR=i,
            "nrep"={x@nrep<-value},
            "datsim"={x@datsim<-value},
            stop("No such attribute\n")
    )
    validObject(x)
    return(x)
  }
)


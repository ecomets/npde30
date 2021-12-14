#######################################################
# Documentation for generic methods created by npde
#######################################################
# TODO: move generic for read back into NpdeData-method (only needed for that file)

#' Read data into structures (internal)
#'
#' Creates an NpdeData object containing the observed data, either from disk or from a dataframe
#'
#' @name read
#' @aliases read-methods read,NpdeData read,NpdeData-methods
#' @aliases read,NpdeSimData-methods
#'
#' @usage read(object, dat, detect=TRUE, verbose=FALSE, ...)
#'
#' @param object an object
#' @param dat a dataframe containing the data to be analysed
#' @param detect a boolean; if TRUE, automatic recognition of names will be attempted to detect necessary items 
#' for a NpdeData, these include longitudinal data structure, missing data and censoring information
#' for a NpdeSimData, the required columns should be called idsim, xsim, ysim, representing respectively simulated id, simulated 
#' predictor and simulated response
#' @param verbose a boolean; messages are printed if verbose is TRUE (defaults to FALSE)
#' @param dots additional arguments for compatibility with generic
#'
#' @return an object of class \code{"\linkS4class{NpdeData}"} or \code{"\linkS4class{NpdeSimData}"}
#' @docType methods
#' @rdname read
#' @exportMethod read
#' @keywords methods
#' @keywords internal
#' @include NpdeData.R
#' @include NpdeSimData.R
#' @importFrom methods new
#' @importFrom utils head read.table modifyList
#' @importFrom graphics abline
#' @importFrom stats complete.cases
## #' @import testthat

setGeneric(name="read",
           def=function(object, dat, detect=TRUE,verbose=FALSE, ...) standardGeneric("read"))

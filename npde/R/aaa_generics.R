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
#' @usage read(object, name.data, header=TRUE, sep="", na.strings=c("NA","."), detect=TRUE,
#' verbose=FALSE)
#'
#' @param object an object
#' @param name.data character string giving the name of the dataset (can be a file on disk or an R dataframe)
#' @param header boolean indicating whether the file has a header (mandatory if
#' detect is TRUE)
#' @param sep field separator (for files on disk)
#' @param na.strings strings to be considered as indicating NA
#' @param detect a boolean; if TRUE, automatic recognition of names will be attempted to detect necessary items (longitudinal data structure, missing data and censoring information)
#' @param verbose a boolean; messages are printed if verbose is TRUE (defaults to FALSE)
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

setGeneric(name="read",
           def=function(object,name.data,header=TRUE,sep="",na.strings=c("NA","."),detect=TRUE,verbose=FALSE){standardGeneric("read")})

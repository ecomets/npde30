##################################################################################

#' Class "NpdeData" representing the structure of the longitudinal data
#'
#' A longitudinal data structure
#'
#' @name NpdeData-class
#' @aliases NpdeData NpdeData-class print,NpdeData-method
#' summary,NpdeData-method npde.qqplot,NpdeData-method
#' @docType class
#' @section Objects from the Class: NpdeData objects are typically created by a call to \code{\link{npdeData}} contain the following slots:
#'
#' \describe{
#' \item{name.group}{character string giving the name of the grouping term (ID)}
#' \item{name.predictor}{character string giving the name of the predictor (X)}
#' \item{name.response}{character string giving the name of the response (Y)}
#' \item{name.cens}{character string giving the name of the censoring indicator}
#' \item{name.mdv}{character string giving the name of the missing data indicator}
#' \item{name.covariates}{vector of character string giving the name(s) of the covariates}
#' \item{name.ipred}{character string giving the name of the individual predictions}
#' \item{units}{(optional) a list with the units for X, Y, and covariates}
#' \item{data}{a dataframe containing the data}
#' \item{N}{number of subjects}
#' \item{ntot.obs}{total number of non-missing observations}
#' \item{nind.obs}{vector of size N giving the number of non-missing observations for each subject}
#' \item{ind}{index of non-missing observations}
#' \item{icens}{index of censored observations (non-missing)}
#' \item{not.miss}{a vector of boolean indicating for each observation whether it is missing (FALSE) or available (TRUE)}
#' \item{loq}{the censoring value}
#' }
#' @section Methods:
#' \describe{
#'   \item{show(npde.data):}{Prints a short summary of object npde.data}
#'   \item{qqplot.npde(npde.data):}{QQ-plot for NpdeData object (TODO: change for NpdeObject in final package)}
#' }
#' @keywords classes
#' @examples
#' \dontrun{
#' methods(class="NpdeData")
#'
#' showClass("NpdeData")}
#'
#' @exportClass NpdeData

setClass(
  Class="NpdeData",
  representation=representation(
    name.group="character",	# name of column with ID
    name.predictor="character",# name of column(s) with predictors
    name.response="character",	# name of column with response
    name.cens="character",	# name of column with censoring information
    name.miss="character",	# name of column indicating missing data (not censored)
    name.covariates="character",# name of column(s) with covariates (can be used for graphs)
    name.ipred="character",	# name of column indicating individual predictions (if available in the dataset)
    units="list",		# units (list with components for x, y, and cov), used in plots
    data="data.frame",          # the data: data frame with columns name.group (subject id), index (id renamed to 1:N), name.predictors (predictors), name.response (possibly transformed during fit), cens (1 if data is censored; the corresponding response is the censoring value), mdv (1 if the data is considered as missing), name.covariates (covariates, binary covariates are modified to 0/1), name.ipred (individual predictions, if available)
    ind="numeric",		# index of the non-missing observations # ECO TODO remove ?
    icens="numeric",		# index of the censored observations (non-missing)
    not.miss="logical",		# vector of logical, TRUE if present (=not missing), FALSE for missing data
    N="numeric",		# number of subjects
    ntot.obs="numeric",		# total number of observations
    nind.obs="numeric",		# number of observations for each subject
    loq="numeric"		# LOQ
))

###############################
# ECO validity ne semble pas etre appele automatiquement quand on cree un objet => il faut l'appeler dans initialize

#' @importFrom methods validObject

setMethod(
  f="initialize",
  signature="NpdeData",
  definition= function (.Object,name.group,name.predictor, name.response,name.covariates,name.cens,name.miss,name.ipred,units,data){

    if(missing(name.group)) name.group<-character()
    # ECO TODO: reconnaissance automatique (avant affectation a la valeur 2) ?
    if(missing(name.predictor)) name.predictor<-character()
    if(missing(name.response)) name.response<-character()
    if(missing(name.covariates) || length(name.covariates)==0 || name.covariates[1]=="") name.covariates<-character()
    if(missing(name.cens) || length(name.cens)==0 || name.cens=="") name.cens<-character()
    if(missing(name.miss) || length(name.miss)==0 ||name.miss=="") name.miss<-character()
    if(missing(name.ipred) || length(name.ipred)==0 ||name.ipred=="") name.ipred<-character()
    .Object@name.group<-name.group
    .Object@name.predictor<-name.predictor
    .Object@name.response<-name.response
    .Object@name.covariates<-name.covariates
    .Object@name.cens<-name.cens
    .Object@name.miss<-name.miss
    .Object@name.ipred<-name.ipred


  .avoid_code = function() {


    # name.groupe
    if (missing(name.group)) {.Object@name.group <- character()}
    if (!missing(name.group) && is.character(name.group)==TRUE){
      if((nchar(name.group)==0)){
        print("name group is empty")
        FALSE}
      else{.Object@name.group <- name.group}
    }

    # name.predictor
    if (missing(name.predictor)) {.Object@name.predictor <- character()}
    if (!missing(name.predictor) && is.character(name.predictor)==TRUE){
      if((nchar(name.predictor)==0)){
        print("name predictor is empty")
        FALSE}
      else{.Object@name.predictor <- name.predictor}
    }

    # name.response
    if (missing(name.response)) {.Object@name.predictor <- character()}
    if (!missing(name.response) && is.character(name.response)==TRUE){
      if((nchar(name.response)==0)){
        print("name response is empty")
        FALSE}
      else{.Object@name.response <- name.response}
    }

    # name.covariates
    if (missing(name.covariates)) {.Object@name.covariates <- character()}
    if (!missing(name.covariates) && is.character(name.covariates)==TRUE){
      if((nchar(name.covariates)==0)){
        print("name covariates is empty")
        FALSE}
      else{.Object@name.covariates <- name.covariates}
    }

    # name.cens
    if (missing(name.cens)) {.Object@name.cens <- character()}
    if (!missing(name.cens) && is.character(name.cens)==TRUE){
      if((nchar(name.cens)==0)){
        print("name cens is empty")
        FALSE}
      else{.Object@name.cens <- name.cens}
    }

    # name.miss
    if (missing(name.miss)) {.Object@name.miss <- character()}
    if (!missing(name.miss) && is.character(name.miss)==TRUE){
      if((nchar(name.miss)==0)){
        print("name miss is empty")
        FALSE}
      else{.Object@name.miss <- name.miss}
    }

    # name.ipred
    if (missing(name.ipred)) {.Object@name.ipred <- character()}
    if (!missing(name.ipred) && is.character(name.ipred)==TRUE){
      if((nchar(name.ipred)==0)){
        print("name ipred is empty")
        FALSE}
      else{.Object@name.ipred <- name.ipred}
    }

  }

    if(missing(units)) units<-list(x="-",y="-")
    if(is.null(units$x)) units$x<-"-"
    if(is.null(units$y)) units$y<-"-"

    ncov<-length(name.covariates)

    if(ncov>0) {
      nunit<-length(units$covariates)
      if(nunit==0) units$covariates<-rep("-",ncov)
      if(nunit>ncov) units$covariates<-units$covariates[1:ncov]
      if(nunit<ncov) {
        length(units$covariates)<-ncov
        units$covariates[(nunit+1):ncov]<-"-"
      }
    }

    .Object@units<-units
    .Object@N<-0

    # Object validation
    validObject(.Object,test = TRUE)
    return (.Object )

  }
)

##################################################################################

#' Get/set methods for NpdeData object
#'
#' Access slots of a NpdeData using the object["slot"] format
#'
#' @name [
#'
#' @param x	object from which to extract element(s) or in which to replace element(s)
#' @param i,j	indices specifying elements to extract or replace. Indices are numeric or character vectors or empty (missing) or NULL
#' @param drop For matrices and arrays. If TRUE the result is coerced to the lowest possible dimension (see the examples). This only works for extracting elements, not for the replacement. See drop for further details
#' @param value typically an array-like R object of a similar class as x
#'
#' @keywords methods
#' @exportMethod [
#' @exportMethod [<-
#' @aliases [,NpdeData-method [<-,NpdeData-method
#' @docType methods
#' @rdname extract-methods

#### NpdeData
# Getteur
setMethod(
  f ="[",
  signature = "NpdeData" ,
  definition = function (x,i,j,drop ){
    switch (EXPR=i,
            "header"={return(x@header)},
            "sep"={return(x@sep)},
            "na"={return(x@na)},
            "name.group"={return(x@name.group)},
            "name.predictor"={return(x@name.predictor)},
            "name.response"={return(x@name.response)},
            "name.cens"={return(x@name.cens)},
            "name.miss"={return(x@name.miss)},
            "name.covariates"={return(x@name.covariates)},
            "name.ipred"={return(x@name.ipred)},
            "units"={return(x@units)},
            "loq"={return(x@loq)},
            "data"={return(x@data)},
            "ind"={return(x@ind)},
            "icens"={return(x@icens)},
            "not.miss"={return(x@not.miss)},
            "N"={return(x@N)},
            "ntot.obs"={return(x@ntot.obs)},
            "nind.obs"={return(x@nind.obs)},
            stop("No such attribute\n")
    )
  }
)


#' replace names of NpdeData
#'
#' @name [
#' @aliases [<-,NpdeData-method
#' @docType methods
#' @rdname extract-methods

# Setteur
setReplaceMethod(
  f ="[",
  signature = "NpdeData" ,
  definition = function (x,i,j,value){
    switch (EXPR=i,
            "name.group"={x@name.group<-value},
            "name.predictor"={x@name.predictor<-value},
            "name.response"={x@name.response<-value},
            "name.cens"={x@name.cens<-value},
            "name.miss"={x@name.miss<-value},
            "name.covariates"={x@name.covariates<-value},
            "name.ipred"={x@name.ipred<-value},
            "units"={x@units<-value},
            "loq"={x@loq<-value},
            "data"={x@data<-value},
            "ind"={x@ind<-value},
            "icens"={x@icens<-value},
            "not.miss"={x@not.miss<-value},
            "N"={x@N<-value},
            "ntot.obs"={x@ntot.obs<-value},
            "nind.obs"={x@nind.obs<-value},
            stop("No such attribute\n")
    )
    validObject(x)
    return(x)
  }
)

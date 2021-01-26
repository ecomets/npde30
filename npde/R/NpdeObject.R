##################################################################################

##' Class "NpdeObject"
##'
##' An object of class NpdeObject
##'
##' @name NpdeObject-class
##' @aliases NpdeObject print,NpdeObject-method
##' showall,NpdeObject-method summary,NpdeObject-method test,NpdeObject-method
##' npde.main,NpdeObject npde.save,NpdeObject npde.graphs,NpdeObject plot,NpdeObject
##' @docType class
##' @section Objects from the Class: NpdeObject objects are typically created by calls to \code{\link{npde}} or \code{\link{autonpde}}. They contain the following slots:
##'
##' \describe{
##' \item{data}{an object of class NpdeData, containing the observed data}
##' \item{sim.data}{an object of class NpdeSimData, containing the simulated data}
##' \item{results}{an object of class NpdeRes, containing the results}
##' \item{options}{a list of options}
##' \item{prefs}{a list of graphical preferences for the plots}
##' }
##' @section Methods:
##' \describe{
##'   \item{print(x):}{Prints a summary of object}
##'   \item{show(x):}{Prints a short summary of object}
##'   \item{showall(x):}{Prints a detailed summary of object}
##'   \item{plot(x):}{Diagnostic and other plots. More details can be found in \code{\link{plot.NpdeObject}}}
##'   \item{summary(x):}{Returns a summary of object x in list format}
##'   \item{gof.test(x, parametric=TRUE, ...):}{Returns goodness-of-fit tests}
##'   \item{set.plotoptions(x):}{Sets options for graphs (internal method used in plots)}
##' }
##' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{NpdeData}}, \code{\link{NpdeSimData}}, \code{\link{NpdeRes}}, \code{\link{gof.test}}
##' @keywords classes
##' @examples
##' \dontrun{
##' methods(class="NpdeObject")
##'
##' showClass("NpdeObject")}
##'
##' @exportClass NpdeObject
##' @include NpdeRes.R

setClass(Class="NpdeObject",
  representation=representation(
    data="NpdeData",		# Data
    results="NpdeRes",		# Fit results
    sim.data="NpdeSimData", 	# Simulated data
    options="list",		# Options and parameters for algorithm
    prefs="list"		# Options for graphs
  ),
  validity=function(object){
#    cat ("--- Checking NpdeObject object ---\n")
    validObject(object@data)
    validObject(object@sim.data)
    return(TRUE)
  }
)

setMethod(
  f="initialize",
  signature="NpdeObject",
  definition= function (.Object,data,sim.data,options=list(),prefs=list()){
    .Object@data<-data
    .Object@sim.data<-sim.data
    .Object@results<-new(Class="NpdeRes")
    .Object@results["ntot.obs"]<-data["ntot.obs"]
    .Object@results["not.miss"]<-data["not.miss"]
    .Object@results["icens"]<-data["icens"]
    opt<-npdeControl()
    if(length(options)>0) {
      for(i in names(options)) {
      	if(length(grep(i,names(opt)))==0) message("Option",i, "not found, check spelling\n") else
      	opt[i]<-options[i]
      }
      i1<-grep("namsav",names(options))
      if(length(i1)!=0 && !is.na(i1)) {
        opt$namres<-paste(options[i1],".npde",sep="")
        opt$namgr<-paste(options[i1],".",opt$type.graph,sep="")
      }
    }
# Checking options
    opt<-check.control.options(opt)
    .Object@options<-opt
    graph.opt<-set.plotoptions(.Object)
    if(length(prefs)>0) {
    	for(i in names(prefs)) {
    		if(length(grep(i,names(graph.opt)))==0) message("Graphical option",i, "not found, check spelling\n") else	graph.opt[i]<-prefs[i]
    	}
    }
    .Object@prefs<-graph.opt
    # Object validation
    validObject(.Object)
    return (.Object )
  }
)

##################################################################################

#' @rdname extract-methods
#' @aliases [,NpdeObject-method [<-,NpdeObject-method
#' @exportMethod [
#' @exportMethod [<-

# Getteur
setMethod(
  f ="[",
  signature = "NpdeObject" ,
  definition = function (x,i,j,drop ){
    switch (EXPR=i,
            "data"={return(x@data)},
            "sim.data"={return(x@sim.data)},
            "results"={return(x@results)},
            "options"={return(x@options)},
            "prefs"={return(x@prefs)},
            stop("No such attribute\n")
    )
  }
)

# Setteur
setReplaceMethod(
  f ="[",
  signature = "NpdeObject" ,
  definition = function (x,i,j,value){
    switch (EXPR=i,
            "data"={x@data<-value},
            "sim.data"={x@sim.data<-value},
            "results"={x@results<-value},
            "options"={x@options<-value},
            "prefs"={x@prefs<-value},
            stop("No such attribute\n")
    )
    validObject(x)
    return(x)
  }
)

###########################	Setting options		#############################

######## Control options to run npde (algorithms, methods, input/output)

#' Set options for an NpdeObject
#'
#' Set, replace and check options for an NpdeObject
#'
#' @name npdeControl
#' @aliases npdeControl replace.control.options check.control.options
#'
#' @usage npdeControl(boolsave = TRUE, namsav = "output", type.graph = "eps",
#' verbose = FALSE, calc.npde = TRUE, calc.pd = TRUE, decorr.method = "cholesky",
#' cens.method = "omit", ties = TRUE, sample = FALSE)
#' @usage check.control.options(opt)
#' @usage replace.control.options(opt,...)
#'
#' @param boolsave whether to save the results (a file containing the numerical results and a file with the graphs)
#' @param namsav the root name of the files to save to (the file with the results will be named ROOTNAME.npde and the graphs will be saved to ROOTNAME.format where format is given by the type.graph argument)
#' @param type.graph type of graph to save to (one of "eps", "pdf", "jpeg", "png")
#' @param verbose a boolean; if TRUE, a message is printed as the computation of the npde begins for each new subject
#' @param calc.pd a boolean; TRUE to compute pd
#' @param calc.npde a boolean; TRUE to compute npde
#' @param decorr.method the method used to decorrelate simulated and observed data (see \code{\link{npde.decorr.method}})
#' @param cens.method the method used to handle censored data (see \code{\link{npde.cens.method}})
#' @param ties if FALSE, a smoothing will be applied to prediction discrepancies to avoid ties
#' @param sample if TRUE, the test on the pd will be performed after randomly sampling only pd per subject
#' @param opt a list of control options to be checked
#' @param ... named parameters to be changed. The names will be compared to the names of the control variables and changed, with warnings issued for names that do not match.
#'
#' @export
#' @keywords methods

npdeControl<-function(boolsave=TRUE,namsav="output",type.graph="eps", verbose=FALSE,calc.npde=TRUE,calc.pd=TRUE,decorr.method="cholesky",cens.method="omit",ties=TRUE, sample=FALSE) {
  # decorrelation methods:
  #### cholesky: Cholesky decomposition
  #### inverse: unique square root
  #### polar: Cholesky decomposition combined with diagonalisation

  # censoring methods: ECO TODO: find proper names
  #### none: censored data removed, corresponding pd & npde set to NaN
  #### pd.impute: when y<LOQ, sample pd in U(0,p_LOQ)
  #### ipred: when y<LOQ, impute y as the model prediction and compute pd/npde for the completed dataset

  # sample
  #### when TRUE, for the tests based on pd, one sample per subject is randomly drawn
  namres<-paste(namsav,".npde",sep="")
  namgr<-paste(namsav,".",type.graph,sep="")
  return(list(calc.pd=calc.pd,calc.npde=calc.npde,verbose=verbose, boolsave=boolsave,type.graph=type.graph,namsav=namsav,namres=namres,namgr=namgr, decorr.method=decorr.method,cens.method=cens.method,ties=ties,sample=sample))
}

######## Replacing options on the fly

#' @rdname npdeControl
#' @export

replace.control.options<-function(opt,...) {
  args1<-match.call(expand.dots=TRUE)
  # These arguments are used by other functions and may be passed on via "..."
  legacy<-c("fix")
  if(length(args1)>2) {
    # General arguments: col, pch
    for(i in 3:length(args1)) {
      if(match(names(args1)[i],names(opt),nomatch=0)>0) {
        if(!is.null(eval(args1[[i]]))) opt[[names(args1)[i]]]<-eval(args1[[i]])
      } else {
        if(is.na(match(names(args1)[i],legacy))) message("Argument",names(args1)[i],"not available, check spelling.\n")
      }
    }
  }
  opt<-check.control.options(opt)
  return(opt)
}

#' @rdname npdeControl
#' @export

check.control.options<-function(opt) {
  if(!(opt$cens.method %in% c("omit","loq","ipred","ppred","fix","cdf"))) {
    cat("Warning: Censoring method",opt$cens.method,"unavailable, switching to default method (cdf)\n")
    opt$cens.method<-"cdf"
  }
  if(!(opt$decorr.method %in% c("cholesky","inverse","polar"))) {
    cat("Warning: Method",opt$decorr.method,"to decorrelate residuals is unavailable, switching to default method (cholesky)\n")
    opt$decorr.method<-"cholesky"
  }
  if(!(opt$type.graph %in% c("eps","png","pdf","jpeg"))) {
    cat("Warning: Type",opt$type.graph,"unrecognised; type of graph must be one of eps, png, pdf, jpeg, switching to default type (eps=Postcript)\n")
    opt$type.graph<-"eps"
  }
  for(bool.true in c("boolsave","verbose","calc.pd","calc.npde","ties")) {
    if(is.na(as.logical(opt[bool.true]))) {
      cat("Warning: Option",bool.true,"must be a logical (TRUE/FALSE), setting it to TRUE.\n")
      opt[bool.true]<-TRUE
    }
  }
  if(!is.logical(opt$sample)) {
    cat("Warning: Option",opt$sample,"must be a logical (TRUE/FALSE), setting it to FALSE.\n")
    opt$sample<-FALSE
  }
  invisible(opt)
}

######## Graphical options

#' Set graphical preferences
#'
#' This function is used to set options for graphs
#'
#' @name set.plotoptions
#' @aliases set.plotoptions set.plotoptions.default set.plotoptions,NpdeData-method
#' @aliases set.plotoptions,NpdeObject-method set.plotoptions.NpdeData
#'
#' @usage set.plotoptions(object)
#'
#' @param object an object of class NpdeData or NpdeObject
#'
#' @return a list of options for graphs
#' @details See documentation for a list of available options.
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}
#'
#' @keywords plot
#' @export

set.plotoptions <- function(object) {
  if(missing(object)) set.plotoptions(0) else UseMethod("set.plotoptions",object)
}

set.plotoptions.default <- function(object) {

  plot.opt<-list(
    # General graphical options

    ## All booleans

    #ask=FALSE,				# whether the program should ask before creating a new page
    #new=TRUE,				# whether a new page should be created for the plot
    interactive=FALSE,			# whether the user should be prompted before computing predictions or performing simulations for VPC, npde and wres
    #mfrow=c(),				# page layout (if empty, defaults to the default layout for each graph type)
    #cex=1,
    #cex.axis=1,
    #cex.lab=1,
    #cex.main=1,

    covsplit=FALSE, # covsplit if covariate
    #axes=TRUE,				# Whether to plot the axes
    #frame.plot=TRUE,	# Whether to add a box around the plotting region

    plot.box = FALSE, # for boxplot

    # log scales x-y axis
    xlog=FALSE,
    ylog=FALSE,

    plot.loq=TRUE,			# Whether data under the LOQ should be plotted
    # line.loq

    line.loq=FALSE,			# Whether an horizontal line should be plotted at Y=LOQ in data and VPC plots
    impute.loq=FALSE,		# When TRUE, the imputed values are plotted for data under the LOQ; defaults to TRUE

    bands=TRUE,			#	# Whether prediction bands should be added to the plots (including VPC)
    plot.obs=TRUE,	#		# Whether observations, pd/ndpe should be plotted on top of the prediction bands (only applies if bands=TRUE)

    grid = FALSE, # grid or not

    # plot x-y axis
    xaxt = TRUE,
    yaxt = TRUE,

    approx.pi=TRUE,			# Whether approximate prediction bands should be obtained for the distribution plots (see documentation)

    plot.default = FALSE, # to get waffle plot

    plot.tnpde=FALSE,

    smooth=FALSE,
    line.smooth="s",
    box=FALSE,				# Whether boxplots should be performed instead of scatterplots

    # which  normalised prediction distribution errors
    which="npde",
    axis="x",
    # title, subtitle
    main="",				# title#
    size.main = 14,
    sub="",					# sub-title#
    size.sub = 12,

    # axis albels
    xlab="",
    ylab="",
    size.xlab = 12,
    size.ylab = 12,

    # axis limits
    xlim = c(),
    ylim = c(),

    # size text x-y labels
    size.text.x = 10,
    size.text.y = 10,

    # number of breaks for x-y axis
    breaks.x = 10,
    breaks.y = 10,

    # outliers for bands
    fill.outliers.med = "red",
    fill.outliers.bands = "red",
    alpha.outliers.med = 1,
    alpha.outliers.bands = 1,

    # parameters by defaults for the obervations
    col = "slategray4",
    lty = 1,
    lwd = 0.5,
    pch = 20,
    alpha = 1,
    size = 1.5,
    fill ="white",

    # parameters for the bands sup and inf
    alpha.bands = 0.3,
    fill.bands = "steelblue2",
    col.bands = "steelblue4",
    lty.bands = 2,
    lwd.bands = 0.25,

    # parameters for the bands median
    alpha.med = 0.5,#
    fill.med = "pink",#
    col.med = "salmon4",#
    lty.med = 2,#
    lwd.med = 0.5,#

    # parameters for points observations
    col.pobs ="slategray4", #
    pch.pobs = 20,	#
    size.pobs = 1.5,##
    alpha.pobs = 0.5,##

    #  parameters for lines observations
    col.lobs = "slategray4",
    lty.lobs = 1,
    lwd.lobs = 0.5,

    # parameters for points observations censored
    col.pcens = "red",#
    pch.pcens = 8,#
    size.pcens = 1.2,#
    alpha.pcens = 1,#

    # parameters for loq plot line
    col.line.loq = "black",
    lty.line.loq  = 3,
    lwd.line.loq  = 1,

    # type line for qqplot and scatter
    type = "b",# "b" = line + point

    which.cov="all",			# which covariates to plot
    ncat=3,				# number of categories to bin continuous covariates
    which.resplot=c("res.vs.x","res.vs.pred","dist.qqplot","dist.hist"), # which type of residual plots

    boxwex=0.2,				# factor to scale width in boxplots
    varwidth=TRUE,			# use relative width for boxplots

    # Colours for prediction bands: VPC, npde, distribution plots

    range=3,

    # Binning options
    bin.method="equal",			# method (one of "equal"=same nb of points in each interval, "width"=equally spaced intervals (on the log-scale if xlog=TRUE), "user"=user-defined breaks, "optimal"=Marc's optimal binning algorithm); for "user", the breaks must be specified in bin.breaks (otherwise defaults back to "equal"), while for the other methods the number of bins must be specified in bin.number
    bin.number=10,				# nb of bins; the coordinates of the point used to summarise the data in each bin are the mean of the X and Y values of all points within the bins.
    bin.breaks=NULL,			# user-defined breaks
    bin.extreme=NULL, # can be set to a vector of 2 values to fine-tune the behaviour of the binning algorithm at the boundaries; specifying c(0.01,0.99) with the "equal" binning method and bin.number=10 will create 2 extreme bands containing 1% of the data on the X-interval, then divide the region within the two bands into the remaining 8 intervals each containing the same number of data; in this case the intervals will all be equal except for the two extreme intervals, the size of which is fixed by the user; complete fine-tuning can be obtained by setting the breaks with the bin.method="user"

    pi.size=0.95,			# width of the prediction interval on the quantiles

    bin.beta=0.2,			# value of beta used to compute the variance-based criterion (Jopt,beta(I)) in the clustering algorithm
    bin.lambda=0.3,			# value of lambda used in the penalised criterion to select the number of bins (if bin.number=NULL)

    vpc.interval=0.95) # size of the prediction intervals

  return(plot.opt)
}

#' @rdname set.plotoptions
#' @export

set.plotoptions.NpdeData<-function(object) {
  # setting default plot options
  plot.opt <- set.plotoptions()
  plot.opt$ilist <- c(1:object["N"])
  plot.opt$name.X <- object["name.predictor"]
  plot.opt$name.Y <- object["name.response"]
  plot.opt$xlab <- paste(plot.opt$name.X," (",object["units"]$x,")", sep="")
  plot.opt$ylab <- paste(plot.opt$name.Y," (", object["units"]$y,")",sep="")
  plot.opt$type.graph <- "eps"	# for compatibility with npde.graphs function
  return(plot.opt)
}

#' @rdname set.plotoptions
#' @export

set.plotoptions.NpdeObject<-function(object) {
  # setting default plot options
  plot.opt <- set.plotoptions()
  plot.opt$ilist <- c(1:object["data"]["N"])
  plot.opt$name.X <- object["data"]["name.predictor"]
  plot.opt$name.Y <- object["data"]["name.response"]
  plot.opt$xlab <- paste(plot.opt$name.X," (",object["data"]["units"]$x,")", sep="")
  plot.opt$ylab <- paste(plot.opt$name.Y," (", object["data"]["units"]$y,")",sep="")
  plot.opt$type.graph <- object["options"]$type.graph	# for compatibility with npde.graphs function
  return(plot.opt)
}

######## Replacing graphical options on the fly

#' Replace graphical options
#'
#' This function is used to replace graph options (available in the prefs slot of the NpdeObject object) for plotting NpdeObject objects
#'
#' @usage replace.plotoptions(plot.opt,...)
#' @param plot.opt a list of graphical preferences
#' @param \dots names and values of the options to be replaced
#'
#' @return an updated list of options for graphs
#' @details See documentation for a list of available options. During replacement, invalid (including misspelled) options will raise warnings.
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}
#' @keywords plot internal

replace.plotoptions<-function(plot.opt,...) {
  args1<-match.call(expand.dots=TRUE)
  #	print(args1)
  #	print(args1[[3]])

  # These arguments are used by other functions and may be passed on via "...", so we want to ignore them. Other arguments not in list will raise warnings
  legacy<-c("plot.type","namsav","namgr","loq")
  if(length(args1)>2) {
    # General arguments: col, pch
    i1<-match("col",names(args1))
    if(!is.na(i1)) {
      plot.opt$col<-eval(args1[[i1]])
      if(is.na(match("pcol",names(args1)))) plot.opt$pcol<-eval(args1[[i1]])
      if(is.na(match("lcol",names(args1)))) plot.opt$lcol<-eval(args1[[i1]])
      if(is.na(match("ablinecol",names(args1)))) plot.opt$ablinecol<-eval(args1[[i1]])
      if(is.na(match("col.pobs",names(args1)))) plot.opt$col.pobs<-eval(args1[[i1]])
      if(is.na(match("col.lobs",names(args1)))) plot.opt$col.lobs<-eval(args1[[i1]])
      if(is.na(match("col.pcens",names(args1)))) plot.opt$col.pcens<-eval(args1[[i1]])
      #			if(is.na(match("col.lcdf",names(args1)))) plot.opt$col.lcdf<-eval(args1[[i1]])
      #			if(is.na(match("col.fill",names(args1)))) plot.opt$col.fill<-eval(args1[[i1]])
      #			if(is.na(match("col.fillcdf",names(args1)))) plot.opt$col.fillcdf<-eval(args1[[i1]])
    }
    i1<-match("lty",names(args1))
    if(!is.na(i1)) {
      plot.opt$lty<-eval(args1[[i1]])
      if(is.na(match("lty.lobs",names(args1)))) plot.opt$lty.lobs<-eval(args1[[i1]])
      if(is.na(match("ablinelty",names(args1)))) plot.opt$ablinelty<-eval(args1[[i1]])
    }
    i1<-match("lwd",names(args1))
    if(!is.na(i1)) {
      plot.opt$lwd<-eval(args1[[i1]])
      if(is.na(match("lwd.lobs",names(args1)))) plot.opt$lwd.lobs<-eval(args1[[i1]])
      if(is.na(match("ablinelwd",names(args1)))) plot.opt$ablinelwd<-eval(args1[[i1]])
    }
    i1<-match("pch",names(args1))
    if(!is.na(i1)) {
      plot.opt$pch<-eval(args1[[i1]])
      if(is.na(match("pch.pobs",names(args1)))) plot.opt$pch.pobs<-eval(args1[[i1]])
      if(is.na(match("pch.pcens",names(args1)))) plot.opt$pch.pcens<-eval(args1[[i1]])
    }
    # Other arguments
    for(i in 3:length(args1)) {
      if(match(names(args1)[i],names(plot.opt),nomatch=0)>0) {
        #    plot.opt[[names(args1)[i]]]<-args1[[i]] else {
        if(!is.null(eval(args1[[i]]))) plot.opt[[names(args1)[i]]]<-eval(args1[[i]])
      } else {
        if(is.na(match(names(args1)[i],legacy))) message("Argument",names(args1)[i],"not available, check spelling.\n")
      }
    }
  }
  return(plot.opt)
}


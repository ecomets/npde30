##################################################################################
################### S4 methods

#' @include NpdeRes.R
#' @rdname show
#' @aliases show.NpdeRes show,NpdeRes-method
#' @importFrom methods show
#' @exportMethod show

# ECO TODO: FINIR, mettre le test dans print
setMethod("show","NpdeRes",
          function(object) {
            cat("Object of class NpdeRes\n")
            if(length(object@res)[1]==0) cat("    currently empty\n") else {
              cat("  containing the following elements:\n")
              if(!is.null(object@res$ypred)) {
                cat("    predictions (ypred)\n")
              }
              if(!is.null(object@res$pd)) {
                cat("    prediction discrepancies (pd)\n")
              }
              if(!is.null(object@res$npde)) {
                cat("    normalised prediction distribution errors (npde)\n")
              }
              if(!is.null(object@res$ycomp)) {
                cat("    completed responses (ycomp) for censored data\n")
              }
              if(!is.null(object@res$ydobs)) {
                cat("    decorrelated responses (ydobs)\n")
              }
              if(length(object@ploq)>0)  cat("    ploq: probability of being <LOQ for each observation\n")
              if(length(object@ntot.obs)) {
                cat("  the dataframe has ",object@ntot.obs,"non-missing observations ")
                if(dim(object@res)[1]>object@ntot.obs) cat("and",dim(object@res)[1],"lines")
                cat(".\n")
              }
              if(length(object@res)>0) {
                nlines<-10
                tab<-object@res[object@not.miss,]
                cat("First",nlines,"lines of results, removing missing observations:\n")
                if(nlines==(-1)) {
                  print(tab)
                } else {
                  nrowShow <- min (nlines , nrow(tab))
                  print(tab[1:nrowShow,])
                }
              }            }
            }
)

##################################################################################
################### S3 methods

#' @aliases print.NpdeRes
#' @rdname print.NpdeData
#' @export

print.NpdeRes <- function(x, nlines=10,...) {
  digits<-2;nsmall<-2
  cat("Object of class NpdeRes\n")
  cat("    resulting from a call to npde or autonpde\n")
  if(length(x@res)[1]==0) cat("    currently empty\n") else {
    cat("    containing the following elements:\n")
    if(!is.null(x@res$ypred)) {
      cat("    predictions (ypred)\n")
      print(summary(x@res$ypred[x@not.miss]))
    }
    if(!is.null(x@res$pd)) {
      cat("    prediction discrepancies (pd)\n")
      print(summary(x@res$pd[x@not.miss]))
    }
    if(!is.null(x@res$npde)) {
      cat("    normalised prediction distribution errors (npde)\n")
      print(summary(x@res$npde[x@not.miss]))
    }
    if(!is.null(x@res$ycomp)) {
      cat("    completed responses (ycomp) for censored data\n")
    }
    if(!is.null(x@res$ydobs)) {
      cat("    decorrelated responses (ydobs)\n")
    }
    if(length(x@ploq)>0)  cat("    ploq: probability of being <LOQ for each observation\n")
    if(length(x@ntot.obs)) {
      cat("  the dataframe has ",x@ntot.obs,"non-missing observations ")
      if(dim(x@res)[1]>x@ntot.obs) cat("and",dim(x@res)[1],"lines")
      cat(".\n")
    }
    gof.test(x)
  }
}

#' @aliases showall.NpdeRes
#' @rdname showall
#' @export

showall.NpdeRes <- function(object) {
  cat("Object of class NpdeRes\n")
  if(length(object@res)[1]==0) cat("    currently empty\n") else {
    cat("    containing the following elements:\n")
    if(!is.null(object@res$ypred)) {
      cat("    predictions (ypred)\n")
    }
    if(!is.null(object@res$pd)) {
      cat("    prediction discrepancies (pd)\n")
    }
    if(!is.null(object@res$npde)) {
      cat("    normalised prediction distribution errors (npde)\n")
    }
    if(!is.null(object@res$ycomp)) {
      cat("    completed responses (ycomp) for censored data\n")
    }
    if(!is.null(object@res$ydobs)) {
      cat("    decorrelated responses (ydobs)\n")
    }
    if(length(object@ploq)>0)  cat("    ploq: probability of being <LOQ for each observation\n")
    if(length(object@ntot.obs)) {
      cat("  the dataframe has ",object@ntot.obs,"non-missing observations ")
      if(dim(object@res)[1]>object@ntot.obs) cat("and",dim(object@res)[1],"lines")
      cat(".\n")
    }
    nlines<-10
    cat("First",nlines,"lines of results, removing missing observations:\n")
    tab<-object@res[object@not.miss,]
    nrowShow <- min (nlines , nrow(tab))
    print(tab[1:nrowShow,])
  }
}

#' @aliases summary.NpdeRes
#' @rdname summary.NpdeData
#' @export

summary.NpdeRes <- function(object, print=TRUE, ...) {
  if(length(object@res)==0) {
    cat("Object of class NpdeRes, empty.\n")
    return()
  }
  res<-list(N=object@N,data=object@res,ntot.obs=object@ntot.obs)
  res$ploq<-object@ploq
  invisible(res)
}

##################################################################################

#' Kurtosis
#'
#' Computes the kurtosis.
#'
#' If \eqn{N = \mathrm{length}(x)}{N = length(x)}, then the kurtosis of \eqn{x}
#' is defined as: \deqn{N sum_i (x_i-\mathrm{mean}(x))^4 (sum_i
#' (x_i-\mathrm{mean}(x))^2)^(-2) - }{N sum_i (x_i-mean(x))^4 (sum_i
#' (x_i-mean(x))^2)^(-2) - 3}\deqn{3}{N sum_i (x_i-mean(x))^4 (sum_i
#' (x_i-mean(x))^2)^(-2) - 3}
#'
#' @usage kurtosis(x)
#' @param x a numeric vector containing the values whose kurtosis is to be
#' computed. NA values are removed in the computation.
#' @return The kurtosis of \code{x}.
#' @references G. Snedecor, W. Cochran. \emph{Statistical Methods},
#' Wiley-Blackwell, 1989
#' @keywords univar
#' @examples
#' \dontrun{
#' x <- rnorm(100)
#' kurtosis(x)}
#' @export
#' @importFrom stats ks.test pchisq qnorm sd shapiro.test t.test var wilcox.test

kurtosis<-function (x)
{
  #from Snedecor and Cochran, p 80
  x<-x[!is.na(x)]
  m4<-sum((x - mean(x))^4)
  m2<-sum((x - mean(x))^2)
  kurt<-m4*length(x)/(m2**2)-3
  return(kurtosis=kurt)
}

#' Skewness
#'
#' Computes the skewness.
#'
#' If \eqn{N = \mathrm{length}(x)}{N = length(x)}, then the skewness of \eqn{x}
#' is defined as \deqn{N^{-1} \mathrm{sd}(x)^{-3} \sum_i (x_i -
#' \mathrm{mean}(x))^3.}{ N^(-1) sd(x)^(-3) sum_i (x_i - mean(x))^3.}
#'
#' @usage skewness(x)
#' @param x a numeric vector containing the values whose skewness is to be
#' computed. NA values are removed in the computation.
#' @return The skewness of \code{x}.
#' @references G. Snedecor, W. Cochran. \emph{Statistical Methods},
#' Wiley-Blackwell, 1989
#' @keywords univar
#' @examples
#'\dontrun{
#' x <- rnorm(100)
#' skewness(x)
#'}
#' @export

skewness<-function (x)
{
  #from Snedecor and Cochran, p 79
  x<-x[!is.na(x)]
  m3<-sum((x - mean(x))^3)
  m2<-sum((x - mean(x))^2)
  skew<-m3/(m2*sqrt(m2/length(x)))
  return(skewness=skew)
}

#' @export


######## gof.test

#' @title Goodness-of-fit tests for npde
#'
#' @description Performs test on the selected variable (which=one of npde, pd or npd) or on a numeric vector
#'
#' @name gof.test
#' @aliases gof.test.default gof.test.numeric gof.test.NpdeRes printgoftest gof.test.NpdeObject
#'
#' @param object an object (currently has methods for types numeric, NpdeRes and NpdeObject)
#' @param parametric a boolean. If TRUE (default), parametric tests are performed
#' @param \dots additional arguments passed on to the function; special arguments are \code{na.action}, which controls how to handle NAs in the results (\code{\link{na.action}}), \code{verbose} (if FALSE, suppresses printing of the results) and \code{covsplit} which requests the tests to be performed split by categories or quantiles of the data. If \code{covsplit} is TRUE, continuous covariates will be split in 3 categories (<Q1, Q1-Q3, >Q3) (see details in the PDF documentation), but this behaviour can be overriden by passing the argument \code{ncat=XXX} where XXX is the number of categories to divide the continuous covariates in.
#' @param which character string giving (used by printgoftest)
#'
#' @return A list with the following elements:
#' \describe{
#' \item{mean}{mean}
#' \item{se.mean}{standard error of the mean}
#' \item{var}{variance}
#' \item{se.var}{standard error on variance}
#' \item{kurtosis}{kurtosis (see \code{\link{kurtosis}})}
#' \item{skewness}{skewness (see \code{\link{skewness}})}
#' \item{p.value}{p-values for several tests (see below)}
#' }
#' @details If object is an NpdeObject and an argument covsplit=TRUE is given in \dots, in addition to the global descriptive statistics and tests, tests will be performed for each covariate in \code{which.cov}. This argument can be set in \dots; barring an explicit specification, the component \code{which.cov} of the prefs slot for a NpdeObject object will be used. The default value is \code{which.cov="all"}, which produces tests for each covariate in the dataset. Two additional dataframes will then be present:
#' \describe{
#' \item{cov.stat}{descriptive statistics and test p-values split by covariate and by categories}
#' \item{cov.p.value}{p-values split by covariate; for each covariate, two tests are performed: the first test is a correlation test for continuous covariates and a Chi-square test for categorical covariates; the second test is defined using the p-values of the global tests split by each category, and appling a Bonferroni correction to obtain an overall p-value (see PDF documentation for details)}
#' }
#' The p.value elements is a named vector with four components:
#' \describe{
#' \item{p.mean}{p-value for the mean test (Wilcoxon test if parametric=FALSE, Student test if parametric=TRUE)}
#' \item{p.var}{p-value for the variance test (parametric=FALSE, Fisher test if parametric=TRUE)}
#' \item{p.dist}{p-value for the distribution test (XXX if parametric=FALSE, XXX if parametric=TRUE)}
#' \item{p.global}{p-value for the global test (combination of the mean, variance and distribution tests with a Bonferroni correction)}
#' }
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F. Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.
#' @references K. Brendel, E. Comets, C. Laffont, and F. Mentre. Evaluation of different tests based on observations for external model evaluation of  population analyses. \emph{Journal of Pharmacokinetics and Pharmacodynamics}, 37:49--65, 2010.
#' @seealso \code{\link{kurtosis}}, \code{\link{skewness}}
#' @examples
#'\dontrun{
##' data(theopp)
##'}
#' @docType methods
#' @keywords methods
#' @keywords test
#' @export

##  #' @param which for NpdeRes or NpdeObject objects, a character string to indicate on which variable the test is to be performed. Appropriate values are "npde" (default), "pd" or "npd"

# Need to create a gof.test 'generic' S3 function to dispatch

gof.test <- function(object, parametric=TRUE, ...) {
  UseMethod("gof.test",object)
}

#' @export

gof.test.numeric<-function(object, parametric=TRUE, ...) {
  # Default is to compare the distribution in object to N(0,1) using parametric tests
  ### parametric=TRUE: use parametric tests for mean (t.test) and variance (Fisher)
  ### if na.omit (default), missing values are removed before testing
  # ECO TODO: non-parametric equivalent of variance test for one-sample ?
  ### ECO TODO: test for pd pd~U(0,1) using test from Magalie
  args1<-match.call(expand.dots=TRUE)
  verbose<-TRUE
  i1<-match("verbose",names(args1))
  if(!is.na(i1) && !is.na(as.logical(as.character(args1[[i1]])))) verbose<-as.logical(as.character(args1[[i1]]))
  i1<-match("which",names(args1))
  if(!is.na(i1) && !is.na(as.logical(as.character(args1[[i1]])))) which<-as.character(args1[[i1]]) else which<-"npde"
  object<-object[!is.na(object)]
  sev<-var(object)*sqrt(2/(length(object)-1))
  semp<-sd(object)
  n1<-length(object)
  sem<-semp/sqrt(length(object))
  res<-list(mean=mean(object),se.mean=sem,var=var(object),se.var=sev, kurtosis=kurtosis(object),skewness=skewness(object))
  object<-object[!is.na(object)]
  myres<-rep(0,4)
  if(parametric) {
    if(which=="pd") y<-t.test(object,mu=0.5) else y<-t.test(object)
  } else {
    if(which=="pd") y<-wilcox.test(object,mu=0.5) else y<-wilcox.test(object)
  }
  myres[1]<-y$p.val
  # ECO TODO: ici utiliser le test de Magalie
  if(which=="pd") y<-ks.test(object,"punif",min=min(object,na.rm=TRUE), max=max(object,na.rm=TRUE)) else y<-shapiro.test(object)
  myres[3]<-y$p.val

  # one sample variance test
  # chi=s2*(n-1)/sigma0 and test of H0={s=sigma0} vs chi2 with n-1 df
  #    if(parametric) {
  chi<-(semp**2)*(n1-1)
  if(which=="pd") chi<-chi*12 # X~U(0,1) => var(X)=1/12
  y<-2*min(pchisq(chi,n1-1),1-pchisq(chi,n1-1))
  myres[2]<-y
  #    } else {
  # ECO TODO: non-parametric equivalent of variance test for one-sample ?
  #    }
  xcal<-3*min(myres[1:3])
  myres[4]<-min(1,xcal)
  if(parametric)
    names(myres)<-c("  t-test                    ","  Fisher variance test      ","  SW test of normality      ", "Global adjusted p-value     ") else
      names(myres)<-c("  Wilcoxon signed rank test ","  Fisher variance test      ", "  SW test of normality      ","Global adjusted p-value     ")
  if(which=="pd") names(myres)[3]<-"KS test of uniformity       "
  res$p.value<-myres
  res$nobs<-n1
  #	if(verbose) printgoftest(res, which=which)
  invisible(res)
}

#' @export

gof.test.NpdeRes<-function(object, parametric=TRUE, ...) {
  # Performs test on the selected variable (one of npde, pd or npd)
  # test on npde, npd:
  ### mean=0
  ### variance=1
  ### normality (Shapiro-Wilks)
  # test on pd:
  ### mean=0.5
  ### variance=1/12
  ### uniformity (Kolmogorov-Smirnov)
  args1<-match.call(expand.dots=TRUE)
  if(length(object@res)==0) {
    cat("No data available.\n")
    return()
  }
  i1<-match("which",names(args1))
  if(!is.na(i1) && !is.na(as.logical(as.character(args1[[i1]])))) which<-as.character(args1[[i1]]) else which<-"npde"
  if(!which%in%c("pd","npde","npd")) {
    cat("Tests can be performed on one of: npde (default), pd, npd. Please choose one using the which argument.\n")
    return()
  }
  if(which=="npde" & length(object@res$npde)==0) {
    cat("    Missing npde object.\n")
    return()
  }
  if(which%in%c("pd","npd") & length(object@res$pd)==0) {
    cat("    Missing pd object.\n")
    return()
  }
  npde<-switch(which,npde=object@res$npde,pd=object@res$pd, npd=qnorm(object@res$pd))
  npde<-npde[object@not.miss] # Removing values for MDV=1 (pd, npde not computed)
  verbose<-TRUE
  i1<-match("verbose",names(args1))
  if(!is.na(i1) && !is.na(as.logical(as.character(args1[[i1]])))) verbose<-as.logical(as.character(args1[[i1]]))
  args1<-match.call(expand.dots=TRUE)
  i1<-match("na.action",names(args1))
  na.action<-"na.omit"
  if(!is.na(i1) && as.character(args1[[i1]]) %in% c("na.omit","na.fail","na.exclude","na.pass")) na.action<-as.character(args1[[i1]])
  if(na.action=="na.fail" & sum(is.na(npde))>0) {
    cat("Missing values and na.action is set to na.fail.\n")
    return()
  }
  if(na.action=="na.pass" & sum(is.na(npde))>0) {
    if(verbose) cat("Warning: there are missing values and na.action is set to na.pass. Results and tests will be obtained removing the missing data (nmis=", sum(is.na(npde)),").\n")
  }
  npde<-eval(call(na.action,npde))
  res<-gof.test(npde,which=which,parametric=parametric)
  if(verbose)
    printgoftest(res, which=which)
  invisible(res)
}

# Prints a summary of a gof.test result

#' @rdname gof.test
#' @export

printgoftest<-function(object, which="npde", ...) {
  cat("---------------------------------------------\n")
  cat("Distribution of",which,":\n")
  cat("      nb of obs:",object$nobs,"\n")
  cat("           mean=",format(object$mean,digits=4),"  (SE=",format(object$se.mean,digits=2),")\n")
  cat("       variance=",format(object$var,digits=4),"  (SE=",format(object$se.var,digits=2),")\n")
  cat("       skewness=",format(object$skewness,digits=4),"\n")
  cat("       kurtosis=",format(object$kurtosis,digits=4),"\n")
  cat("---------------------------------------------\n\n")
  cat("Statistical tests\n")
  for(i in 1:4) {
    cat(names(object$p.value)[i],": ")
    #if (myres[i]<1)
    cat(format(object$p.value[i],digits=3))
    #else cat(myres[i])
    if(as.numeric(object$p.value[i])<0.1 & as.numeric(object$p.value[i])>=0.05)
      cat(" .")
    if(as.numeric(object$p.value[i])<0.05) cat(" *")
    if(as.numeric(object$p.value[i])<0.01) cat("*")
    if(as.numeric(object$p.value[i])<0.001) cat("*")
    cat("\n")
  }
  cat("---\n")
  cat("Signif. codes: '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 \n")
  cat("---------------------------------------------\n")
}
##################################################################################

##################################################################################
################### S4 methods

#' @include NpdeObject.R
#' @rdname show
#' @aliases show.NpdeObject show,Object-method
#' @importFrom methods show
#' @exportMethod show

setMethod("show","NpdeObject",
          function(object) {
            #    cat("Object of class NpdeObject\n")
            cat("Object of class NpdeObject\n")
            cat("-----------------------------------------\n")
            cat("----        Component data           ----\n")
            cat("-----------------------------------------\n")
            show(object@data)
            cat("-----------------------------------------\n")
            if(length(object@results@res)>0) {    
              cat("----        Component results        ----\n")
              cat("-----------------------------------------\n")
              show(object@results)
            } else cat("  No results\n")
          }
)

################### S3 methods

######## print for NpdeObject

#' @aliases print.NpdeObject
#' @rdname print.NpdeData
#' @export
#' 
print.NpdeObject<-
  function(x,nlines=10,...) {
    cat("Object of class NpdeObject\n")
    cat("-----------------------------------\n")
    cat("----          Data             ----\n")
    cat("-----------------------------------\n")
    print(x@data,nlines=nlines)
    if(length(x@data@data)>0) {
      cat("\nSummary of original data:\n")
      cat("    vector of predictor",x@data@name.predictor,"\n")
      print(summary(x@data@data[,x@data@name.predictor]))
      cat("    vector of response",x@data@name.response,"\n")
      print(summary(x@data@data[,x@data@name.response]))
    }
    cat("-----------------------------------\n")
    cat("----         Key options       ----\n")
    cat("-----------------------------------\n")
    decmet<-c("Cholesky decomposition (upper triangular)","Inverse through diagonalisation","Polar method: Cholesky decomposition with diagonalisation")
    imet<-grep(x@options$decorr.method,c("cholesky","inverse","polar"))
    censmet<-c("Omit LOQ data (removed from data)", "Impute pd* and compute y* as F-1(pd*)","Impute y* to the LOQ", "Impute y* as the population model prediction", "Impute y* as the individual model prediction")
    icens<-grep(x@options$cens.method,c("omit","cdf","loq","ppred","ipred"))
    cat("Methods\n")
    cat("    compute prediction discrepancies (pd): ", ifelse(x@options$calc.pd,"yes","no"),"\n")
    cat("    compute normalised prediction distribution errors (npde): ", ifelse(x@options$calc.npde,"yes","no"),"\n")
    cat("    method for decorrelation: ",decmet[imet],"\n")
    cat("    method to treat censored data: ",censmet[icens],"\n")
    cat("Input/output\n")
    cat("    verbose (prints a message for each new subject): ", x@options$verbose,"\n")
    cat("    save the results to a file, save graphs:",x@options$boolsave,"\n")
    if(x@options$boolsave) {
      cat("    type of graph (eps=postscript, pdf=adobe PDF, jpeg, png):", x@options$type.graph,"\n")
      cat("    file where results should be saved: ",x@options$namres,"\n")
      cat("    file where graphs should be saved: ",x@options$namgr,"\n")
    }
    cat("-----------------------------------\n")
    if(length(x@results@res)>0) {
      cat("----      Results      ----\n")
      cat("-----------------------------------\n")
      print(x@results,nlines=nlines)
    } else cat("  No results\n")
  }
#)

######## showall for NpdeObject

#' @aliases showall.NpdeObject
#' @rdname showall
#' @export
#' 
showall.NpdeObject<-function(object) {
  #    cat("Object of class NpdeObject\n")
  digits<-2;nsmall<-2
  cat("Object of class NpdeObject\n")
  cat("-----------------------------------\n")
  cat("----          Data             ----\n")
  cat("-----------------------------------\n")
  showall(object@data)
  cat("-----------------------------------\n")
  cat("----           Options         ----\n")
  cat("-----------------------------------\n")
  decmet<-c("Cholesky decomposition (upper triangular)","Inverse through diagonalisation")
  imet<-grep(object@options$decorr.method,c("cholesky","inverse"))
  censmet<-c("Omit LOQ data (removed from data)", "Impute pd* and compute y* as F-1(pd*)","Impute y* to the LOQ", "Impute y* as the population model prediction", "Impute y* as the individual model prediction")
  icens<-grep(object@options$cens.method,c("omit","cdf","loq","ppred","ipred"))
  cat("Methods\n")
  cat("    compute prediction discrepancies (pd): ", ifelse(object@options$calc.pd,"yes","no"),"\n")
  cat("    compute normalised prediction distribution errors (npde): ", ifelse(object@options$calc.npde,"yes","no"),"\n")
  cat("    method for decorrelation: ",decmet[imet],"\n")
  cat("    method to treat censored data: ",censmet[icens],"\n")
  cat("Input/output\n")
  cat("    verbose (prints a message for each new subject): ", object@options$verbose,"\n")
  cat("    save the results to a file, save graphs:",object@options$boolsave,"\n")
  if(object@options$boolsave) {
    cat("    type of graph (eps=postscript, pdf=adobe PDF, jpeg, png):", object@options$type.graph,"\n")
    cat("    file where results should be saved: ",object@options$namres,"\n")
    cat("    file where graphs should be saved: ",object@options$namgr,"\n")
  }
  cat("-----------------------------------\n")
  if(length(object@results@res)>0) {
    cat("----              Results            ----\n")
    cat("-----------------------------------------\n")
    showall(object@results)
  } else cat("  No results\n")
}

######## summary for NpdeObject

#' @aliases summary.NpdeObject
#' @rdname summary.NpdeData
#' @export

summary.NpdeObject <- function(object,...) {
  cat("Object of class NpdeObject")
  if(length(object@data)>0) {
    cat(" containing the following main components\n")
    cat("   data: data\n")
    cat("       N=",object@data@N,"subjects\n")
    cat("       ntot.obs=",object@data@ntot.obs,"non-missing observations\n")
    cat("        subject id:",object@data@name.group,"\n")
    cat("        predictor (X):",object@data@name.predictor,"\n")
    cat("        response (Y):",object@data@name.response,"\n")
    if(length(object@data@name.covariates)>0)
      cat("        covariates:",object@data@name.covariates,"\n")
    if(length(object@sim.data)>0) {
      cat("   sim.data: simulated data: \n")
      cat("        number of replications: nrep=",object@sim.data@nrep,"\n")
    }
    if(length(object@results)>0) {
      cat("   results: results of the computation\n")
      cat("        ypred: individual predictions (E_i(f(theta_i,x)))\n")
      cat("        pd: prediction discrepancies\n")
      cat("        npde: normalised prediction distribution errors\n")
      cat("        ycomp: imputed responses for censored data\n")
      cat("        ploq: probability of being <LOQ for each observation\n")
    }
    cat("   options: options for computations\n")
    cat("   prefs: options for graphs\n")
  } else cat(", currently empty\n")
  # The first elements of the list are the same as those returned previously, to maintain compatibility with the previous version of npde (1.2)
  obsdat<-data.frame(id=object@data@data[,object@data@name.group], xobs=object@data@data[,object@data@name.predictor], yobs=object@data@data[,object@data@name.response])
  addcol<-c(object@data@name.covariates,object@data@name.miss,object@data@name.cens,object@data@name.ipred)
  if(length(addcol)>0)
    obsdat<-cbind(obsdat,object@data@data[,addcol])
  res<-list(obsdat=obsdat,ydobs=object@results@res$ydobs, ydsim=object@sim.data@datsim$ydsim,xerr=object@results@xerr, npde=object@results@res$npde,pd=object@results@res$pd, N=object@data@N, ntot.obs=object@data@ntot.obs, id=object@data@data[,object@data@name.group], x=object@data@data[,object@data@name.predictor], y=object@data@data[,object@data@name.response],nrep=object@sim.data@nrep, ypred=object@results@res$ypred,ycomp=object@results@res$ycomp, ploq=object@results@ploq,options=object@options,prefs=object@prefs)
  invisible(res)
}

# Previous version of npde
# names(x)
# [1] "obsdat" "ydobs"  "ydsim"  "ypred"  "xerr"   "npde"   "pd"    

######## subset for NpdeObject

#' @aliases subset.NpdeObject
#' @rdname subset.NpdeData
#' @export

subset.NpdeObject<-function (x, subset, ...) {
  if (missing(subset)) 
    return(x)
  else {
    e <- substitute(subset)
    xdat<-as.data.frame(x["data"]["data"])
    r <- eval(e, xdat, parent.frame())
    if (!is.logical(r)) 
      stop("'subset' must evaluate to logical")
    r <- r & !is.na(r)
  }
  # Data
  x1<-x
  x1["data"]["data"]<-x["data"]["data"][r,,drop=FALSE]
  if(length(x1["data"]["not.miss"])>0) {
    x1["data"]["not.miss"]<-x["data"]["not.miss"][r]
    x1["data"]["icens"]<-which(!x1["data"]["not.miss"])
  }
  id<-x1["data"]["data"][,x1["data"]["name.group"]]
  x1["data"]["N"]<-length(unique(id))
  nind.obs<-tapply(id,id,length) # individual numbers of observations (1xN)
  nind.obs<-c(nind.obs[match(unique(id),names(nind.obs))])
  x1["data"]["nind.obs"]<-nind.obs
  x1["data"]["ntot.obs"]<-length(id)
  x1["data"]["ind"]<-rep(1:x1["data"]["N"],times=nind.obs)
  # Simulated data
  rsim<-rep(r,x["sim.data"]["nrep"])
  x1["sim.data"]["datsim"]<-x["sim.data"]["datsim"][rsim,,drop=FALSE] 
  # Results
  x1["results"]["not.miss"]<-logical(0)
  x1["results"]["res"]<-x["results"]["res"][r,,drop=FALSE]
  x1["results"]["icens"]<-x1["data"]["icens"]
  x1["results"]["ntot.obs"]<-x1["data"]["ntot.obs"]
  if(length(x1["results"]["ploq"])>0) x1["results"]["ploq"]<-x["results"]["ploq"][r]
  if(length(x1["results"]["npde.sim"])>0) x1["results"]["npde.sim"]<-x["results"]["npde.sim"][r]
  if(length(x1["results"]["pd.sim"])>0) x1["results"]["pd.sim"]<-x["results"]["pd.sim"][r]
  return(x1)
}

######## Goodness-of-fit tests for NpdeObject

#' @include NpdeRes.R
#' @include NpdeObject.R
#' @include NpdeRes-methods.R
#' @importFrom stats anova cor.test kruskal.test lm quantile runif
#' @export 

gof.test.NpdeObject<-function(object, parametric=TRUE, ...) {
  # Performs test on the selected variable (one of npde, pd or npd) of the results element of an NpdeObject
  # If covsplit is TRUE, performs the test split by covariate
  args1<-match.call(expand.dots=TRUE)
  if(length(object@results)==0) {
    cat("No results\n")
    return()
  }
  i1<-match("which",names(args1))
  if(!is.na(i1) && !is.na(as.logical(as.character(args1[[i1]])))) which<-as.character(args1[[i1]]) else which<-"npde"
  if(!which%in%c("pd","npde","npd")) {
    cat("Tests can be performed on one of: npde (default), pd, npd. Please choose one using the which argument.\n")
    return()
  }
  x<-switch(which, npde=object@results@res$npde, pd=object@results@res$pd, npd=qnorm(object@results@res$pd))
  x<-x[object["data"]["not.miss"]]
  # i1<-match("verbose",names(args1))
  # verbose <- object@options$verbose
  # if(!is.na(i1) && !is.na(as.logical(as.character(args1[[i1]])))) verbose<-as.logical(as.character(args1[[i1]]))
  i1<-match("covsplit",names(args1))
  covsplit<-FALSE
  if(!is.na(i1) && !is.na(as.logical(as.character(args1[[i1]])))) covsplit<-as.logical(as.character(args1[[i1]]))
  i1<-match("sample",names(args1))
  sample.pd<-FALSE
  if(!is.na(i1) && !is.na(as.logical(as.character(args1[[i1]])))) sample.pd<-as.logical(as.character(args1[[i1]]))
  # 	verbose<-!(covsplit)
  # 	i1<-match("verbose",names(args1))
  # 	if(!is.na(i1) && !is.na(as.logical(args1[[i1]]))) verbose<-as.logical(args1[[i1]])
  if(covsplit & length(object["data"]["name.covariates"])>0) {
    which.cov<-object@prefs$which.cov
    i1<-match("which.cov",names(args1))
    if(!is.na(i1)) which.cov<-as.character(args1[[i1]])
    idx1<-!is.na(as.numeric(which.cov))
    if(length(idx1)>0) which.cov[idx1]<-object["data"]["name.covariates"][as.numeric(which.cov)[idx1]]
    if(which.cov[1]=="all") which.cov2<-object@data@name.covariates else
      which.cov2<-which.cov[match(which.cov, object@data@name.covariates,nomatch=0)>0]
    which.cov2<-unique(which.cov2)
    if(length(which.cov2)==0) {
      cat("No covariate matching names",which.cov,", performing a global test.\n")
      covsplit<-FALSE
    } else which.cov<-which.cov2
  } else covsplit<-FALSE
  if(sample.pd) {
    idobs<-object["data"]["data"][object["data"]["not.miss"], object["data"]["name.group"]]
    nind<-tapply(idobs,idobs,length) # individual numbers of observations (1xN)
    nind<-nind[match(unique(idobs),names(nind))]
    isamp<-cumsum(c(0,nind[-length(nind)]))+ceiling(runif(length(nind),0,nind))
    myres<-try(gof.test(x[isamp], parametric=parametric, which=which))
    if(class(myres)!="try-error") printgoftest(myres,which=as.character(which),...)
  } else {
    myres<-try(gof.test(x, parametric=parametric, which=which))
    if(class(myres)!="try-error") printgoftest(myres,which=as.character(which),...)
  }
  if(covsplit) {
    i1<-match("ncat",names(args1))
    ncat.cont<-NA
    if(!is.na(i1) && !is.na(as.integer(as.character(args1[[i1]])))) ncat.cont<-as.integer(as.character(args1[[i1]]))
    covtest<-glcov<-NULL
    for(icov in which.cov) {
      zecov<-object["data"]["data"][object["data"]["not.miss"],icov]
      idobs<-object["data"]["data"][object["data"]["not.miss"], object["data"]["name.group"]]
      ucov<-zecov[match(unique(idobs),idobs)]
      #			if(!is.numeric(ucov) & length(unique(ucov))<=4) 
      if(is.factor(ucov)) {
        # Categorical covariate: lm+anova if parametric, Kruskal-Wallis test otherwise
        covcont<-FALSE
        ncat<-length(unique(ucov))
        namcat<-paste(icov,sort(unique(zecov)),sep="=")
        if(parametric) {
          y<-try(anova(lm(x~zecov)))
          xval<-ifelse(class(y)=="try-error",NA,y$Pr[1])
          l1<-c(Covariate=icov,nb.categories=ncat,corr.pearson=xval)
        } else {
          y<-try(kruskal.test(x~zecov))
          xval<-ifelse(class(y)=="try-error",NA,y$p.value)
          l1<-c(Covariate=icov,nb.categories=ncat,corr.kruskal=xval)
        }
      } else {
        # By default, test split by "<Q1","Q1-Q3",">Q3"
        # If ncat.cont is given by the user, will be split by quantiles
        covcont<-TRUE
        if(is.na(ncat.cont)) ncat<-3 else ncat<-ncat.cont
        # Correlation test: Pearson if parametric, Spearman otherwise
        if(parametric) {
          y<-try(cor.test(zecov,x,method="pearson"))
          xval<-ifelse(class(y)=="try-error",NA,y$p.value)
          l1<-c(Covariate=icov,nb.categories=ncat,corr.pearson=xval)
        } else {
          y<-try(cor.test(zecov,x,method="spearman"))
          xval<-ifelse(class(y)=="try-error",NA,y$p.value)
          l1<-c(Covariate=icov,nb.categories=ncat,corr.spearman=xval)
        }
        if(is.na(ncat.cont)) {
          zecov<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)
          namcat<-paste(icov,c("<Q1","Q1-Q3",">Q3"),sep=": ")
        } else {
          zecov<-cut(zecov,breaks=quantile(ucov,seq(0,1,length.out=(ncat+1))), include.lowest=TRUE, ordered_result=TRUE)
          namcat<-paste(icov,": Quantile ",0:(ncat-1),"/",ncat,"-",1:ncat,"/", ncat,sep="")
        }
      }
      icovtest<-NULL
      for(ic.cov in sort(unique(zecov))) {
        idx<-which(zecov==ic.cov & !is.na(x))
        if(sample.pd) {
          idx.obs<-idobs[idx]
          nind<-tapply(idx.obs,idx.obs,length) # individual numbers of observations (1xN)
          nind<-nind[match(unique(idx.obs),names(nind))]
          isamp<-cumsum(c(0,nind[-length(nind)]))+ceiling(runif(length(nind),0, nind))
          idx<-idx[isamp]
        }
        res1<-try(gof.test(x[idx], parametric=parametric, which=which))
        if(class(res1)=="try-error") xval<-rep(NA,10) else xval<-unlist(res1)
        icovtest<-rbind(icovtest,c(ic.cov, namcat[which(sort(unique(zecov))==ic.cov)],xval))
      }
      for(i in 9:12) {
        xcal<-ncat*min(as.numeric(icovtest[,i]))
        l1<-c(l1,min(1,xcal))
      }
      glcov<-rbind(glcov,l1)
      covtest<-rbind(covtest,icovtest)
    }
    if(parametric) colnames(glcov)[4:7]<-c("  t-test                    ","  Fisher variance test      ","  SW test of normality      ", "Global adjusted p-value     ") else colnames(glcov)[4:7]<-c("  Wilcoxon signed rank test ","  Fisher variance test      ", "  SW test of normality      ","Global adjusted p-value     ")
    if(which=="pd") colnames(glcov)[6]<-"KS test of uniformity       "
    myres$cov.stat<-covtest
    myres$cov.p.value<-glcov
  }
  invisible(myres)
}

####################################################################################

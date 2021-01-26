##################################################################################
################### S4 methods

######## show for NpdeData - imported from methods

#' @name show
#' @title Displays npde objects
#'
#' @description Prints the structure of objects from the package
#'
#' @param object an object from the npde package (NpdeData, NpdeRes, NpdeObject)
#'
#' @aliases show show,NpdeData-method show.NpdeData show,NpdeSimData-method show.NpdeSimData
#' @docType methods
#' @keywords methods
#' @rdname show
#'
#' @importFrom methods show
#' @exportMethod show

setMethod("show","NpdeData",
          function(object) {
            cat("Object of class NpdeData\n")
            if(length(object@ntot.obs)==0) cat("    no data\n") else {
              st1<-paste(object@name.response," ~ ",paste(object@name.predictor,collapse=" + ")," | ", object@name.group,sep="")
              cat("    Structured data:",st1,"\n")
              if(length(object@name.covariates)>0) cat("    Covariates:",object@name.covariates,"\n")

              cat("This object has the following components:\n")
              cat("     data: data\n")
              cat("     with",object@N,"subjects\n")
              cat("     ",object@ntot.obs,"observations\n")
              cat("The data has the following components\n")
              cat("     X:",object@name.predictor,paste("(",object@units$x,")",sep=""),"\n")
              cat("     Y:",object@name.response,paste("(",object@units$y,")",sep=""),"\n")
              if(length(object@name.ipred)>0) cat("     individual model predictions:", object@name.ipred,"\n")
              if(length(object@name.miss)>0) cat("     missing data:",object@name.miss," (1=missing)\n")
              if(length(object@name.cens)>0) cat("     censored data:",object@name.cens," (1=censored)\n")
              if(length(object@loq)>0) cat("      LOQ:    ",object@loq,"\n")
              #      cat("     \n")
            }
          }
)

#' @rdname show
#' @exportMethod show

setMethod("show","NpdeSimData",
          function(object) {
            print(object)
          }
)

######## read for NpdeData - documentation in aaa_generics.R
# @usage read(object, header, sep, na.strings, detect, verbose)

#' @include aaa_generics.R
#' @include NpdeData.R
#' @include NpdeSimData.R
#' @rdname read
#' @exportMethod read

setMethod("read",
          signature="NpdeData",
          function(object,name.data,header=TRUE,sep="",na.strings=c("NA","."),detect=TRUE,verbose=FALSE) {
            ow <- options("warn")
            options("warn"=-1)
            if(class(name.data)!="character") {
              cat("Please provide the name of the data (data.frame or path to file on disk) as a character string.\n")
              return("Creation of npdeData failed")
            }
            if(exists(name.data)) {
              if(verbose) cat("Using the object called",name.data,"in this R session as the data.\n")
              dat<-get(name.data)
            } else {
              if(verbose) cat("Reading data from file",name.data,"\n")
              dat<-try(read.table(name.data,header=header,sep=sep,na.strings=na.strings))
              if(class(dat)=="try-error") stop("The file ",name.data," does not exist. Please check the name and path.\n")
              if(verbose) {
                cat("These are the first lines of the dataset as read into R. Please check the format of the data is appropriate, if not, modify the na and/or sep items and retry:\n")
                print(head(dat))
              }
            }
            if(dim(dat)[2]<2) {
              cat("The dataset contains only one column. To compute npde, we need at least 3 columns, with subject ID, predictor (at least one) and response. \nPlease check the field separator, currently given as:", paste("sep=\"",sep,"\"",sep=""), "\n")
              return("Creation of npdeData failed")
            }
            # Automatic recognition of columns
            #    ID (one of id, subject or sujet regardless of case)
            #    response (one of Y, conc, concentration, resp, response regardless of case)
            #    predictors (time and/or dose, regardless of case)
            # ECO TODO: improve automatic recognition ?
            # check that we have at least a column id, response and X
            if(!is.na(as.integer(object@name.group))) {
              # group given as a column number
              object@name.group<-colnames(dat)[as.integer(object@name.group)]
            }
            if(is.na(object@name.group) || object@name.group=="") {
              if(!detect) {
                cat("Missing ID column and automatic detection is OFF. Please provide a valid name for the ID column\n")
                return("Creation of npdeData failed")
              }
              if(verbose) cat("Missing ID column, attempting to detect it\n")
              object@name.group<-""
              i1<-match("id",tolower(colnames(dat)))
              if(length(i1)==0 | is.na(i1)) {
                i1<-c(match(c("subject","sujet","group"),tolower(colnames(dat))))
              }
              if(length(i1)>0) {
                object@name.group<-colnames(dat)[i1[1]]
                if(verbose) cat("    no name for the group variable (ID) given, will use column --",object@name.group,"-- in the dataset.\n")
              }
            }
            if(object@name.group=="" | is.na(match(object@name.group,colnames(dat)))) {
              cat("Please provide a name for the ID column.\n")
              return("Creation of npdeData failed")
            }
            # Predictors
            i1<-as.integer(object@name.predictor[!is.na(as.integer(object@name.predictor))])
            if(length(i1)>0) {
              object@name.predictor[!is.na(as.integer(object@name.predictor))]<- colnames(dat)[i1]
            }
            if(is.na(object@name.predictor) | length(object@name.predictor)==0 | (length(object@name.predictor)==1 & object@name.predictor[1]=="")) {
              if(!detect) {
                cat("Missing X column and automatic detection is OFF. Please provide a valid name for the column with the predictor.\n")
                return("Creation of npdeData failed")
              }
              if(verbose) cat("Missing predictor column, attempting to detect it\n")
              object@name.predictor<-""
              i1<-c(match(c("xobs","time","temps","tps","tim","x","dose"), tolower(colnames(dat))))
              i1<-i1[!is.na(i1)]
              if(length(i1)>0) {
                object@name.predictor<-colnames(dat)[i1][1]
                if(verbose) cat("    no name for the predictor variable given, will use column(s) --",object@name.predictor,"-- in the dataset.\n")
              }
            }
            id1<-match(object@name.predictor,colnames(dat),nomatch=0)
            if(length(id1[id1==0])>0) {
              if(verbose) cat("    cannot find column(s) --",object@name.predictor[id1==0],"-- dropping them from the data.\n")
            }
            xnam<-object@name.predictor[id1>0]
            if(length(xnam)==0) object@name.predictor<-"" else object@name.predictor<-xnam
            if(length(xnam)==0) {
              cat("Please provide at least one predictor.\n")
              return("Creation of npdeData failed")
            }
            # Response
            if(!is.na(as.integer(object@name.response))) {
              # response given as a column number
              object@name.response<-colnames(dat)[as.integer(object@name.response)]
            }
            if(is.na(object@name.response) || object@name.response=="") {
              if(!detect) {
                cat("Missing response column and automatic detection is OFF. Please provide a valid name for the column with the response.\n")
                return("Creation of npdeData failed")
              }
              if(verbose) cat("Missing response column, attempting to detect it\n")
              object@name.response<-""
              i1<-match("y",tolower(colnames(dat)))
              if(length(i1)==0 | is.na(i1)) {
                i1<-c( match(c("yobs","resp","conc"),tolower(colnames(dat))), grep("response",tolower(colnames(dat)),fixed=TRUE),grep("concentration", tolower(colnames(dat)),fixed=TRUE))
                i1<-i1[!is.na(i1)]
              }
              if(length(i1)>0) {
                object@name.response<-colnames(dat)[i1[1]]
                if(verbose) cat("    no name for the response variable given, will use column --",object@name.response,"-- in the dataset.\n")
              }
            }
            if(is.na(object@name.response)) object@name.response<-""
            if(object@name.response=="" | is.na(match(object@name.response,colnames(dat)))) {
              cat("Please provide a name for the response column.\n")
              return("Creation of npdeData failed")
            }
            # ECO TODO: verifier que les colonnes existent et sinon corriger

            # IPRED : column with individual predictions
            detect.ipred<-FALSE
            if(length(object@name.ipred)>0 && !is.na(as.integer(object@name.ipred))) # ipred given as a column number
              object@name.ipred<-colnames(dat)[as.integer(object@name.ipred)]
            if(length(object@name.ipred)>0 && match(object@name.ipred,colnames(dat),nomatch=0)==0) {
              if(detect & verbose) cat("Can't find a column named",object@name.ipred,"in the dataset for individual predictions, will attempt automatic detection.\n")
              object@name.ipred<-character()
            }
            if(length(object@name.ipred)==0 || is.na(object@name.ipred)) detect.ipred<-TRUE
            if(detect.ipred) {
              i1<-c(grep("ipred",tolower(colnames(dat)),fixed=T))
              if(length(i1)>0) {
                object@name.ipred<-colnames(dat)[i1[1]]
                if(detect.ipred & verbose) cat("    assuming that individual predictions are given in column --",object@name.ipred,"-- in the dataset (to ignore this column, add the argument detect=FALSE in the call to npdeData()).\n")
              }
            }
            # CENS : column indicating censoring
            detect.cens<-FALSE
            if(length(object@name.cens)>0 && !is.na(as.integer(object@name.cens))) # cens given as a column number
              object@name.cens<-colnames(dat)[as.integer(object@name.cens)]
            if(length(object@name.cens)>0 && match(object@name.cens,colnames(dat),nomatch=0)==0) {
              if(detect & verbose) cat("Can't find a column named",object@name.cens,"in the dataset containing censoring, will attempt automatic detection.\n")
              object@name.cens<-character()
            }
            if(length(object@name.cens)==0 || is.na(object@name.cens)) detect.cens<-TRUE
            if(detect.cens) {
              i1<-c(grep("cens",tolower(colnames(dat)),fixed=T))
              if(length(i1)>0) {
                object@name.cens<-colnames(dat)[i1[1]]
                if(detect.cens & verbose) cat("    assuming that censoring information is given in column --",object@name.cens,"-- in the dataset (to ignore this column, add the argument detect=FALSE in the call to npdeData()).\n")
              }
            }
            if(length(object@name.cens)>0) { # checking validity of censoring column
              if(!isTRUE(all.equal(sort(unique(dat[,object@name.cens]), na.last=TRUE),as.integer(c(0,1))))) {
                if(verbose) cat("The column with censoring information should only contain 0 and 1s.\n")
                object@name.cens<-character()
              }}
            # MDV : column indicating missing data
            detect.miss<-FALSE
            if(length(object@name.miss)>0 && !is.na(as.integer(object@name.miss))) # miss given as a column number
              object@name.miss<-colnames(dat)[as.integer(object@name.miss)]
            if(length(object@name.miss)>0 && match(object@name.miss,colnames(dat),nomatch=0)==0) {
              if(detect & verbose) cat("Can't find a column named",object@name.miss,"in the dataset containing missing data status, will attempt automatic detection.\n")
              object@name.miss<-character()
            }
            if(length(object@name.miss)==0 || is.na(object@name.miss)) detect.miss<-TRUE
            if(detect.miss) {
              i1<-c(grep("mdv",tolower(colnames(dat)),fixed=T), grep("miss",tolower(colnames(dat)),fixed=T))
              if(length(i1)>0) {
                object@name.miss<-colnames(dat)[i1[1]]
                if(detect.miss & verbose) cat("    assuming that column --",object@name.miss,"-- in the dataset contains missing data information (to ignore this column, add the argument detect=FALSE in the call to npdeData()).\n")
              }
            }
            if(length(object@name.miss)>0) { # checking validity of MDV column
              if(!isTRUE(all.equal(sort(unique(dat[,object@name.miss]), na.last=TRUE),as.integer(c(0,1)))) & !isTRUE(all.equal(sort(unique(dat[,object@name.miss]), na.last=TRUE),as.integer(c(0)))) & !isTRUE(all.equal(sort(unique(dat[,object@name.miss]), na.last=TRUE),as.integer(c(1))))) {
                if(verbose) cat("The column with information about missing data should only contain 0 and 1s.\n")
                object@name.miss<-character()
              }}
            # Covariates
            if(length(object@name.covariates)>0 & object@name.covariates[1]!="") {
              is.int <- which(!is.na(as.integer(object@name.covariates)))
              is.int <- is.int[as.integer(object@name.covariates[is.int])<dim(dat)[2]]
              object@name.covariates[is.int] <- colnames(dat)[as.integer(object@name.covariates[is.int])]
              nam2 <- colnames(dat)[match(object@name.covariates,colnames(dat))]
              if(sum(is.na(nam2))>0 & verbose) cat("Covariates not found:","-",paste(object@name.covariates[is.na(nam2)],collapse=" - "),"-\n")
              object@name.covariates <- object@name.covariates[!is.na(nam2)]
              object@units$covariates <- object@units$covariates[!is.na(nam2)]
            }
            # Saving covariates in the original format in ocov, transforming binary covariates in dat to factors - No: here we can keep covariates as factors
            #     object@ocov<-dat[,object@name.covariates,drop=FALSE]
            #     for(icov in object@name.covariates) {
            #     	if(length(unique(dat[,icov]))==2) dat[,icov]<-as.integer(factor(dat[,icov]))-1
            #     }

            if(nchar(object@name.group)*length(object@name.predictor)* nchar(object@name.response)<=0) {
              stop("Please check the structure of the data file and provide information concerning which columns specify the group structure (ID), the predictors (eg dose, time) and the response (eg Y, conc). See documentation for automatic recognition of column names for these elements.\n")
            }
            # Data
            all.names<-c(object@name.group,object@name.predictor,object@name.response, object@name.covariates,object@name.miss,object@name.cens,object@name.ipred)
            tab<-dat[,all.names]
            # Index (ID may be numbers, strings,...)
            id<-tab[,object@name.group]
            # ECO TODO: et si un sujet n'a que des donnees NA ???
            object@N<-length(unique(id))
            nind.obs.full<-tapply(id,id,length) # individual numbers of observations (1xN)
            nind.obs.full<-nind.obs.full[match(unique(id),names(nind.obs.full))]
            tab<-data.frame(index=rep(1:object@N,times=nind.obs.full),tab)
            # Missing data
            if(length(object@name.miss)>0) mdv<-tab[,object@name.miss] else {
              mdv<-rep(0,length(id))
              object@name.miss<-"mdv"
            }
            mdv[is.na(tab[,object@name.response])]<-1
            tab[,object@name.miss]<-mdv
            object@data<-tab
            object@ind<-which(mdv==0)
            icens<-numeric()
            if(length(object@name.cens)>0) {
              icens<-which(mdv==0 & dat[,object@name.cens]==1)
            }
            object@icens<-icens
            object@not.miss<-(mdv==0)
            # ECO TODO: what about missing data in covariates & predictor columns
            if(length(object@name.covariates)>0 && sum(is.na(object@data[object@not.miss,object@name.covariates]))>0) {
              tab<-object@data
              for(icov in object@name.covariates) {
                for(i in 2:dim(tab)) {
                  if(is.na(tab[i,icov])) tab[i,icov]<-tab[(i-1),icov]
                }
              }
              object@data<-tab
            }
            #    for(i in object@name.covariates)
            #      object@data[is.na(object@data[,i]),object@name.miss]<-1
            tb1<-tab[tab[,object@name.miss]==0,]
            id1<-tb1[,1]
            object@ntot.obs<-dim(tb1)[1] # total number of observations
            nind.obs<-tapply(id1,id1,length) # individual numbers of observations (1xN)
            nind.obs<-nind.obs[match(unique(id1),names(nind.obs))]
            object@nind.obs<-c(nind.obs)

            #    object@names<-list(group=object@name.group,predictors=object@name.predictor, response=object@name.response, covariates=object@name.covariates)
            options(ow) # reset
            validObject(object)
            return(object)
          }
)

######## read - NpdeSimData
#' @rdname read
#' @exportMethod read

setMethod("read",
          signature="NpdeSimData",
          function(object, name.data, header=TRUE, sep="", na.strings=c("NA","."), verbose=FALSE) {
            ow <- options("warn")
            options("warn"=-1)
            if(exists(name.data)) {
              if(verbose) cat("Using the object called",name.data,"in this R session as the data.\n")
              dat<-get(name.data)
            } else {
              if(verbose) cat("Reading data from file",name.data,"\n")
              if(missing(header)) {
                x1<-try(read.table(name.data,nrows=1))
                if(is.numeric(x1[1,2])) header<-FALSE else header<-TRUE
              }
              if(missing(sep)) sep<-""
              if(missing(na.strings)) na.strings<-c(".","NA")
              dat<-try(read.table(name.data,header=header,sep=sep,na.strings=na.strings))
              if(class(dat)=="try-error") stop("The file ",name.data," does not exist. Please check the name and path.\n")

              x1<-unlist(strsplit(as.character(dat[1,1]),",",fixed=TRUE))
              mysep<-""
              if(length(x1)>1) mysep<-","
              x1<-unlist(strsplit(as.character(dat[1,1]),";",fixed=TRUE))
              if(length(x1)>1) mysep<-";"
              if(mysep!="") dat<-read.table(name.data,na.strings=c(".","NA"),sep=mysep)
              if(!is.numeric(dat[1,1]))
                dat<-read.table(name.data,na.strings=c(".","NA"),header=TRUE,sep=mysep)
              if(!is.numeric(dat[1,1])) {
                cat("The format of the file containing the simulated data is unknown.\n")
                cat("Please use a standard R table format, with or without header,\n")
                cat("and with one of the following separators: \n")
                cat("         TAB or space(s), commas (',') or semicolons (';')\n")
                cat("Also note that a dot should be used to indicate digits in numbers.\n")
                stop("Exiting npde\n")
              }
              if(verbose) {
                cat("These are the first lines of the dataset as read into R. Please check the format of the data is appropriate, if not, modify the na and/or sep items and retry:\n")
                print(head(dat))
              }
            }
            colnames(dat)<-c("idsim","xsim","ysim")
            object@datsim<-dat

            options(ow) # reset
            validObject(object)
            return(object)
          }
)

##################################################################################
######## npdeData

#' Creates a NpdeData object
#'
#' This function is used to create a NpdeData object, representing a longitudinal data structure, and fill it with data from a dataframe or a file on disk
#'
#' @usage npdeData(name.data,header=TRUE,sep="",na.strings=c(".","NA"),name.group,
#' name.predictor, name.response, name.covariates,name.cens,name.miss,name.ipred,
#' units=list(x="",y="",covariates=c()), detect=TRUE,verbose=FALSE)
#' @param name.data name of the file containing the observed data, or a dataframe
#' containing the observed data
#' @param sep field separator (for files on disk)
#' @param na.strings strings to be considered as indicating NA
#' @param header boolean indicating whether the file has a header (mandatory if
#' detect is TRUE)
#' @param name.group name/number of the column in the observed data containing the
#' patient ID (if missing and detect is TRUE, columns named id, subject or sujet
#' (regardless of case) will be assumed to contain this information)
#' @param name.predictor name/number of the column in the observed data containing
#' the independent variable X (if missing and detect is TRUE, columns named xobs,
#' time, dose, x, temps, tim (regardless of case) will be assumed to
#' contain this information)
#' @param name.response name/number of the column in the observed data containing
#' the dependent variable Y (if missing and detect is TRUE, columns named yobs,
#' response, resp, conc, concentration (regardless of case) will be assumed to
#' contain this information)
#' @param name.miss name/number of the column containing information about missing
#' data (MDV) (if missing and detect is TRUE, column called mdv or miss
#' (regardless of case) will be assumed to contain this information)
#' @param name.cens name/number of the column containing information about censored
#' data (cens) (if missing and detect is TRUE, column with a name containing cens
#' (regardless of case) will be assumed to contain this information)
#' @param name.covariates name/number of the column(s) containing covariate
#' information (optional)
#' @param name.ipred name/number of the column(s) with individual predictions
#' (ipred)  (if missing and detect is TRUE, column with a name containing ipred
#' (regardless of case) will be assumed to contain this information)
#' @param units a list with components x, y and cov (optional), specifying the
#' units respectively for the predictor (x), the response (y), and the covariates
#' (a vector of length equal to the number of covariates). Units will default to (-) if not given.
#' @param detect a boolean controlling whether automatic recognition of columns in the dataset is on, defaults to TRUE
#' @param verbose whether to print warning messages, defaults to FALSE (set to TRUE to check how data is being handled)
#'
#' @return an object of class NpdeData
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentr{\'e}. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords models
#' @export
#' @examples
#' \dontrun{
#' data(theopp)
#'
#' x<-npdeData(theopp) # Automatic detection
#' print(x)
#' x<-npdeData(theopp,name.group="ID",name.predictor="Time",name.response="Conc",
#' name.covariates=c("Wt"),units=list(x="hr",y="mg/L",covariates="kg")) # Explicit
#' print(x)
#' plot(x)}
npdeData<-function(name.data,header=TRUE,sep="",na.strings=c(".","NA"),name.group, name.predictor,
                   name.response, name.covariates,name.cens,name.miss,name.ipred, 
                   units=list(x="",y="",covariates=c()),detect=TRUE,verbose=FALSE) {
  # setting proper types for the NpdeData class
  if(missing(name.data) ||length(name.data)==0) {
    cat("Error in npdeData: please provide the name of the datafile or dataframe (between quotes)\n")
    return("Creation of NpdeData failed")
  }
  if(is.data.frame(name.data)) name.data<-deparse(substitute(name.data))
  if(missing(name.group)) name.group<-"" else name.group<-as.character(name.group)
  if(missing(name.predictor)) name.predictor<-"" else name.predictor<-as.character(name.predictor)
  if(missing(name.response)) name.response<-"" else  name.response<-as.character(name.response)
  if(missing(name.covariates) || name.covariates[1]==0) name.covariates<-character() else name.covariates<-as.character(name.covariates)
  if(missing(name.miss) || name.miss==0) name.miss<-character() else name.miss<-as.character(name.miss)
  if(missing(name.cens) || name.cens==0) name.cens<-character() else name.cens<-as.character(name.cens)
  if(missing(name.ipred) || name.ipred==0) name.ipred<-character() else name.ipred<-as.character(name.ipred)
  if(missing(detect)) detect<-TRUE
  x<-new(Class="NpdeData",name.group=name.group, name.predictor=name.predictor,name.response=name.response, name.covariates=name.covariates,name.cens=name.cens,name.miss=name.miss, name.ipred=name.ipred,units=units)
  #  showall(x)
  if(detect & verbose) cat("Automatic detection of variables is ON. The program will attempt to detect both mandatory variables (ID, X, Y) and optional variables (IPRED, MDV, CENS) when they are not specifically given or when the user-specified names are not found in the dataset, by looking in the names of the columns (to override this behaviour, please use argument detect=FALSE in the call to npdeData().\n")
  x1<-read(x,name.data,header=header,sep=sep,na.strings=na.strings,detect=detect, verbose=verbose)
  if(class(x1)!="character") {
    if(length(x1["name.cens"])==0) loq<-NA else {
      if(sum(x1["data"][x1["data"][,x1["name.miss"]]==0,x1["name.cens"]])>0) {
        yloq<-x1["data"][x1["data"][,x1["name.cens"]]==1 & x1["data"][,x1["name.miss"]]==0,x1["name.response"]]
        if(length(unique(yloq))==1) {
          loq<-unique(yloq)
          if(verbose) cat("Same LOQ for all missing data, loq=",loq,"\n")
        } else {
          loq<-min(unique(yloq),na.rm=TRUE)
          if(verbose)cat("There are different LOQ for different observations, setting loq to the lowest value of",loq,"\n")
        }
      }
      x1["loq"]<-loq
    }
    if(verbose) {
      cat("\n\nThe following NpdeData object was successfully created:\n\n")
      print(x1,nlines=0)
    }
  } else x1<-"Creation of NpdeData failed"
  return(x1)
}

######## npdeSimData

#' Creates a NpdeSimData object
#'
#' This function is used to create a NpdeSimData object containing the simulated data corresponding to an NpdeData object
#'
#' @usage npdeSimData(npde.data, name.simdata, header=TRUE, verbose=FALSE)
#'
#' @param npde.data a NpdeData object
#' @param name.simdata name of the file containing the simulated data, or a dataframe containing it
#' @param header boolean indicating whether the file has a header (mandatory if
#' detect is TRUE)
#' @param verbose whether to print warning messages, defaults to FALSE (set to TRUE to check how data is being handled)
#'
#' @return an object of class NpdeSimData
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{NpdeData}}, \code{\link{npde}}, \code{\link{autonpde}}
#' @export

npdeSimData<-function(npde.data,name.simdata,header=TRUE,verbose=FALSE) {
  if(is.data.frame(name.simdata)) name.simdata<-deparse(substitute(name.simdata))
  ierror<-FALSE
  if(missing(npde.data)) {
    ierror<-TRUE
    cat("   Error: Missing first argument.\n")
  }
  if(!ierror) {
    x1<-try(class(npde.data))
    if(class(x1)=="try-error") {
      ierror<-TRUE
      cat("   Error:", deparse(substitute(npde.data)),"does not exist.\n")
    }
    if(!ierror && x1!="NpdeData") {
      ierror<-TRUE
      cat("   Error:", deparse(substitute(npde.data)),"is not a NpdeData object.\n")
    }
  }
  if(ierror) {
    cat("Function npdeSimData requires two mandatory arguments: first, a NpdeData object created by a call to npdeData() (see help page for the syntax of that function), and the name of a matching dataset containing the simulated data (either a file on disk or a data.frame. Please refer to the documentation for details and examples.\n")
    return("Creation of NpdeSimData failed")
  }
  x1<-new(Class="NpdeSimData")
  x<-read(x1,name.data=name.simdata,header=header,verbose=verbose)
  if(sum(npde.data["data"][,npde.data["name.miss"]])>0) {
    if(verbose) cat("There are rows with MDV=1 in the original dataset, the corresponding rows will be removed from the simulated dataset.\n")
  }
  nrep<-dim(x@datsim)[1]/dim(npde.data@data)[1]
  x@nrep<-as.integer(nrep)
  if(nrep<1000 & verbose) {
    cat("Warning: the number of simulations is",nrep,"which may be too small.\n")
    cat("We advise performing at least 1000 simulations to compute npde.\n")
  }
  irsim<-rep(1:nrep,each=dim(npde.data@data)[1])
  x@datsim$irsim<-irsim


  return(x)
}
##################################################################################
################### S3 methods

# print/showall/summary/subset
# alias in class documentation

######## print for NpdeData and NpdeSimData

#' Prints objects from the npde package
#'
#' prints  objects of classes NpdeData, NpdeSimData, NpdeRes and NpdeObject
#'
#' @aliases print.NpdeData
#' @param x an object of class NpdeData, NpdeSimData, NpdeRes or NpdeObject
#' @param nlines number of lines from the dataset to print
#' @param ... Additional arguments (ignored)
#' @export

print.NpdeData <- function(x,nlines=10,...) {
  digits<-2;nsmall<-2
  cat("Object of class NpdeData\n")
  cat("    longitudinal data\n")
  if(length(x@name.group)>0) {
    st1<-paste(x@name.response," ~ ",paste(x@name.predictor,collapse=" + ")," | ", x@name.group,sep="")
    cat("    Structured data:",st1,"\n")
    cat("    predictor:",x@name.predictor,paste("(",x@units$x,")",sep=""),"\n")
    if(length(x@name.covariates)>0) {
      cat("    covariates:",paste(paste(x@name.covariates," (",x@units$covariates,")",sep=""),collapse=", "),"\n")
    }
    if(dim(x@data)[1]>0) {
      if(nlines==0) return()
      cat("Dataset characteristics:\n")
      cat("    number of subjects:    ",x@N,"\n")
      cat("    number of non-missing observations:",x@ntot.obs,"\n")
      cat("    average/min/max nb obs:",format(mean(x@nind.obs),digits=digits, nsmall=nsmall), " / ", min(x@nind.obs)," / ",max(x@nind.obs),"\n")
      if(length(x@loq)>0) cat("      LOQ:    ",x@loq,"\n")
      #    if(length(x@tab)>0) print(x@tab)
      if(nlines==(-1)) {
        cat("Data:\n")
        print(x@data)
      } else {
        cat("First",nlines,"lines of data:\n")
        nrowShow <- min (nlines , nrow(x@data))
        print(x@data[1:nrowShow,])
      }
    } else cat("No data.\n")
  } else cat("Empty object\n")
}

#' @export

print.NpdeSimData <- function(x,nlines=10,...) {
  digits<-2;nsmall<-2
  cat("Object of class NpdeSimData\n")
  cat("    simulated data\n")
  if(length(x@nrep)>0) {
    cat("    Number of replications:",x@nrep,"\n")
    if(dim(x@datsim)[1]>0) {
      if(nlines==0) return()
      if(nlines==(-1)) {
        cat("Data:\n")
        print(x@datsim)
      } else {
        cat("First",nlines,"lines of data:\n")
        nrowShow <- min (nlines , nrow(x@datsim))
        print(x@datsim[1:nrowShow,])
      }
    } else cat("No data.\n")
  } else cat("Empty object\n")
}


######## showall

# Could be print, with only head of data

#' Contents of an object
#'
#' Prints the contents of an object
#'
#' @name showall
#' @aliases showall showall.NpdeData showall,NpdeData-method showall.default showall,method
#' @param object a NpdeData object
#' @keywords print
#' @export

#setMethod("showall","NpdeData",
# Need to create a showall 'generic' S3 function to dispatch
showall <- function(object)
  UseMethod("showall",object)

#' @export
# Default showall is to print
showall.default <- function(object)
  print(object)

#' @export
# Showall for NpdeData as an S3 method
showall.NpdeData <- function(object) {
  digits<-2;nsmall<-2
  cat("Object of class NpdeData\n")
  cat("    longitudinal data\n")
  if(length(object@N)>0) {
    st1<-paste(object@name.response," ~ ",paste(object@name.predictor,collapse=" + ")," | ", object@name.group,sep="")
    cat("    Structured data:",st1,"\n")
    cat("    subject identifier:    ",object@name.group,"\n")
    cat("    predictor:       ",object@name.predictor, paste("(",object@units$x,")",sep=""),"\n")
    cat("    response:        ",object@name.response,paste("(",object@units$y,")",sep=""),"\n")
    if(length(object@name.covariates)>0) {
      cat("    covariates:",paste(paste(object@name.covariates," (", object@units$covariates,")",sep=""),collapse=", "),"\n")
    }
    cat("This object has the following components:\n")
    cat("     data: data\n")
    cat("     with",object@N,"subjects\n")
    cat("     ",object@ntot.obs,"observations\n")
    cat("The data has the following components\n")
    cat("     X:",object@name.predictor,"\n")
    cat("     Y:",object@name.response,"\n")
    if(length(object@name.ipred)>0) cat("     individual model predictions:", object@name.ipred,"\n")
    if(length(object@name.miss)>0) cat("     missing data:",object@name.miss," (1=missing)\n")
    if(length(object@name.cens)>0) cat("     censored data:",object@name.cens," (1=censored)\n")
    if(length(object@loq)>0) cat("      LOQ:    ",object@loq,"\n")
    cat("Dataset characteristics:\n")
    cat("    number of subjects:    ",object@N,"\n")
    if(object@N>0) {
      cat("    number of non-missing observations:",object@ntot.obs,"\n")
      cat("    average/min/max nb obs:",format(mean(object@nind.obs),digits=digits, nsmall=nsmall), " / ", min(object@nind.obs)," / ",max(object@nind.obs),"\n")
      #    if(length(object@orig)>0) print(object@orig)
    }
    if(dim(object@data)[1]>0) {
      cat("First lines of data:\n")
      nrowShow <- min (10 , nrow(object@data))
      print(object@data[1:nrowShow,])
    } else cat("No data.\n")
  } else cat("Empty object\n")
}
#)

######## summary for NpdeData

#' Summary of a NpdeData object
#'
#' Extracts elements from a NpdeData object
#'
#' @aliases summary.NpdeData
#' @param object A NpdeData object
#' @param print whether to print to data to stdev
#' @param ... Additional arguments (ignored)
#' @export

summary.NpdeData <- function(object, print=TRUE, ...) {
  if(length(object@data)==0) {
    cat("Object of class NpdeData, empty.\n")
    return()
  }
  res<-list(N=object@N,data=object@data, ntot.obs=object@ntot.obs,nind.obs=object@nind.obs)
  if(length(object@loq)>0) res$loq<-object@loq
  invisible(res)
}

######## subset for NpdeData

#' Subsetting a NpdeData object
#'
#' Return subset of data from a NpdeData object
#'
#' @aliases subset.NpdeData
#' @param x A NpdeData object
#' @param subset logical expression indicating elements or rows to keep: missing values are taken as false.
#' @param ... Additional arguments (ignored)
#' @export

subset.NpdeData<-function (x, subset, ...) {
  if (missing(subset))
    return(x)
  else {
    e <- substitute(subset)
    xdat<-x["data"]
    r <- eval(e, xdat, parent.frame())
    if (!is.logical(r))
      stop("'subset' must evaluate to logical")
    r <- r & !is.na(r)
  }
  x1<-x
  x1["data"]<-x["data"][r,,drop=FALSE]
  if(length(x1["not.miss"])>0) {
    x1["not.miss"]<-x["not.miss"][r]
    x1["icens"]<-which(!x1["not.miss"])
  }
  id<-x1["data"][,x1["name.group"]]
  x1["N"]<-length(unique(id))
  nind.obs<-tapply(id,id,length) # individual numbers of observations (1xN)
  nind.obs<-c(nind.obs[match(unique(id),names(nind.obs))])
  x1["nind.obs"]<-nind.obs
  x1["ntot.obs"]<-length(id)
  x1["ind"]<-rep(1:x1["N"],times=nind.obs)
  return(x1)
}

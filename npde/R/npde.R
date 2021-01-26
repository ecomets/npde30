# Unlinked documentation for package npde
# main help file + datasets + censoring method

############################# help file for main function (autonpde)
#' Normalised prediction distribution errors for nonlinear mixed-effect models
#'
#' Routines to compute normalised prediction distribution errors, a metric
#' designed to evaluate non-linear mixed effect models such as those used in
#' pharmacokinetics and pharmacodynamics
#'
#' \tabular{ll}{ Package: \tab npde\cr Type: \tab Package\cr Version: \tab
#' 3.0\cr Date: \tab 2017-01-02\cr License: \tab GPL version 2 or later\cr }
#' See the documentation for npde for details
#'
#' @name npde-package
#' @docType package
#' @author Emmanuelle Comets, Karl Brendel, Thi Huyen Tram Nguyen, France Mentre
#'
#' Maintainer: Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentr{\'e}. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords models
#' @examples
#' \dontrun{
#' data(theopp)
#' data(simtheopp)
#'
#' # Calling autonpde with dataframes
#'
#' x<-autonpde(theopp,simtheopp,ix="Time",iy="Conc",iid="ID",boolsave=FALSE)
#' print(x)
#'
#' # Calling autonpde with names of files to be read from disk
#'
#' write.table(theopp,"theopp.tab",quote=FALSE,row.names=FALSE)
#' write.table(simtheopp,"simtheopp.tab",quote=FALSE,row.names=FALSE)
#' x<-autonpde(namobs="theopp.tab", namsim="simtheopp.tab", iid = 1,
#' ix = 3, iy = 4, boolsave = FALSE)
#'
#' print(x)
#' }
"_PACKAGE"
#> [1] "_PACKAGE"

############################# help files for datasets included with the package
#' Pharmacokinetics of theophylline
#'
#' The \code{theopp} data frame has 132 rows and 5 columns of data from an
#' experiment on the pharmacokinetics of theophylline.
#'
#' Boeckmann, Sheiner and Beal (1994) report data from a study by Dr. Robert
#' Upton of the kinetics of the anti-asthmatic drug theophylline.  Twelve
#' subjects were given oral doses of theophylline then serum concentrations
#' were measured at 11 time points over the next 25 hours.
#'
#' These data are analyzed in Davidian and Giltinan (1995) and Pinheiro and
#' Bates (2000) using a two-compartment open pharmacokinetic model.
#'
#' These data are also available in the library \code{datasets} under the name
#' \code{Theoph} in a slightly modified format and including the data at time
#' 0.  Here, we use the file in the format provided in the \emph{NONMEM}
#' installation path (see the User Guide for that software for details).
#'
#' @docType data
#' @name theopp
#' @usage theopp
#' @format This data frame contains the following columns: \describe{
#' \item{ID}{ an ordered factor with levels \code{1}, \dots{}, \code{12}
#' identifying the subject on whom the observation was made.  The ordering is
#' by Time at which the observation was made.  } \item{Dose}{ dose of
#' theophylline administered orally to the subject (mg/kg).  } \item{Time}{
#' time since drug administration when the sample was drawn (hr).  }
#' \item{Conc}{ theophylline concentration in the sample (mg/L).  } \item{Wt}{
#' weight of the subject (kg).  } }
#' @source Boeckmann, A. J., Sheiner, L. B. and Beal, S. L. (1994),
#' \emph{NONMEM Users Guide: Part V}, NONMEM Project Group, University of
#' California, San Francisco.
#'
#' Davidian, M. and Giltinan, D. M. (1995) \emph{Nonlinear Models for Repeated
#' Measurement Data}, Chapman & Hall (section 5.5, p. 145 and section 6.6, p.
#' 176)
#'
#' Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models in S and
#' S-PLUS}, Springer (Appendix A.29)
#' @keywords datasets
#' @examples
#' \dontrun{
#' data(theopp)
#'
#' #Plotting the theophylline data
#' plot(Conc~Time,data=theopp,xlab="Time after dose (hr)",
#' ylab="Theophylline concentration (mg/L)")
#'
#'}
NULL

#' Simulated data for the computation of normalised prediction distribution
#' errors in the theophylline dataset
#'
#' The \code{simtheopp} dataset contains 100 simulations using the design of
#' dataset \code{\link{theopp}}. These simulations are used to compute npde.
#' The control file used to perform the simulations can be found in the
#' subdirectory 'doc' within the library npde.
#'
#' See \code{\link{theopp}} for a description of the original dataset.
#'
#' The simulated data was obtained using the software \emph{NONMEM}. A
#' one-compartment model was fit to the data. An exponential interindividual
#' variability was assumed for the three parameters (absorption rate constant
#' ka, volume of distribution V and clearance CL) and a combined additive and
#' proportional residual error model was usd. The estimated parameters were
#' then used to simulate 100 datasets with the same structure as the original
#' dataset.  Thus, for each observation in the original dataset, the simulated
#' dataset contains 100 simulations under the model used for the estimation.
#'
#' This dataset is provided so that users can figure out what type of data is
#' needed for the computation of prediction distribution errors. More
#' information can be found in the User Guide distributed along with this
#' package, which contains a run-through of the theophylline example.
#'
#' @docType data
#' @name simtheopp
#' @usage simtheopp
#' @format A data frame with 132000 rows and 3 variables
#' This data frame contains the following columns: \describe{
#' \item{ID}{ an ordered factor with levels \code{1}, \dots{}, \code{12}
#' identifying the subject on whom the observation was made.  The ordering is
#' first by simulation then by increasing time.  } \item{xsim}{ time since drug
#' administration when the sample was drawn (hr).  } \item{ysim}{ simulated
#' theophylline concentration (mg/L).  } }
#' @seealso \code{\link{theopp}}
#' @source Boeckmann, A. J., Sheiner, L. B. and Beal, S. L. (1994),
#' \emph{NONMEM Users Guide: Part V}, NONMEM Project Group, University of
#' California, San Francisco.
#' @keywords datasets
#' @examples
#'\dontrun{
#' data(simtheopp)
#'
#' # Plotting the simulated data for subject 1 in the first simulation
#' plot(ysim[2:12]~xsim[2:12],data=simtheopp,xlab="Time after dose (hr)",
#' ylab="Theophylline concentration (mg/L)",type="l",
#' main="Example of simulated data for subject 1")
#'
#' # Plotting a 90% prediction interval for the observations in theopp
#' # using the simulated data in simtheopp
#' # note : differences in doses between subjects are not taken into account
#' data(theopp)
#' xpl<-c(0,0.25,0.5,1,2,3.5,5,7,9,12,24)
#' xpl1<-list(c(0,0.1),c(0.2,0.4),c(0.5,0.65),c(0.9,1.2),c(1.9,2.2),c(3.4,4),
#' c(4.9,5.2),c(6.9,7.2),c(8.8,9.4),c(11.5,12.2),c(23.7,24.7))
#'
#' ypl<-cbind(xpl=xpl,binf=xpl,median=xpl,bsup=xpl)
#' for(i in 1:(length(xpl))) {
#'   vec<-simtheopp$ysim[simtheopp$xsim>=xpl1[[i]][1] &simtheopp$xsim<=xpl1[[i]][2]]
#'   ypl[i,2:4]<-quantile(vec,c(0.05,0.5,0.95))
#' }
#' plot(Conc~Time,data=theopp,xlab="Time after dose (hr)",
#' ylab="Theophylline concentration (mg/L)")
#' lines(ypl[,1],ypl[,3],lwd=2)
#' lines(ypl[,1],ypl[,2],lty=2)
#' lines(ypl[,1],ypl[,4],lty=2)
#'}
#'
NULL


#' Pharmacokinetics of warfarin
#'
#' The \code{warfarin} data frame has 251 rows and 8 columns of data containing
#' data on the pharmacokinetics of warfarin, an anticoagulant drug used in the 
#' prevention of thrombosis and thromboembolism.
#' 
#' The dataset is the PK part of a larger dataset including both warfarin concentrations
#' and prothrombin complex activity (PCA), which measures the decreased coagulation 
#' activity resulting from the inhibition of vitamin K recycling, the mechanism of 
#' action of warfarin. It contains the concentrations measured in 32 healthy subjects 
#' after a single oral dose of warfarin sodium (1.5 mg/kg of body weight). The subjects 
#' in the study were sampled at different times over a period of up to 120 hours.
#' 
#' The data is distributed with the Monolix software as a demo for PK/PD modelling.
#' The data has been slightly reformated for R, removing the line at time=0
#' and filling the amt column with the dose for each subject, following the output
#' of simulx which was used to simulate data from two alternative models to fit this
#' dataset.
#' 
#' Two datasets containing simulated data are associated with the \code{warfarin} data. For
#' each dataset, 1000 simulations of the original data were performed.
#' \describe{
#' \item{simwarfarinBase}{the data in this dataset was simulated according to a base model without covariates: the PK model was a two-compartment model, with first-order absorption and a time-delay. Interindividual variability was modelled as log-normal distributions for parameters Tlag, ka, Cl and V1, and the error model was a combined error model. The parameters were estimated by Monolix.}
#' \item{simwarfarinCov}{the data in this dataset was simulated according to a model including several covariates: an age (centered on 30 yr) effect on Cl, a weight (centered on 70 kg) effect on Cl and V1, and a gender effect on V1. The covariate model was built in Monolix.}
#' }
#' 
#' @docType data
#' @name warfarin
#' @aliases simwarfarinBase simwarfarinCov
#' @usage warfarin
#' @format This data frame contains the following columns: 
#' \describe{
#' \item{id}{an integer identifying the subject on whom the observation was made}
#' \item{time}{time since drug administration when the sample was drawn (hr)}
#' \item{amt}{total dose received by the subject (mg)}
#' \item{dv}{warfarin concentration in the sample (mg/L)} 
#' \item{dvid}{observation type (1 for all observations)}
#' \item{wt}{weight of the subject (kg)} 
#' \item{sex}{subject gender (0=female, 1=male)} 
#' \item{age}{age of the subject (yr) } 
#' }
#' 
#' @source O’Reilly (1968). Studies on coumarin anticoagulant drugs. Initiation 
#' of warfarin therapy without a loading dose. Circulation 1968, 38:169-177.
#' @keywords datasets
#' @examples
#' \dontrun{
#' data(warfarin)
#'
#' #Plotting the warfarin PK data
#' plot(dv~time,data=warfarin,xlab="Time after dose (hr)",
#' ylab="Warfarin concentration (mg/L)")
#'
#'}
NULL

############################# help file for censoring method

#' @name npde.cens.method
#' @aliases computenpde.omit computenpde.loq
#' @title Method used to handle censored data
#'
#' @description Specifies the method used to handle censored data (data below the limit of quantification LOQ
#'
#' @details Several methods are available to handle censored data.
#' \describe{
#' \item{omit}{pd and npde for censored data will be set to NA}
#' \item{cdf}{for an observation ycens_ij under the LOQ, a pd_ij will be imputed in the uniform distribution [0-pLOQ_ij] where pLOQ_ij is the probability that y_ij is below LOQ, according to the model; the predictive distribution will then be used to obtain a corresponding y*_ij. This is also performed for all simulated data, and the npde are then computed on the completed dataset containing the observed y_ij for the uncensored data and the y*_ij imputed for the censored data. This method is the default.}
#' \item{ipred}{an observation ycens_ij is replaced by the individual prediction according to the model (ipred, which must be present in the dataset). Simulated data are left untouched.}
#' \item{ppred}{an observation ycens_ij is replaced by the population prediction according to the model. Simulated data are left untouched.}
#' \item{loq}{an observation ycens_ij is replaced by the value of the LOQ. Simulated data are left untouched.}
#' }
#' More details can be found in the PDF documentation.
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords methods
#'
NULL

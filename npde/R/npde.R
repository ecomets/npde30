# Unlinked documentation for package npde
# main help file + datasets + censoring method

############################# help file for main function (autonpde)
#' Normalised prediction distribution errors for nonlinear mixed-effect models
#'
#' Routines to compute normalised prediction distribution errors, a metric
#' designed to evaluate non-linear mixed effect models such as those used in
#' pharmacokinetics and pharmacodynamics
#'
#' @name npde-package
#' @docType package
#' @aliases npde-package
#' @author Emmanuelle Comets, Karl Brendel, Thi Huyen Tram Nguyen, France Mentre
#'
#' Maintainer: Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' 
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentr{\'e}. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @references PDF documentation for npde 3.0: \url{https://github.com/ecomets/npde30/blob/main/userguide_npde_3.0.pdf}
#' 
#' @keywords models
#' @examples
#' data(theopp)
#' data(simtheopp)
#'
#' # Calling autonpde with dataframes
#'
#' x<-autonpde(theopp,simtheopp,ix="Time",iy="Conc",iid="ID",boolsave=FALSE)
#' print(x)
#'

NULL

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
#' 
#' @references PDF documentation for npde 3.0: \url{https://github.com/ecomets/npde30/blob/main/userguide_npde_3.0.pdf}
#' 
#' @keywords datasets
#' @examples
#' data(theopp)
#'
#' #Plotting the theophylline data
#' plot(Conc~Time,data=theopp,xlab="Time after dose (hr)",
#' ylab="Theophylline concentration (mg/L)")
#'
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
#' 
#' @references PDF documentation for npde 3.0: \url{https://github.com/ecomets/npde30/blob/main/userguide_npde_3.0.pdf}
#' 
#' @keywords datasets
#' @examples
#'\donttest{
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
#' each dataset, 1000 simulations of the original data were performed for the computation of npde.
#' The package contains only the simulated data simwarfarinCov because of size constraints. simwarfarinBase can be downloaded from
#' the github for npde3.0: \url{https://github.com/ecomets/npde30/blob/main/keep/data/simwarfarinBase.tab}
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
#' 
#' @references PDF documentation for npde 3.0: \url{https://github.com/ecomets/npde30/blob/main/userguide_npde_3.0.pdf}
#' 
#' @keywords datasets
#' @examples
#' data(warfarin)
#'
#' #Plotting the warfarin PK data
#' plot(dv~time,data=warfarin,xlab="Time after dose (hr)",
#' ylab="Warfarin concentration (mg/L)")
#'
NULL

#' Simulated HIV viral loads in HIV patients
#'
#' This is simulated data, based on real data obtained in a phase II clinical trial supported by the French Agency for AIDS Research, the COPHAR 3-ANRS 134 trial (Goujard et al., 2010). The original study included 35 patients, who received a once daily dose containing atazanavir (300 mg), ritonavir (100 mg), tenofovir disoproxil (245 mg) and emtricitabine (200 mg) during 24 weeks. Viral loads were measured 6 times over a treatment period of 24 weeks (day 0, 28, 56, 84, 112, 168). 
#' 
#' The datasets were generated in a simulation study designed to evaluate the new method proposed to handle BQL data (Nguyen et al., 2011). Data was simulated using a simple bi-exponential HIV dynamic model describing the two-phase decline of viral load during anti-retroviral treatment.
#' 
#' The \code{virload} data frame has 300 rows and 4 columns of data. The dataset was then censored at two different LOQ levels (LOQ=20 or 50~copies/mL) to generate two datasets containing different proportions of BQL data, creating the data frames \code{virload20} and\code{virload50} respectively
#' 
#' The file simvirload contains 500 simulations under the same model. A full version of the simulated data with 1000 simulations 
#' can be downloaded from the github for npde3.0: \url{https://github.com/ecomets/npde30/blob/main/keep/data/simvirload.tab}
#' 
#' @docType data
#' @name virload
#' @aliases virload20 virloadMDV20 virload50 simvirload
#' @usage virload
#' @format This data frame contains the following columns: 
#' \describe{
#' \item{ID}{an ordered factor with levels \code{1}, \dots, \code{50} identifying the subject on whom the observation was made.  The ordering is by Time at which the observation was made.}
#' \item{Time}{time since the beginning of the study (days)}
#' \item{Log_VL}{logarithm (base 10) of the viral load (copies/L)}
#' \item{cens}{ indicator variable (cens=1 for censored data, cens=0 for observed data)} 
#' \item{ipred}{individual predictions}
#' }
#' 
#' @source Goujard, C., Barrail-Train, A., Duval, X., Nembot, G., Panhard, X., Savic, R., Descamps, D., Vrijens, B., Taburet, A., Mentre, F., and the ANRS 134 study group (2010). 
#' Virological response to atazanavir, ritonavir and tenofovir/emtricitabine: relation to individual pharmacokinetic parameters and adherence measured by medication events monitoring system (MEMS) in naive HIV-infected patients (ANRS134 trial). 
#' \emph{International AIDS Society 2010}, Abstr WEPE0094.
#' 
#' @references PDF documentation for npde 3.0: \url{https://github.com/ecomets/npde30/blob/main/userguide_npde_3.0.pdf}
#' 
#' @keywords datasets
#' @examples
#' \donttest{
#'   data(virload)
#'   str(virload)
#'   data(virload50)
#'   # Plotting the data
#'   plot(Log_VL~Time,data=virload,xlab="Time (d)",ylab="Viral loads, base 10 log-scale (cp/mL)")
#'   plot(Log_VL~Time,data=virload50,xlab="Time (d)",ylab="Viral loads, base 10 log-scale (cp/mL)")
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
#' \item{cdf}{for an observation ycens_ij under the LOQ, a pd_ij will be imputed in the uniform distribution [0-pLOQ_ij] where pLOQ_ij is the probability that y_ij is below LOQ, according to the model; 
#' the predictive distribution will then be used to obtain a corresponding y*_ij. 
#' This is also performed for all simulated data, and the npde are then computed on the completed dataset containing the observed y_ij for the uncensored data and the y*_ij imputed for the censored data. This method is the default.}
#' \item{ipred}{an observation ycens_ij is replaced by the individual prediction according to the model (ipred, which must be present in the dataset). Simulated data are left untouched.}
#' \item{ppred}{an observation ycens_ij is replaced by the population prediction according to the model. Simulated data are left untouched.}
#' \item{loq}{an observation ycens_ij is replaced by the value of the LOQ. Simulated data are left untouched.}
#' }
#' 
#' @return This is not a function and does not have a return value, this is a statistical method.
#' 
#' More details can be found in the PDF documentation.
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords methods
#' @examples
#' \dontrun{
#' # You need to have gridExtra installed to successfully run this example
#' if(requireNamespace("gridExtra", quietly=TRUE)) {
#'   data(warfarin)
#'   data(simwarfarinCov)
#'   wcov<-autonpde(namobs=warfarin,namsim=simwarfarinCov, iid=1,ix=2,iy=4,icov=c(3,6:8),
#'   namsav="warfCov", units=list(x="hr",y="mg/L", covariates=c("mg","kg","-","yr")))
#'   
#'   # Diagnostic plots for warfarin with a covariate model
#'   plot(wcov)
#'   
#'   # Covariate plots
#'   xwt.scatt<-plot(wcov, plot.type="x.scatter", covsplit=TRUE, which.cov="wt")
#'   xwt.qqplot<-plot(wcov, plot.type="qqplot", covsplit=TRUE, which.cov="wt")
#'   xwt.box<-plot(wcov, plot.type="covariates", which.cov="wt")
#'   xsex.scatt<-plot(wcov, plot.type="x.scatter", covsplit=TRUE, which.cov="sex")
#'   xsex.qqplot<-plot(wcov, plot.type="qqplot", covsplit=TRUE, which.cov="sex")
#'   xsex.box<-plot(wcov, plot.type="covariates", which.cov="sex")
#'   
#'   # Transforming the reference profile for npd, compared to a VPC plot
#'   plot.tnpd<-plot(wcov, plot.type="x.scatter", ref.prof=list(id=2), 
#'   main="tnpd with reference profile ID=2")
#'   plot.vpc<-plot(wcov, plot.type="vpc", main="VPC")
#'   gridExtra::grid.arrange(grobs=list(plot.tnpd, plot.vpc), nrow=1, ncol=2)
#'   }
#' }
#' @importFrom gridExtra grid.arrange
#'
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# Avoid problems with named variables in ggplot2 triggering NOTES on CRAN
# very very bad practice from CRAN as this prevents from checking actual problems with code (=> replaced with .data$XXX)
.x<-NULL # used in logscale by format... can't remove ???

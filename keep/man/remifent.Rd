\name{remifent}
\docType{data}
\alias{remifent}
\alias{simremifent}
\alias{simremifent_base}
\title{Pharmacokinetics of Remifentanil}
\description{
  The \code{remifent} data frame has 1992 rows and 12 columns of data from
  an experiment on the pharmacokinetics of remifentanil in healthy volunteers.}
\usage{remifent}
\format{
  This data frame contains the following columns:
  \describe{
    \item{ID}{
      a numeric vector identifying the subject on whom the observation was made.
      The ordering is by Time at which the observation was made.
    }
    \item{Time}{
      time since drug administration when the sample was drawn (min).
    }
    \item{Conc}{
      remifentanil concentration in the sample (ug/L).
    }
    \item{Rate}{
      infusion rate (ug/min).
    }
    \item{AMT}{
      total dose of remifentanil administered orally to the subject (ug).
    }
    \item{Gender}{
      a factor with levels \code{Male} and \code{Female}.
    }
    \item{Ht}{
      height of the subject (cm).
    }
    \item{Wt}{
      weight of the subject (kg).
    }
    \item{BSA}{
      body surface area (m2).
    }
    \item{LBM}{
      lean body mass (kg).
    }
    \item{age.grp}{
      an ordered factor dividing age in 3 age groups, young (\code{20-40}), middle-aged (\code{40-65}) and elderly (\code{over 65}).
    }
  }
}
\details{
  This dataset is one of the datasets distributed in R in the nlme library. The original data was collected in a study by Minto et al, who studied the pharmacokinetics and pharmacodynamics of remifentanil in 65 healthy volunteers. Remifentanil is a synthetic opioid derivative, used as a major analgesic before surgery or in critical care. In the study, the subjects were given remifentanil as a continuous infusion over 4 to 20 min, and measurements were collected over a period of time varying from 45 to 230 min (mean 80 min), along with EEG measurements. The following covariates were recorded: gender, age, body weight, height, body surface area and lean body mass. The recruitment was specifically designed to investigate the effect of age, with recruitment over 3 age groups (young (20-40 yr), middle-aged (40-65 yr) and elderly (over 65 yr)).

This dataset is used to illustrate the new covariate graphs available in the \code{npde} library. It has been modified from the original Remifentanil dataset to include a Rate column and an age group column.
}
\source{
  Minto CF, Schnider TW, Egan TD, Youngs E, Lemmens HJ, Gambus PL, Billard V, Hoke JF, Moore KH, Hermann DJ, Muir KT, Mandema JW, Shafer SL (1997). Influence of age and gender on the pharmacokinetics and pharmacodynamics of remifentanil. I. Model development. \emph{Anesthesiology} 86(1):10-23.

  Minto CF, Schnider TW, Shafer SL (1997). Pharmacokinetics and pharmacodynamics of remifentanil. II. Model application. \emph{Anesthesiology} 86(1):24-33.

  Pinheiro JC, Bates DM. (2000) \emph{Mixed-effects Models in S and S-PLUS}, Springer (Appendix A.29)
}
\examples{
data(remifent)
str(remifent)

#Plotting the remifentanil data
plot(Conc~Time,data=remifent,xlab="Time after dose (min)", ylab="Theophylline concentration (ug/L)")

}
\keyword{datasets}


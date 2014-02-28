\name{getRb2Sens}
\alias{getRb2Sens}
\title{Calculate basal respiration}
\description{Calculate basal respiration from decomposed signal and Q10 value}
\usage{getRb2Sens(tau_lf, rho_lf, tau, rho, S)}
\arguments{
  \item{tau_lf}{numeric vector: low frequency component of normalized temperature time series (T - Tref)/gamma}
  \item{rho_lf}{numeric vector: low frequency component of logarithmic respiration time series log(respiration)}
  \item{tau}{numeric vector: normalized temperature time series e.g. (T - Tref)/gamma}
  \item{rho}{numeric vector: logarithmic respiration time series log(respiration)}
  \item{S}{numeric: estimated Q10 value or}
}
\details{This function can be useful to calculate predicted Rb for surrogate and time-lagged SCAPE output data
}
\value{time series of estimated basal respiration}

\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans, Miguel Mahecha, Max-Planck-Institute for Biogeochemistry, Jena}






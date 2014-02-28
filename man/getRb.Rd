\name{getRb}
\alias{getRb}
\title{Calculate basal respiration}
\description{Calculate basal respiration from decomposed signal and Q10 value}
\usage{getRb(tau_lf, rho_lf, tau, rho, Q10)}
\arguments{
  \item{tau_lf}{numeric vector: low frequency component of normalized temperature time series (T - Tref)/gamma}
  \item{rho_lf}{numeric vector: low frequency component of logarithmic respiration time series log(respiration)}
  \item{tau}{numeric vector: normalized temperature time series (T - Tref)/gamma}
  \item{rho}{numeric vector: logarithmic respiration time series log(respiration)}
  \item{Q10}{numeric: estimated Q10 value}
}
\details{This function can be useful to calculate predicted Rb for surrogate and time-lagged SCAPE output data
}
\value{time series of estimated basal respiration}

\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans, Miguel Mahecha, Max-Planck-Institute for Biogeochemistry, Jena}






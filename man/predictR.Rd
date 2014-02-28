\name{predictR}
\alias{predictR}
\title{Predict respiration}
\description{Calculate respiration for given basal respiration and Q10 value}
\usage{predictR(Rb, S, tau, lag = 0)}
\arguments{
  \item{Rb}{numeric vector: basal respiration}
  \item{S}{numeric: Sensitivity}
  \item{tau}{numeric vector: converted temperature time series}
  \item{lag}{
}
}
\details{This function can be useful to calculate predicted respiration for surrogate and time-lagged SCAPE output data
}
\value{time series of predicted respiration}

\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans, Miguel Mahecha, Max-Planck-Institute for Biogeochemistry, Jena}






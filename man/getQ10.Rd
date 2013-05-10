\name{getQ10}
\alias{getQ10}
\title{determine Q10 value of a respiration and temperature time series}
\description{Function to determine the Q10-value based on a given rtemeprature and respiration time series  }
\usage{getQ10(temperature, respiration, sf, gam = 10, fborder = 30, 
    M = c(90, 30), nss = 10, method = "Fourier", gapFilling = TRUE)}
\arguments{
  \item{temperature}{numeric vector: temperature time series}
  \item{respiration}{numeric vector: respiration time series}
  \item{sf}{numeric: sampling rate, number of measurements per day}
  \item{gam}{numeric: gamma value in Q10 equation}
  \item{fborder}{numeric: boundary for dividing high- and low-frequency parts in days}
  \item{M}{numeric vector: size of SSA window in days}
  \item{nss}{numeric vector: number of surrogate samples}
  \item{method}{String: method to be applied for signal decomposition (choose from "Fourier","SSA","MA","EMD")}
  \item{gapFilling}{Logical: Choose whether Gap-Filling should be applied}
}
\details{This function ....}
\value{Data frame containing decomposed time series, Q10 values etc}

\author{Fabian Gans, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de
Fabian Gans}






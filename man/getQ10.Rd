\name{getQ10}
\alias{getQ10}
\title{determine Q10 value of a respiration and temperature time series}
\description{Function to determine the Q10-value based on a given rtemeprature and respiration time series  }
\usage{getQ10(temperature, respiration, sf, Tref = 15, gam = 10, fborder = 30, 
    M = -1, nss = 0, method = "SSA", gapFilling = TRUE, plot = FALSE)}
\arguments{
  \item{temperature}{numeric vector: temperature time series}
  \item{respiration}{numeric vector: respiration time series}
  \item{sf}{numeric: sampling rate, number of measurements (per day)}
  \item{Tref}{numeric: reference temperature in Q10 equation}
  \item{gam}{numeric: gamma value in Q10 equation}
  \item{fborder}{numeric: boundary for dividing high- and low-frequency parts (in days)}
  \item{M}{numeric vector: size of SSA window (in days)}
  \item{nss}{numeric vector: number of surrogate samples}
  \item{method}{String: method to be applied for signal decomposition (choose from "Fourier","SSA","MA","EMD","Spline")}
  \item{gapFilling}{Logical: Choose whether Gap-Filling should be applied}
  \item{plot}{Logical: Choose whether Surrogates should be plotted}
}
\details{This function estimates the temperature sensitivity of a respiration time series
based on the SCAPE principle ("SCAle dependent Parameter Estimation") as described in
Mahecha et al. (2010).

We assume a very basic respiration model as: $R(t) = R_b(t)Q_10^{tau(t)}$}
\value{Data frame containing decomposed time series, Q10 values etc}

\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans}






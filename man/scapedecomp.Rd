\name{scapedecomp}
\alias{scapedecomp}
\title{Decompose a signal into given frequency bands }
\description{Decompose a signal into given frequency bands using SSA, EMD, Fourier or MA-Filter   }
\usage{scapedecomp(x, sf, fborder, Ms = 90, method = "Fourier")}
\arguments{
  \item{x}{numeric vector: time series}
  \item{sf}{numeric: sampling frequency (samples per day)}
  \item{fborder}{numeric: boundary time scale (in days)}
  \item{Ms}{vector: SSA window length (only applicable for SSA)}
  \item{method}{String: decomposition methodm can be "SSA", "EMD", "Fourier", "Spline" or "MA"}
}
\details{This function decomposes the time series into a low frequency and a high frequency component while making 
sure that both components added together result agasin in the original signal. }
\value{data frame containing low frequency and high frequency parts of the data frame}

\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans}






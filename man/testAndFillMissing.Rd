\name{testAndFillMissing}
\alias{testAndFillMissing}
\title{determine if one or both of the time series have gaps and fill them }
\description{Function that tests the respiration and temperature time series for gaps. }
\usage{testAndFillMissing(DAT, sf)}
\arguments{
  \item{DAT}{data frame: contains temperature and respiration time series}
  \item{sf}{numeric: sampling frequency of the time series}
}
\details{Function that tests the respiration and temperature time series for gaps. 
If gaps are detected, they are filled using SSA. A weight vector will be created which sets the weight for the filled values to 0}


\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans, Miguel Mahecha, Max-Planck-Institute for Biogeochemistry, Jena}






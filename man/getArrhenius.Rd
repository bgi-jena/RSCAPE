\name{getArrhenius}
\alias{getArrhenius}
\title{Estimate \eqn{E_a}{Ea} value and time varying \eqn{R_b}{Rb} from temperature and efflux time series including uncertainty.}
\description{Function to determine the temperature sensitivity for an Arrhenius-like model and time varying 
basal efflux (\eqn{R_b(i)}{Rb(i)}) from a given temperature and efflux (usually respiration) time series 
according the principle of "SCAle dependent Parameter Estimation, SCAPE" (Mahecha et al. 2010).  }
\usage{getArrhenius(temperature, respiration, sf, fborder = 30, M = -1, 
    nss = 0, method = "Fourier", weights = NULL, lag = NULL, 
    gapFilling = TRUE, doPlot = FALSE)}
\arguments{
  \item{temperature}{numeric vector: temperature time series}
  \item{respiration}{numeric vector: respiration time series}
  \item{sf}{numeric: sampling rate, number of measurements (per day)}
  \item{fborder}{numeric: boundary for dividing high- and low-frequency parts (in days)}
  \item{M}{numeric vector: size of SSA window (in days)}
  \item{nss}{numeric vector: number of surrogate samples}
  \item{method}{String: method to be applied for signal decomposition (choose from "Fourier","SSA","MA","wavMODWT","Spline")}
  \item{weights}{numeric vector: optional vector of weights to be used for linear regression, points can be set to 0 for bad data points}
  \item{lag}{numeric vector: optional vector of time lags between respiration and temprature signal}
  \item{gapFilling}{Logical: Choose whether Gap-Filling should be applied}
  \item{doPlot}{Logical: Choose whether Surrogates should be plotted}
}
\details{Function to determine the temperature sensitivity (\eqn{E_a}{Ea} value) and time varying basal efflux (\eqn{R_b}{Rb}) from a given temperature and efflux (usually respiration) time series. 
The following Arrhenius model could be used to describe the respiration:

\eqn{Resp(i) = R_b e^\frac{- Ea}{R T(i)} }{Resp(i) = R_b * exp(- Ea / (R T(i)))},

where \eqn{i}{i} is the time index. It has been shown, however, that this model is misleading when \eqn{R_b}{Rb} is varying over time which can be expected in many real world examples (e.g. Sampson et al. 2008).

If \eqn{R_b}{Rb} varies slowly, i.e. with some low frequency then the "scale dependent parameter estimation, SCAPE" 
allows us to identify this oscillatory pattern. As a consequence, the estimation of \eqn{E_a}{Ea} can be substantially stabilized (Mahecha et al. 2010). The model becomes 

\eqn{Resp(i) = R_b(i) e^\frac{- Ea}{R T(i)} }{Resp(i) = R_b(i) * exp(- Ea / (R T(i)))},

where \eqn{R_b(i)}{Rb(i)} is the time varying "basal respiration", i.e. the respiration expected at \eqn{Tref}{Tref}. The convenience function getArrhenius allows to extract the \eqn{E_a}{Ea} value minimizing the confounding factor of the time varying \eqn{R_b}{Rb}. Four different spectral methods can be used and compared. A surrogate technique (function by curtsey of Dr. Henning Rust, written in the context of Venema et al. 2006) is applied to propagate the uncertainty due to the decomposition.

The user is strongly encouraged to use the function with caution, i.e. see critique by Graf et al. (2011).}
\value{A list with elements

$SCAPE_Ea : the estimated \eqn{E_a}{Ea} with the SCAPE principle and the method chosen.
$Conv_Ea : the conventional \eqn{E_a}{Ea} (assuming constant Rb)
$DAT$SCAPE_R_pred : the SCAPE prediction of respiration 
$DAT$SCAPE_Rb : the basal respiration based on the the SCAPE principle
$DAT$Conv_R_pred : the conventional prediction of respiration 
$DAT$Conv_Rb : the conventional (constant) basal respiration}

\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans, Miguel Mahecha, Max-Planck-Institute for Biogeochemistry, Jena}






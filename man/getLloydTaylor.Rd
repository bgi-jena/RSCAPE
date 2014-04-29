\name{getLloydTaylor}
\alias{getLloydTaylor}
\title{Estimate $E_a$ value and time varying $R_b$ from temperature and efflux time series including uncertainty.}
\description{Function to determine the temperature sensitivity ($E_a$ value) and time varying 
basal efflux (R$_b(i)$) from a given temperature and efflux (usually respiration) time series 
according the principle of "SCAle dependent Parameter Estimation, SCAPE" (Mahecha et al. 2010).  }
\usage{getLloydTaylor(temperature, respiration, sf, Tref = 15, T0 = -46.02, 
    fborder = 30, M = -1, nss = 0, method = "Fourier", weights = NULL, 
    lag = NULL, gapFilling = TRUE, doPlot = FALSE)}
\arguments{
  \item{temperature}{numeric vector: temperature time series}
  \item{respiration}{numeric vector: respiration time series}
  \item{sf}{numeric: sampling rate, number of measurements (per day)}
  \item{Tref}{numeric: Reference temperature (in deg C)}
  \item{T0}{numeric: Minimum temperature (in deg C) at which respiration becomes 0. Make sure that all temperature value are greater than this value}
  \item{fborder}{numeric: boundary for dividing high- and low-frequency parts (in days)}
  \item{M}{numeric vector: size of SSA window (in days)}
  \item{nss}{numeric vector: number of surrogate samples}
  \item{method}{String: method to be applied for signal decomposition (choose from "Fourier","SSA","MA","EMD","Spline")}
  \item{weights}{numeric vector: optional vector of weights to be used for linear regression, points can be set to 0 for bad data points}
  \item{lag}{numeric vector: optional vector of time lags between respiration and temprature signal}
  \item{gapFilling}{Logical: Choose whether Gap-Filling should be applied}
  \item{doPlot}{Logical: Choose whether Surrogates should be plotted}
}
\details{Function to determine the activation energy ($Ea$ value) and time varying saturation efflux (R$_b$) from a given temperature and efflux (usually respiration) time series. 
The following model was proposed by .....:

Resp(i) = R_b exp(1/(Tref-T0) - 1/(T(i)-T0)),

where $i$ is the time index. It has been shown, however, that this model is misleading when $R_b$ is varying over time which can be expected in many real world examples (e.g. Sampson et al. 2008).

If $R_b$ varies slowly, i.e. with some low frequency then the "scale dependent parameter estimation, SCAPE" 
allows us to identify this oscillatory pattern. As a consequence, the estimation of $E_a$ can be substantially stabilized (Mahecha et al. 2010). The model becomes 

Resp(i) = R_b(i) exp(1/(Tref-T0) - 1/(T(i)-T0)),

where $R_b(i)$ is the time varying "basal respiration", i.e. the respiration expected at $Tref$. The convenience function gettau allows to extract the $E_a$ value minimizing the confounding factor of the time varying $R_b$. Four different spectral methods can be used and compared. A surrogate technique (function by curtsey of Dr. Henning Rust, written in the context of Venema et al. 2006) is applied to propagate the uncertainty due to the decomposition.

The user is strongly encouraged to use the function with caution, i.e. see critique by Graf et al. (2011).}
\value{A list with elements

$SCAPE_Ea : the estimated E_a with the SCAPE principle and the method chosen.
$Conv_Ea : the conventional E_a (assuming constant Rb)
$DAT$SCAPE_R_pred : the SCAPE prediction of respiration 
$DAT$SCAPE_Rb : the basal respiration based on the the SCAPE principle
$DAT$Conv_R_pred : the conventional prediction of respiration 
$DAT$Conv_Rb : the conventional (constant) basal respiration}

\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans, Miguel Mahecha, Max-Planck-Institute for Biogeochemistry, Jena}






\name{evalQ10}
\alias{evalQ10}
\title{Evaluating the SCAPE Q_10 performance}
\description{Function to properly evaluating the model based on the SCAPE estimated sensitivities. 
COMMENT: This function is still experimental}
\usage{evalQ10(SCAPE_res, Rb = NA)}
\arguments{
  \item{SCAPE_res}{list: ouput from a successful run of the getQ10 function}
  \item{Rb}{vector: in case it applies: an Rb time series}
}
\details{Function to properly evaluating the model based on the SCAPE estimated sensitivities. 
A straight forward evaluation of the SCAPE predictions e.g. via the RMSE or other error metrics are 
possible, these could be overoptimistic. The reason is that the time varying basal respiration is 
extracted as part of the original observations. Hence, a model that includes this part of the signal 
is hence comparing a fraction of the signal with itself. The evaluation wrapper uses the output of 
the getQ10 model and performs the evaluation based on the spectrally decomposed signals 
(i.e. in frequency ranges where Rb does not play a direct role), using the same spectral method, 
parameterization, and surrogate setting. Metrics used here are }
\value{List with evaluation metrics, the root mean square error and the modelling efficiency are calculated
<< If nss>0 for evaluation metric will be derived for surrogates, too, as well as for time-lagged results. }

\author{Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
Fabian Gans, Miguel Mahecha, Max-Planck-Institute for Biogeochemistry, Jena}






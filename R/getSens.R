#     R-Code to calculate Q10-value based on SCAPE
#     Copyright (C) 2013  Fabian Gans, Miguel Mahecha
# 
#     This program is free software: you can redistribute it and/or modify
#     it under the terms of the GNU General Public License as published by
#     the Free Software Foundation, either version 3 of the License, or
#     (at your option) any later version.
# 
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#     GNU General Public License for more details.
# 
#     You should have received a copy of the GNU General Public License
#     along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

getSens <-function(
  ##title<< Estimate $Q_{10}$ value and time varying $R_b$ from temperature and efflux time series including uncertainty.
  ##description<< Function to determine the temperature sensitivity ($Q_{10}$ value) and time varying 
  ## basal efflux (R$_b(i)$) from a given temperature and efflux (usually respiration) time series 
  ## according the principle of "SCAle dependent Parameter Estimation, SCAPE" (Mahecha et al. 2010).  
  temperature, ##<< numeric vector: temperature time series
  respiration, ##<< numeric vector: respiration time series
  sf,   ##<< numeric: sampling rate, number of measurements (per day)
  gettau, ##<< numeric: function to transform the exponent in the sensitivity model
  fborder=30, ##<< numeric: boundary for dividing high- and low-frequency parts (in days)
  M=-1, ##<< numeric vector: size of SSA window (in days)
  nss=0, ##<< numeric vector: number of surrogate samples 
  method="Fourier", ##<< String: method to be applied for signal decomposition (choose from "Fourier","SSA","MA","EMD","Spline")
  weights=NULL, ##<< numeric vector: optional vector of weights to be used for linear regression, points can be set to 0 for bad data points
  lag=NULL, ##<< numeric vector: optional vector of time lags between respiration and temprature signal
  gapFilling=TRUE, ##<< Logical: Choose whether Gap-Filling should be applied
  doPlot=FALSE ##<< Logical: Choose whether Surrogates should be plotted
) 
##details<<
##Function to determine the temperature sensitivity ($Q_{10}$ value) and time varying basal efflux (R$_b$) from a given temperature and efflux (usually respiration) time series. 
##Conventionally, the following model is used in the literature:
##
##  Resp(i) = R_b Q_{10}^((T(i)-Tref)/(gamma),
##
##where $i$ is the time index. It has been shown, however, that this model is misleading when $R_b$ is varying over time which can be expected in many real world examples (e.g. Sampson et al. 2008).
##
##If $R_b$ varies slowly, i.e. with some low frequency then the "scale dependent parameter estimation, SCAPE" 
##allows us to identify this oscillatory pattern. As a consequence, the estimation of $Q_{10}$ can be substantially stabilized (Mahecha et al. 2010). The model becomes 
##
##Resp(i) = R_b(i)Q_{10}^((T(i)-Tref)/(gamma),
##
##where $R_b(i)$ is the time varying "basal respiration", i.e. the respiration expected at $Tref$. The convenience function getQ10 allows to extract the $Q_{10}$ value minimizing the confounding factor of the time varying $R_b$. Four different spectral methods can be used and compared. A surrogate technique (function by curtsey of Dr. Henning Rust, written in the context of Venema et al. 2006) is applied to propagate the uncertainty due to the decomposition.
##
##The user is strongly encouraged to use the function with caution, i.e. see critique by Graf et al. (2011).

##author<<
##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
{
  # Check if weights are given
  cat("Checking and preparing data ")
  if (length(weights)==0)      weights=rep(1,length(temperature))
  if ((length(weights)!=length(temperature)) | (length(weights)!=length(respiration))) stop("Error: Input data must have the same length")
  
  DAT               <- data.frame(temperature,respiration,weights)
  
  DAT<-testAndFillMissing(DAT,sf)
  
  if (sd(DAT$temperature)==0 | sd(DAT$respiration)==0) {
    stop("Constant time series not allowed!")
  }
  
  if (mean(DAT$temperature)<150) {
    cat("assuming temperature is given in deg C")
  } else {
    cat("assuming temperature is given in K")
    DAT$temperature<-DAT$temperature-273.15
  }

  
  if (sum(DAT$respiration<=0)>0) {
    warning("Some respiration data values are below 0. Please check your dataset.")
    rold<-DAT$respiration
  }
  # define the weights
  DAT$weights[DAT$respiration <= 0] <- 0
  DAT$respiration[DAT$respiration <= 0] <-  quantile(DAT$respiration[DAT$respiration>0],0.01)                 # make sure there are no nonsense values
  
  # define tau which will be decomposed  
  DAT$tau <- gettau(temperature)
  
  #if (model == "Q10") {
  #  DAT$tau <- (DAT$temperature -Tref)/gam  
  #} else if (model == "Arrhenius") {
  #  R_gas_const = 8.3144621 # units J K−1 mol−1 
  #  DAT$temperature = DAT$temperature + 273.15 # Backtransform to Kelvin
  #  DAT$tau <- -1/DAT$temperature
  #} else if (model == "LloydTaylor") {
  #  Tref = Tref + 273.15
  #  T0 = 227.13
  #  DAT$temperature = DAT$temperature + 273.15 # Backtransform to Kelvin
  #  DAT$tau <- ( 1/(Tref-T0) - 1/(DAT$temperature-T0) )
  #}
  
  # add rho which will be decomposed  
  DAT$rho<-log(DAT$respiration)
  
  if (any(is.na(DAT$rho))) {
    dump.frames("debug_info",to.file=TRUE)
    stop("Error, NA values in rho after file prep! See debug_info for details")
  }
  
  
  output<-list()
  
  output$settings            <-  list()
  output$settings$sf         <-  sf
  output$settings$fborder    <-  fborder
  output$settings$M          <-  M
  output$settings$nss        <-  nss
  output$settings$method     <-  method
  output$settings$lag        <-  lag
  output$settings$gapFilling <-  gapFilling
  cat(" ok\n")
  
  cat("Decomposing datasets")
  #Decompose temperature
  x<-scapedecomp(x=DAT$tau, sf=sf, fborder=fborder, method=method, Ms=M)
  DAT$tau.dec.lf <- x[, 1]
  DAT$tau.dec.hf <- x[, 2]
  
  #Decompose respiration
  x<-scapedecomp(x=DAT$rho, sf=sf, fborder=fborder, method=method, Ms=M)
  DAT$rho.dec.lf <- x[, 1]
  DAT$rho.dec.hf <- x[, 2]
  cat(" ok\n")
  
  calcSensModel <- function(rho, tau, weights, lag) {
    if (lag > 0) {
      l       <- length(rho)
      rho     <- rho[(lag+1):l]
      tau     <- tau[1:(l-lag)]
      weights <- weights[(lag+1):l] * weights[1:(l-lag)]
    } else if (lag < 0) {
      l       <- length(rho)
      rho     <- rho[1:(l+lag)]
      tau     <- tau[(1-lag):l]
      weights <- weights[1:(l+lag)] * weights[(1-lag):l]
    }
    
    lmres <- lm(rho~tau-1, weights=weights)  
    
    return(list(XYZ = lmres$coefficients, Confint=confint(lmres)[2]-confint(lmres)[1]))
  }
  
  #Generate Ensemble of surrogate base-respiration data
  if (nss>0) {
    cat("Generating surrogates")
    sur.rho.hf <- iAAFTSurrogateEnsemble(DAT$rho.dec.hf,nss)
    sur.rho <- array(data=rep(DAT$rho.dec.lf,nss),dim=c(nrow(DAT),nss))+sur.rho.hf+mean(DAT$rho)
    
    sur.tau.hf <- iAAFTSurrogateEnsemble(DAT$tau.dec.hf,nss)
    sur.tau <- array(data=rep(DAT$tau.dec.lf,nss),dim=c(nrow(DAT),nss))+sur.tau.hf+mean(DAT$tau)
    cat(" ok\n")
    
    cat("Decomposing surrogates")
    ens.dec <- aaply(.data=sur.rho,.fun=scapedecomp,.margins=2,sf=sf,fborder=fborder,Ms=M,method=method)
    ens.rho.dec.lf <- t(ens.dec[,,1])
    ens.rho.dec.hf <- array(data=rep(DAT$rho,nss),dim=c(nrow(DAT),nss))-ens.rho.dec.lf
    ens.rho.dec.hf <- apply(ens.rho.dec.hf,2,function(z) z-mean(z))
    
    ens.dec<-aaply(.data=sur.tau,.fun=scapedecomp,.margins=2,sf=sf,fborder=fborder,Ms=M,method=method)
    ens.tau.dec.lf <- t(ens.dec[,,1])
    ens.tau.dec.hf <- array(data=rep(DAT$tau,nss),dim=c(nrow(DAT),nss))-ens.tau.dec.lf
    ens.tau.dec.hf <- apply(ens.tau.dec.hf,2,function(z) z-mean(z))
    cat(" ok\n")
    
    cat("fitting surrogate models")
    output$SCAPE_XYZ_surr <- array(data=0,dim=c(nss,nss))
    output$SCAPE_Rb_surr <- array(data=0,dim=c(nss,nss,nrow(DAT)))
    output$SCAPE_Rpred_surr <- array(data=0,dim=c(nss,nss,nrow(DAT)))
    for (i in 1:nss) {
      for (j in 1:nss) {
        output$SCAPE_XYZ_surr[i,j]    <- calcSensModel(ens.rho.dec.hf[,i],ens.tau.dec.hf[,j],DAT$weights,0)[[1]]
        output$SCAPE_Rb_surr[i,j,]    <- getRb2Sens(tau_lf=ens.tau.dec.lf[,j],rho_lf=ens.rho.dec.lf[,i],tau=sur.tau[,i],rho=sur.rho[,j],S=output$SCAPE_XYZ_surr[i,j])
        output$SCAPE_Rpred_surr[i,j,] <- predictR(Rb=output$SCAPE_Rb_surr[i,j,],S=output$SCAPE_XYZ_surr[i,j],tau=DAT$tau,lag=0)
      }
    }
    output$surrogates<-list()
    output$surrogates$rho.dec.lf <- ens.rho.dec.lf
    output$surrogates$rho.dec.hf <- ens.rho.dec.hf
    output$surrogates$tau.dec.lf <- ens.tau.dec.lf
    output$surrogates$tau.dec.hf <- ens.tau.dec.hf
    cat(" ok\n")
  }
  
  
  cat("Regression of SCAPE and Conventional method")
  # No surrogates but taking confidence interval of linear fit
  lmres_SCAPE <- calcSensModel(DAT$rho.dec.hf,DAT$tau.dec.hf,DAT$weights,0)
  output$SCAPE_XYZ <- unname(lmres_SCAPE[[1]])
  output$SCAPE_XYZ_regression_confint <- unname(lmres_SCAPE[[2]])
  
  # Another comparison, calculate Q10 with linear fit using logarithmic formula
  lmres_Conv <- calcSensModel(DAT$rho-mean(DAT$rho), DAT$tau-mean(DAT$tau), DAT$weights, 0)
  output$Conv_XYZ <- unname(lmres_Conv[[1]])
  output$Conv_Rb <- unname(exp(mean(DAT$rho)-lmres_Conv[[1]]*mean(DAT$tau)))
  output$Conv_XYZ_regression_confint <- unname(lmres_Conv[[2]])
  cat(" ok\n")
  
  # Time lagged linear fits
  if (length(lag>0)) {
    cat("Calculating time-lagged results")
    output$lag_results     <- list()
    output$lag_results$S <- array(NA,dim=c(length(lag),4),list(Lag=as.character(lag),Value=c("SCAPE_XYZ","+/-","Conv_XYZ","+/-")))
    ilag                      		   <- 1
    for (tl in lag) {
      lmres_SCAPE                      <- calcSensModel(DAT$rho.dec.hf,DAT$tau.dec.hf,DAT$weights,tl)
      lmres_Conv                       <- calcSensModel(DAT$rho,DAT$tau,DAT$weights,tl)
      output$lag_results$S[ilag,]    <- c(lmres_SCAPE[[1]],lmres_SCAPE[[2]]/2,lmres_Conv[[1]],lmres_Conv[[2]]/2)
      ilag<-ilag+1
    }
    if (nss>0) {
      output$lag_results$surrogate_XYZ <- array(NA,dim=c(length(lag),nss,nss),list(Lag=as.character(lag),sur_rho=as.character(1:nss),sur_tau=as.character(1:nss)))
      for (i in 1:nss) {
        for (j in 1:nss) {
          ilag                      <- 1
          for (tl in lag) {
            lmres_SCAPE                      <- calcSensModel(ens.rho.dec.hf[,i],ens.tau.dec.hf[,j],DAT$weights,tl)
            output$lag_results$surrogate_XYZ[ilag,i,j] <- c(lmres_SCAPE[[1]])
            ilag<-ilag+1
          }
        } 
      }
    }
    cat(" ok\n")
  }
  
  
  # For comparison estimate Q10 by nonlinear model without decomposition
  #try( {
  #  nlmres<-nls(respiration~rb*Q10^((temperature-Tref)/gam),data=DAT,start=list(rb=1,Q10=2),weights=DAT$weights)
  #  output$Q10_simple<-coef(nlmres)[2]
  #  names(output$Q10_simple)<-"Q10"
  #  DAT$respiration_pred_simple<-predict(nlmres)
  #  #output$MEF_simple<-MEFW(DAT$respiration_pred_simple,DAT$respiration,w=DAT$weights)
  #})
  
  cat("Reconstructing Rb")
  output$SCAPE_Rb  <- getRb2Sens(tau_lf=DAT$tau.dec.lf,rho_lf=DAT$rho.dec.lf,tau=DAT$tau,rho=DAT$rho,S=output$SCAPE_XYZ)
  DAT$SCAPE_R_pred <- predictR(Rb=output$SCAPE_Rb,S=output$SCAPE_XYZ,tau=DAT$tau,lag=0)
  DAT$Conv_R_pred  <- predictR(Rb=output$Conv_Rb,S=output$Conv_XYZ,tau=DAT$tau,lag=0)
  #output$MEF<-MEFW(DAT$respiration_pred,DAT$respiration,w=DAT$weights)
  cat(" ok\n")
  output$DAT<-DAT
  if (doPlot) {
    if (nss==0) warning("No ensemble plot possible, because number of surrogates is set to 0")
    else plotensembles(output)
  }
  

  
  #if (model=="Arrhenius") output=subnames(output,"Q10","Ea")
  #if (model=="LloydTaylor") output=subnames(output,"Q10","Ea")

  ##value<< 
  ##A list with elements
  ##
  ##$SCAPE_Q10 : the estimated Q_{10} with the SCAPE principle and the method chosen.
  ##$Conv_Q10 : the conventional Q_{10} (assuming constant Rb)
  ##$DAT$SCAPE_R_pred : the SCAPE prediction of respiration 
  ##$DAT$SCAPE_Rb : the basal respiration based on the the SCAPE principle
  ##$DAT$Conv_R_pred : the conventional prediction of respiration 
  ##$DAT$Conv_Rb : the conventional (constant) basal respiration
  
  return(output)
}

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

getQ10 <-function(
  ##title<< Estimate $Q_{10}$ value and time varying $R_b$ from temperature and efflux time series including uncertainty.
  ##description<< Function to determine the temperature sensitivity ($Q_{10}$ value) and time varying 
  ## basal efflux (R$_b(i)$) from a given temperature and efflux (usually respiration) time series 
  ## according the principle of “SCAle dependent Parameter Estimation, SCAPE” (Mahecha et al. 2010).  
  temperature, ##<< numeric vector: temperature time series
  respiration, ##<< numeric vector: respiration time series
  sf,   ##<< numeric: sampling rate, number of measurements (per day)
  Tref=15, ##<< numeric: reference temperature in Q10 equation
  gam=10, ##<< numeric: gamma value in Q10 equation
  fborder=30, ##<< numeric: boundary for dividing high- and low-frequency parts (in days)
  M=-1, ##<< numeric vector: size of SSA window (in days)
  nss=0, ##<< numeric vector: number of surrogate samples 
  method="SSA", ##<< String: method to be applied for signal decomposition (choose from "Fourier","SSA","MA","EMD","Spline")
  gapFilling=TRUE, ##<< Logical: Choose whether Gap-Filling should be applied
  plot=FALSE, ##<< Logical: Choose whether Surrogates should be plotted
  flag = array(1, length = length(respiration))
) 
##details<<
##Function to determine the temperature sensitivity ($Q_{10}$ value) and time varying basal efflux (R$_b$) from a given temperature and efflux (usually respiration) time series. 
##Conventionally, the following model is used in the literature:
##
##  Resp(i) = R_b Q_{10}^((T(i)-Tref)/(gamma),
##
##where $i$ is the time index. It has been shown, however, that this model is misleading when $R_b$ is varying over time which can be expected in many real world examples (e.g. Sampson et al. 2008).
##
##If $R_b$ varies slowly, i.e. with some low frequency then the “scale dependent parameter estimation, SCAPE” 
##allows us to identify this oscillatory pattern. As a consequence, the estimation of $Q_{10}$ can be substantially stabilized (Mahecha et al. 2010). The model becomes 
##
##Resp(i) = R_b(i)Q_{10}^((T(i)-Tref)/(gamma),
##
##where $R_b(i)$ is the time varying “basal respiration”, i.e. the respiration expected at $Tref$. The convenience function getQ10 allows to extract the $Q_{10}$ value minimizing the confounding factor of the time varying $R_b$. Four different spectral methods can be used and compared. A surrogate technique (function by curtsey of Dr. Henning Rust, written in the context of Venema et al. 2006) is applied to propagate the uncertainty due to the decomposition.
##
##The user is strongly encouraged to use the function with caution, i.e. see critique by Graf et al. (2011).

##author<<
##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
{  
  DAT               <- data.frame(temperature,respiration)
  
  DAT<-testAndFillMissing(DAT,sf)
  
  if (sd(DAT$temperature)==0 | sd(DAT$respiration)==0) {
    stop("Constant time series not allowed!")
  }
  
  if (mean(DAT$temperature)<150) {
    print("Assuming temperature is given in deg C")
  } else {
    print("Assuming temperature is given in K")
    DAT$temperature<-DAT$temperature-273.15
  }
  
  if (sum(DAT$respiration<0)>0) {
    warning("Some respiration data values are below 0. Please check your dataset.")
  }
  # define the weights
  DAT$weights[DAT$respiration <= 0] <- 0
  DAT$weights[flag != 1] <- 0
  DAT$respiration[DAT$respiration <= 0] <-  quantile(DAT$respiration[DAT$respiratio>0],0.01)                 # make sure there are no nonsense values
  
  
  DAT$tau<-(DAT$temperature -Tref)/gam  # Define tau and rho which are decomposed
  DAT$rho<-log(DAT$respiration)
  
  output<-list()
  
  output$settings            <-  list()
  output$settings$sf         <-  sf
  output$settings$Tref       <-  Tref
  output$settings$gam        <-  gam
  output$settings$fborder    <-  fborder
  output$settings$M          <-  M
  output$settings$nss        <-  nss
  output$settings$method     <-  method
  output$settings$gapFilling <-  gapFilling
  
  #Decompose temperature
  x<-scapedecomp(x=DAT$tau,sf=sf,fborder=fborder,method=method,Ms=M)
  DAT$tau.dec.lf<-x[,1]
  DAT$tau.dec.hf<-x[,2]
  
  #Decompose respiration
  x<-scapedecomp(x=DAT$rho,sf=sf,fborder=fborder,method=method,Ms=M)
  DAT$rho.dec.lf<-x[,1]
  DAT$rho.dec.hf<-x[,2]
  
  #Define function to calculate Rb
  getRb<-function(tau_lf,rho_lf,rho,tau,Q10) {
    rho_lf_tau<-(tau_lf+mean(tau))*log(Q10)
    return(exp(rho_lf+mean(rho)-rho_lf_tau))
  }
  
  
  #Generate Ensemble of surrogate base-respiration data
  if (nss>0) {
    sur.rho.hf <- iAAFTSurrogateEnsemble(DAT$rho.dec.hf,nss)
    sur.rho <- array(data=rep(DAT$rho.dec.lf,nss),dim=c(nrow(DAT),nss))+sur.rho.hf+mean(DAT$rho)
    
    sur.tau.hf <- iAAFTSurrogateEnsemble(DAT$tau.dec.hf,nss)
    sur.tau <- array(data=rep(DAT$tau.dec.lf,nss),dim=c(nrow(DAT),nss))+sur.tau.hf+mean(DAT$tau)
    
    ens.dec <- aaply(.data=sur.rho,.fun=scapedecomp,.margins=2,sf=sf,fborder=fborder,Ms=M,method=method)
    ens.rho.dec.lf <- t(ens.dec[,,1])
    ens.rho.dec.hf <- array(data=rep(DAT$rho,nss),dim=c(nrow(DAT),nss))-ens.rho.dec.lf
    ens.rho.dec.hf <- apply(ens.rho.dec.hf,2,function(z) z-mean(z))
    
    ens.dec<-aaply(.data=sur.tau,.fun=scapedecomp,.margins=2,sf=sf,fborder=fborder,Ms=M,method=method)
    ens.tau.dec.lf <- t(ens.dec[,,1])
    ens.tau.dec.hf <- array(data=rep(DAT$tau,nss),dim=c(nrow(DAT),nss))-ens.tau.dec.lf
    ens.tau.dec.hf <- apply(ens.tau.dec.hf,2,function(z) z-mean(z))
    
    
    output$SCAPE_Q10_surr <- array(data=0,dim=c(nss,nss))
    output$SCAPE_Rb_surr <- array(data=0,dim=c(nss,nss,nrow(DAT)))
    output$SCAPE_Rpred_surr <- array(data=0,dim=c(nss,nss,nrow(DAT)))
    for (i in 1:nss) {
      for (j in 1:nss) {
        output$SCAPE_Q10_surr[i,j]    <- exp(lm(ens.rho.dec.hf[,i]~ens.tau.dec.hf[,j],weights=DAT$weights)$coefficients[2])
        output$SCAPE_Rb_surr[i,j,]    <- getRb(ens.tau.dec.lf[,j],ens.rho.dec.lf[,i],sur.rho[,i],sur.tau[,j],output$SCAPE_Q10_surr[i,j])
        output$SCAPE_Rpred_surr[i,j,] <- output$SCAPE_Rb_surr[i,j,]*output$SCAPE_Q10_surr[i,j]^((DAT$temperature-Tref)/gam)
      }
    }
    output$surrogates<-list()
    output$surrogates$rho.dec.lf <- ens.rho.dec.lf
    output$surrogates$rho.dec.hf <- ens.rho.dec.hf
    output$surrogates$tau.dec.lf <- ens.tau.dec.lf
    output$surrogates$tau.dec.hf <- ens.tau.dec.hf
  }
  
  
  # No surrogates but taking confidence interval of linear fit
  lmres <- lm(DAT$rho.dec.hf~DAT$tau.dec.hf,weights=DAT$weights)
  output$SCAPE_Q10 <- exp(lmres$coefficients[2])
  names(output$SCAPE_Q10) <- "Q10"
  output$SCAPE_Q10_regression_confint <- exp(confint(lmres)[2,])
  
  # Another comparison, calculate Q10 with linear fit using logarithmic formula
  lmres2 <- lm(DAT$rho~DAT$tau,weights=DAT$weights)
  output$Conv_Q10 <- exp(lmres2$coefficients[2])
  output$Conv_Rb <- rep(exp(lmres2$coefficients[1]),nrow(DAT))
  names(output$Conv_Q10) <- "Q10"
  output$Conv_Q10_regression_confint <- exp(confint(lmres2)[2,])
  
  # For comparison estimate Q10 by nonlinear model without decomposition
  #try( {
  #  nlmres<-nls(respiration~rb*Q10^((temperature-Tref)/gam),data=DAT,start=list(rb=1,Q10=2),weights=DAT$weights)
  #  output$Q10_simple<-coef(nlmres)[2]
  #  names(output$Q10_simple)<-"Q10"
  #  DAT$respiration_pred_simple<-predict(nlmres)
  #  #output$MEF_simple<-MEFW(DAT$respiration_pred_simple,DAT$respiration,w=DAT$weights)
  #})
  
  
  output$SCAPE_Rb  <- getRb(DAT$tau.dec.lf,DAT$rho.dec.lf,DAT$rho,DAT$tau,output$SCAPE_Q10)
  DAT$SCAPE_R_pred <- output$SCAPE_Rb*output$SCAPE_Q10^((DAT$temperature-Tref)/gam)
  DAT$Conv_R_pred  <- output$Conv_Rb*output$Conv_Q10^((DAT$temperature-Tref)/gam)
  #output$MEF<-MEFW(DAT$respiration_pred,DAT$respiration,w=DAT$weights)
  
  output$DAT<-DAT
  if (plot) {
    if (nss==0) warning("No ensemble plot possible, because number of surrogates is set to 0")
    else plotensembles(output)
  }
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

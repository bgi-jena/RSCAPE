#     R-Code to calculate Q10-value based on SCAPE
#     Copyright (C) 2013  Fabian Gans
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

getQ10<-function(
  ##title<< determine Q10 value of a respiration and temperature time series
  ##description<< Function to determine the Q10-value based on a given rtemeprature and respiration time series  
  temperature, ##<< numeric vector: temperature time series
  respiration, ##<< numeric vector: respiration time series
  sf,   ##<< numeric: sampling rate, number of measurements per day
  gam=10, ##<< numeric: gamma value in Q10 equation
  fborder=30, ##<< numeric: boundary for dividing high- and low-frequency parts in days
  M=c(90,30), ##<< numeric vector: size of SSA window in days
  nss=10, ##<< numeric vector: number of surrogate samples
  method="Fourier", ##<< String: method to be applied for signal decomposition (choose from "Fourier","SSA","MA","EMD")
  gapFilling=TRUE ##<< Logical: Choose whether Gap-Filling should be applied
) 
##details<<
##  This function ....

##author<<
##Fabian Gans, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de
{  
  DAT               <- data.frame(temperature,respiration)
  
  DAT<-testAndFillMissing(DAT)
  
  if (mean(DAT$temperature)<150) {
    print("Assuming temperature is given in deg C")
  } else {
    print("Assuming temperature is given in K")
    DAT$temperature<-DAT$temperature-273.15
  }
  
  if (sum(DAT$respiration<0)>0) {
    warning("Some respiration data values are below 0. Please check your dataset.")
  }
  DAT$respiration[DAT$respiration <= 0] <- 1e-10                  # make sure there are no nonsense values
  
  
  DAT$tao<-(DAT$temperature - 15)/gam  # Define tao and rho which are decomposed
  DAT$rho<-log(DAT$respiration)

  output<-list()
  
  
  #Decompose temperature
  x<-scapedecomp(x=DAT$tao,sf=sf,fborder=fborder,method=method,Ms=M)
  DAT$tao.dec.lf<-x[,1]
  DAT$tao.dec.hf<-x[,2]
  
  #Decompose respiration
  x<-scapedecomp(DAT$rho,gam,sf,fborder,Ms=M)
  DAT$rho.dec.lf<-x[,1]
  DAT$rho.dec.hf<-x[,2]
  
  #Generate Ensemble of surrogate base-respiration data
  sur.rho.hf<-iAAFTSurrogateEnsemble(DAT$rho.dec.hf,nss)
  sur.rho<-array(data=rep(DAT$rho.dec.lf,nss),dim=c(nrow(DAT),nss))+sur.rho.hf+mean(DAT$rho)
  
  sur.tao.hf<-iAAFTSurrogateEnsemble(DAT$tao.dec.hf,nss)
  sur.tao<-array(data=rep(DAT$tao.dec.lf,nss),dim=c(nrow(DAT),nss))+sur.tao.hf+mean(DAT$tao)
  
  ens.dec<-aaply(.data=sur.rho,.fun=scapedecomp,.margins=2,gam=gam,sf=sf,fborder=fborder,Ms=M)
  ens.rho.dec.lf<-t(ens.dec[,,1])
  ens.rho.dec.hf<-array(data=rep(DAT$rho,nss),dim=c(nrow(DAT),nss))-ens.rho.dec.lf
  ens.rho.dec.hf <- apply(ens.rho.dec.hf,2,function(z) z-mean(z))
  
  ens.dec<-aaply(.data=sur.tao,.fun=scapedecomp,.margins=2,gam=gam,sf=sf,fborder=fborder,Ms=M)
  ens.tao.dec.lf<-t(ens.dec[,,1])
  ens.tao.dec.hf<-array(data=rep(DAT$tao,nss),dim=c(nrow(DAT),nss))-ens.tao.dec.lf
  ens.tao.dec.hf <- apply(ens.tao.dec.hf,2,function(z) z-mean(z))
  
  output$ens.Q10<-array(data=0,dim=c(nss,nss))
  for (i in 1:nss) {
    for (j in 1:nss) {
      output$ens.Q10[i,j]<-exp(lm(ens.rho.dec.hf[,i]~ens.tao.dec.hf[,j],weights=DAT$weights)$coefficients[2])
    }
  }
  output$ensembles<-list()
  output$ensembles$rho.dec.lf<-ens.rho.dec.lf
  output$ensembles$rho.dec.hf<-ens.rho.dec.hf
  output$ensembles$tao.dec.lf<-ens.tao.dec.lf
  output$ensembles$tao.dec.hf<-ens.tao.dec.hf
  
  output$ensemble_sd<-sd(as.vector(output$ens.Q10))
  
  
  #DAT_log <- data.frame(tao = decomposed_ts[,4], rho = decomposed_ts[,2])        
  #Q10_log.boot <- boot(na.omit(DAT_log), statistic = Q10.optim_log, R = 100)
  #param_final <-  colMeans(Q10_log.boot$t)
  #param_final_std <-  apply(Q10_log.boot$t, 2, sd)
  #output$Q10_est<-exp(param_final[2])
  #output$Q10_ensemble<-exp(Q10_log.boot$t[,2])
  
  # Avoiding bootstrap but taking confidence interval of linear fit
  lmres<-lm(DAT$rho.dec.hf~DAT$tao.dec.hf,weights=DAT$weights)
  output$Q10_est<-exp(lmres$coefficients[2])
  names(output$Q10_est)<-"Q10"
  output$Q10_95_confint<-exp(confint(lmres)[2,])
  
  # Another comparison, claculate Q10 with linear fit using logarithmic formula
  lmres2<-lm(DAT$rho~DAT$tao,weights=DAT$weights)
  output$Q10_simple_log<-exp(lmres2$coefficients[2])
  names(output$Q10_simple_log)<-"Q10"
  
  # For comparison estimate Q10 by nonlinear model without decomposition
  try( {
    nlmres<-nls(respiration~rb*Q10^((temperature-15)/gam),data=DAT,start=list(rb=1,Q10=2),weights=DAT$weights)
    output$Q10_simple<-coef(nlmres)[2]
    names(output$Q10_simple)<-"Q10"
    DAT$respiration_pred_simple<-predict(nlmres)
    output$MEF_simple<-MEFW(DAT$respiration_pred_simple,DAT$respiration,w=DAT$weights)
  })
  
  rho_lf_tao<-(DAT$tao.dec.lf+mean((DAT$temperature-15)/gam))*lmres$coefficients[2]
  
  rho_lf<-DAT$rho.dec.lf
  
  rho_orig<-log(DAT$respiration)
  
  DAT$respiration_base_est<-exp(rho_lf+mean(rho_orig)-rho_lf_tao)
  DAT$respiration_pred<-DAT$respiration_base_est*output$Q10_est^((DAT$temperature-15)/gam)
  output$MEF<-MEFW(DAT$respiration_pred,DAT$respiration,w=DAT$weights)
  
  output$DAT<-DAT
  ##value<< Data frame containing decomposed time series, Q10 values etc
  return(output)
}

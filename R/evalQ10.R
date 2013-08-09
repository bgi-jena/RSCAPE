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

evalQ10<-function(
  ##title<< Evaluating the SCAPE Q_10 performance
  ##description<< Function to properly evaluating the model based on the SCAPE estimated sensitivities. 
  ## COMMENT: This function is still experimental
  SCAPE_res ##<< list: ouput from a successful run of the getQ10 function
) 
##details<<
##  Function to properly evaluating the model based on the SCAPE estimated sensitivities. 
##  A straight forward evaluation of the SCAPE predictions e.g. via the RMSE or other error metrics are 
##  possible, these could be overoptimistic. The reason is that the time varying basal respiration is 
##  extracted as part of the original observations. Hence, a model that includes this part of the signal 
##  is hence comparing a fraction of the signal with itself. The evaluation wrapper uses the output of 
##  the getQ10 model and performs the evaluation based on the spectrally decomposed signals 
##  (i.e. in frequency ranges where Rb does not play a direct role), using the same spectral method, 
##  parameterization, and surrogate setting. Metrics used here are …

##author<<
##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
{ 
  #First collect original settings
  
  SCAPE_res$settings$sf         ->  sf
  SCAPE_res$settings$Tref       ->  Tref
  SCAPE_res$settings$gam        ->  gam
  SCAPE_res$settings$fborder    ->  fborder
  SCAPE_res$settings$M          ->  M
  SCAPE_res$settings$nss        ->  nss
  SCAPE_res$settings$method     ->  method
  SCAPE_res$settings$gapFilling ->  gapFilling
  
  rho_pred<-log(SCAPE_res$DAT$SCAPE_R_pred) #Define rho from SCAPE prediction
  rho_pred_conv<-log(SCAPE_res$DAT$Conv_R_pred) #Define rho from SCAPE prediction
  rho<-log(SCAPE_res$DAT$respiration) #Define rho from SCAPE prediction
  # Then do spectral decomposition of the prediction
  
  #Decompose original resp
  x<-scapedecomp(x=rho,sf=sf,fborder=fborder,method=method,Ms=M)
  rho_hf<-x[,2]
  #Decompoes SCAPE predicted respiration
  x<-scapedecomp(x=rho_pred,sf=sf,fborder=fborder,method=method,Ms=M)
  rho_pred_hf<-x[,2]
  #Decompoes SCAPE conventional predicted respiration
  x<-scapedecomp(x=rho_pred_conv,sf=sf,fborder=fborder,method=method,Ms=M)
  rho_pred_conv_hf<-x[,2]
  
  
  
  sq   <-function(x) return(x*x)
  rmse <-function(x1,x2) return(sqrt(mean(sq(x1-x2))))
  r2   <-function(y,m) return(1-sum(sq(y-m))/sum(sq(y-mean(y))))
  
  results               <- list()
  results$SCAPE         <- list()
  results$Conv          <- list()
  results$SCAPE$RMSE    <- rmse(rho_hf,rho_pred_hf)
  results$SCAPE$R2      <- r2(rho_hf,rho_pred_hf)
  results$Conv$RMSE     <- rmse(rho_hf,rho_pred_conv_hf)
  results$Conv$R2      <- r2(rho_hf,rho_pred_conv_hf)
  
  
  ##value<< List with evaluation metrics
  return(results)
}

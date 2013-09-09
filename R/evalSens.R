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

evalSens <-function(
  ##title<< Evaluating the SCAPE Q_10 performance
  ##description<< Function to properly evaluating the model based on the SCAPE estimated sensitivities. 
  ## COMMENT: This function is still experimental
  SCAPE_res, ##<< list: ouput from a successful run of the getQ10 function
  Rb = NA ##<<vector: in case it applies: an Rb time series
) 
  ##details<<
  ##  Function to properly evaluating the model based on the SCAPE estimated sensitivities. 
  ##  A straight forward evaluation of the SCAPE predictions e.g. via the RMSE or other error metrics are 
  ##  possible, these could be overoptimistic. The reason is that the time varying basal respiration is 
  ##  extracted as part of the original observations. Hence, a model that includes this part of the signal 
  ##  is hence comparing a fraction of the signal with itself. The evaluation wrapper uses the output of 
  ##  the getQ10 model and performs the evaluation based on the spectrally decomposed signals 
  ##  (i.e. in frequency ranges where Rb does not play a direct role), using the same spectral method, 
  ##  parameterization, and surrogate setting. Metrics used here are 
  
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
  SCAPE_res$settings$lag        ->  lag
  SCAPE_res$settings$model      ->  model
  SCAPE_res$settings$gapFilling ->  gapFilling
  SCAPE_res$settings$invGetSensPar ->  invS
    
  rho_pred <- log(SCAPE_res$DAT$SCAPE_R_pred) #Define rho from SCAPE prediction
  rho_pred_conv <- log(SCAPE_res$DAT$Conv_R_pred) #Define rho from SCAPE prediction
  rho <- log(SCAPE_res$DAT$respiration) #Define rho from SCAPE prediction
  # Then do spectral decomposition of the prediction
  
  l<-length(rho)
  
  
  # Decompose original resp
  x <- scapedecomp(x = SCAPE_res$DAT$respiration,sf=sf,fborder=fborder,method=method,Ms=M)
  resp_hf <- x[, 2]
  # Decompoes SCAPE predicted respiration
  x <- scapedecomp(x = SCAPE_res$DAT$SCAPE_R_pred,sf=sf,fborder=fborder,method=method,Ms=M)
  resp_pred_hf <- x[, 2]
  # Decompose SCAPE conventional predicted respiration
  x <- scapedecomp(x = SCAPE_res$DAT$Conv_R_pred,sf=sf,fborder=fborder,method=method,Ms=M)
  resp_pred_conv_hf <- x[,2]
  
  if (nss > 0) {
    resp_pred_sur_hf <- aaply(.data=SCAPE_res$SCAPE_Rpred_surr,.fun=scapedecomp,.margins=c(1,2),sf=sf,fborder=fborder,Ms=M,method=method)[,,,2]
  }
  
  
  sq   <- function(x) return(x*x)
  rmse <- function(x1,x2,w) return(sqrt(weighted.mean(sq(x1-x2),w=w,na.rm=TRUE)))
  mef  <- function(x1,x2,w) return(1-sum(w*sq(x1-x2),na.rm=TRUE)/sum(w*sq(x1-weighted.mean(x1,w=w,na.rm=TRUE)),na.rm=TRUE))
    
  w    <- SCAPE_res$DAT$weights/sum(SCAPE_res$DAT$weights,na.rm=TRUE)
  
  results               <- list()
  results$Conv          <- list()
  results$SCAPE         <- list()
  
  results$Conv$RMSE     <- rmse(resp_hf, resp_pred_conv_hf, w)
  results$Conv$MEF      <- mef(resp_hf, resp_pred_conv_hf, w)

  results$SCAPE$RMSE    <- rmse(resp_hf, resp_pred_hf, w)
  results$SCAPE$MEF     <- mef(resp_hf, resp_pred_hf, w)
  
  if (length(lag)>0) {
    results$lag        <- list()
    results$lag$RMSE   <- vector(mode='numeric',length=length(lag))
    results$lag$MEF    <- vector(mode='numeric',length=length(lag))
    names(results$lag$RMSE) <- as.character(lag)
    names(results$lag$MEF)  <- as.character(lag)
    
    ilag<-1
    for (tl in lag) {
      rb                 <- getRb2Sens(tau_lf=SCAPE_res$DAT$tau.dec.lf,rho_lf=SCAPE_res$DAT$rho.dec.lf,
                         tau=SCAPE_res$DAT$tau,rho=SCAPE_res$DAT$rho,S=invS(SCAPE_res$lag_results[[1]][ilag,1]))
      pred               <- predictR(Rb=rb,S=invS(SCAPE_res$lag_results[[1]][ilag,1]),tau=SCAPE_res$DAT$tau,lag=tl)
      nalist             <- is.na(pred)
      x                  <- scapedecomp(x = pred[!nalist],sf=sf,fborder=fborder,method=method,Ms=M)
      resp_pred_lag_hf   <- x[, 2]
      results$lag$RMSE[ilag] <- rmse(resp_hf[!nalist], resp_pred_lag_hf, w[!nalist])
      results$lag$MEF[ilag]  <- mef(resp_hf[!nalist], resp_pred_lag_hf, w[!nalist])
      ilag <- ilag+1
    }
    
  }
  
  if (nss > 0) {
    results$surrogates      <- list()
    results$surrogates$RMSE <- array(NA, c(nss, nss))
    results$surrogates$MEF  <- array(NA, c(nss, nss))
    for (i in 1:nss) {
      for (j in 1:nss) {
        results$surrogates$RMSE[i, j] <- rmse(resp_hf, resp_pred_sur_hf[i,j,], w)
        results$surrogates$MEF[i, j]  <- mef(resp_hf, resp_pred_sur_hf[i,j,], w)
      }
    }
  }
  
  # Rb evaluation
  if (!any(is.na(Rb))) {
    
    results$Conv$Rb = list()
    results$Conv$Rb$RMSE = rmse(Rb, SCAPE_res$Conv_Rb,w)
    results$Conv$Rb$MEF = mef(Rb, SCAPE_res$Conv_Rb,w)
    
    results$SCAPE$Rb = list()
    results$SCAPE$Rb$RMSE = rmse(Rb, SCAPE_res$SCAPE_Rb,w)
    results$SCAPE$Rb$MEF = mef(Rb,  SCAPE_res$SCAPE_Rb,w)
    
    if (nss > 0) {
      results$surrogates$Rb = list()
      results$surrogates$Rb$RMSE = array(NA, c(nss, nss))
      results$surrogates$Rb$MEF = array(NA, c(nss, nss))
      for (i in 1:nss) {
        for (j in 1:nss) {
          results$surrogates$Rb$RMSE[i, j] = rmse(Rb, as.vector(SCAPE_res$SCAPE_Rb_surr[i, j, ]),w)
          results$surrogates$Rb$MEF[i, j] = mef(Rb, as.vector(SCAPE_res$SCAPE_Rb_surr[i, j, ]),w)
        }
      }
    }
  }
  
  ##value<< List with evaluation metrics, the root mean square error and the modelling efficiency are calculated
  ## << If nss>0 for evaluation metric will be derived for surrogates, too, as well as for time-lagged results. 
  return(results)
}

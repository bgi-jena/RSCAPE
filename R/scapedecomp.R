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
scapedecomp=function(
  ##title<< Decompose a signal into given frequency bands 
  ##description<< Decompose a signal into given frequency bands using SSA, EMD, Fourier or MA-Filter   
  x, ##<< numeric vector: time series
  sf, ##<< numeric: sampling frequency (samples per day)
  fborder, ##<< numeric: boundary time scale (in days)
  Ms=90, ##<< vector: SSA window length (only applicable for SSA)
  method="Fourier" ##<< String: decomposition methodm can be "SSA", "EMD", "Fourier", "Spline", "MA", "wavMODWT
) {
  ##details<<
  ## This function decomposes the time series into a low frequency and a high frequency component while making 
  ## sure that both components added together result agasin in the original signal. 
  
  ##author<<
  ##Fabian Gans, Miguel D. Mahecha, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de mmahecha@bgc-jena.mpg.de
  l <-length(x)
  
  #Make sure fborder is kind of sensible
  if (fborder*sf>l/3) {
    warning("Smoothing time window is larger than a third of time series length, reset to max value")
    fborder=floor(l/3/sf)
  } else if (fborder*sf<2) {
    warning("Smoothing time window is smaller than 2 time steps, reset to min value")
    fborder=ceiling(2/sf)
  }

  
  # frequencies to extract
  borders.wl      <- data.frame(s0 = c(fborder, Inf), # annual freq
                                s1 = c(0, fborder))*sf    # rest
  
  if (method=="SSA") {
#    library("spectral.methods")
    
    # number of components for reconstruction
    n.comp          <- c(100, 100)
    
    # number of harmonics (to be more precise with respect to extracted modes)
    harmonics       <- c(2, 0)
    
    # not sure
    choose.highest  <- c(TRUE, FALSE)
    
    # threshold for neighbours
    find.neighbours <- c(0.2, 0)
    
    # a priori definition of variance threshold
    var.thresh.ratio<- 0.05
    
    # corresponding embedding dimensions
    if (Ms==-1) M=l/2.5
    M              <- c(floor(min(Ms[1]*sf,l/3)), floor(Ms[1]*sf/3))
    # decompose the time series    
    dat.dec    <- filterTSeriesSSA(x,
                                   borders.wl       = borders.wl,
                                   var.thresh.ratio = var.thresh.ratio,
                                   choose.highest   = choose.highest,
                                   M                = M,
                                   n.comp           = floor(M/2.1),
                                   harmonics        = harmonics,
                                   find.neighbours  = find.neighbours,
                                   recstr.type      = 'substraction',
                                   plot.spectra     = FALSE,
                                   second.axis      = FALSE,
                                   open.plot        = FALSE
    )
    
    
    dat.dec<-t(dat.dec$dec.series)
    x <- apply(dat.dec, 2, function(z) z-mean(z))
    
	
  } else if (method=="EMD") {
    y       <- emd(x,boundary="none",sm="none",max.imf=20)
    dat.dec <- matrix(0,nrow=l,ncol=ncol(borders.wl))
    if (y$nimf==0) {
      dat.dec[,1]<-x
      dat.dec[,2]<-0
      warning("Could not extract high frequency signal. Sensitivity will not make sense!")
      return(apply(dat.dec, 2, function(z) z-mean(z)))
    }
    freqs   <- vector(mode="numeric",length=y$nimf)
    for (i in 1:y$nimf) {
      freqs[i] <- 1/calcFrequency(y$imf[,i])
    }
    colind<-(freqs<fborder) 
    if (sum(colind)>1) {
      dat.dec[,2]<-rowSums(y$imf[,colind])
    } else if (sum(colind)==1) {
        dat.dec[,2]<-y$imf[,colind]
    } else {
      warning("Caution, EMD could not extract low frequency signal, please choose a lower frequency boundary!")
      dat.dec[,2]<-rep(0,l)
    }
    dat.dec[,1]<-x-dat.dec[,2]
    x <- apply(dat.dec, 2, function(z) z-mean(z))
    
	
  } else if (method=="MA") {
    x<-x-mean(x)
    dat.dec<-matrix(0,nrow=l,ncol=ncol(borders.wl))
    fx<-ma(x,fborder*sf)
    #Fill edges
    for (i in 1:(fborder*sf/2)) {
      fx[i]<-mean(x[1:(i+fborder*sf/2)])
      fx[l+1-i]<-mean(x[(l+1-i-fborder*sf/2):l])
    }
    attributes(fx)<-NULL
    dat.dec[,1]<-fx
    dat.dec[,2]<-x-dat.dec[,1]
    x <- apply(dat.dec,2,function(z) z-mean(z))
  
  
  } else if (method=="Fourier") {
    x<-x-mean(x)
    ffx<-fft(x)
    nborder<-floor(l/fborder/sf)
    ffx[(nborder+2):(l-nborder)]<-0
    dat.dec<-matrix(0,nrow=l,2)
    dat.dec[,1]<-Re(fft(ffx,inverse=TRUE))/l
    dat.dec[,2]<-x-dat.dec[,1]
    x <- apply(dat.dec,2,function(z) z-mean(z))
  
  
  } else if (method=="Spline") {
    x<-x-mean(x)
    dat.dec <- matrix(0,nrow=l,2)
    nk<-ceiling(l/fborder/sf)
    dat.dec[,1] <- smooth.spline(x,nknots=nk)$y
    dat.dec[,2] <- x-dat.dec[,1]
    x <- apply(dat.dec,2, function(z) z-mean(z))
	
	
  } else if (method == "wavMODWT") {
    x <- x - mean(x)
	y <- wavShift(wavMODWT(x, wavelet = "s8", n.levels=6, keep.series=TRUE))
	freqs   <- vector(mode="numeric", length = (length(y$data)))
    dat.dec <- matrix(0, nrow = l, ncol = ncol(borders.wl))
	decomp.wavMODWT <- array(NA, c(l, length(y$data)))
    for (i in 1:length(y$data)) {
      decomp.wavMODWT[, i] <- unlist(y$data[i])
    } 
    for (i in 1:length(y$data)) {
      freqs[i] <- 1/calcFrequency(decomp.wavMODWT[, i])
    }
    colind <- (freqs < fborder) 
    if (sum(!colind) > 1) {
      dat.dec[, 1] <- rowSums(decomp.wavMODWT[, !colind])
    } else if (sum(!colind)==1) {
        dat.dec[, 1] <- decomp.wavMODWT[, !colind]
    } else {
      warning("Caution, discrete wavelet transform did not extract low frequency signal, please choose a lower frequency boundary!")
      dat.dec[, 1] <- decomp.wavMODWT[, length(y$data)]
    }
    dat.dec[ , 2] <- x-dat.dec[, 1]
    x <- apply(dat.dec, 2, function(z) z-mean(z))
  }
  else stop(paste("Invalid decomposition method:",method))
  ##value<< data frame containing low frequency and high frequency parts of the data frame
  return(x)
}

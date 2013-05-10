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
#
scapedecomp=function(
  ##title<< Decompose a signal into given frequency bands 
  ##description<< Decompose a signal into given frequency bands using SSA, EMD, Fourier or MA-Filter   
  x, ##<< numeric vector: time series
  sf, ##<< numeric: sampling frequency (samples per day)
  fborder, ##<< numeric: boundary time scale (in days)
  Ms=c(90,30), ##<< vector: SSA window length (only applicable for SSA)
  method="Fourier" ##<< String: decomposition methodm can be "SSA", "EMD", "Fourier" or "MA"
) {
  ##details<<
  ## This function decomposes the time series into a low frequency and a high frequency component while making 
  ## sure that both components added together result agasin in the original signal. 
  
  ##author<<
  ##Fabian Gans, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de
  l<-length(x)
  browser()
  if (method=="SSA") {
    library("Rssa")
    
    # frequencies to extract
    borders.wl      <- data.frame(s0 = c(fborder, Inf), # annual freq
                                  s1 = c(0, fborder))*sf    # rest
    
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
    M              <- c(floor(min(Ms[1]*sf,l/3)), floor(Ms[2]*sf))
    
    # decompose the time series    
    dat.dec    <- FilterTSeriesSSA(x,
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
    x <- apply(dat.dec,2,function(z) z-mean(z))
    
  } else if (method=="EMD") {
    library("EMD")
    y<-emd(x,boundary="periodic",sm="spline",spar=3,smlevels=1:20,max.imf=20,plot=TRUE)
    freqs<-vector(mode="numeric",length=y$nimf)
    dat.dec<-matrix(0,nrow=l,ncol=ncol(borders.wl))
    for (i in 1:y$nimf) {
      freqs[i]<-1/DetermineFreq(y$imf[,i])
    }
    i<-2
    for (i in seq(ncol(borders.wl),2,by=-1)) {
      colind<-((freqs>borders.wl[1,i]) & (freqs<borders.wl[2,i])) 
      dat.dec[,i]<-rowSums(y$imf[,colind])
    }
    if (ncol(dat.dec)>2) {
      dat.dec[,1]<-x-rowSums(dat.dec[,2:ncol(dat.dec)])
    } else {
      dat.dec[,1]<-x-dat.dec[,2]
    }
    x <- apply(dat.dec,2,function(z) z-mean(z))
    
  } else if (method=="MA") {
    library("signal")
    f<-Ma(rep(1/(fborder*sf),fborder*sf))
    dat.dec<-matrix(0,nrow=l,ncol=ncol(borders.wl))
    dat.dec[,1]<-filter(f,x)
    dat.dec[,2]<-x-dat.dec[,1]
    x <- apply(dat.dec,2,function(z) z-mean(z))
    
  } else if (method=="Fourier") {
    ffx<-fft(x)
    nborder<-floor(l/fborder/sf)
    ffx[(nborder+1):(l-nborder)]<-0
    dat.dec<-matrix(0,nrow=l,ncol=ncol(borders.wl))
    dat.dec[,1]<-fft(x,inverse=TRUE)
    dat.dec[,2]<-x-dat.dec[,1]
    x <- apply(dat.dec,2,function(z) z-mean(z))
  }
  ##value<< data frame containing low frequency and high frequency parts of the data frame
  return(x)
}

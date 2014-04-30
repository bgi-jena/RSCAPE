###################################################################################
###################################################################################
### iAAFT.R                    
###
### Amplitude adjusted Fourier transform (AAFT) and
### iterative amplitude adjusted Fourier transform (iAAFt)
### as described in e.g.
### Surrogat Time Series                
### T. Schreiber, A. Schmitz            
### Physica D 142,pp.346-382 (2000)     
### are implemented in R. Convergence for iAAFT is controlled
### via either the ACF or the periodogram. A relative and
### an absolute convergence criterion is implemented
###
### Copyright (C) 2006         Henning Rust
### 
### This program is free software; you can redistribute it and/or
### modify it under the terms of the GNU General Public License
### as published by the Free Software Foundation; either version 2
### of the License, or (at your option) any later version.
### 
### This program is distributed in the hope that it will be useful,
### but WITHOUT ANY WARRANTY; without even the implied warranty of
### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### GNU General Public License for more details.
### 
### You should have received a copy of the GNU General Public License
### along with this program; if not, write to the Free Software
### Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
###
###################################################################################
###################################################################################

## rank ordering to rescale process to distribution
.rescale <- function(X,Y,method=c("shell")){
  ## rescale X to have distribution from Y
  ## rank-oder Y and rank-order X. Substitute the X values with the Y values and undo the sort
  X[sort(X,index=T,method=method)$ix] <- sort(Y,method=method)
  return(X)
}

### use fourier phase shuffling on Xcorrelogram and
### rank ordering to retain the distribution of Xdistribution 
.iAAFT <- function(Xcor,Xdist=Xcor,tolerance=0.01,maxit=100,adjust.var=TRUE,
                  zero.mean=TRUE,quiet=TRUE,diff=FALSE,
                  criterion=c("periodogram"),rel.convergence=TRUE,
                  method=c("shell"))
  
  ### This function iteratively generates a surrogate time series with the same spectrum and distribution 
  
  ##Henning Rust
  { 

  ## Parameters
  ## ----------
  ## Xcor            : record whos periodogram should be kept
  ## Xdist           : record whos distribution should be kept
  ## tolerance       : convergence criterion
  ## maxit           : maximal numbers of iteration
  ## adjust.var      : set var(Xcor) to var(Xdist)
  ## zero.mean       : set the mean of Xdist to sero and add it later
  ## quiet           : forces silence
  ## diff            : creates a difference vector for every iteration
  ## criterion       : "periodogram" or "acf" as criterion for convergence
  ## rel.convergence : use a relative convergence criterion
  ## method          : sorting method to use

  ## check criterion
  if(criterion!="periodogram" && criterion!="acf"){
    cat("Criterion",criterion,"unknown, using periodogram\n")
    criterion <- c("periodogram")
  }

  ## get lenght of data set
  n <- length(Xcor)

  if(n==length(Xdist)){

    ## adjust var
    if(adjust.var){
      sigXcor <- sqrt(var(Xcor))
      sigXdist <- sqrt(var(Xdist))
      Xcor <- Xcor*sigXdist/sigXcor
    }

    ## set the mean of Xdist to zero, add it later
    Xdist.mean <- NULL
    if(zero.mean){
      Xdist.mean <- mean(Xdist)
      Xdist <- Xdist-Xdist.mean
    }
    
    ## get ranked distribution
    c <- sort(Xdist,method=method)

    ## calculate desired fourier amplitudes from Xcorr
    S <- fft(Xcor)
    
    ## generate zero order r, shuffle Xdist 
    r <- sample(Xdist)

    ## prepare convergence criterion
    if(criterion=="acf")
      Xacf <- acf(Xcor,plot=FALSE,lag.max=n-1) ## for acf convergence criterion
    else
      Xspc <- spectrum(Xcor,plot=FALSE)    ## for periodogram convergence criterion 

    
    ## prepare iteration scheme
    convergence <- FALSE
    it <- 0                              ## initialize iteration counter
    if(diff) diff.vect <- rep(NA,maxit)  ## take along a difference vector
    else diff.vect <- NULL
    rel.diff <- NA                       ## initialize relative difference
    
    ## iteration
    while(!convergence && it<=maxit){
      
      ## FFT from r
      R <- fft(r)
      
      ## replace amplitudes from R by the desired ones Xcor and backtransform
      s <- fft(complex(modulus=Mod(S),argument=Arg(R)),inverse=TRUE)/n
      
      ## rescale s to have the proper distribution 
      r.new <- .rescale(Re(s),c,method=method)

      ## calculate a convercgence criteria
      ## criterion acf
      if(criterion=="acf"){
        Racf <- acf(r,plot=FALSE,lag.max=n-1)
        diff.new <- sum((Racf$acf-Xacf$acf)^2)/n
      }
      
      ## criterion "periodogram"
      else{
        Rspc <- spectrum(r,plot=FALSE) 
        diff.new <- sum((Rspc$spec-Xspc$spec)^2)/n
      }

      ## check for convergence, using relative or absolute difference
      if(it>=1){
        if(rel.convergence)
          diff <- abs((diff.new-diff.old)/diff.old)
        else
          diff <- diff.new
        if(diff<tolerance)
          convergence <- TRUE
      }
      if(!quiet) cat("Iteration: ",it,", Difference in ACF =",diff.new,", relative Improvement = ",diff,"\n")

      ## save new difference for next run
      diff.old <- diff.new 

      ## store difference vector
      if(diff) diff.vect[it] <- diff.new

      ## prepare for nex iteration
      r <- r.new
      it <- it+1
    }

    ## add mean if substracted before
    if(!is.null(Xdist.mean))
      r <- r+Xdist.mean
    
    ## return the desired series
    #return(list(x=r,it=it,diff=diff.vect))
    ## changed by FG, return only the new time series
    return(r)
  }
  else cat("Series need to have same length, exiting!!\n")
}


### use fourier phase shuffling on Xcorrelogram and
### rank ordering to retain the distribution of Xdistribution
### but here transform to gaussian, shuffle phases and retransform
AAFT <- function(X,method=c("shell")){

  ## get lenght of data set
  n <- length(X)

  ## draw a gaussian for FFT purpose
  gauss <- rnorm(n)
  
  ## transform the Xdist to gaussian
  X.gauss <- .rescale(X,gauss,method=method)

  ## FFT the gaussian version
  X.fft <- fft(X.gauss)

  ## randomise phases
  phases.rand <- sample(Arg(X.fft))

  ## backtransform
  X.back <- fft(complex(modulus=Mod(X.fft),argument=phases.rand),inverse=TRUE)/n

  ## rescale back
  X.rescaled <- .rescale(Re(X.back),X,method=method)

  ## return the desired series
  return(list(x=X.rescaled))
           
}

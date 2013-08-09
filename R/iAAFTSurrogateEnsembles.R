iAAFTSurrogateEnsemble<-function(
  ##title<< Get random ensemble time series
  ##description<< calls the iAAFT function written by Henning Rust to get an ensemble of surrogate time series
  x, ##<< input time series
  nsamples=10 ##<< number of surrogate time series
  ) {
  ##author<<
  ##Fabian Gans, MPI BGC Jena, Germany, fgans@bgc-jena.mpg.de
  l<-length(x)
  a<-array(data=rep(x,nsamples),dim=c(l,nsamples))
  ens<-aaply(.data=a,.margins=2,.fun=RSCAPE::iAAFT,tolerance=0.001,rel.convergence=TRUE)
  ##value<< Array containing a ensemble of time series with same distribution an spectrum as the input
  return(t(ens))
}

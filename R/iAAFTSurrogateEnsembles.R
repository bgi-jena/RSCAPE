iAAFTSurrogateEnsemble<-function(x,nsamples=10) {
  l<-length(x)
  a<-array(data=rep(x,nsamples),dim=c(l,nsamples))
  ens<-aaply(.data=a,.margins=2,.fun=iAAFT,tolerance=0.001,rel.convergence=TRUE)
  return(t(ens))
}
.transformOutput <- function(x,getSensPar,sensname) {
  
    oldnames<-names(x)
    if (length(names(x))>0) names(x)<-gsub(pattern="XYZ",replacement=sensname,x=oldnames)
    ind <- grep("XYZ",oldnames)
    for (i in ind) {
      if (class(x)=="list") x[[i]]<-getSensPar(x[[i]])
      else x[i]<-getSensPar(x[i])
    }
    if (class(x)=="list") {
      for (i in 1:length(x)) {
        x[[i]]<-.transformOutput(x[[i]],getSensPar,sensname)
      }
    }
    return(x)
  
}

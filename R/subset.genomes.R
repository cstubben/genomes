subset.genomes<-function(x, ...)
{
  y <- subset.data.frame(x, ...)
  ## name and released columns are needed for genome tables
  z<-match(c("name", "released"), names(y))
  if( any( is.na(z)) ){
     class(y)<-"data.frame"
  }
  else{   
     attr(y, "date")   <- attr(x, "date")
     attr(y, "subsetof") <- as.character(substitute(x))
  }
  y
}

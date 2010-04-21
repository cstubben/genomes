plot.genomes<-function(x, subset, xlab="Release Date", ylab="Genomes",
                     type='l', col="blue", ...)
{
   if (!missing(subset)) {   
     r <- eval(substitute(subset), x)
     if (!is.logical(r)) { stop("'subset' must evaluate to logical")}
     x<-x[r, ]
   }
   if (nrow(x[!is.na(x$released),]) == 0) { stop("No rows to plot")}
  
   genomes <- table(x$released)
   plot(as.Date(names(genomes)), cumsum(genomes), 
     type=type, col=col,
     xlab=xlab, ylab=ylab, ...)
}

 

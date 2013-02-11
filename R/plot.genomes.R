plot.genomes<-function(x, subset, xlab, ylab="Genomes",
                     type='l', col="blue",  ...)
{
   if (!missing(subset)) {   
     r <- eval(substitute(subset), x)
     if (!is.logical(r)) { stop("'subset' must evaluate to logical")}
     x<-x[r, ]
   }
   # possible date columns
  valid <- c("released", "created", "submitted")
  n <- which(names(x) %in% valid)[1] 
  if(is.na(n)){ stop("No released, created or submitted date column found")}

  if(missing(xlab)) {xlab <- names(x)[n]
        xlab <- paste(toupper(substr(xlab,1,1)), substring(xlab,2), sep="") 
   }
   if (nrow(x[!is.na(x[,n]),]) == 0) { stop("No rows to plot")}
  
   #   genomes <- table(x[,n])
   # inlcude today's date
   genomes <- table(c(x[,n], Sys.Date()) )
   genomes[length(genomes)] <- 0
   plot(as.Date(names(genomes)), cumsum(genomes), 
     type=type, col=col,
     xlab=xlab, ylab=ylab, ...)
}

 

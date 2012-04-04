lines.genomes<-function(x, subset, ...)
{
  if (!missing(subset)) {   
     r <- eval(substitute(subset), x)
     if (!is.logical(r)) { stop("'subset' must evaluate to logical")}
     x<-x[r, ]
   }
  valid <- c("released", "created", "submitted")
  n <- which(names(x) %in% valid)[1] 
  if(is.na(n)){ stop("No released, created or submitted date column found")}

   if (nrow(x[!is.na(x[,n]),]) == 0) { stop("No rows to plot")}

  ## add new tables to plot (use plotby to plot table subsets)
  genomes <- table(x[,n])
  lines(as.Date(names(genomes)), cumsum(genomes), ...)

}

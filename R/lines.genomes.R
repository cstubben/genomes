lines.genomes<-function(x, subset, ...)
{
  if (!missing(subset)) {   
     r <- eval(substitute(subset), x)
     if (!is.logical(r)) { stop("'subset' must evaluate to logical")}
     x<-x[r, ]
   }
   if (nrow(x[!is.na(x$released),]) == 0) { stop("No rows to plot")}

  ## add new tables to plot (use plotby to plot table subsets)
  genomes <- table(x$released)
  lines(as.Date(names(genomes)), cumsum(genomes), ...)

}

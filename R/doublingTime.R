doublingTime <- function (x, subset, time = "days", curdate=TRUE) 
{
   if (class(x)[1] != "genomes") {
      stop("x should be a \"genomes\" table with release dates")
   }
   if (!missing(subset)) {
        r <- eval(substitute(subset), x, parent.frame())
        if (!is.logical(r)) 
            stop("'subset' must evaluate to logical")
        x <- x[r, ]
   } 
   z <- cumsum(table(x$released))
   # ADD Current date
   if(curdate){
      z<- c( z,  CDATE =max(z) )
      names(z)[length(z)]<-as.character( Sys.Date() )
   }
   days <- as.Date(names(z))
   lm1 <- lm(log(z) ~ days)
   mu <- lm1$coef[2]
   dbt <- log(2)/mu
   if (grepl("^m", time, TRUE)) {
        c(months = round(as.vector(dbt)/(365/12), 1))
   }
   else if (grepl("^y", time, TRUE)) {
      c(years = round(as.vector(dbt)/365, 2))
   }
   else round(dbt)
}


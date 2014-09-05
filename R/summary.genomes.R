summary.genomes<-function(object, subset, top=5, ...)
{   
   if (!missing(subset)) {   
     r <- eval(substitute(subset), object, parent.frame())
     if (!is.logical(r)) 
            stop("'subset' must evaluate to logical")
     object <- object[r, ]
   }
   if (nrow(object) == 0) { stop("No rows to summarize")}
   if (top < 1){top <- 1}


   # no release date?
  if(!"released" %in% names(object)){
     valid <- c( "created", "submitted")
     n <- which(names(object) %in% valid)[1] 
     if(is.na(n)){ stop("No released, created or submitted date columns found")}
     names(object)[n]<-"released"
  }

   
   if (!"status" %in% names(object)) {

     d2 <- head(object[order(object$released, decreasing=TRUE), c("released", "name")], n=top)
     rownames(d2) <- NULL
     
     ## format does not justify column names 
     names(d2) <- c( "DATE      ",
     sprintf( paste("%-",  max(nchar( d2[,2])), "s", sep=""), "NAME")  )
     ans <- list(
            "Total genomes" = noquote(paste( nrow(object), "genome projects on",
                               format( attributes(object)$date, "%b %d, %Y"))),
       "Recent submissions" = format(d2, justify="left")) 
   }

   else{
     d2 <- head(object[order(object$released, decreasing=TRUE), c("released", "name", "status")], n=top)
     rownames(d2) <- NULL
     
     ## format does not justify column names 
     names(d2) <- c( "released  ",
     sprintf( paste("%-",  max(nchar( d2[,2])), "s", sep=""), "name"),
     sprintf( paste("%-",  max(nchar( d2[,3])), "s", sep=""), "status")  )
     ans <- list(
            "Total genomes" = noquote(paste( nrow(object), "genome projects on",
                               format( attributes(object)$date, "%b %d, %Y"))),
                "By status" = table2(object$status),
       "Recent submissions" = format(d2, justify="left"))
    }
     ans
}



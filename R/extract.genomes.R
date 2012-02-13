"[.genomes" <- function(x, i, j, drop)
{
   ## use [.data.frame (next class after genomes)
   y <- NextMethod("[")
   
   ## check if this is still a genomes dataframe...
   if( class(y)[1] == "genomes"){
     z <- match(c("name", "released"), names(y))
     if( any( is.na(z)) ){
        class(y)<-"data.frame"
     }
     else{   
        attr(y, "date")   <- attr(x, "date")
        attr(y, "subsetof") <- as.character(substitute(x))
     }
   }
   y
}

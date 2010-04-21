print.genomes<-function (x, ...) 
{ 
   if(nrow(x) > 6){
      cat("  A genomes data.frame with", nrow(x), "rows and", ncol(x), "columns\n\n")
      if(ncol(x) > 4){
         x  <- x[ , 1:4]
         h1 <-head(x,6)
         x  <- format(cbind(rbind(h1, tail(x,1)), "..."="...") )
         x[6,] <- "..."
         rownames(x)[6] <- "..."
         print(x)
      }
      else{
        h1<-head(x,6)
        x<-format( rbind(h1, tail(x,1)) )
        x[6,] <- "..."
        rownames(x)[6] <- "..."
        print(x)
      }
   }
   else{ print.data.frame(x) }
}


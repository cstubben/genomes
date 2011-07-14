acc2dateCommon<-function(id, random=25 )
{  
   url1 <- "http://www.ncbi.nlm.nih.gov/nuccore/"
   url2 <- "?report=girevhist"

   url <- paste(url1, id[1], url2, sep="")
   xhist <- readLines(url, warn=FALSE)

   ## check if common revision history needed!
   n <- grep("Show revision history", xhist)
   if(!length(n) > 0){stop("No replaced sequences found")}
    x <- xhist[n]
    z <- unlist(  strsplit(x, "<div>"))
    n1 <- grep("replaces ",z)
    ids2 <- gsub("(.*replaces[^>]*>)([^<]*)(<.*)", "\\2",   z[n1])
    n <- length(ids2)
    if(n > random){
       ids2 <- sample(ids2, random)
       print(paste("Finding earliest date from ", random, " replaced sequences (", n, " total)", sep="" ))
    }else{
       print(paste("Finding earliest date from", n, "replaced sequences"))
    }
    x <- acc2date(ids2)  
    min(x$released)
 }

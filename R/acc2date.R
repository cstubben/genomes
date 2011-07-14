acc2date<-function(ids, db="nuccore")
{
   # in case ids are a vector of comma-separated lists
   ids <- paste(ids, collapse=",")
   ids <- gsub(" *", "", ids)
   ids <- unlist(strsplit(ids,","))
  
   n <- length(ids)
   revhist <- vector("list",n)
   # revision history now only a display setting in Entrez nucleotide
   url1 <- "http://www.ncbi.nlm.nih.gov/"
   url2 <- "?report=girevhist"
   for(i in 1:n){
     url <- paste(url1, db, "/", ids[i], url2, sep="")
     xhist  <- readLines(url, warn=FALSE)
     n1 <- grep("first seen", xhist)
     ## check if common revision history needed
     n2 <- grep("Show revision history", xhist)
     if(length(n2) > 0){print(paste( "Warning: check common revision history for", ids[i]))}
     x2 <- gsub("(.*was first seen at NCBI on )(.*[[:digit:]]{4})(.*)", "\\2",   xhist[n1])
     revhist[i] <- x2
   }
  data.frame(id=ids, released=as.Date(unlist(revhist), format = "%b %d, %Y"))
}

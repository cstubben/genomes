acc2date<-function(ids, db="nuccore", batchsize=20)
{
   # in case ids are a vector of comma-separated lists (see lproks table)
   ids <- paste(ids, collapse=",")
   ids <- gsub(" *", "", ids)

   ##  run queries in batches...
   ids <- unlist(strsplit(ids,","))
     n <- length(ids)
   batchn <- ceiling(n/batchsize)
   batchids <- split(ids, rep(1: batchn, each= batchsize)[1:n])

   revhist <- vector("list", batchn)

   # revision history a display setting in Entrez nucleotide
   url1 <- "http://www.ncbi.nlm.nih.gov/"
   url2 <- "?report=girevhist"

   for(i in 1: batchn){
      url <- paste(url1, db, "/", paste(batchids[[i]], collapse=",") , url2, sep="")
      # only print in more than 1 batch
      if(batchn>1) print(paste("Checking batch", i, "of", batchn)) 

      xhist  <- readLines(url, warn=FALSE)
      # all on one line?
      n <- grep("first seen at NCBI", xhist)
      if(length(n) == 0){   stop("No results found") }
      # check?
      #if(length(n) > 1) { stop("Warning: First seen dates on multiple lines")}
      x1 <- unlist ( strsplit(  xhist[n],  "</div></div></div>") )
      # check for common revision histories (replaces...)
      nX <- grep("Show revision history", x1)
       # remove html tags
       x1 <- gsub("<[^>]*>", "", x1)
      n1 <- grep("first seen at NCBI", x1)
      x2 <- gsub("(.*Accession )(.*)( was first seen at NCBI on )(.*[[:digit:]]{4})(.*)", "\\2---\\4",   x1[n1])
      x2 <- do.call("rbind", strsplit(x2, "---"))
      z <- data.frame( x2, stringsAsFactors=FALSE)
      colnames(z) <- c("id", "released")

      # CHECK if ids are same as original list...
      if(! identical(z$id, batchids[[i]])){print("Warning: some returned ids are different/missing")}

      if(length(nX)==1){ print(paste("Warning:",  z$id[nX], "has common revision history")) } 
      if(length(nX) >1){ print(paste("Warning:",  paste(z$id[nX], collapse=", "),  "have common revision histories")) }

      z$released  <- as.Date(z$released, format = "%b %d, %Y")
      revhist[[i]] <- z
   }
   do.call("rbind", revhist)
}




#old:  ONE ACC per query
#acc2date<-function(ids, db="nuccore")
#{
#   # in case ids are a vector of comma-separated lists
#   ids <- paste(ids, collapse=",")
#   ids <- gsub(" *", "", ids)
#   ids <- unlist(strsplit(ids,","))
  
#   n <- length(ids)
#   revhist <- vector("list",n)
   # revision history now only a display setting in Entrez nucleotide
#   url1 <- "http://www.ncbi.nlm.nih.gov/"
#   url2 <- "?report=girevhist"
#   for(i in 1:n){
#     url <- paste(url1, db, "/", ids[i], url2, sep="")
#     xhist  <- readLines(url, warn=FALSE)
#     n1 <- grep("first seen", xhist)
#     ## check if common revision history needed
#     n2 <- grep("Show revision history", xhist)
#     if(length(n2) > 0){print(paste( "Warning: check common revision history for", ids[i]))}
#     x2 <- gsub("(.*was first seen at NCBI on )(.*[[:digit:]]{4})(.*)", "\\2",   xhist[n1])
#     revhist[i] <- x2
#   }
#  data.frame(id=ids, released=as.Date(unlist(revhist), format = "%b %d, %Y"))
#}

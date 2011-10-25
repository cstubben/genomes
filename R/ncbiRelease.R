ncbiRelease <-function(ids, db="nuccore", common=TRUE, random=20)
{

   # in case ids are a vector of comma-separated lists (see fields in lproks table)
   ids <- paste(ids, collapse=",")
   ids <- gsub(" *", "", ids)
   ids <- unlist(strsplit(ids,","))
   n   <- length(ids)
   # only 20 IDS can be viewed on a single page and therefore included in the url string 
   batchsize <-20
   batchn <- ceiling(n / batchsize)
   batchids <- split(ids, rep(1: batchn, each= batchsize)[1:n])

   # save all histories to revhist
   revhist <- vector("list", batchn)

   # revision history is a display setting in Entrez nucleotide and other sequence databases
   url1 <- "http://www.ncbi.nlm.nih.gov/"
   url2 <- "?report=girevhist"

   for(i in 1: batchn){
      url <- paste(url1, db, "/", paste(batchids[[i]], collapse=",") , url2, sep="")
      # only print in more than 1 batch
      if(batchn>1) print(paste("Checking batch", i, "of", batchn)) 

      xhist  <- readLines(url, warn=FALSE)
      # RETURNS all revision histories on one line
      n <- grep("first seen at NCBI", xhist)
      if(length(n) == 0){   stop("No results found") }
   
      x <- unlist ( strsplit(  xhist[n],  "</div></div></div>") )

      # RUN LOOP
      n <- grep("first seen at NCBI", x)
      x <- x[n]
      rvBatch <- vector("list", length(x))
      for(j in 1:length(x) )
      {
         x1 <- gsub("<[^>]*>", "", x[j])
         acc  <- gsub(".*Accession (.*) was first seen at NCBI on.*",    "\\1",   x1)
         released <- gsub(".*was first seen at NCBI on (.*)", "\\1",  x1) 
         released <- as.Date(released, format = "%b %d, %Y")
         replaces <- FALSE
          # IF sequence replaces earlier accessions  
          if( grepl("Show revision history", x[j]) ){
            replaces <- TRUE
            if(common){
               print(paste(acc, "has a common revision history"))
               z    <- unlist(  strsplit(x[j], "<div>"))
               n1   <- grep("replaces ",z)
               ids2 <- gsub("(.*replaces[^>]*>)([^<]*)(<.*)", "\\2",   z[n1])
               n <- length(ids2)
               if(n > random){
                  ids2 <- sample(ids2, random)
                  print(paste("  Finding earliest date from ", random, " replaced sequences (", n, " total)", sep="" ))
               }else{
                  print(paste("  Finding earliest date from", n, "replaced sequences"))
               }         
               crvh <- ncbiRelease(ids2, common=FALSE)  
               released <- min(crvh$released, na.rm=TRUE)
             }else{
                print(paste("WARNING:", acc, "has common revision history"))
             }
          }
          rvBatch[[j]]<-data.frame(acc, released, replaces, stringsAsFactors=FALSE)
      }
      revhist[[i]] <- do.call("rbind",rvBatch)
   }
   do.call("rbind", revhist)
}




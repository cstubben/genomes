enaExperiment <- function( accs , batchsize = 100)
{  
   if(any(is.na(accs))){accs<-accs[!is.na(accs)] }
   if(any(duplicated(accs))){ accs <- unique(accs) }
   if(!all(substr(accs, 3,3)=="X")){stop("Study ids should have ERX or SRX prefix")}
   
   n <- length(accs)
   # PASS comma-separated list to URL up to batchsize
   batchn <- ceiling(n/batchsize)
   batchids <- split(accs, rep(1:batchn, each = batchsize)[1:n])
   exps <- vector("list", batchn)
   url1 <-"http://www.ebi.ac.uk/ena/data/view/"


   for (i in 1:batchn) {
      url <- paste(url1,  paste(batchids[[i]], collapse = ","), "&display=xml", sep="")
      if(batchn > 1)  print(paste("Checking batch", i, "of", batchn))
      x <- readLines(url)
      connecterror <- grepl("ERROR", x)
      if (any(connecterror)) {
        print(paste("Error connecting to NCBI ", x[connecterror]))
      } 
      doc <- xmlParse(x)
      z <- getNodeSet(doc, "//EXPERIMENT")
      n <- length(z)
      exp <- vector("list", n)
      for (j in 1:n) {
         z2 <- xmlDoc(z[[j]])

         experiment <- xattr(z2, "//EXPERIMENT", "accession")
         submission <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-SUBMISSION")
         title     <- xvalue(z2, "//TITLE")
         #xvalue(z2, "DESIGN_DESCRIPTION")
         # PLATFORM
         platform  <- xpathSApply(z2, "//PLATFORM/*", xmlName)
         model     <- xvalue(z2, "//INSTRUMENT_MODEL")
         # LIBRARY
         name      <- xvalue(z2, "//LIBRARY_NAME")
         # strategy  <-xvalue(z2, "//LIBRARY_STRATEGY") # all wgs?
         layout    <- xpathSApply(z2, "//LIBRARY_LAYOUT/*", xmlName)
         source    <- xvalue(z2, "//LIBRARY_SOURCE")
         selection <- xvalue(z2, "//LIBRARY_SELECTION")
         bases  <- as.numeric( xtags(z2, "//EXPERIMENT_ATTRIBUTE", "TAG", "VALUE", "ENA-BASE-COUNT") )
         reads  <- as.numeric( xtags(z2, "//EXPERIMENT_ATTRIBUTE", "TAG", "VALUE", "ENA-SPOT-COUNT") )

         exp[[j]] <- data.frame(experiment, title, platform, model, layout, source, selection, name, submission, bases, reads, stringsAsFactors = FALSE)
         free(z2)
       }
       exps[[i]] <- do.call("rbind", exp)
   }
   do.call("rbind", exps)
}

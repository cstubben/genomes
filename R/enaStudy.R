enaStudy <- function( accs , batchsize = 100)
{ 
    if(any(is.na(accs))){accs<-accs[!is.na(accs)] }
   if(!all(substr(accs, 3,3)=="P")){stop("Study ids should have ERP or SRP prefix")}
   if(any(duplicated(accs))){ accs <- unique(accs) }
   n <- length(accs)
   # PASS comma-separated list to URL up to batchsize
   batchn <- ceiling(n/batchsize)
   batchids <- split(accs, rep(1:batchn, each = batchsize)[1:n])
   studies <- vector("list", batchn)
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

      z <- getNodeSet(doc, "//STUDY")
      n <- length(z)
      stdy <- vector("list", n)
      for (j in 1:n) {
         z2 <- xmlDoc(z[[j]])

         study <- xattr(z2, "//STUDY", "accession")
         # broker <- xattr(z2, "//STUDY", "broker_name")
         center <- xattr(z2, "//STUDY", "center_name")

         title     <- xvalue(z2, "//STUDY_TITLE")
         type <- xattr(z2, "//STUDY_TYPE", "existing_study_type")

         #xvalue(z2, "CENTER_PROJECT_NAME")  # same as title
         description <- xvalue(z2, "//STUDY_DESCRIPTION") 
         
         # samples, experiment, run, submission, and anlysis
         sample <-  xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-SAMPLE") 
         analysis<- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-ANALYSIS") 

         bases  <- as.numeric( xtags(z2, "//STUDY_ATTRIBUTE", "TAG", "VALUE", "ENA-BASE-COUNT") )
         reads  <- as.numeric( xtags(z2, "//STUDY_ATTRIBUTE", "TAG", "VALUE", "ENA-SPOT-COUNT") )

         stdy[[j]] <- data.frame(study, title, type, center, description, sample, analysis, bases, reads, stringsAsFactors = FALSE)
         free(z2)
       }
       studies[[i]] <- do.call("rbind", stdy)
   }
   do.call("rbind", studies)
}

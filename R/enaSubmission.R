enaSubmission <- function( accs , batchsize = 100)
{ 
   if(!all(substr(accs, 3,3)=="A")){stop("Submission ids should have ERA or SRA prefix")}
   if(any(duplicated(accs))){ accs <- unique(accs) }
   n <- length(accs)
   # PASS comma-separated list to URL up to batchsize
   batchn <- ceiling(n/batchsize)
   batchids <- split(accs, rep(1:batchn, each = batchsize)[1:n])
   submit <- vector("list", batchn)
   url1 <-"http://www.ebi.ac.uk/ena/data/view/"

   for (i in 1:batchn) {
      url <- paste(url1,  paste(batchids[[i]], collapse = ","), "&display=xml", sep="")
      if(batchn > 1)  print(paste("Checking batch", i, "of", batchn))
      x <- readLines(url)
      connecterror <- grepl("ERROR", x)
      if (any(connecterror)) {
        print(paste("Error connecting to NCBI ", x[connecterror]))
      } 
      doc   <- xmlParse(x)
      y     <- xpathApply(doc, "//SUBMISSION", xmlAttrs)
      title <- xpathSApply(doc, "//TITLE", xmlValue)
      submission <- as.vector( sapply(y, "[", "accession") )
      date <- as.Date(substring( sapply(y, "[", "submission_date"), 1,10))
      submit[[i]] <- data.frame(submission, title, date,  stringsAsFactors=FALSE)
   }
    do.call("rbind", submit)
}

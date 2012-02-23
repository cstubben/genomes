ncbiNucleotide<-function(term, fulltable = FALSE, verbose=TRUE, sort=TRUE)
{
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   db  <- "nuccore"
   if(length(term) > 1){ term  <- paste(term, collapse = ",") }  # comma-separated list of accessions?
   #E-search 
   esearch <- paste(url, "esearch.fcgi?retmax=1&tool=ncbiNucleotide.R&email=stubben@lanl.gov&usehistory=y&db=", db, "&term=", 
        gsub(" ", "%20", term), sep = "")

   gp <- readLines(esearch)
   ##  connection errors?
   connecterror<- grepl("ERROR", gp)
   if(any( connecterror)){
     stop("Error connecting to NCBI ", gp[connecterror])
   }
   x <- unlist( strsplit( gp[3], "<[^>]*>" ))
   x <- x[x != ""]
   if (!x[1] > 0) { stop("No matches to ", term, " found") }
   if(verbose)  print(paste( "Matches", x[1], "sequences"))

   ## E-summary
   esum <- paste(url, "esummary.fcgi?db=", db, "&query_key=", 
        x[4], "&WebEnv=", x[5], sep = "")
   gp <- readLines(esum)
   ##  PARSE XML
   doc <- xmlRoot(xmlParse(gp))
   ## get values for Id and 18 Items
   x <- xmlSApply(doc, function(x) xmlSApply(x, xmlValue))
   #transpose table
   x <- t(x)
   rownames(x) <- 1:nrow(x)

   # add column names
   colnames(x)<-c("id", "acc", "name" ,    "defline" , "gi", "released",
          "updated", "flags", "taxid" , "size", "status", "replace", "comment")
  
   # convert to data frame
   x2 <- as.data.frame(x, stringsAsFactors=FALSE)
   # format dates
   x2$released <- as.Date(substr(x2$released, 1, 10),"%Y/%m/%d")
   x2$released[x2$released < "1970-1-1"]<-NA

   # size and taxid are characters
   x2$size  <- as.numeric(x2$size)
   x2$taxid <- as.numeric(x2$taxid)
   x2$updated <- as.Date(x2$updated,"%Y/%m/%d")

   x2 <- x2[, c(2,3, 6, 10,9, 5, 11, 1,4,7,8,12,13)] 
   #6  columns?  acc, name, released, size, taxid, and gi
   if (!fulltable) {   x2 <- x2[, 1:6]  } 

   if(sort) x2<-x2[ order(x2$name), ]

   rownames(x2) <- 1:nrow(x2)
   ##  add class
   class(x2) <- c("genomes", "data.frame")
   ## save date for updates -then save term??
   attr(x2, "date")   <- Sys.Date()
   attr(x2, "term")   <- term
   x2
}

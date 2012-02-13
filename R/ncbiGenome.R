ncbiGenome<-function(term)
{
   db  <- "genome"
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   ## E-search 
   esearch <- paste(url, "esearch.fcgi?retmax=1&tool=ncbiGenome.R&email=stubben@lanl.gov&usehistory=y&db=", db, "&term=", 
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
        print(paste( "Matches", x[1], "genomes"))              
   querykey <- x[4]
   webenv   <- x[5]
   esum <- paste(url, "esummary.fcgi?db=genome&query_key=", querykey, "&WebEnv=", webenv, sep = "")
   gp <- readLines(esum)
   ##  PARSE XML
   doc <- xmlRoot(xmlParse(gp))

       # z <- getNodeSet(doc, "//DocSum")   # esummary returns docsum
       # x <- lapply(z, function(x) xmlSApply(x, xmlValue))
       # x <- do.call("rbind", x)
       # x <- data.frame(x, stringsAsFactors = FALSE)
      
   x1 <- xmlSApply(doc, function(x) xmlSApply(x, xmlValue))
   #transpose table
   x1 <- t(x1)
     rownames(x1) <- NULL    # for as.data.frame
      colnames(x1) <- c("id",  "name" , "kingdom",  "defline" , "pid", 
  "chr", "pla", "org", "assembly", "acc", "aid", "created", "options")

   x1 <- as.data.frame(x1, stringsAsFactors = FALSE)
      # format dates
      x1$created <- as.Date(substr(x1$created, 1,10), "%Y/%m/%d")
      x1 <- x1[ order(x1$name),]
      rownames(x1) <- NULL
      ##  add class
      # class(x1) <- c("genomes", "data.frame")
      ## save date for updates -then save term??
      # attr(x1, "date") <- Sys.Date()
      # attr(x1, "term") <-term
      x1
}

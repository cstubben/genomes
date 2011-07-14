term2summary<-function(term)
{
   db <- "bioproject"
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   esearch <- paste(url, "esearch.fcgi?retmax=1&tool=term2summary.R&email=stubben@lanl.gov&usehistory=y&db=", 
        db, "&term=", gsub(" ", "%20", term), sep = "")
   gp <- readLines(esearch)
   connecterror <- grepl("ERROR", gp)
   if (any(connecterror)) {
      print(paste("Error connecting to NCBI ", gp[connecterror]))
   }else{
    x <- gsub("</[^>]*>", "-!-", gp[3])
    x <- gsub("<[^>]*>", "", x)
    x <- unlist(strsplit(x, "-!-"))
    if (!x[1] > 0) {
        print(paste("No matches to", term, "found"))
    }else{
     esum <- paste(url, "esummary.fcgi?db=", db, "&query_key=", 
        x[4], "&WebEnv=", x[5], sep = "")
     gp <- readLines(esum)
  
     doc <- xmlRoot(xmlParse(gp))
    ## SEE doc[[1]][[1]]
    ## table(xpathSApply(doc, "//*", xmlName))

     z <- getNodeSet(doc, "//DocumentSummary")
     x <- lapply(z, function(x) xmlSApply(x, xmlValue))

     x <- do.call("rbind", x)
     x <-data.frame(x, stringsAsFactors=FALSE)
    names(x) <- tolower(names(x))
    x
   }
  }
}

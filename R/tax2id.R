tax2id <- function( name )
{
   if (length(name ) > 1) { name<-name[1]; print("Warning: only the first element in vector will be used") }
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
 
   esearch <- paste(url, "esearch.fcgi?db=taxonomy&usehistory=y&term=",  gsub(" ", "%20", name) , sep = "")
   gp <- readLines(esearch) 
     # CHECK FOR errors
   connecterror <- grepl("ERROR", gp)
   if (any(connecterror)) {
      # R CMD check on bioconductor may fail if using stop() 
      print(paste("Error connecting to NCBI ", gp[connecterror]))
   # PARSE Query key and web env
   }else{
      # Esearch returns all results on line 3.  Parse into 5 fields: count, retmax, retstart, querykey, webenv
      x <- unlist( strsplit( gp[3], "<[^>]*>" ))
      x <- x[x != ""]
      if (!x[1] > 0) { stop("No matches to ", name, " found") }
      esum <- paste(url, "esummary.fcgi?db=taxonomy&query_key=", 
                x[4], "&WebEnv=", x[5], sep = "")
      gp  <- readLines(esum)
      doc <- xmlRoot(xmlParse(gp))

      id <- xpathSApply(doc, "//Id", xmlValue)
      if(length(id)>1) { 
         div <- xpathSApply(doc, '//Item[@Name="Division"]', xmlValue)
         print(paste("Warning: ", length(id), " taxonomy ids assigned to ", name, ". Using taxonomy id ", id[1], " from ", div[1], ".", sep=""))
         as.numeric( id[1])
      }else{
       print(paste(name, "matches taxonomy id", id))
          as.numeric(id)
      }
   }                          
}

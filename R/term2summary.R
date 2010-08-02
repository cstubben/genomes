term2summary<-function(term, db = "genomeprj", sortdate = FALSE, fulltable = FALSE)
{
    
    url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   if (!db %in% c("genome", "genomeprj")) {stop("db must be either genome or genomeprj")}

   #E-search 
   esearch <- paste(url, "esearch.fcgi?retmax=1&tool=term2summary.R&email=stubben@lanl.gov&usehistory=y&db=", db, "&term=", 
        gsub(" ", "%20", term), sep = "")

   gp <- readLines(esearch)
   ##  connection errors?
   connecterror<- grepl("ERROR", gp)
   if(any( connecterror)){
     stop("Error connecting to NCBI ", gp[connecterror])
   }
    
   x <- gsub("</[^>]*>", "-!-", gp[3])  # delimiter...
   x <- gsub("<[^>]*>", "", x)
   x <- unlist(strsplit(x, "-!-"))
   ## rows in x[1]
   if(!x[1]>0){stop("No matches to ", term, " found",  call.=FALSE)}
   ## E-summary
   esum <- paste(url, "esummary.fcgi?db=", db, "&query_key=", 
        x[4], "&WebEnv=", x[5], sep = "")
   gp <- readLines(esum)
   ##  PARSE XML
   doc <- xmlRoot(xmlTreeParse(gp,  useInternalNodes = TRUE))
   ## get values for Id and 18 Items
   x <- xmlSApply(doc, function(x) xmlSApply(x, xmlValue))
   #transpose table
   x <- t(x)
   rownames(x) <- 1:nrow(x)

    # add column names
   if(db =="genome") {
       colnames(x)<-c("gi", "acc", "name" ,    "defline" , "gi2", "released",
          "updated", "flags", "taxid" , "size", "status", "replace", "comment")
   } else {
       colnames(x)<-c("pid", "name", "kingdom" ,    "group" , "taxid", "center", 
           "status", "type", "size" , "chromosomes", "trace_code", "trace_name",
           "trace_type", "defline", "mitochondrion", "plastid", "plasmids",
           "created", "released", "options")        
   }
   # convert to data frame
   x2 <- as.data.frame(x, stringsAsFactors=FALSE)
   # format dates
   x2$released <- as.Date(substr(x2$released, 1, 10),"%Y/%m/%d")
   x2$released[x2$released < "1970-1-1"]<-NA

   if (db =="genome") {
      # size and taxid are characters
      x2$size  <- as.numeric(x2$size)
      x2$taxid <- as.numeric(x2$taxid)
       x2$updated <- as.Date(x2$updated,"%Y/%m/%d")
      
      if (!fulltable) {
      # 6 columns?  acc, name, released, taxid, size, status
      x2 <- x2[, c(2,3, 11, 6, 9, 10)]       
      } else{
      x2<- x2[, c(2,3, 11, 6, 9, 10, 1,4,5,7,8,12,13)] 
      }

      
   } else {
     ## fix status (new genomes are missing status!)
      x2$status[x2$status == "complete"]   <- "Complete"
      x2$status[x2$status == "wgs"]        <- "Assembly"
      x2$status[x2$status == "inprogress"] <- "In Progress"
  
      ## 4 columns instead of full 20 (size is often incorrect)
      if (!fulltable) {
        ### remove Overviews (=Top Level), Refseq projects (duplicate of genome sequencing, but with release dates!)
        x2 <- subset(x2, x2$type=="Genome sequencing")
        if(nrow(x2) == 0) {stop("No genome sequencing types found.  Try adding full=TRUE")}
        x2 <-   x2[, c(1, 2, 19, 7)]    
      } else{
         x2<-   x2[, c(1, 2, 19, 7, 3:6,8:18,20)] 
      }
   }
   
   
   ## sort by release date? or just status and name???
   if (sortdate) {
     x2<-x2[ order(x2$status, x2$released, x2$name), ]
   } else {
     x2<-x2[ order(x2$name), ]
   }
   rownames(x2) <- 1:nrow(x2)
   ##  add class
   class(x2) <- c("genomes", "data.frame")
   ## save date for updates -then save term??
   attr(x2, "date")   <- Sys.Date()
   attr(x2, "term")   <- term
   attr(x2, "update") <- paste("term2summary(\"", term, "\", \"", db, "\", ", sortdate, "," , fulltable, ")", sep="")
   x2
}

ncbiProject<-function(term, refseq=TRUE )
{
   db <- "bioproject"
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   esearch <- paste(url, "esearch.fcgi?retmax=1&tool=ncbiProject.R&email=stubben@lanl.gov&usehistory=y&db=", 
        db, "&term=", gsub(" ", "%20", term), sep = "")
   gp <- readLines(esearch)
   connecterror <- grepl("ERROR", gp)
   if (any(connecterror)) {
      print(paste("Error connecting to NCBI ", gp[connecterror]))
   }else{

      x <- unlist( strsplit( gp[3], "<[^>]*>" ))
      x <- x[x != ""]   

      if (!x[1] > 0) { stop("No matches to ", term, " found") }
      print(paste( "Matches", x[1], "projects"))
  
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

         x <- x[, c(2, 15, 29, 14, 1, 3:13, 16:28, 30:32)]

         # rename project_id, project_name, sequencing_status, registration_date
         names(x)[1:4]<-c("pid", "name", "status", "released")
         x$pid<-as.numeric(x$pid)
         x$taxid<-as.numeric(x$taxid)

         x$released[x$released=="1/01/01 00:00"]<-NA
         x$released <- as.Date(substr(x$released, 1,10), "%Y/%m/%d")

         if (!refseq) {
           # SKIP Organism overview AND Umbrella project
           x <- subset(x, x$project_type=="Primary submission" )
           # SKIP RefSeq genomes 
           x <- subset(x, x$project_data_type!="RefSeq Genome" )
           
           print(paste( "Keeping", nrow(x), "primary submissions (excluding RefSeq)"))
           
         }
         x <- x[order(x$name),]
         rownames(x)<-NULL
         class(x)<-c("genomes", "data.frame")
         ## save date 
         attr(x, "date")   <- Sys.Date()
         attr(x, "term")   <- term
         x
    }
}

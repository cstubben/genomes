ncbiProject<-function(term, refseq=FALSE )
{
   db <- "bioproject"
   x <- esummary(esearch(term, db))

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

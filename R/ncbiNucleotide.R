ncbiNucleotide<-function(term)
{
   db <- "nuccore"
   x <- esummary(esearch(term, db, verbose=FALSE))

   # add column names
   colnames(x)<-c("id", "acc", "name" ,    "defline" , "gi", "created",
          "updated", "flags", "taxid" , "size", "status", "replace", "comment")
   # format dates
   x$created <- as.Date(substr(x$created, 1, 10),"%Y/%m/%d")
   x$created[x$created < "1970-1-1"]<-NA

   # size and taxid are characters
   x$size  <- as.numeric(x$size)
   x$taxid <- as.numeric(x$taxid)
   x$updated <- as.Date(x$updated,"%Y/%m/%d")

   x <- x[, c(2,3, 6, 10,9, 5, 11, 1,4,7,8,12,13)] 
  
   ##  add class?
   # class(x) <- c("genomes", "data.frame")
   attr(x, "date")   <- Sys.Date()
   attr(x, "term")   <- term
   x
}

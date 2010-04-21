updatewgs<-function()
{
   url <- "http://www.ncbi.nlm.nih.gov/genomes/genlist.cgi?taxid=2&type=3"
   dtime <- system.time( x <- readLines(url) ) 
   n <- grep("^<tr><td><a target", x)

   ## parse columns
   
   name  <- gsub("<[^>]*>", "", x[n])
   contig<- gsub("<[^>]*>", "", x[n+1])
   acc   <- gsub("<[^>]*>", "", x[n+2])
   size  <- gsub("<[^>]*>", "", x[n+3])
   prots <- gsub("<[^>]*>", "", x[n+4])
   rna   <- gsub("<[^>]*>", "", x[n+5])
   genes <- gsub("<[^>]*>", "", x[n+6])
   rel   <- gsub("<[^>]*>", "", x[n+7])
   upd   <- gsub("<[^>]*>", "", x[n+8])

   ## remove " nt" from size
   size <- gsub("&nbsp.*", "", size)
   rel  <- as.Date(rel, "%b %d %Y")
   upd  <- as.Date(upd, "%b %d %Y")

   ## remove 00000000 from acc?
   acc<-gsub("00000000", "", acc)
   
   prj <- data.frame(acc=acc, name=name, status="Assembly", released=rel, size=size, proteins=prots,
              rna=rna, genes=genes,  updated=upd, stringsAsFactors=FALSE)
   ## add plasmid or chromosome name only if type = 1 or 2   

   ## attributes

   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url")   <- url
   attr(prj, "date")  <- Sys.Date()
   attr(prj, "stats") <- paste(dim(prj)[1], "rows in", round(dtime[3], 1), "seconds")
    attr(prj, "update") <-"updatewgs()"
    prj
   
   
}

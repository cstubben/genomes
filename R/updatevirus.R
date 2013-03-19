updatevirus <- function()
{  
   ftp <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/viruses.txt"
   x <- read.delim(ftp, comment.char="", stringsAsFactors=FALSE, na.strings="-")
  if(ncol(x)!=15){
      print("Warning - number of columns have changed")
      prj <- x
   }else{
      names(x) <- c("name", "taxid", "acc", "pid", "group", "subgroup", "size", "gc",
       "host", "segments", "genes", "proteins", "released", "modified", "status")
      x$released <- as.Date(x$released)
      x$modified <- as.Date(x$modified)
      prj <- x[order(x$name), c(4,1,15,13, 2,3, 5:12,14)]
      rownames(prj)<-NULL
   }
   ## attributes
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url")    <- ftp
   attr(prj, "date")   <- Sys.Date()
   attr(prj, "update") <- "updatevirus()"
   prj
}

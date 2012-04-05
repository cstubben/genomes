updatevirus <- function()
{  
   ftp <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/viruses.txt"
   x <- read.delim(ftp, comment.char="", stringsAsFactors=FALSE, na.strings="-")
   names(x) <- c("name", "acc", "group", "subgroup", "size", "gc",
     "host", "segments", "genes", "proteins", "released", "modified", "status")
   x$released <- as.Date(x$released)
   x$modified <- as.Date(x$modified)
   prj <-x[order(x$name), c(2,1,13,11, 3:10,12)]
   rownames(prj)<-NULL
   ## attributes
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url")    <- ftp
   attr(prj, "date")   <- Sys.Date()
   attr(prj, "update") <- "updatevirus()"
   prj
}

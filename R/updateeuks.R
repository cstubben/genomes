updateeuks <- function()
{
   ftp <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/eukaryotes.txt"
   x <- read.delim(ftp, comment.char="", stringsAsFactors=FALSE, na.strings="-")
   names(x) <- c("name", "acc", "group", "subgroup", "size", "gc",
    "assembly", "chromosomes", "organelles", "plasmids",
    "wgs", "scaffolds", "genes", "proteins", "released", "modified", "status")
   x$released <- as.Date(x$released)
   x$modified <- as.Date(x$modified)
   prj <-x[order(x$name), c(2,1,17,15, 3:14,16)]
   rownames(prj)<-NULL
   ## attributes
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url")    <- ftp
   attr(prj, "date")   <- Sys.Date()
   attr(prj, "update") <- "updateeuks()"  
   prj
}

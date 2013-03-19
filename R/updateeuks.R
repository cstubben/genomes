updateeuks <- function()
{
   ftp <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/eukaryotes.txt"
   x <- read.delim(ftp, comment.char="", stringsAsFactors=FALSE, na.strings="-")
   if(ncol(x) != 20){
      print("Warning - number of columns have changed")
      prj <- x
   }else{
       names(x) <- c("name", "taxid", "acc", "pid", "group", "subgroup", "size", "gc",
      "assembly", "chromosomes", "organelles", "plasmids",
      "wgs", "scaffolds", "genes", "proteins", "released", "modified", "status", "center")
      x$released <- as.Date(x$released)
      x$modified <- as.Date(x$modified)
      x$status[x$status == "Scaffolds or contigs"] <- "Assembly"
      prj <- x[order(x$name), c(4,1,19,17, 2, 3, 5:16,18, 20)]
      rownames(prj) <- NULL

   }
   ## avoid warning for non-ASCII characters in R CMD check
  if( capabilities("iconv")) prj$center <- latin2char(prj$center) 



   ## attributes
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url")    <- ftp
   attr(prj, "date")   <- Sys.Date()
   attr(prj, "update") <- "updateeuks()"  
   prj
}

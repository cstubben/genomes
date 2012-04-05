updateproks <- function()
{      
   ftp <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/prokaryotes.txt"
   x   <- read.delim(ftp, comment.char="", stringsAsFactors=FALSE, na.strings="-")
   names(x) <- c("name", "acc", "group", "subgroup", "size", "gc", "refseq", "insdc", 
     "prefseq", "pinsdc", "wgs", "scaffolds", "genes", "proteins", "released", "modified", "status")
   x$released <- as.Date(x$released)
   x$modified <- as.Date(x$modified)
   x$status[x$status == "Scaffolds or contigs"] <- "Assembly"
   prj <- x[order(x$name), c(2,1,17,15, 3:14,16)]
   rownames(prj) <- NULL

   #--------------------------------------------------#      
   # SET attributes
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url") <- ftp
   attr(prj, "date") <- Sys.Date()
   attr(prj, "update") <- "updateproks()"
   prj
}



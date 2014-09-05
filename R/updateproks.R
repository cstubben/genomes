updateproks <- function()
{      
   ftp <- "ftp://ftp.ncbi.nlm.nih.gov/genomes/GENOME_REPORTS/prokaryotes.txt"
   x   <- read.delim(ftp, comment.char="", stringsAsFactors=FALSE, na.strings="-")
   if(ncol(x) != 25){
      print("Warning - number of columns have changed")
      prj <- x
   }else{
    
       names(x) <- c("name", "taxid", "bioproject", "pid", "group", 
            "subgroup", "size", "gc", "refseq", "insdc", 
            "plasmid.refseq", "plasmid.insdc", "wgs", "scaffolds", "genes", 
            "proteins",  "released", "modified", "status", "center", 
            "biosample", "assembly", "reference", "ftp", "pubmed" )
        x$released <- as.Date(x$released)
        x$modified <- as.Date(x$modified)

      prj <- x[order(x$name), c(4,1,19,17, 2, 3, 5:16,18, 20:25)]
      rownames(prj) <- NULL

   }
  ## avoid warning for non-ASCII characters in R CMD check
  if( capabilities("iconv")) prj$center <- latin2char(prj$center) 


   #--------------------------------------------------#      
   # SET attributes
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url") <- ftp
   attr(prj, "date") <- Sys.Date()
   attr(prj, "update") <- "updateproks()"
   prj
}



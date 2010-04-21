updatehmp <- function()
{      
   ## goldtable.xls is actually a tab delimited file
   url <- "http://durian.jgi-psf.org/~kliolios/HMP/hmp.xls"
   dtime <- system.time( prj <- read.delim(url, stringsAsFactors=FALSE )  )

   # keep all columns?
     ## also put id, name, status, released in first 4 columns (for printing)
   prj <- prj[ , c(1,7,  20, 21, 14, 22, 3,  11,12, 18, 19,13, 29:33, 10)]

   names(prj)<-c("pid", "name", "status", "released",
                 "entrezid", "taxid", "phylum",  "site",
                 "subsite", "goal", "submission", "center",
                "assembler", "reads", "coverage", "contigs", "platform", "comment")
  
   ## sort by name
   prj<-prj[ order(prj$name),]
   rownames(prj)<-1:nrow(prj)

   ## dates
   prj$released<-as.Date(prj$released,"%d-%b-%y")
   
   ## attributes
   class(prj) <- c("genomes", "data.frame")

   attr(prj, "url")    <- url
   attr(prj, "date")   <- Sys.Date()
   attr(prj, "stats")  <- paste(dim(prj)[1], "rows in" , round(dtime[3], 1), "seconds")
   attr(prj, "update") <- "updatehmp()"
   prj

}

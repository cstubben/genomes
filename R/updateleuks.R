updateleuks <- function()
{
   url <- "http://www.ncbi.nlm.nih.gov/genomes/leuks.cgi?dump=selected"

   dtime <- system.time( prj <- read.delim(url, skip=1, stringsAsFactors=FALSE, na.strings = c("NA", "-") )  )
   names(prj) <- names(prj)[-1]
   prj <- prj[, -length(prj)]
   ## remove extra column name "Sequencing.Status"
   names(prj) <- names(prj)[-6] 
   prj <- prj[ ,-length(prj)]

   if (ncol(prj) != 20) {stop("Eukaryotic genome table has changed - one or more columns have been added", call.=FALSE)}
   
   ## column names
   names(prj) <-  c("pid", "name", "group", "subgroup", "taxid", "size", "chromosomes",
                  "status","method", "depth", "released", "center", "genbank", "pubmed",
                  "refseq","gene","traces","blast", "mapview", "ftp")
   
   prj$released <- as.Date(prj$released,"%m/%d/%Y")
   ## change traces to character vector to match blast, gene, other columns
   prj$traces <- substr(as.character(prj$traces), 1, 1)

   ## re-order by name
   prj <- prj[order(prj$name), ]
   rownames(prj) <- 1:nrow(prj)

   ## put id, name, status, released in first 4 columns (for printing)
   prj <- prj[,c(1,2,8, 11, 3:7,9,10, 12:20)]
   
   #  ASCII characters required for package datasets
   if( capabilities("iconv")){
      isLatin1 <- apply(prj, 2, function(y) any( is.na(iconv(y, "latin1","ASCII")))) 
      isLatin1 <- names(isLatin1)[isLatin1]
      if(length(isLatin1) > 0){
         for(i in 1:length( isLatin1 )){
            prj[,isLatin1[i] ] <- latin2char(prj[,isLatin1[i]])
         }
      }
   }
   
   ## attributes
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url")    <- url
   attr(prj, "date")   <- Sys.Date()
   attr(prj, "stats")  <- paste( dim(prj)[1], "rows in" , round(dtime[3],1), "seconds")
   attr(prj, "update") <- "updateleuks()"
   
   
   prj
}

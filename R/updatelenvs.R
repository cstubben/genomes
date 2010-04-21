updatelenvs <- function()
{
    
    url <- "http://www.ncbi.nlm.nih.gov/genomes/lenvs.cgi?dump=selected"  
    dtime <- system.time(prj <- read.delim(url, skip = 1, stringsAsFactors = FALSE, 
        na.strings = c("NA", "-") ))  
    names(prj) <- names(prj)[-1]
    prj <- prj[ , -length(prj)]

    
    if (ncol(prj) != 10) {stop("Metagenomes table has changed - one or more columns have been added", call.=FALSE)}
    names(prj) <- c("parent", "pid", "name", "type", "source", 
        "accession", "released", "center", "blast", "traces")

   ## re-order by name
   prj <- prj[order(prj$name), ]
   rownames(prj) <- 1:nrow(prj)

   ## put id, name, released, source in first 4 columns (for printing)
   prj <- prj[,c(2,3,7,5,4,6,1,8:10)]
   
    prj$released <- as.Date(prj$released, "%m/%d/%Y")
    prj$traces   <- substr(as.character(prj$traces), 1, 1)
    prj$type     <- factor(as.character(prj$type), labels = c("E","O"))

   #  ASCII characters required for package datasets
   # prj$name <- latin2char(prj$name)
   # or check all columns
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
    attr(prj, "url")   <- url
    attr(prj, "date")  <- Sys.Date()
    attr(prj, "stats") <- paste(dim(prj)[1], "rows in", round(dtime[3], 
        1), "seconds")
    ## attr(prj, "oldnames") <- names(prj)
    attr(prj, "update") <-"updatelenvs()"

    
    prj
    
}

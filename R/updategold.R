updategold <- function()
{      
   ## goldtable.xls is actually a tab delimited file
   url <- "http://www.genomesonline.org/DBs/goldtable.xls"
   dtime <- system.time( prj <- read.delim(url, quote="", stringsAsFactors=FALSE )  )

  # 
  # if (ncol(prj) != 105) {stop("Gold table has changed - one or more columns have been added", call.=FALSE)}
   
  # column names
   names(prj) <- tolower(names(prj))
   names(prj) <- gsub("ncbi.", "", names(prj))  ## remove ncbi prefix
   names(prj) <- gsub("\\.$",  "", names(prj))  ## remove trailing dot
   
  # genomes table should have name, released, and status columns
  names(prj)[names(prj) == "organism.name"]     <- "name"
  names(prj)[names(prj) == "completion.date"]   <- "released"
  names(prj)[names(prj) == "sequencing.status"] <- "status"

   ## remove trailing white spaces in 1000+ names 
   prj$name <- sub(' +$', '', prj$name)
   ## or ~4 leading spaces   grep("^ +", gold$name, value=TRUE)
   prj$name <- sub('^ +', '', prj$name)
   ## replace two spaces with one
   prj$name <- sub('  ', ' ', prj$name)
   prj$name <- sub('  ', ' ', prj$name)

   # lower-case domain names
   prj$domain <- tolower(prj$domain)
   ## superkingdom errror (and other ranks)
   prj$superkingdom[ prj$superkingdom=="Error!!!"] <- ""
   # One "Incomplete" - change to in progress???
   prj$status[prj$status=="Incomplete"] <- "In progress" 
   # dates
   prj$released[prj$released==""] <- NA
   prj$released <-  as.Date(prj$released)

   ## some names missing...
   prj$name[prj$name==""]<-prj$common.name[prj$name==""]
   
   ## order gold table
   prj <- prj[order(prj$name),]
   rownames(prj) <- 1:nrow(prj)

   ## put id, name, status, released in first 4 columns (for printing)
   x<-names(prj)
   n1<-match( c("goldstamp", "name", "status", "released") , x)
   n2<- which( !(1:ncol(prj) %in% n1)) 
   prj <- prj[,c(n1, n2)]
   

   #attributes
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url")    <- url
   attr(prj, "date")   <- Sys.Date()
   attr(prj, "stats")  <- paste(dim(prj)[1], "rows in" , round(dtime[3], 1), "seconds")
   attr(prj, "update") <- "updategold()"
   prj

}

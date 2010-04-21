updatelproks <- function()
{      
   #--------------------------------------------------#   
   # DOWNLOAD ALL three views (0,1, and 2)
   url1 <- "http://www.ncbi.nlm.nih.gov/genomes/lproks.cgi?dump=selected&view="
   ## create list of three tables (Org Info, Complete, and In Progress = tab 0,1, and 2 on web page)
   prj <- vector("list", 3)
   dtime <- system.time( 
     for (i in 1:3) {
        url2 <- paste(url1, i-1, sep="")  
       
        prj[[i]] <- read.delim(url2, skip=1, stringsAsFactors=FALSE, na.strings = c("NA", "-")   ) 
        # header row has an extra field called "## Columns:", so column names are shifted to the right
        # and the table includes an extra column that is filled with missing values (NA)
        names(prj[[i]]) <- names(prj[[i]])[-1]
        ## remove final column with NAs
        prj[[i]] <- prj[[i]][,-ncol(prj[[i]])]
     }
   )
   #--------------------------------------------------#
   # FIXES, most likely number of columns will change in the future  
   # Complete
   # Dec 2008 - GenBank accessions added
   if (ncol(prj[[2]]) != 15) {stop("Complete Genome table has changed - one or more columns have been added", call.=FALSE)}
   names(prj[[2]]) <-  c("pid", "taxid", "name", "kingdom", "group", "size", "GC", "chromosomes",
                    "plasmids", "released", "modified", "genbank", "refseq", "publication", "center")
   
   # format DATES
   prj[[2]]$released <- as.Date(prj[[2]]$released, "%m/%d/%Y")
   prj[[2]]$modified <- as.Date(prj[[2]]$modified, "%m/%d/%Y")
   # publications are separated by &uid= , replace with comma
   prj[[2]]$publication  <-  gsub("&uid=", ",", prj[[2]]$publication)

   # In progress
   if (ncol(prj[[3]]) != 14) {stop("In Progress table has changed - one or more columns have been added", call.=FALSE)}
   names(prj[[3]]) <-  c("pid", "taxid", "name", "kingdom", "group", "status", "accession",
                 "contigs", "cds", "size", "GC", "released", "center", "url")

   # Dates have 2 digit year (%y)
   prj[[3]]$released <- as.Date(prj[[3]]$released,"%m/%d/%y")
   if(ncol(prj[[1]]) != 20){stop("Organism Info table has changed - one or more columns have been added", call.=FALSE)}
   # Organism info
   names(prj[[1]]) <-  c("pid", "taxid", "name", "kingdom", "group", "status", "size", "GC", "gram", "shape", "arrange",
                   "endospore", "motility", "salinity", "oxygen", "habitat", "temp", "range", "pathogen" ,"disease")
   #--------------------------------------------------#   
   # join tabs into one large table   
   # Keep column order from Complete - 15 columns
   x <- data.frame(prj[[2]], status="Complete", contigs=apply(prj[[2]][,8:9], 1, sum, na.rm=TRUE),
                    cds=NA, url=NA,      stringsAsFactors=FALSE)

   ## add In Progress - 4 additional columns = status contigs cds url

   ## status column is not reliable  - use accession  (fixed apr 2010)
   prj[[3]]$status <- "In Progress"
   n<-prj[[3]]$accession %like% 'NZ*'
   prj[[3]]$status[n] <- "Assembly"
   ## even a few NC*
   n<-prj[[3]]$accession %like% 'NC*'
   prj[[3]]$status[n] <- "Complete"
   
 
   y <- data.frame(prj[[3]][, c(1:5,10:11)], chromosomes=NA,plasmids=NA, released=prj[[3]][,12], modified=NA, genbank=NA,
                 refseq=prj[[3]][,7], publication=NA, prj[[3]][,c(13, 6, 8, 9, 14)], stringsAsFactors=FALSE)
   ## combine Complete and In progress
   z <- rbind(x, y)
   ## merge Org Info - 12 columns
   z <- merge(z, prj[[1]][, c(1,9:20)], all.x=TRUE)

   ## re-order by name
   prj <- z[order(z$name), ]
   rownames(prj) <- 1:nrow(prj)

   ## put id, name, status, released in first 4 columns (for printing)
   prj <- prj[,c(1,3, 16, 10, 2,4:9,11:15, 17:31)]

   ## extra spaces in center (and probably others)
   lproks$center<-gsub("  ", " ", lproks$center)
   lproks$center<-gsub("  ", " ", lproks$center)

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
   
   #--------------------------------------------------#      
   # SET attributes
   ## lproks class first...
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url") <- strsplit(url1, "\\?")[[1]][1]
   attr(prj, "date") <- Sys.Date()
   attr(prj, "stats")  <- paste( dim(prj)[1], "rows from 3 tables in" , round(dtime[3],1), "seconds")
   attr(prj, "update") <- "updatelproks()"
   prj
}



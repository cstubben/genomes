updatevirus <- function()
{  
   url <- "http://www.ncbi.nlm.nih.gov/genomes/GenomesGroup.cgi?taxid=10239&opt=Virus&sort=genome&cmd=download"
   dtime <- system.time( prj <-  read.table(url, sep="\t", fill=TRUE, as.is=TRUE, quote="") )

   # feb 2011 - host column added... need better checks.  

   ##columns 5 and 12 are empty
   prj <- prj[,c(1:4,6:11)]  
   ## column names
   names(prj) <- c( "name", "refseq",   "isolate",  "segments" ,
                  "size", "proteins", "neighbors", "host", "released", "updated")
   ## Release and updated date
   prj$released <- as.Date(prj$released, "%m/%d/%Y")
   prj$updated <- as.Date(prj$updated, "%m/%d/%Y")
   
   #  Virus table has segment details under expanded nodes  [-]  (closed by default on web page)
   #   refseq like NC*  - 1 segment
   #   refseq = '-'     - 2 or more segments
   #   refseq = ''      - segment details (segments are space delimited, so all data in 1st column)

   ### get accessions and sizes for mulit-segmented viruses
   n <- grep("^-$", prj$refseq)
   ## total segments (are segment counts ever wrong?)
   n2 <- as.numeric( prj$segments[n])
   n2 <- n2 + n
   ## loop through segments
   for( i in 1:length(n) )
   {
      x <- prj[ (n[i] +1 ) : n2[i],  1 ] 
      ## get ACCESSION numbers
      acc<-gsub("(.*)(NC_\\d*)(.*)", "\\2", x, perl=TRUE)
      ## get SIZEs
      sizes<-gsub("(.*?)(\\d+)( nt.*)", "\\2", x, perl=TRUE)
      ## add comman separated lists back to table
      prj$refseq[ n[i] ]<- paste(acc, collapse=",")
      prj$size[ n[i] ]<- paste(sizes, collapse=",")
   }

   ## drop segment details 
   prj <- subset(prj, prj$refseq != "")
     

   # segments=="-" are 1 segment
   prj$segments[prj$segments == "-"] <- 1
   class(prj$segments) <- "numeric"
   
   # neighbors =="-" are zero
   prj$neighbors[prj$neighbors == "-"] <- 0
   class(prj$neighbors) <- "numeric"

   #REMOVE nt from size 
   prj$size <- gsub(" nt", "", prj$size)

   ## proteins with "-"   - change to zero???
   prj$proteins[prj$proteins == "-"] <- 0
   class(prj$proteins) <- "numeric"

    ## trailing space on isolate names
   prj$isolate <- gsub(" $", "", prj$isolate)
   
   ## re-number rows (already alphabetical)
   row.names(prj) <- 1:nrow(prj)
   ## put  name, released in first 2 columns (for printing)
   prj <- prj[,c(1,9,7,4,2,3,5,6,8, 10)]
   
   ## attributes
   class(prj) <- c("genomes", "data.frame")
   attr(prj, "url")    <- url
   attr(prj, "date")   <- Sys.Date()
   attr(prj, "stats")  <- paste( dim(prj)[1], "rows in" , round(dtime[3],1), "seconds")
   attr(prj, "update") <- "updatevirus()"
   prj
}

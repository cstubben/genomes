term2neighbor<-function(term, derived = FALSE, sortdate = FALSE, fulltable = FALSE)
{
   db  <- "genome"
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   ## E-search 
   esearch <- paste(url, "esearch.fcgi?retmax=1&tool=term2neighbor.R&email=stubben@lanl.gov&usehistory=y&db=", db, "&term=", 
        gsub(" ", "%20", term), sep = "")
   gp <- readLines(esearch)
   ## parse line 3 
   ## <eSearchResult><Count>1</Count><RetMax>1</RetMax><RetStart>0</RetStart>
   ## <QueryKey>1</QueryKey><WebEnv>NCID_1_1277409_130...</WebEnv>
   x <- gsub("</[^>]*>", "-!-", gp[3])  # replace end tags </.*> with a delimiter...
   x <- gsub("<[^>]*>", "", x)          # replace start tags
   x <- unlist(strsplit(x, "-!-"))      # split into 5 fields (count, retmax, retstart, querykey, webenv)
   ## count in x[1]
   if (!x[1] > 0) {stop("No matches to ", term, " found",  call.=FALSE)}
   querykey <- x[4]
   webenv   <- x[5]

   
   # GET  Neighbors in  genome_nuccore_samespecies  

   ## E-Link using neighbor history
   elink <- paste(url, "elink.fcgi?dbfrom=", db,
           "&db=nuccore&cmd=neighbor_history&linkname=genome_nuccore_samespecies&query_key=", 
            querykey, "&WebEnv=", webenv, sep = "")
   gp2 <- readLines(elink)
   ## lots of \t
   gp2 <- gsub("\t", "", gp2)
   ## always 16 elements? then query key in row 12 and webenv in row 14...
   n1 <- grep("<QueryKey>", gp2)
   ## no matches if no query key???
   if (length(n1) == 0) {stop("No neighbors matching ", term, " found",  call.=FALSE)}
   n2 <- grep("<WebEnv>", gp2)
   querykey2 <- gsub("<[^>]*>", "", gp2[n1])
   webenv2   <- gsub("<[^>]*>", "", gp2[n2])

   ## E-summary
   esum <- paste(url, "esummary.fcgi?db=nuccore&query_key=", querykey2, "&WebEnv=", webenv2, sep = "")
   gp3 <- readLines(esum)

   ##  PARSE XML
   doc <- xmlRoot(xmlTreeParse(gp3,  useInternalNodes = TRUE))
   ## get values for Id and 18 Items
   x1 <- xmlSApply(doc, function(x) xmlSApply(x, xmlValue))
   #transpose table
   x1 <- t(x1)

   ## also get genbank sequences that references were derived from in genome_nuccore
   ## uses a delimited list of ids in url string 
   if (derived) {
      ## E-Link 
      elink<-paste(url, "elink.fcgi?dbfrom=", db, "&db=nuccore&cmd=neighbor&linkname=genome_nuccore&query_key=", 
                   x[4], "&WebEnv=", x[5], sep = "")
      gp2 <- readLines(elink)
      ## get ids after Dbto
      n   <- grep("<DbTo>", gp2)[1]
      ids <- grep("<Id>", gp2)
      ## only get rows after dbTo (and not ids  from DbFrom)
      ids <- gp2[ids[ids>n] ]
      ids <- gsub("\t", "", ids)    # remove \t
      ids <- gsub("<[^>]*>", "", ids) #  remove tags
      ids <- paste(ids, collapse=",") # comma spearate list...
  
      ## E-summary using list of ids (not sure what max ids is??)
      esum <- paste(url, "esummary.fcgi?db=nuccore", "&id=", ids, sep = "")
      gp3 <- readLines(esum)
      doc <- xmlRoot(xmlTreeParse(gp3,  useInternalNodes = TRUE))
      ## get values for Id and 18 Items
      x2 <- xmlSApply(doc, function(x) xmlSApply(x, xmlValue))
      #transpose table
      x2 <- t(x2)
      ## add column to flag dervied vs neighbors?
      x1<-rbind(x1,x2)
    }

   rownames(x1) <- 1:nrow(x1)
   colnames(x1) <- c("gi", "acc", "name" ,    "defline" , "gi2", "released", 
        "updated", "flags", "taxid" , "size", "status", "replace", "comment")

   # convert to data frame
   x2 <- as.data.frame(x1, stringsAsFactors=FALSE)
   # format dates
   x2$released <- as.Date(substr(x2$released, 1,10), "%Y/%m/%d")
   x2$released[x2$released < "1970-1-1"] <- NA

   if (!fulltable) {
      # acc, name, released (status always = live), taxid, size
       x2<-   x2[, c(2,3, 6, 9,10)]    
   }

   # size and taxid are characters
   x2$size  <- as.numeric(x2$size)
   x2$taxid <- as.numeric(x2$taxid)
   
    ## sort by release date? 
   if (sortdate) {
      x2 <- x2[ order(x2$released, x2$name),]
   } else {
      x2 <- x2[ order(x2$name),]
   }
   rownames(x2) <- 1:nrow(x2)
   ##  add class
   class(x2) <- c("genomes", "data.frame")
   ## save date for updates -then save term??
   attr(x2, "date") <- Sys.Date()
   attr(x2, "term") <-term
   attr(x2, "update") <- paste("term2neighbor(\"", term, "\", ",
                           derived, ", ", sortdate, ", " , fulltable, ")", sep="")
   x2
}

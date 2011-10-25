ncbiTaxonomy <- function(term, results="summary", full=FALSE) 
{
   #check if numbers passed as character - 2 or "2" - either need to epost "2" or change to "2[UID]"
   if(!any( is.na(suppressWarnings(as.numeric(term)))) ){term <-as.numeric(term)}
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   # use EPost if term is a numeric vector 
   if (is.numeric(term) ) {   
       epost <- paste(url, "epost.fcgi?&db=taxonomy&id=", paste(term, collapse=","),sep="" )
       gp <- readLines(epost)
    } else{ 
       if(length(term) >1) term  <- paste(term, collapse = " OR ")
       esearch <- paste(url, "esearch.fcgi?db=taxonomy&usehistory=y&term=",  gsub(" ", "%20", term ) , sep = "")
       gp <- readLines(esearch) 
    }
    # CHECK FOR errors
    connecterror <- grepl("ERROR", gp)
    if (any(connecterror)) {
        # R CMD check on bioconductor may fail if using stop() 
        print(paste("Error connecting to NCBI ", gp[connecterror]))
    #-----------------------------------------------------------------------
    # PARSE Query key and web env
    }else{
        # EPost returns query key in line 4 and web env in line 5
        if ( is.numeric(term) ) {
          x<-gsub("<[^>]*>", "", gp)
          x <- gsub("\t", "", x)
        }else{
           # Esearch returns all results on line 3.  Parse into 5 fields: count, retmax, retstart, querykey, webenv
           x <- unlist( strsplit( gp[3], "<[^>]*>" ))
           x <- x[x != ""]
           if (!x[1] > 0) { stop("No matches to ", term, " found") }
        }
        #-------------------------------------------------------------------
        ## ESEARCH
        if(results=="summary"){
           esum <- paste(url, "esummary.fcgi?db=taxonomy&query_key=", 
                x[4], "&WebEnv=", x[5], sep = "")
           gp  <- readLines(esum)
           doc <- xmlRoot(xmlParse(gp))
           z   <- getNodeSet(doc, "//DocSum")
           # EPost does not report counts, so check here for no results
           if( length(z)==0 ) { stop("No matches found") }
           x <- sapply(z, function(x) xmlSApply(x, xmlValue))
           x <- data.frame(t(x), stringsAsFactors = FALSE)
           names(x) <- c("id", "rank", "division", "name", "common", "taxid", "nucl", "prot", "struct", "genome", "gene", "genus", "species", "subsp")
           # change counts to numeric
           x[c(1,6:11)] <- apply( x[c(1,6:11)], 2, as.numeric)
           x <- x[order(x$name),]
           rownames(x) <- NULL
           if(full){
              x
           } else{ 
              x <- x[, c(6,4,2,3,7:11)]
              x  
           } 
        #-------------------------------------------------------------------
        ## EFETCH  see http://eutils.ncbi.nlm.nih.gov/corehtml/query/static/efetchtax_help.html   
        }else{
           efetch <- paste(url, "efetch.fcgi?db=taxonomy&report=xml&query_key=", 
                x[4], "&WebEnv=", x[5], sep = "")
           # warning about incomplete final line 
           gp <- suppressWarnings( readLines(efetch) )
           # REMOVE html tags mixed with xml 
           gp <- gsub("&lt;", "<", gp)
           gp <- gsub("&gt;", ">", gp)
           gp <- gp[-(1:2)]
           gp <- gp[-length(gp)]
           gp <- gsub("&quot;", "\"", gp)

           doc <- xmlRoot(xmlParse(gp))

           z <- getNodeSet(doc, "//TaxaSet/Taxon")
           if( length(z)==0 ) { stop("No matches found") }

           x<- lapply(z, function(x) xmlSApply(x, xmlValue))
           ## a1<-xpathSApply(doc, "//TaxaSet/Taxon/TaxId", xmlValue)
           a1 <- sapply(x, "[", "TaxId")
           a2 <- sapply(x, "[", "ScientificName")
           a3 <- sapply(x, "[", "Rank")
           a4 <- sapply(x, "[", "Lineage")
           a5 <- sapply(x, "[", "CreateDate")
           x  <- data.frame(taxid=as.numeric(a1), name=a2, rank=a3, lineage=a4, created=as.Date(a5), stringsAsFactors=FALSE)
           x  <- x[order(x$name),]
           rownames(x) <- NULL
           x
        }
    }
}

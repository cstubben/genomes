enaSRA <-function(tax, limit = 5000)
{
   if( length(tax) > 1){tax <- tax[1]; print("Warning: only the first id will be used")}
   #check if number passed as character, eg  2  = "2"
   if(!is.na(suppressWarnings(as.numeric( tax)))){ tax <-as.numeric(tax) }
   if(!is.numeric(tax)){ tax <- tax2id(tax)  }
   url1 <- "http://www.ebi.ac.uk/ena/data/view/Taxon:"
   url2 <- paste( "&portal=sra_sample&limit=", limit, "&subtree=true&display=xml", sep="")
   url  <- paste(url1, tax, url2, sep="")
   x <- readLines(url)
     connecterror <- grepl("ERROR", x)
     if (any(connecterror)) {
        print(paste("Error connecting to NCBI ", x[connecterror]))
     }
   # IF no result using display =xml , then 2 lines returned with request in root tag
   if(length(x) == 2 ){ print("Result not found.") 
   } else{
    doc <- xmlParse(x)
    ###   CREATE LOOP
    z <- getNodeSet(doc, "//SAMPLE")
    n <- length(z)
    sra <- vector("list", n)
    for (i in 1:n) {
        z2 <- xmlDoc(z[[i]])
        taxid <- as.numeric( xvalue(z2, "//TAXON_ID") )
        name <- xvalue(z2, "//SCIENTIFIC_NAME")
        title <- xvalue(z2, "//TITLE")
        title <- gsub("([^;]*).*", "\\1", title)   # multiple titles ?
        # use title if scientific name is missing or keep separate?
        if(is.na(name)) name <- title
        sample  <- xattr(z2, "//SAMPLE", "accession")
        alias   <- xattr(z2, "//SAMPLE", "alias")
        alias <- gsub(name, "", alias)  # alias is often duplicate of name
        center <- xattr(z2, "//SAMPLE", "center_name")
        center <- gsub("([^,]*).*", "\\1",center)    # multiple centers?
        study      <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-STUDY")
        submission <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-SUBMISSION")
        experiment <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-EXPERIMENT")
        #run       <- xtags(z2, "//XREF_LINK", "DB", "ID", "ENA-RUN")
        bases  <- as.numeric( xtags(z2, "//SAMPLE_ATTRIBUTE", "TAG", "VALUE", "ENA-BASE-COUNT") )
        reads  <- as.numeric( xtags(z2, "//SAMPLE_ATTRIBUTE", "TAG", "VALUE", "ENA-SPOT-COUNT") )
        sra[[i]] <- data.frame(taxid, name, alias, sample, submission, study, experiment, center, bases, reads, stringsAsFactors = FALSE)
        free(z2)
    }
    sra <- do.call("rbind", sra)
    sra <- sra[order(sra$name, sra$alias),]
    rownames(sra) <- NULL
    sra
  } 
}

# enaSRA requires these 3 functions to loop through samples (returning NA if missing) and MATCH:

# 1) values matching tag
#     <TAXON_ID>520450</TAXON_ID>

xvalue <- function(doc, tag) {
        n <- xpathSApply(doc, tag, xmlValue)
        if ( length(n)>0 ) 
           ## if multiple values, return first (for pubmed) or paste?
           n[1]  
           #paste(n, collapse=",")
        else NA
    }

# 2) attributes within tags, for example, find the center within the SAMPLE tag
# <SAMPLE accession="SRS000899" center_name="Broad Institute, Cambridge, MA, USA" alias="24604.0"> 


xattr <-function(doc, tag, att) {
      y <- xpathApply(doc, tag, xmlAttrs)
     # no matches to tag (give warning)
      if(length(y)==0){ 
         print(paste("Warning: no matches to", tag))
         NA 
     } else{
      if( att %in% names(y[[1]]) )
         y[[1]][[att]]
      else NA
     }
}


# 3) values within REPEATED tags, for example, find the ID associated with DB=ENA-STUDY in the XREF_LINK tag 

#    <XREF_LINK>
#        <DB>ENA-STUDY</DB>
#        <ID>SRP000850</ID>
#    </XREF_LINK>
#    <XREF_LINK>
#        <DB>ENA-EXPERIMENT</DB>
#        <ID>SRX001146</ID>
#    </XREF_LINK>

xtags <- function(doc, tag, subtag1, subtag2, value1 ) {
         n <- xpathSApply(doc, paste(tag, subtag1, sep="/"), xmlValue)==value1
         if ( any(n) ) 
             xpathSApply(doc, paste(tag, subtag2, sep="/"), xmlValue)[n] 
         else NA
}




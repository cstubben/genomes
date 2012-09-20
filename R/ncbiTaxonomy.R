ncbiTaxonomy <- function(term, summary=TRUE) 
{
   if(length(term)>1){
      if(any( is.na(suppressWarnings(as.numeric(term)))) ){
         term <- paste(term, collapse=" OR ") ## NAMES
      }else{
         term <- paste(term, collapse=",")    #IDs
      }
   }

   # CHECK if IDs (and skip esearch)
   if( grepl("^[0-9, ]*$", term)){
      if(summary){ 
         x <- esummary(term, "taxonomy")
      }else{ 
        # suppress warning about incomplete final line in XML file
         x <- suppressWarnings( efetch( term, "taxonomy", rettype="xml"))
      }
   ## SEARCH terms
   }else{
      if(summary){ 
         x <- esummary(esearch( term, "taxonomy"))
      }else{ 
         x <- suppressWarnings( efetch( esearch( term, "taxonomy"), rettype="xml"))
      }
   }
   if(summary){
       ## sep 19, 2012 - esummary output columns changed from 14 to 12 (remove counts of nucl, protein, gene, genomes)
       ## check if empty data.frame... problems with esummary
       if(nrow(x)>0){
           # check if 12 columns ... changes to esummary output
           if(ncol(x)==12){
              names(x) <- c("id", "status", "rank", "division", "name", "common", "taxid", "akataxid", "genus", "species", "subsp", "modified")
              # change taxid to numeric
              x[,7] <- as.numeric(x[,7])
              # modified date
              x[,12] <- as.Date(substr(x[,12], 1,10))
              x <- x[, c(7,5,3,4, 9:11, 6,2,12)]   # subset?
           }
        }
        x  
    ## Efetch
   }else{
        z <- xmlParse(x)
       # TaxId, ScientificName and Rank are repeated within LineageEx
        taxid    <- as.numeric(xpathSApply(z, "/TaxaSet/Taxon/TaxId", xmlValue))
        name     <- xpathSApply(z, "/TaxaSet/Taxon/ScientificName", xmlValue)
        parentid <- as.numeric(xpathSApply(z, "//ParentTaxId", xmlValue))
        rank     <- xpathSApply(z, "/TaxaSet/Taxon/Rank", xmlValue)
        lineage  <- xpathSApply(z, "//Lineage", xmlValue)

        data.frame(taxid, name, parentid, rank, lineage, stringsAsFactors=FALSE)



   }
}

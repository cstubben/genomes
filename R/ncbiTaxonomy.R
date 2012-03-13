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
   }else{
      if(summary){ 
         x <- esummary(esearch( term, "taxonomy"))
      }else{ 
         x <- suppressWarnings( efetch( esearch( term, "taxonomy"), rettype="xml"))
      }
   }
   if(summary){
       names(x) <- c("id", "rank", "division", "name", "common", "taxid", "nucl", "prot", "struct", "genome", "gene", "genus", "species", "subsp")
        # change counts to numeric
        x[c(1,6:11)] <- apply( x[c(1,6:11)], 2, as.numeric)
        x <- x[, c(6,4,2,3,7:11)]   # subset?
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

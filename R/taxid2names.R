taxid2names<-function(ids)
{
   n <- length(ids)
   if(n > 500){ stop("Please use < 500 taxonomy ids")}
   url <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
   efetch <- paste(url, "efetch.fcgi?db=taxonomy&tool=taxid2names.R&email=stubben@lanl.gov&report=xml&id=", paste(ids, collapse=","), sep = "")
   gp <- readLines(efetch, warn=FALSE)
   ## parse taxids (in case some ids are not valid)
   n0 <- grep("^  &lt;TaxId&gt", gp)
   x  <- gsub("(.*TaxId&gt;)(.*)&lt;/TaxId&gt;", "\\2", gp[n0])
   #lineage
   n1 <- grep(";Lineage&gt", gp)
   y  <- gsub("(.*Lineage&gt;)(.*)&lt;/Lineage&gt;", "\\2", gp[n1])
   #names
   n2 <- grep("^&lt;Taxon", gp)
   z  <- gsub("(.*)&gt;(.*)&lt;(.*)gt;", "\\2", gp[n2+2])
   if(length(x) != length(y) | length(x) != length(z)) { stop("Cannot parse all names and lineages from XML report")}
    data.frame(taxid = as.numeric(x), name = z, lineage = y, stringsAsFactors = FALSE)
}

enaTaxonomy<-function(tax, h=TRUE, round=0 )
{
   if( length(tax) > 1){tax <- tax[1]; print("Warning: only the first id will be used")}
   #check if number passed as character, eg  2  = "2"
   if(!is.na(suppressWarnings(as.numeric( tax)))){ tax <-as.numeric(tax) }
   # if character name, use NCBI to lookup taxid
   if(!is.numeric(tax)){ tax <- tax2id(tax)  }

   # URL returns tab-delimited file
   url <- paste("http://www.ebi.ac.uk/ena/data/stats/taxonomy", tax, sep="/")
   x   <- read.table(url, sep="\t", row.names=1)
   x   <- x[c( 1,5,2,4,7,3,6,8) , c(1,3,2,4)]  # re-order like web
   colnames(x) <- c("direct",  "size" , "subtree" ,  "subsize")
   if(h){
      x$size    <- bases(x$size, round=round)
      x$subsize <- bases(x$subsize, round=round)
   }
   x
}

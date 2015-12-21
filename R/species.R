species <- function(x ){
   # remove brackets and single quotes
   x <- gsub("\\[|\\]|'", "", x)
   ## remove Candidate species
   x <- gsub("candidate |Candidatus ", "", x)
   y  <- strsplit(x, "\\s+")
   ge <- sapply(y, "[", 1)
   sp <- sapply(y, "[", 2)
   paste(ge, sp)
}




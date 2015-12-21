genus <- function(x) {
   
   # remove brackets and single quotes
   x <- gsub("\\[|\\]|'", "", x)
   x <- gsub("candidate |Candidatus ", "", x) ## remove Candidate species??? 
   y <- strsplit(x, "\\s+")
   sapply(y, '[', 1) 
}


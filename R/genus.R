genus <- function(x) {
  x <- gsub("'", "", x)   # remove quotes from 'Nostoc azollae' 0708
  x <- gsub("Candidatus ", "", x) ## remove Candidate species??? 
  y <- strsplit(x, "\\s+")
  sapply(y, '[', 1) 
}


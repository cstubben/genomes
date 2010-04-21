species <- function(x, abbrev=FALSE, epithet=FALSE){
  x  <- as.character(x)
  x  <- gsub("'", "", x) # remove quotes from 'Nostoc azollae' 0708
  x  <- gsub("Candidatus ", "", x) ## remove Candidate species??? 
  y  <- strsplit(x, "\\s+")
  ge <- sapply(y, "[", 1)
  sp <- sapply(y, "[", 2)
  # if specific epithet only
  if (epithet) {
    sp
  } else {
    if (abbrev) {
      ## dont abbreviate unknown species
      n <- sp != "sp."
      ge[n] <- substr(ge[n], 1, 1)
      paste(ge, ". ", sp, sep="") 
    } else {
      paste(ge, sp)
    }
  }
}




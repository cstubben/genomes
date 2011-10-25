abbrev<-function(x)
{  
    x <- gsub("Candidatus ", "", x) # or abbreviate to Ca.
    ## dont abbreviate unknown species
     n<- !(grepl(" sp.", x, fixed=TRUE) | is.na(x))
    x[n] <- paste(substring(x[n], 1, 1), ". ", substring(x[n],regexpr(" ", x[n]) + 1), sep="")
    # shorten biovar/serovar?
    x <- gsub(" biovar ", " bv. ", x)
    x <- gsub(" serovar ", " sv. ", x)
    x
}

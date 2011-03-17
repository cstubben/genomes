abbrev<-function(x)
{
    n<-!is.na(x)
    x <- gsub("'", "", x)   # remove quotes from some taxa 'Nostoc' 
    x <- gsub("Candidatus ", "", x) # or abbreviate to Ca. 
    x[n] <- paste(substring(x[n], 1, 1), ". ", substring(x[n],regexpr(" ", x[n]) + 1), sep="")
    # shorten biovar/serovar?\
    x <- gsub(" biovar", " bv.", x)
    x <- gsub(" serovar", " sv.", x)
    x
}

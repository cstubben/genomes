abbrev<-function(x)
{
    x <- gsub("'", "", x)   # remove quotes from some taxa 'Nostoc' 
    x <- gsub("Candidatus ", "", x) # or abbreviate to Ca.
    x <- paste(substring(x, 1, 1), ". ", substring(x,regexpr(" ", x) + 1), sep="")
    # shorten biovar/serovar?\
    x <- gsub(" biovar", " bv.", x)
    x <- gsub(" serovar", " sv.", x)
    x
}

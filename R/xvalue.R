# enaSRA requires these 3 functions to loop through samples (returning NA if missing) and MATCH:

# 1) values matching tag
#     <TAXON_ID>520450</TAXON_ID>

xvalue <- function(doc, tag) {
        n <- xpathSApply(doc, tag, xmlValue)
        if ( length(n)>0 ) 
           ## if multiple values, return first (for pubmed) or paste?
           n[1]  
           #paste(n, collapse=",")
        else NA
    }

# 2) attributes within tags, for example, find the center within the SAMPLE tag
# <SAMPLE accession="SRS000899" center_name="Broad Institute, Cambridge, MA, USA" alias="24604.0"> 


xattr <-function(doc, tag, att) {
      y <- xpathApply(doc, tag, xmlAttrs)
     # no matches to tag (give warning)
      if(length(y)==0){ 
         # print(paste("Warning: no matches to", tag))
         NA 
     } else{
      if( att %in% names(y[[1]]) )
         y[[1]][[att]]
      else NA
     }
}


# 3) values within REPEATED tags, for example, find the ID associated with DB=ENA-STUDY in the XREF_LINK tag 

#    <XREF_LINK>
#        <DB>ENA-STUDY</DB>
#        <ID>SRP000850</ID>
#    </XREF_LINK>
#    <XREF_LINK>
#        <DB>ENA-EXPERIMENT</DB>
#        <ID>SRX001146</ID>
#    </XREF_LINK>

xtags <- function(doc, tag, subtag1, subtag2, value1 ) {
         n <- xpathSApply(doc, paste(tag, subtag1, sep="/"), xmlValue)==value1
         if ( any(n) ) 
             xpathSApply(doc, paste(tag, subtag2, sep="/"), xmlValue)[n] 
         else NA
}


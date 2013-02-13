\name{esearch}
\alias{esearch}
\title{ Entrez database search  }
\description{ Search Entrez databases at NCBI }
\usage{
esearch(term, db = "pubmed", usehistory = "y", parse = TRUE, verbose=TRUE, showURL=FALSE, ...)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms or a vector of accessions}
  \item{db}{ An Entrez database, default pubmed}
  \item{usehistory}{ Save results to History server for subsequent calls }
  \item{parse}{ If false, the XML output is returned }
  \item{verbose }{ Print number of results found }
  \item{showURL }{ Print url string }
  \item{\dots}{ Other key-value pairs passed to esearch url string }
}
\details{ See \code{einfo()} for a list of valid Entrez database names and search fields. If usehistory="n", the 
default number of ids returned is 20 (set a retmax option to increase the default limit).  If 
a vector of accessions are input, the terms are pasted together in a comma-separated list 
for searching by Primary Acession. 

}
\value{ Either an EntrezHistory data.frame listing the database, query_key and WebEnv (default), a vector of
 Ids if usehistory="n", or the raw XML output if parse=FALSE. 
The default EntrezHistory object may be passed directly to the other E-utilities.
}
\references{
 \url{http://www.ncbi.nlm.nih.gov/books/NBK25499}
}
\author{ Chris Stubben }
% \note{ }
%\seealso{ }
\examples{
\dontrun{
# EntrezHistory object
esearch("bioconductor[TITLE]", showURL=TRUE)
# taxonomy IDs
esearch("mouse", db="taxonomy", usehistory="n")
esearch("AE017223 OR ACBJ00000000", db="nuccore")
# comma-separated (or vector) to search Primary accessions
esummary( esearch("AE017223,ACBJ00000000", db="nuccore"))
}
}
\keyword{ methods }

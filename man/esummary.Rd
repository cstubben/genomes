\name{esummary}
\alias{esummary}
\title{ Entrez database summaries }
\description{ Summaries of Entrez database records at NCBI  }
\usage{
esummary(id, db = "pubmed", parse = TRUE, ...)
}
\arguments{
  \item{id}{ An EntrezHistory object or vector of Ids }
  \item{db}{ An Entrez database, default pubmed }
  \item{parse}{ Parse the XML results into a data.frame  }
  \item{\dots}{ Other key-value pairs passed to the esummary url string }
}
%\details{  }
\value{  A data.frame or XML results if parse=FALSE }
\references{ \url{http://www.ncbi.nlm.nih.gov/books/NBK25499} }
\author{ Chris Stubben }
\note{ If EntrezHistory results are the input, then the database listed in that object is used. 
  If using a vector of Ids, the database option must be included.  Also, do not pass more than 200 Ids 
  to the url (use the History or see the NCBI help pages for other suggestions).

 Some records may be missing fields and then constructing a data.frame will return warnings.  
  For example, the DOI field is missing in many Pubmed records. You can also set the version="2.0" to 
  return the version 2.0 ESummary XML.  

}
%\seealso{}
\examples{

\dontrun{
# BioC articles published in the last year
x <- esearch("bioconductor[TITLE]", reldate=360)
y <- esummary(x, version="2.0")
y[, c(1, 42, 6, 3, 8, 10)]

# Y. pestis CO92 refseqs
x <- esearch( "Yersinia pestis CO92[ORGN] AND refseq[FILTER]", "nuccore")
y <- esummary(x)
y[, c(2,3,5,10)]
# Taxonomy database
esummary(esearch("Mouse[Subtree]", db="taxonomy"))
}
}
\keyword{ methods }

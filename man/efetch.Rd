\name{efetch}
\alias{efetch}
\title{  Entrez database downloads }
\description{  Retrieve Entrez database records at NCBI in a variety of formats
}
\usage{
efetch(id, db = "pubmed", rettype = "", retmode = "text", showURL = FALSE, destfile, ...)
}
\arguments{
  \item{id}{ An EntrezHistory object or vector of Ids }
  \item{db}{An Entrez database, default pubmed }
  \item{rettype}{Retrieval type, see note for details  }
  \item{retmode}{Retrieval mode, see note for details  }
  \item{showURL}{ display URL string }
  \item{destfile}{ location to save downloaded file using download.file. If missing, the url is loaded into R using readLines }
  \item{\dots}{ Other key-value pairs passed to the efetch url string, e.g seq_stop }
}
%\details{ }
\value{ A character vector for the given retrieval type and mode. }
\references{ \url{http://www.ncbi.nlm.nih.gov/books/NBK25499 } }
\author{ Chris Stubben }
\note{  See Table 1  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1} 
for a list of valid  retrieval types and modes.

If EntrezHistory results are the input, then the database listed in that object is used. 
  If using a vector of Ids, the database option must be included.  Also, do not pass more than 200 Ids 
  to the url (use the History or see the NCBI help pages for other suggestions).
}
% \seealso{}
\examples{

\dontrun{
# abstracts from recent bioC articles - use ids to limit the number 
x <- esearch("bioconductor[TITLE]", usehistory="n", retmax=5, reldate=360 )
x
efetch(x, rettype="abstract")
# only first 500 bases
efetch( esearch( "Yersinia pestis CO92[ORGN] AND refseq[FILTER] AND plasmid[Filter]", "nuccore"), rettype="fasta", seq_stop=500)
efetch(16082679, "nuccore", "fasta")
}

}
\keyword{ methods }

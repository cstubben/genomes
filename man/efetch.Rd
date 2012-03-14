\name{efetch}
\alias{efetch}
\title{  Entrez database downloads }
\description{  Retrieve Entrez database records at NCBI in a variety of formats
}
\usage{
efetch(id, db = "pubmed", rettype = "", retmode = "text", seq_stop = 700, ...)
}
\arguments{
  \item{id}{ An EntrezHistory object or vector of Ids }
  \item{db}{An Entrez database, default pubmed }
  \item{rettype}{Retrieval type, see Table 1 for details  }
  \item{retmode}{Retrieval mode, see Table 1 for details  }
  \item{seq_stop}{ Last sequence base to retrieve.  The stop is set low to avoid 
     unintentional downloads of large sequences.  Set to NA or an empty string to download the entire sequence.}
  \item{\dots}{ Other key-value pairs passed to the efetch url string }
}
%\details{ }
\value{ A character vector with the number of lines read for the given retrieval type and mode. }
\references{ Table 1 \url{ http://www.ncbi.nlm.nih.gov/books/NBK25499 } }
\author{ Chris Stubben }
\note{  See  \url{http://www.ncbi.nlm.nih.gov/books/NBK25499/table/chapter4.chapter4_table1} 
for a list of valid  retrieval types and modes.

If EntrezHistory results are the input, then the database listed in that object is used. 
  If using a vector of Ids, the database option must be included.  Also, do not pass more than 200 Ids 
  to the url (use the History or see the NCBI help pages for other suggestions).
}
% \seealso{}
\examples{


# abstracts from recent bioC articles - use ids to limit the number 
x <- esearch("bioconductor[TITLE]", usehistory="n", retmax=5, reldate=360 )
x
efetch(x, rettype="abstract")

# Sequence default is 700 sequences
efetch( esearch( "Yersinia pestis CO92[ORGN] AND refseq[FILTER] AND plasmid[Filter]", "nuccore"), rettype="fasta")

# set seq_stop = "" for full sequence
efetch(16082679, "nuccore", "fasta", seq_stop="")

}
\keyword{ methods }

\name{pub2date}
\alias{pub2date}
\title{
Retrieve the published date from NCBI's PubMed database
}
\description{ 
Searches the PubMed database at NCBI and returns a short
citation with author, year, title, journal and published date. 
 }
\usage{
pub2date(pmids)
}
\arguments{
  \item{pmids}{ a vector or comma-separated list of pubmed IDs}
}
\details{
 Searches the Pubmed database using EFetch and parses the XML summary
to return a short citation.
}
\value{
A data.frame with 9 columns: pmid, authors, year, title, journal,
volume, pages, published date, and article date.
}
%\references{}
\author{ Chris Stubben }
\note{ 
The article date is the date an electronic copy was available. See
\code{\link{pubmed}} for additional details about columns.
 }

%\seealso{  }
\examples{
data(lproks)
yp<-subset(lproks, name \%like\% 'Yersinia*CO92')
# comma-separated list
yp$publication
pub2date(yp$publication)
# or vector
pub2date( c(7542800, 7569993))

}

\keyword{methods }

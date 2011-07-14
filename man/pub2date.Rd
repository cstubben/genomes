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
A data.frame with 9 columns
   \item{\code{pmid}}{PubMed id}
    \item{\code{authors}}{first 3 author names}
    \item{\code{year}}{year journal was published }
    \item{\code{title}}{ title}
    \item{\code{journal}}{journal name}
    \item{\code{volume}}{volume number}
    \item{\code{pages}}{pages}
    \item{\code{pubdate}}{ date journal was published (from PubDate tag)}
    \item{\code{artdate}}{ date electronic copy was available (from ArticleDate tag)}

}
%\references{}
\author{ Chris Stubben }
%\note{ }

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

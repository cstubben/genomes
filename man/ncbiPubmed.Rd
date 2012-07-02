\name{ncbiPubmed}
\alias{ncbiPubmed}
\title{
NCBI PubMed database
}
\description{ 
Searches the PubMed database at NCBI and returns a short
citation with author, year, title, journal and published date. 
 }
\usage{
ncbiPubmed(term)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms or a vector of pubmed IDs}
}
\details{
  The function searches the PubMed database and parses the efetch XML summary to return a short citation
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
%\seealso{ }
\examples{
ncbiPubmed( c(7542800, 7569993))
# OR ncbiPubmed("7542800,7569993")
}

\keyword{methods }

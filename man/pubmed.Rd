\name{pubmed}
\alias{pubmed}
\docType{data}
\title{
Complete microbial genome publications in PubMed
}
\description{
Publications for complete microbial genomes in the PubMed database at NCBI
}
\usage{data(pubmed)}
\format{
  A data frame with 747 observations on the following 9 variables.
  \describe{
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
}
\details{
This table was created by taking the \emph{first} pubmed ID in the
\code{\link{lproks}} table (publication column) and using
\code{\link{pub2date}} to return the citation for each unique pubmed ID. 
 In some cases, the genome publication may not be the first pubmed ID in \code{lproks}
 and no attempt was made to correct these rows (except for deleting 4 publications before 1995).  

}
\source{ PubMed database at NCBI}

%\references{ }

\examples{
data(pubmed)

pubmed[1:2,c(1,3,4,8)]

# Streptomyces coelicolor A3(2) should use the second pmid.
# even worse, the release date uses the wrong published date!
data(lproks)
subset(lproks, pid==242, c(1,2,4,16))
pub2date(12000953)



}
\keyword{datasets}

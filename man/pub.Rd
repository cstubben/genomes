\name{pub}
\alias{pub}
\docType{data}
\title{  Complete microbial genome publications }
\description{  Complete microbial genome publications at NCBI }
\usage{data(pub)}
\format{
  A data frame with 1000 observations on the following 10 variables.
  \describe{
    \item{\code{pmid}}{PubMed id}
    \item{\code{date}}{published date }
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
\details{ This file was created by selecting 1160 complete microbial genomes
with publications in the \code{\link{lproks}} table and downloading the unique citations using \code{\link{ncbiPubmed}}.
 The 113 genomes with two or more listed publications were checked to
 identify the likely genome paper from the list of comma-separated pubmed
 IDs (the genome paper was the first pubmed ID in 75 of the 113 projects).
 The published date was added by formatting the pubdate column, except for 237
 papers with only a year listed - in these cases the artdate column was used.
}

\source{  The lproks table at \url{http://www.ncbi.nlm.nih.gov/genomes/lproks.cgi} }
%\references{ }
\examples{
data(pub)
pub[1:2,]
z<-table2(pub$journal, pub$year, n=15)
image2(z[,-ncol(z)], sort=TRUE, mar=c(1,10,3,1), cex=.8, log=TRUE)
}
\keyword{datasets}

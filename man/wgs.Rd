\name{wgs}
\Rdversion{1.1}
\alias{wgs}
\docType{data}
\title{
Bacterial assembly sequences
}
\description{
Bacterial whole genome shotgun (wgs) assemblies in the Entrez Genome database
}
\usage{data(wgs)}
\format{
  A genomes data frame with 1000+ observations on the following 9 variables.
  \describe{
    \item{\code{acc}}{accession number without trailing 00000000}
    \item{\code{name}}{taxonomy name}
    \item{\code{status}}{status, always Assembly}
    \item{\code{released}}{release date}
    \item{\code{size}}{size (bp)}
    \item{\code{proteins}}{proteins}
    \item{\code{rna}}{RNA}
    \item{\code{genes}}{genes}
    \item{\code{updated}}{update date}
  }
}

\details{ See the left side bar at the Genomes home page at 
  \url{ http://www.ncbi.nlm.nih.gov/sites/entrez?db=genome} }
\source{ \url{http://www.ncbi.nlm.nih.gov/genomes/genlist.cgi?taxid=2&type=3}
}
%\references{}

\examples{
data(wgs)
wgs
summary(wgs)[[1]]
data(lproks)
## missing dates in genome project - paste 8 zeros to acc and match to refseq without dates
x <- subset(wgs, paste( wgs$acc, "00000000", sep="")  \%in\% lproks$refseq[is.na(lproks$released)], 
         c(name, released, status))
x
table(format(x$released, "\%Y-\%m"))
## better plot than 
## plotby(lproks, lsort=FALSE)
plotby(rbind(x, lproks[,c("name", "released", "status")]), log='y')
}
\keyword{datasets}

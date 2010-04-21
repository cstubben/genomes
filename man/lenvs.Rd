\name{lenvs}
\alias{lenvs}
\docType{data}
\title{ Metagenome sequencing projects at NCBI}
\description{
  Metagenome sequencing projects from the Entrez genome project at NCBI
}
\usage{data(lenvs)}
\format{
  A genomes data frame with observations on the following 10 variables.
  \describe{
    \item{\code{pid}}{ genome project id }
    \item{\code{name}}{ metagenome title or taxonomy name}
     \item{\code{released}}{released date }
    \item{\code{source}}{ metagenome source }
    \item{\code{type}}{ metagenome type, environmental (E) or
        organismal (O)}
    \item{\code{accession}}{ comma-separated list of accession numbers}
   \item{\code{parent}}{parent genome project id}
    \item{\code{center}}{sequencing center }
    \item{\code{blast}}{ has blast page }
    \item{\code{traces}}{ has traces }
  }
}
%\details{  }
\source{
  downloaded from \url{http://www.ncbi.nlm.nih.gov/genomes/lenvs.cgi} 
}
%\references{ }
\examples{
data(lenvs)
lenvs
## single row
t(lenvs[1,])
plot(lenvs)
summary(lenvs)

}
\keyword{datasets}

\name{genomes-summary}
\alias{genomes-summary}
\alias{summary.genomes}
\title{ Genome table summaries }
\description{
 Generic function for summarizing genome tables
}
\usage{
\method{summary}{genomes}(object, subset, top = 5, \dots)
}

\arguments{
  \item{object}{ a genomes data frame  }
  \item{subset}{logical vector indicating rows to keep }
  \item{top}{ number of recently released genomes to display, default is
    5}
  \item{\dots}{ additional arguments are currently ignored }
}
%\details{}
\value{
A list with 2 or 3 elements:  the total number of
genomes, counts by status (if column is present), and a table listing recent submissions.

}
%\references{  }
\author{ Chris Stubben }
%\note{ }
\seealso{ \code{\link{plot.genomes}}   }
\examples{
data(leuks)
summary(leuks)
summary(leuks, group=='Fungi')
}

\keyword{methods }

\name{genomes-lines}
\alias{genomes-lines}
\alias{lines.genomes}
\title{ Add lines to a genomes plot }
\description{
   Add lines representing the cumulative number of genomes by
   released date to a genome plot.
}
\usage{
\method{lines}{genomes}(x, subset,  \dots)


}
\arguments{
  \item{x}{  genomes data frame with class 'genomes' }
  \item{subset}{logical vector indicating rows to keep }
  \item{\dots}{ additional arguments passed to lines }

}
\details{ Use \code{\link{plotby}} to plot multiple lines within the same
  genome table.  This function adds new lines from different genome tables
  to the same plot. 
}
%\value{   }
%\references{ }
\author{Chris Stubben }
%\note{ }
\seealso{ \code{\link{plotby}}  }
\examples{
data(proks)
data(euks)

plot(proks, log='y', las=1, lty=3)
lines(euks, col="red", lty=2)



}
\keyword{ hplot }

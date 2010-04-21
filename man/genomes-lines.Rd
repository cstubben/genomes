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
%\value{ A  line  }
%\references{ }
\author{Chris Stubben }
%\note{ }
\seealso{ \code{\link{plotby}}  }
\examples{
data(lproks)
data(leuks)
data(lenvs)
plot(lproks, log='y', las=1, lty=3)
lines(leuks, col="red", lty=2)
lines(lenvs, col="green3", lty=1) 
legend("topleft", c("Microbes", "Eukaryotes", "Metagenomes"),
                  bty='n',   lty=3:1, col=c("blue", "red", "green3"))


}
\keyword{ hplot }

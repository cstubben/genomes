\name{genomes-subset}
\alias{genomes-subset}
\alias{subset.genomes}
\title{ Subset genome tables }
\description{
Return subsets of a genome table.  
}
\usage{ \method{subset}{genomes}(x, \dots)  }
\arguments{
  \item{x}{ a genomes data.frame }
  \item{\dots}{ additional arguments ignored }
}
\details{  Preserves the genomes class and other attributes if name and released
  columns are present, otherwise the subsetting operation will return
  a data.frame.  Update methods will not work on subsets of genome
  tables, but the  other genome functions will work }

%\references{ }
\author{ Chris Stubben}
%\note{ }
%\seealso{ }

\examples{
data(lproks)
yp<-subset(lproks, name \%like\% 'Yersinia pest*')
yp
summary(yp)
}

\keyword{methods }

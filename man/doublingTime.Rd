\name{doublingTime}
\Rdversion{1.1}
\alias{doublingTime}
\title{
Doubling time for genome projects
}
\description{
Calculates the doubling time of genome sequencing project releases
}
\usage{
doublingTime(x, subset, time = "days", curdate=TRUE)
}
\arguments{
  \item{x}{   genomes data frame with class 'genomes'}
  \item{subset}{logical vector indicating rows to keep }
  \item{time}{  return doubling time in days (default), months, or
      years  }
  \item{ curdate}{ include the current date in calculation, if false, then
      default is range of release dates }
}
%\details{ }
\value{
the doubling time
}
%\references{}
\author{ Chris Stubben
}
%\note{ }

%\seealso{ \code{\link{help}}   }

\examples{
data(proks)
doublingTime(proks)
doublingTime(proks, status == 'Complete', time='months')
}

\keyword{ methods }

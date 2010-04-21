\name{doublingTime}
\Rdversion{1.1}
\alias{doublingTime}
\title{
Doubling time for genome projects
}
\description{
Calculates the doubling time of genome sequencing project submissions
}
\usage{
doublingTime(x, subset, time = "days")
}
\arguments{
  \item{x}{   genomes data frame with class 'genomes'}
  \item{subset}{logical vector indicating rows to keep }
  \item{time}{  return doubling time in days (default), months, or
      years  }
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
data(lproks)
doublingTime(lproks)
doublingTime(lproks, status == 'Complete', time='months')
}

\keyword{ methods }

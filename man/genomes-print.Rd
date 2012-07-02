\name{print.genomes}
\Rdversion{1.1}
\alias{print.genomes}
\title{ Print genome tables }
\description{
Print method for genome tables
}
\usage{  \method{print}{genomes}(x, \dots)  }
\arguments{
  \item{x}{ a genomes data.frame }
  \item{\dots}{ additional arguments ignored }
}
\details{  Prints the first four columns and first five and last row of a
  genomes data.frame.   To view all the columns in a genome table,
  you can either select fewer than 7 rows or convert the object
  to a data.frame (\code{data.frame(proks)} ) }

%\references{ }
\author{ Chris Stubben}
%\note{ }
%\seealso{ }

\examples{
data(proks)
proks
## full table printed  if 6 rows or less 
proks[1,] 
}

\keyword{methods }

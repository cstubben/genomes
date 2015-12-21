\name{genus}
\alias{genus}
\title{   Extract the genus name  }
\description{
Extracts the genus name from a scientific name
}
\usage{
genus(x)
}

\arguments{
  \item{x}{ A vector of scientific names  }
}
\details{ Removes single quotes, brackets and candidate qualifiers. }
\value{
A vector of genus names
}
%\references{}
\author{ Chris Stubben }
%\note{ }

\seealso{ \code{\link{species}}   }
\examples{
genus("[Bacillus] selenitireducens")

}

\keyword{ methods }

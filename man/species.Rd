\name{species}
\alias{species}
\title{   Extract the species name  }
\description{
Extracts the species name from a scientific name
}
\usage{
species(x)
}

\arguments{
  \item{x}{ A vector of scientific names  }
}
\details{ Removes single quotes, brackets and candidate qualifiers. }
\value{
A vector of species names
}
%\references{}
\author{ Chris Stubben }
%\note{ }

\seealso{ \code{\link{genus}}   }
\examples{
species("Candidatus Carsonella ruddii PV")
}

\keyword{ methods }

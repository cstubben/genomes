\name{genus}
\Rdversion{1.1}
\alias{genus}
\title{   Extract the genus name  }
\description{
Extracts the genus name from a scientific name (latin binomial)
}
\usage{
genus(x)
}

\arguments{
  \item{x}{ A vector of scientific names  }
}
\details{
Returns the first word in the scientific name.  For candidate
species labeled \emph{Candidatus}, then the second word is returned.
}
\value{
A vector of genus names
}
%\references{}
\author{ Chris Stubben }
%\note{ }

\seealso{ \code{\link{species}}   }
\examples{
genus("Bacillus anthracis Ames")
data(lproks)
x <- table2(genus(lproks$name))[1:10,]
dotplot(rev(x), xlab="Genomes")
}

\keyword{ methods }

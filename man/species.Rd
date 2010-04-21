\name{species}
\Rdversion{1.1}
\alias{species}
\title{   Extract the species name  }
\description{
Extracts the species name from a scientific name
}
\usage{
species(x, abbrev=FALSE, epithet=FALSE)
}

\arguments{
  \item{x}{ A vector of scientific names  }
  \item{abbrev }{ Abbreviate the genus name   }
  \item{epithet}{ Return only the specific epithet (default is genus + specific epithat)  }
}
\details{
Returns the species name.  For candidate
species labeled \emph{Candidatus}, the qualifier is not included }
\value{
A vector of species names
}
%\references{}
\author{ Chris Stubben }
%\note{ }

\seealso{ \code{\link{genus}}   }
\examples{
species("Bacillus anthracis Ames")
species("Bacillus anthracis Ames", abbrev=TRUE)
species("Bacillus anthracis Ames", epithet=TRUE)
data(lproks)
x <- table2(species(lproks$name))[1:10,]
dotplot(rev(x), xlab="Genomes")
## abbreviate genus name
x <- subset(lproks, name \%like\% 'Bacillus*')
x <- table2(species(x$name))[1:10, ]
names(x) <- species(names(x), TRUE)
dotplot(rev(x), xlab=expression(italic(Bacillus) ~ genomes))

}

\keyword{ methods }

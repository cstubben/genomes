\name{ncbiGenome}
\Rdversion{1.1}
\alias{ncbiGenome}
\title{  NCBI Genome Database  }
\description{  Search Entrez Genome at NCBI  }
\usage{
ncbiGenome(term)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms }

}
\details{ Returns summaries in Entrez Genome  } 

\references{ A description of the Entrez programming utilities is at
  \url{http://eutils.ncbi.nlm.nih.gov/}. }
\author{ Chris Stubben }
%\note{   }

\seealso{ \code{\link{ncbiGenomeNuc}} for linked nucleotide sequences    }

\examples{

 x <-ncbiGenome('Yersinia[ORGN]')
 x
}

\keyword{ methods }

\name{term2summary}
\alias{term2summary}
\title{ Retrieve genome summaries from NCBI  }
\description{
  Search the Entrez BioProject (Genome Project)  at NCBI  and
  retrieves a summary table using the Entrez programming
  utilities (eUtils)
}
\usage{
term2summary(term)
}
\arguments{
  \item{term}{ Any valid combination of Entrez search terms  }
}
\details{
Searches the new BioProject database using the ESearch utility, returns document summary
pages using the ESummary utility, and then parses the XML fields using
the \code{XML} package.

}
\value{ A   data frame with 31 columns  }
\references{A description of the Entrez programming utilities is at
  \url{http://eutils.ncbi.nlm.nih.gov/}.


 }
\author{ Chris Stubben }
\note{ BioProject pages at NCBI are currently under development }
\seealso{ \code{\link{term2neighbor}}  }

\examples{
# Genomes sequenced at Los Alamos 
x <- term2summary( "Yersinia[ORGN]")
table(x$project_data_type)
table(x$sequencing_status)
}

\keyword{ methods}

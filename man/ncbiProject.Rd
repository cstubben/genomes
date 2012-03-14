\name{ncbiProject}
\alias{ncbiProject}
\title{ NCBI BioProject database   }
\description{
  Search the Entrez BioProject (Genome Project) at NCBI and retrieve a
  project summary table 

}
\usage{
ncbiProject(term, refseq = FALSE)
}
\arguments{
  \item{term}{ any valid combination of Entrez search terms  }
  \item{refseq}{ include RefSeq and Overview projects, if false then only primary submissions excluding RefSeq. }
}
\details{
Searches the new BioProject database using the ESearch utility

}
\value{ A genomes data frame with 32 summary fields columns  }
\references{A description of the Entrez programming utilities is at
  \url{http://eutils.ncbi.nlm.nih.gov/}.


 }
\author{ Chris Stubben }
%\note{  }
\seealso{ \code{\link{ncbiGenome}}  }

\examples{

#ncbiProject("Pan[ORGN]")
x <- ncbiProject("Yersinia[ORGN]")
x
t(x[2,])  #second row
summary(x)

}
\keyword{ methods}

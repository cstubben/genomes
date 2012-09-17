\name{read.glimmer}
\alias{read.glimmer}
\title{ Read a Glimmer output file
}
\description{ Read a Glimmer3 gene output file from NCBI 
}
\usage{
read.glimmer(file)
}
\arguments{
  \item{file}{ Glimmer3 file  }
}
\details{  Glimmer files are available from the NCBI genomes ftp
  directory, \url{ftp://ftp.ncbi.nih.gov/genomes}.
}
\value{  GRanges with 3 elementMetadata columns: id, frame and score
}
\references{ Details about Glimmer3 are available at \url{http://www.cbcb.umd.edu/software/glimmer}
}
\author{ Chris Stubben }
% \note{ }


\seealso{ \code{\link{read.ncbi.ftp}}  }
\examples{
 file<-"ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Yersinia_pestis_CO92_uid57621/NC_003132.Glimmer3" 
 x <-read.glimmer(file)
 x
 metadata(x)
 table(values(x)$frame)

}
\keyword{ file }

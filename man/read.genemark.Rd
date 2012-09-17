\name{read.genemark}
\alias{read.genemark}
\title{ Read a GeneMark output file }
\description{ Read a GeneMark HMM version 2.6 file from NCBI (version 3) }
\usage{ read.genemark(file) }
\arguments{
  \item{file}{ GeneMark HMM file }
}
\details{ GeneMark HMM files are available from the NCBI genomes ftp
  directory, \url{ftp://ftp.ncbi.nih.gov/genomes}.
}
\value{ GRanges with 2 elementMetadata columns: id and class. }
\references{ see \url{http://exon.gatech.edu} for details about GeneMark }
\author{ Chris Stubben }
\note{ Two GeneMark predictions are available from teh NCBI genomes ftp.  This function 
currently reads the HMM version 2.6 files only
}

\seealso{ \code{\link{read.ncbi.ftp}}  }
\examples{
file <- "ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Yersinia_pestis_CO92_uid57621/NC_003132.GeneMarkHMM-2.6r"
 x <-read.genemark(file)
 x
 metadata(x)

}
\keyword{ file }

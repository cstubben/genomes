\name{read.prodigal}
\Rdversion{1.1}
\alias{read.prodigal}
\title{
Read a Prodigal gene finding output file 
}
\description{
Read a gff formatted Prodigal gene output file from NCBI (version 2.5)
}
\usage{
read.prodigal(file, allScores = FALSE)
}
\arguments{
  \item{file}{ Prodigal gff output file }
  \item{allScores}{ include all scores }
}
\details{Prodigal output files are available from the NCBI genomes ftp
  directory, \url{ftp://ftp.ncbi.nih.gov/genomes}.
}
\value{ GRanges with 7 elementMetadata columns: id, partial flag for genes
  continuing off the edge of a contig, start codon, RBS motif, RBS spacer, coding
  potential/score and start score.

  If allScores is TRUE, then four additional score columns are included:
  total score (sum of coding and start score) and RBS motif score, upstream
  region score, and codon type score (which usually sum to start score). See the README file 
  in the Prodigal distribution for complete details.

}
\references{ Prodigal is a microbial gene finding program developed at University of
Tennessee and Oak Ridge National Laboratory.  See \url{http://prodigal.ornl.gov} for details  }
\author{Chris Stubben }
%\note{ }

\seealso{ \code{\link{read.ncbi.ftp}} }
\examples{

 file<-"ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Yersinia_pestis_CO92_uid57621/NC_003143.Prodigal-2.50" 
 x <-read.prodigal(file)
 x
 metadata(x)
 table2(values(x)$start_type)
 table2(values(x)$rbs_motif)
 hist(values(x)$sscore, br=40, col="blue", main="", xlab="Start score")
}
\keyword{ file }

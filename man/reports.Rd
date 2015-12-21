\name{reports}
\alias{reports}
\title{  Genome and assembly reports}
\description{ Genome and assembly reports from the NCBI genomes FTP}
\usage{
reports(file, assembly = FALSE, ...)
}
\arguments{
  \item{file}{  file name, if missing then the files in the FTP directory are listed }
  \item{assembly}{ use ASSEMBLY_REPORTS directory, default is GENOME_REPORTS }
  \item{\dots}{ additional options passed to \code{read_delim}
}
}
\details{ Uses \code{read_delim} in the \code{readr} package to read genome or assembly reports in the genomes FTP }
\value{ A list of files if file name is missing, otherwise a data.frame with class c("tbl_df", "tbl", "data.frame").  If dplyr is loaded, then printing will return the first ten rows and not the first 10,000.}
\references{ See \url{ftp://ftp.ncbi.nih.gov/genomes/GENOME_REPORTS} and  \url{ftp://ftp.ncbi.nih.gov/genomes/ASSEMBLY_REPORTS}  }
\author{ Chris Stubben }
%\note{ }
%\seealso{ }

\examples{
reports()
\dontrun{
proks <- reports("prokaryotes.txt") 
}

}

\keyword{ methods }


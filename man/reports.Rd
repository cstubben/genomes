\name{reports}
\alias{reports}
\title{  Genome and assembly reports from NCBI }
\description{  Download genome and assembly reports from the NCBI genomes FTP}
\usage{
reports(file, assembly = FALSE)
}
\arguments{
  \item{file}{ a file name in the GENOME_REPORTS or ASSEMBLY_REPORTS directory.  If the file name is missing, the directory listing is returned }
  \item{assembly}{  read files from ASSEMBLY_REPORTS, default is GENOME_REPORTS }
}
\details{  Uses \code{read_delim} in the \code{readr} package to read genome or assembly reports.  

}
\value{ A directory listing if file name is missing, or data.frame with output class c("tbl_df", "tbl", "data.frame").  If \code{dplyr} is loaded, you get an enhanced print method and will see the first ten rows instead of 10,000.


}
\references{  \url{ ftp://ftp.ncbi.nih.gov/genomes/GENOME_REPORTS }  and  \url{ ftp://ftp.ncbi.nih.gov/genomes/ASSEMBLY_REPORTS }  }
\author{  Chris Stubben}
%\note{   }


%\seealso{ }
\examples{
\dontrun{
reports()
proks <- reports("prokaryotes.txt") 
}
}

\keyword{ methods }


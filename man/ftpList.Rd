\name{ftpList}
\alias{ftpList}
\title{ List FTP files and directories }
\description{List FTP files and directories from NCBI and other hosts  }
\usage{
ftpList(ftp, fileonly = FALSE)
}
\arguments{
  \item{ftp}{  ftp directory }
  \item{fileonly}{  only list files}
}
%\details{ }
\value{ a data.frame}
%\references{ }
\author{ Chris Stubben }
% \note{ }
% \seealso{}
\examples{
 # all Y. pestis files
 ftp<- "ftp.ncbi.nih.gov/genomes/Bacteria/Yersinia_pestis_CO92_uid57621"
 x<-ftpList(ftp)
 x

# all genome directories
# ftp<-"ftp.ncbi.nih.gov/genomes/Bacteria"
# x <- ftpList(ftp)

}
\keyword{ file }

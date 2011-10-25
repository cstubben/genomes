\name{enaFiles}
\alias{enaFiles}
\title{ ENA SRA submitted or fastq files }
\description{  Retrieve a list of SRA submitted files or generated fastq files at the ENA }
\usage{
enaFiles(acc, file = "submitted")
}
\arguments{
  \item{acc}{ a vector of SRA accession numbers }
  \item{file}{ return submitted (default) or fastq file names} }
%\details{  }
\value{ a data.frame with experiment details and files names }
\references{ \url{http://www.ebi.ac.uk/ena/about/browser#sra_submitted_files} }
\author{ Chris Stubben }
\note{ Only a single accession number is allowed in the URL string,
  so retrieving files from multiple accessions will be slow  }


%\seealso{ }
\examples{

enaFiles("ERP000141")
enaFiles("ERP000141", "fastq")


}
\keyword{ methods }

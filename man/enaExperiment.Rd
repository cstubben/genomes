\name{enaExperiment}
\alias{enaExperiment}
\title{ ENA SRA experiment details }
\description{ Return details about SRA experiments in the ENA  }
\usage{
enaExperiment(accs, batchsize = 100)
}
\arguments{
  \item{accs}{  a vector of SRA experiments or a range of accessions with prefix SRX, ERX, DRX, etc.}
  \item{batchsize}{number of accs to include in a single comma-separated url string }
}
\details{ Parses some of the tags and values in the XML experiment report }
\value{ a data.frame with platform, model and library details like name, layout, source and selection }
\references{\url{http://www.ebi.ac.uk/ena/about/browser#sra_xml}  }
\author{ Chris Stubben}
%\note{}


\seealso{  \code{\link{enaSRA}}  }
\examples{
# compare to http://www.ebi.ac.uk/ena/data/view/ERX007105
 enaExperiment("ERX007105")

\dontrun{
# chimps
pan<-enaSRA(9596) 
head(pan)
# first experiment in sample
pan2<-enaExperiment(substr(pan$experiment, 1,9))
head(pan2)
table2(pan2$model)
}

}
\keyword{ methods }

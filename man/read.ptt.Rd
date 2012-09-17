\name{read.ptt}
\Rdversion{1.1}
\alias{read.ptt}
\title{ Read a NCBI protein or RNA feature table }
\description{ Read a protein or RNA table from NCBI genomes ftp. }
\usage{
read.ptt(file)
}
\arguments{
  \item{file}{ a protein table }
}
\details{Protein and RNA table (*.ptt and */rnt) are available in the NCBI genomes ftp
  directory at \url{ftp://ftp.ncbi.nih.gov/genomes} 

}
\value{GRanges with 6 elementMetadata columns including locus tag id, 
   length (aa), genbank ID, gene name, cog id and product.

}
%\references{ }
\author{ Chris Stubben}
\note{ Protein tables downloaded from Entrez Genome overview pages have a different format  }


\seealso{\code{\link{read.ptt}} }
\examples{

  file<-"ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Yersinia_pestis_CO92_uid57621/NC_003143.ptt" 
  x <-read.ptt(file)
  x
  table2(substr(values(x)$cog, 1,7), n=6)


}
\keyword{ file }

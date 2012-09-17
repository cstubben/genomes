\name{read.gff}
\alias{read.gff}
\title{ Read a GFF file }
\description{ Read a GFF file from NCBI (version 3) }
\usage{
read.gff(file, only.genes = TRUE, nrows = -1) }
\arguments{
  \item{file}{ a GFF file }
  \item{only.genes}{ only return genes with locus tags }
  \item{nrows}{ number of rows to read }
}
\details{ GFF files are available from the NCBI genomes ftp
  directory, \url{ftp://ftp.ncbi.nih.gov/genomes}.
}
\value{ GRanges with 4 elementMetadata columns: locus, feature, description and gene 
name.  If all rows are returned (\code{only.genes=FALSE}), then score, phase and tags 
are included.  The seqid and source are saved in metadata.
}
\references{ see \url{http://www.sequenceontology.org/gff3.shtml} for details about Generic Feature Format
}
\author{ Chris Stubben }
\note{ By default, the GFF file is parsed to return only genes with a locus tag.  Features, 
descriptions and genes names are assigned from child records by matching Parent tags.  

If a GFF file includes a sequence, use the nrows option to restrict the nubmer of rows scanned }


\seealso{ \code{\link{read.ncbi.ftp}}  }
\examples{
 file<-"ftp://ftp.ncbi.nih.gov/genomes/Bacteria/Yersinia_pestis_CO92_uid57621/NC_003132.gff"
 x <-read.gff(file)
 x
metadata(x)
 


}
\keyword{ file }

\name{read.ncbi.ftp}
\alias{read.ncbi.ftp}
\title{  Read files from the NCBI genomes FTP  }
\description{ Read files from the NCBI genomes FTP  }
\usage{
read.ncbi.ftp(org, filePattern = "ptt$|rnt$", ftp = "genomes/Bacteria", ...)
}
\arguments{
  \item{org}{ organism directory }
  \item{filePattern}{  load files matching a specific pattern, default is protein and rna tables}
  \item{ftp}{  name of base FTP directory }
  \item{\dots}{ other options passed to read functions }
}
\details{ This function reads files in the genomes FTP and loads sequence files 
(faa=protein, fna=genone, ffn=gene, frn=rna) using Biostring functions or converts 
coordinate files (gff, ptt, rnt, GeneMarkHMM, Glimmer, Prodigal)  to GRanges }

\value{ a Biostring or GRanges object }
%\references{ }
\author{ Chris Stubben }
\note{ Does not read asn, gbk, val, GeneMark 2.5 and rpt files.  Use ftp = "genbank/genomes/Bacteria" for genbank submissions}

\seealso{ \code{\link{read.gff}}, \code{\link{read.ptt}}, \code{\link{read.genemark}}, \code{\link{read.glimmer}}, \code{\link{read.prodigal}} }
\examples{


\dontrun{
# list organism directories
ftp<-"ftp.ncbi.nih.gov/genomes/Bacteria"
ftpList(ftp)
read.ncbi.ftp(org)         # Protein and rna tables
read.ncbi.ftp(org, "Prod") # Prodigal annotations
read.ncbi.ftp(org, "gff")  # GFF
read.ncbi.ftp(org, "fna")  # Genome sequences
read.ncbi.ftp(org, "313.*ffn")   # Plasmid genes
}


}
\keyword{ file }

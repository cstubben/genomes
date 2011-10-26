\name{enaTaxonomy}
\alias{enaTaxonomy}
\title{ ENA taxonomy statistics }
\description{ The number of linked records and total size in the taxonomy portal view at the European Nucleotide Archive (ENA)
}
\usage{
enaTaxonomy(tax, h = TRUE, round = 0)
}
\arguments{
  \item{tax}{ a taxonomy ID or name }
  \item{h}{ return bases in human-readable format }
  \item{round}{ number of digits to round bases}
}
% \details{ }
\value{ a data.frame listing direct and subtree records in eight data classes: Assembled Nucleotide Sequences (release), Annotated Nucleotide Sequence update (std_update), Whole Genome Shotgun Sequence update (wgs_update), Genomic Contig Sequence update (con_update), Protein-coding Sequences (cds), Trace Archive (trace), SRA samples (sra_sample) and Projects (project).

}
\references{ see \url{http://www.ebi.ac.uk/ena/about/browser#taxonomy_portal_options} for details }
\author{ Chris Stubben }
\note{ The ENA urls require a taxonomy ID and therefore searching by a taxonomy name will be slower
 since a separate query to the NCBI taxonomy database is needed. }

\seealso{ \code{\link{ncbiTaxonomy}} }
\examples{
# COMPARE to http://www.ebi.ac.uk/ena/data/view/display=html&Taxon:2
enaTaxonomy("Bacteria")
# common names 
enaTaxonomy("human")
# root
enaTaxonomy(1)
}
\keyword{ methods }

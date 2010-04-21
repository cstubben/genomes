\name{gold}
\Rdversion{1.1}
\alias{gold}
\docType{data}
\title{
Genomes OnLine Database (GOLD) table
}
\description{
Genome sequencing projects listed at GOLD
}
\usage{data(gold)}
\format{
  A genomes data frame with 7000+ observations on the following 105 variables.
  \describe{
    \item{\code{goldstamp}}{ GOLD project id }
    \item{\code{name}}{project name (was ORGANISM NAME)}
    \item{\code{status}}{status  (was SEQUENCING STATUS)}
   \item{\code{released}}{ released date (was COMPLETION DATE)}
    \item{\code{superkingdom}}{a character vector}
    \item{\code{phylum}}{a character vector}
    \item{\code{class}}{a character vector}
    \item{\code{order}}{a character vector}
    \item{\code{family}}{a character vector}
    \item{\code{genus}}{a character vector}
    \item{\code{species}}{a character vector}
    \item{\code{domain}}{a character vector}
    \item{\code{strain}}{a character vector}
    \item{\code{serovar.biovar}}{a character vector}
    \item{\code{common.name}}{a character vector}
    \item{\code{project.name}}{a character vector}
    \item{\code{culture.collection}}{a character vector}
    \item{\code{type.strain}}{a character vector}
    \item{\code{old.goldstamp}}{a character vector}
    \item{\code{project.type}}{a character vector}
    \item{\code{project.status}}{a character vector}
    \item{\code{availability}}{a character vector}
    \item{\code{sequencing.centers}}{a character vector}
    \item{\code{funding}}{a character vector}
    \item{\code{contact.name}}{a character vector}
    \item{\code{contact.email}}{a character vector}
    \item{\code{contact.url}}{a character vector}
    \item{\code{locus.tag}}{a character vector}
    \item{\code{publication}}{a character vector}
    \item{\code{project.relevance}}{a character vector}
    \item{\code{taxon.id}}{a numeric vector}
    \item{\code{project.id}}{a numeric vector}
    \item{\code{archive.id}}{a numeric vector}
    \item{\code{short.reads.archive.id}}{a character vector}
    \item{\code{gcat_id}}{a character vector}
    \item{\code{hmp.id}}{a numeric vector}
    \item{\code{homd.id}}{a character vector}
    \item{\code{straininfo.id}}{a numeric vector}
    \item{\code{greengenes.object.id}}{a character vector}
    \item{\code{img.object.id}}{a numeric vector}
    \item{\code{genome.data}}{a character vector}
    \item{\code{sequencing.quality}}{a character vector}
    \item{\code{comments}}{a character vector}
    \item{\code{library.method}}{a character vector}
    \item{\code{reads.count}}{a character vector}
    \item{\code{vector}}{a character vector}
    \item{\code{assembly.method}}{a character vector}
    \item{\code{sequencing.depth}}{a character vector}
    \item{\code{gene.calling.method}}{a character vector}
    \item{\code{contig.count}}{a numeric vector}
    \item{\code{size.kb}}{a numeric vector}
    \item{\code{orfs}}{a numeric vector}
    \item{\code{chromosome.count}}{a numeric vector}
    \item{\code{plasmid.count}}{a numeric vector}
    \item{\code{gc.content}}{a numeric vector}
    \item{\code{sequencing.method}}{a character vector}
    \item{\code{sequencing.country}}{a character vector}
    \item{\code{isolation.site}}{a character vector}
    \item{\code{isolation.source}}{a character vector}
    \item{\code{isolation.comments}}{a character vector}
    \item{\code{collection.date}}{a character vector}
    \item{\code{isolation.country}}{a character vector}
    \item{\code{isolation.pubmed.id}}{a numeric vector}
    \item{\code{geographic.location}}{a character vector}
    \item{\code{latitude}}{a character vector}
    \item{\code{longitude}}{a character vector}
    \item{\code{altitude}}{a character vector}
    \item{\code{depth}}{a character vector}
    \item{\code{host.name}}{a character vector}
    \item{\code{host.taxon.id}}{a numeric vector}
    \item{\code{host.gender}}{a character vector}
    \item{\code{host.age}}{a character vector}
    \item{\code{host.race}}{a character vector}
    \item{\code{host.health}}{a character vector}
    \item{\code{host.medication}}{a character vector}
    \item{\code{host.specificity}}{a logical vector}
    \item{\code{body.sample.site}}{a character vector}
    \item{\code{body.product}}{a character vector}
    \item{\code{body.sample.subsite}}{a character vector}
    \item{\code{host.comments}}{a character vector}
    \item{\code{oxygen.requirement}}{a character vector}
    \item{\code{cell.shape}}{a character vector}
    \item{\code{sporulation}}{a character vector}
    \item{\code{pressure}}{a character vector}
    \item{\code{temperature.range}}{a character vector}
    \item{\code{salinity}}{a character vector}
    \item{\code{ph}}{a character vector}
    \item{\code{cell.diameter}}{a character vector}
    \item{\code{cell.length}}{a character vector}
    \item{\code{color}}{a character vector}
    \item{\code{gram.staining}}{a character vector}
    \item{\code{biotic.relationships}}{a character vector}
    \item{\code{symbiotic.interaction}}{a character vector}
    \item{\code{symbiotic.relationship}}{a character vector}
    \item{\code{symbiont.name}}{a character vector}
    \item{\code{symbiont.taxon.id}}{a numeric vector}
    \item{\code{cell.arrangement}}{a character vector}
    \item{\code{disease}}{a character vector}
    \item{\code{habitat}}{a character vector}
    \item{\code{temperature}}{a character vector}
    \item{\code{metabolism}}{a character vector}
    \item{\code{motility}}{a character vector}
    \item{\code{phenotype}}{a character vector}
    \item{\code{energy.source}}{a character vector}
  }
}
\details{ The column names match the postions and names in the source file except
  for the first four columns.  Note that released dates are listed for
  complete genomes only.
}

\source{
\url{http://www.genomesonline.org/DBs/goldtable.xls}
}
\references{
\url{http://nar.oxfordjournals.org/cgi/content/full/gkp848v1}
}
\examples{
data(gold)
gold
# single row, long format
t(gold[1,])
## release date is completion date
table(!is.na(gold$released), gold$status)
## genomes with Entrez project ID
table(!is.na(gold$project.id), !is.na(gold$released),
      dnn=list("Has project ID?", "Is complete?"))
plotby(gold, "project.type", log='y')
table2(gold$project.type)
plotby(gold, "domain", project.type == 'Genome-Isolate', log='y')
}
\keyword{datasets}

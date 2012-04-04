\name{genomes-plot}
\alias{genomes-plot}
\alias{plot.genomes}
\title{ Genome table plots by release date }
\description{
   Generic function for plotting the cumulative number of genomes by
   released date for genome tables 
}
\usage{
\method{plot}{genomes}(x, subset,  
 xlab, ylab ="Genomes",
 type= "l", col = "blue", ...)


}
\arguments{
  \item{x}{  a genomes data frame with class 'genomes' }
  \item{subset}{logical vector indicating rows to keep }
  \item{xlab}{ x-axis label, default is date column name }
  \item{ylab}{ y-axis label }
  \item{type}{ type of plot, default is a blue line }
  \item{col}{ color }
  \item{\dots}{ additional arguments passed to plot }

}
\details{ Requires a released, created or submitted date column (and plots first column found)}
\value{
A plot of the cumulative total of genomes by release date.
 }
%\references{ }
\author{Chris Stubben }
%\note{ }
\seealso{ \code{\link{plotby}} to plot release dates by any grouping column }
\examples{
data(lproks)
plot(lproks)
plot(lproks, name \%like\% 'Yersinia*', ylab="Yersinia genomes")
}
\keyword{ hplot }

\name{year}
\Rdversion{1.1}
\alias{year}
\alias{month}
\title{
Parse a date string
}
\description{
Parses the year or month from a date
}
\usage{
year(x)
month(x)
}

\arguments{
  \item{x}{ a date }
}
\details{ functions are a shortcut for \code{ as.numeric(format.Date(x, "\%Y"))} }
\value{ the year or month}
% \references{}
\author{
Chris Stubben
}
%\note{}

% \seealso{ }
\examples{

data(proks)
table(year(proks$released))
# just complete genomes
table(year(proks$released[proks$status=="Complete"]))

}

\keyword{ methods }

\name{like}
\alias{like}
\alias{\%like\%}
\title{ Pattern matching using wildcards  }
\description{
 Pattern matching using wildcards 
}
\usage{
x \%like\% pattern
}
\arguments{
  \item{pattern}{ character string containing the pattern to be
      matched }
  \item{x}{ values to be matched }
}
\details{
 Only wildcards matching a single character '?'  or zero or more
characters '*' are allowed.  Matches are case-insensitive.  The
pattern is first converted to a regular expression using
\code{\link{glob2rx}} then matched to values in \code{x} using
\code{\link{grep}}.

This is a shortcut for a commonly used expression found in the
\code{\link[base]{subset}} example where \code{nm \%in\% grep("^M", nm, 
value=TRUE)} simplifies to \code{nm \%like\% 'M*'}.

}
\value{
A logical vector indicating if there is a match or not. This will
mostly be useful in conjunction with the \code{\link[base]{subset}} function.
}
%\references{  }
\author{ Chris Stubben }
%\note{ }
\seealso{  \code{\link{grep}}, \code{\link{glob2rx}}, \code{\link[base]{subset}} }

\examples{
data(lproks)
subset(lproks, name \%like\% 'Yersinia*', c(name, released))
# also works with date or numeric fields
subset(lproks, released \%like\% '2008-01*', c(name, released))
}

\keyword{ manip }

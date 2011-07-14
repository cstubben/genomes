\name{image2}
\alias{image2}
\title{ Display a matrix image }
\description{
  Creates a grid of colored rectangles to display a  matrix 
}
\usage{
image2(x, col = rev(heat.colors(24)), breaks, log = FALSE,
 zeroNA=TRUE, sort01=FALSE, all=FALSE, border = NA, box.offset = 0.1,
 round = 3, cex, text.cex = 1, text.col = "black", mar = c(1, 3, 3, 1),
 labels = 2:3, label.offset = 0.1, label.cex = 1)
}
\arguments{
  \item{x}{ A numeric matrix, typically with row and column names }
  \item{col}{ A vector of colors for boxes}
  \item{breaks}{  A numeric vector of break points or number of
      intervals into which \code{x} is to be \code{\link{cut}}.
      Default is the length of \code{col}  }
  \item{log}{ Cut values in \code{x} using a log scale, default TRUE }
  \item{zeroNA}{ Set zeros to NA (and color white) }
  \item{sort01}{ Sort rows in descending order using the entire string
      of numbers}
  \item{all}{ Display entire matrix, default is first 50 rows and columns }
  \item{border}{ The border color for boxes, default is no borders }
  \item{box.offset}{ Percent reduction in box size (a number between 0
    and 1), default is 10\% reduction }
  \item{round}{ Number of decimal places to display values of \code{x}
    in each box}
  \item{cex}{ Magnification size of text and labels, if specified this will replace
      values in both text.cex and label.cex  }
  \item{text.cex}{ Magnification size of text in cells only }
  \item{text.col}{ Color of text in cells, use NA to skip
    text labels }
  \item{mar}{ Margins on four sides of plot  }
  \item{labels}{ A vector giving sides of the plot (1=bottom, 2=left,
      3=top, 4=right) for row and column labels }
  \item{label.offset}{ Amount of space between label and boxes }
  \item{label.cex}{ Magnification size of labels }

}
\details{ Missing values  (NAs) and zeroes are assigned to the color
  white (unless zeroNA is FALSE) and remaining values are cut into
  groups and colored using the assigned values. 

}
\value{ A image plot of the matrix in \code{x}}

%\references{ ~put references to the literature/web site here ~ }
\author{ Chris Stubben }
%\note{  }
\seealso{  \code{\link{image}}  }
\examples{


## top 20 Genus by year
data(lproks)
z<-table2(genus(lproks$name), year(lproks$released), n=20)
 image2(z[,-ncol(z)], sort=TRUE, mar=c(1,10,3,1), cex=.8)

}

\keyword{ color }

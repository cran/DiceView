\name{contourview.fun}
\alias{contourview.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a contour view of a function }
\description{
  Plot a contour view of a function. Provide a better understanding of the model behaviour.
}

\usage{
contourview.fun(fun, center = NULL, dim = ifelse(is.null(center),2,length(center)),
                 npoints = 20,
                 col = "blue",
                 nlevels = 10,
                 mfrow = NULL,
                 Xname = NULL, yname = NULL, Xscale = 1, yscale = 1,
                 xlim = c(0,1), ylim = NULL, title = NULL, add = FALSE,
                 ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{an object of class \code{"function"}.}
  \item{dim}{the dimension of fun arguments.}
  \item{center}{
    optional coordinates (as a list or data frame) of the
    center of the section view if the model's dimension is > 2.
  }
  \item{npoints}{
    an optional number of points to discretize plot of
    response surface and uncertainties.
  }
  \item{col}{color for the surface.}
  \item{nlevels}{
    number of contour levels to display.
  }
  \item{mfrow}{
    an optional list to force \code{par(mfrow = ...)} call. The default
    value  \code{NULL} is automatically set for compact view.
  }
  \item{xlim}{
    a list to give x range for all plots.}
  \item{ylim}{
    an optional list to force y range for all plots.
  }
  \item{Xname}{an optional list of string to overload names for X. }
  \item{yname}{an optional string to overload name for y.}
  \item{Xscale}{an optional factor to scale X.}
  \item{yscale}{an optional factor to scale y.}
  \item{title}{an optional overload of main title.}
  \item{add}{to print graphics on an existing window.}
  \item{\dots}{further arguments passed to the first call of \code{plot3d}. }
}
\details{
  Experimental points are plotted with fading colors. Points that fall in the
  specified section (if any) have the color specified \code{col_points} while
  points far away from the center have shaded versions of the same
  color. The amount of fading is determined using the Euclidean distance
  between the plotted point and \code{center}.

  The variables chosen with their number are to be found in the \code{X} slot of the
  model. Thus they are 'spatial dimensions' but not 'trend variables'.
  
}
%%\value{No value.}
\author{Yann Richet, IRSN }
\seealso{
  See \code{\link{sectionview3d.fun}}.
}

\keyword{models}
\examples{
## A 2D example - Branin-Hoo function.
contourview.fun(branin,dim = 2)
}

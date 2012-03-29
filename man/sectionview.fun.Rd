\name{sectionview.fun}
\alias{sectionview.fun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot section views of a function}
\description{
  Plot one section view per dimension of a function thus providing
  a better understanding of the model behaviour.
}

\usage{
sectionview.fun(fun, center = NULL, dim = ifelse(is.null(center),1,length(center)),
               npoints = 100,
               col_surf = "blue",
               mfrow = NULL, 
               Xname = NULL, yname = NULL, Xscale = 1, yscale = 1,
               xlim = c(0,1), ylim = NULL, title = NULL, add = FALSE, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{an object of class \code{"function"}.}
  \item{dim}{the dimension of fun arguments.}
  \item{center}{
    optional coordinates (as a list or data frame) of the
    center of the section  view if the model's dimension is > 1.
  }
  \item{npoints}{
    an optional number of points to discretize plot of
    response surface and uncertainties.
  }
  \item{col_surf}{color for the section.}
  \item{mfrow}{
    an optional list to force \code{par(mfrow = ...)} call. The default
    value  \code{NULL} is automatically set for compact view.
  }
  \item{xlim}{a list to give x range for all plots.}
  \item{ylim}{an optional list to force y range for all plots.}
  \item{Xname}{an optional list of string to overload names for X. }
  \item{yname}{an optional string to overload name for y. }
  \item{Xscale}{an optional factor to scale X. }
  \item{yscale}{an optional factor to scale y. }
  \item{title}{an optional overload of main title. }
  \item{add}{to print graphics on an existing window.}
  \item{\dots}{further arguments passed to the first call of \code{plot}. }
}
\details{
  A multiple rows/columns plot is produced.
}
%%\value{No value.}

\author{Yann Richet, IRSN }

\seealso{
  The function \code{\link{sectionview3d.fun}} produces a 3D version.
}
  
\keyword{models}
\examples{
## A 2D example - Branin-Hoo function.
sectionview.fun(branin,center=c(.5,.5))
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contourview.fun.R, R/sectionview.fun.R,
%   R/sectionview3d.fun.R, R/wrapper.R
\docType{methods}
\name{contourview.function}
\alias{contourview.function}
\alias{sectionview.function}
\alias{sectionview3d.function}
\alias{sectionview,function-method}
\alias{sectionview,function,function-method}
\alias{sectionview3d,function-method}
\alias{sectionview3d,function,function-method}
\alias{contourview,function-method}
\alias{contourview,function,function-method}
\title{Plot a contour view of a function.}
\usage{
\method{contourview}{`function`}(
  model,
  dim = ifelse(is.null(center), 2, length(center)),
  center = NULL,
  axis = NULL,
  npoints = 20,
  nlevels = 10,
  col = "blue",
  filled = FALSE,
  mfrow = NULL,
  Xname = NULL,
  yname = NULL,
  Xscale = 1,
  yscale = 1,
  xlim = c(0, 1),
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\method{sectionview}{`function`}(
  model,
  dim = ifelse(is.null(center), 1, length(center)),
  center = NULL,
  axis = NULL,
  npoints = 100,
  col_surf = "blue",
  mfrow = NULL,
  Xname = NULL,
  yname = NULL,
  Xscale = 1,
  yscale = 1,
  xlim = c(0, 1),
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\method{sectionview3d}{`function`}(
  model,
  dim = ifelse(is.null(center), 2, length(center)),
  center = NULL,
  axis = NULL,
  npoints = 20,
  col = "blue",
  Xname = NULL,
  yname = NULL,
  Xscale = 1,
  yscale = 1,
  xlim = c(0, 1),
  ylim = NULL,
  title = NULL,
  add = FALSE,
  ...
)

\S4method{sectionview}{`function`}(
  model,
  dim,
  center = NULL,
  axis = NULL,
  npoints = 100,
  col = "blue",
  mfrow = NULL,
  Xname = NULL,
  yname = NULL,
  Xscale = 1,
  yscale = 1,
  xlim = c(0, 1),
  ylim = NULL,
  title = NULL,
  ...
)

\S4method{sectionview3d}{`function`}(
  model,
  dim,
  center = NULL,
  axis = NULL,
  npoints = 20,
  col = "blue",
  Xname = NULL,
  yname = NULL,
  Xscale = 1,
  yscale = 1,
  xlim = c(0, 1),
  ylim = NULL,
  title = NULL,
  ...
)

\S4method{contourview}{`function`}(
  model,
  dim,
  center = NULL,
  axis = NULL,
  npoints = 20,
  col = "blue",
  nlevels = 10,
  Xname = NULL,
  yname = NULL,
  Xscale = 1,
  yscale = 1,
  xlim = c(0, 1),
  ylim = NULL,
  title = NULL,
  ...
)
}
\arguments{
\item{model}{an object of class \code{"function"}.}

\item{dim}{the dimension of fun arguments.}

\item{center}{optional coordinates (as a list or data frame) of the center of the section view if the model's dimension is > 2.}

\item{axis}{optional matrix of 2-axis combinations to plot, one by row. The value \code{NULL} leads to all possible combinations i.e. \code{choose(D, 2)}.}

\item{npoints}{an optional number of points to discretize plot of response surface and uncertainties.}

\item{nlevels}{number of contour levels to display.}

\item{col}{color for the surface.}

\item{filled}{use filled.contour}

\item{mfrow}{an optional list to force \code{par(mfrow = ...)} call. The default value  \code{NULL} is automatically set for compact view.}

\item{Xname}{an optional list of string to overload names for X.}

\item{yname}{an optional string to overload name for y.}

\item{Xscale}{an optional factor to scale X.}

\item{yscale}{an optional factor to scale y.}

\item{xlim}{a list to give x range for all plots.}

\item{ylim}{an optional list to force y range for all plots.}

\item{title}{an optional overload of main title.}

\item{add}{to print graphics on an existing window.}

\item{...}{further arguments passed to the first call of \code{plot3d}.}

\item{col_surf}{color for the section.}

\item{function}{function, taken as model}
}
\description{
Plot one section view per dimension of a function thus providing a better understanding of the model behaviour.

Plot a 3-D view of a function. Provide a better understanding of the model behaviour.
}
\details{
Experimental points are plotted with fading colors. Points that fall in the specified section (if any) have the color specified \code{col_points} while points far away from the center have shaded versions of the same color. The amount of fading is determined using the Euclidean distance between the plotted point and \code{center}. The variables chosen with their number are to be found in the \code{X} slot of the model. Thus they are 'spatial dimensions' but not 'trend variables'.

A multiple rows/columns plot is produced.
}
\examples{
## A 2D example - Branin-Hoo function.
contourview(branin,dim = 2)
## A 2D example - Branin-Hoo function.
sectionview(branin,center=c(.5,.5))
## A 2D example - Branin-Hoo function.
sectionview3d(branin,dim = 2)
}
\seealso{
See \code{\link{sectionview3d}}.

The function \code{\link{sectionview3d}} produces a 3D version.

\code{\link{sectionview}}
}
\author{
Yann Richet, IRSN

Yann Richet, IRSN

Yann Richet, IRSN
}
\keyword{models}

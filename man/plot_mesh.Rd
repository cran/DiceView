% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mesh_set.R
\name{plot_mesh}
\alias{plot_mesh}
\title{Plot a one dimensional mesh}
\usage{
plot_mesh(mesh, y = 0, color = "black", ...)
}
\arguments{
\item{mesh}{1-dimensional mesh to draw}

\item{y}{ordinate value where to draw the mesh}

\item{color}{color of the mesh}

\item{...}{optional arguments passed to plot function}
}
\description{
Plot a one dimensional mesh
}
\examples{
plot_mesh(mesh_exsets(function(x) x, threshold=.51, sign=1, intervals=rbind(0,1)))
plot_mesh(mesh_exsets(function(x) (x-.5)^2, threshold=.1, sign=-1, intervals=rbind(0,1)))
}

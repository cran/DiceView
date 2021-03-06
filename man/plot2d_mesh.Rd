% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mesh_set.R
\name{plot2d_mesh}
\alias{plot2d_mesh}
\title{Plot a two dimensional mesh}
\usage{
plot2d_mesh(mesh, color = "black", ...)
}
\arguments{
\item{mesh}{2-dimensional mesh to draw}

\item{color}{color of the mesh}

\item{...}{optional arguments passed to plot function}
}
\description{
Plot a two dimensional mesh
}
\examples{
plot2d_mesh(mesh_exsets(f = function(x) sin(pi*x[1])*sin(pi*x[2]),
                        threshold=0,sign=1, mesh="unif",mesh.size=11,
                        intervals = matrix(c(1/2,5/2,1/2,5/2),nrow=2)))
}

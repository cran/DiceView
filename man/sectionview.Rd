\name{sectionview}
\alias{sectionview}
\alias{sectionview,km-method}
\alias{sectionview,list-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a section view of a kriging model or 
  or modelPredict model, including design points }
\description{
  Plot one section view per dimension of a kriging or
  \code{modelPredict} model. It is useful for a better understanding
  of a model behaviour (incuding uncertainty).
}

\usage{
sectionview(model, ...)
}
\details{
   The \code{model} specfies a model predicting a continuous response
   \eqn{y}{y} with a vector \eqn{\mathbf{x}}{x} of continuous
   covariates. This includes \code{lm} as well as spatial proces-based
   models like \code{km}. The plot shows prediction when
   \eqn{\mathbf{x}}{x} has all its components fixed to
   given values, except one say \eqn{x_d}{x[d]}.
   
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ an object of class \code{"km"} or a list usable as a
    model with the \code{modelPredict} function of the \pkg{DiceEval} package.}
  \item{\dots}{ other arguments of the \code{sectionview.km} or \code{sectionview.list} function }
}
  
%%\value{ No value}

\author{ Yann Richet, IRSN }

\seealso{
  See the documentation of \code{\link{sectionview.km}} and \code{\link{sectionview.list}}
  for the arguments.

  The \code{\link{sectionview3d}} method provides a 3D version. 
}

\keyword{models}
\examples{
## A 2D example - Branin-Hoo function
## a 16-points factorial design, and the corresponding response
d <- 2; n <- 16
design.fact <- expand.grid(seq(0, 1, length = 4), seq(0, 1, length = 4))
design.fact <- data.frame(design.fact); names(design.fact) <- c("x1", "x2")
y <- branin(design.fact) 

## kriging model 1 : matern5_2 covariance structure, no trend, no nugget effect
m1 <- km(design = design.fact, response = y)

sectionview(m1, center = c(.333, .333))

}

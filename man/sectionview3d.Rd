\name{sectionview3d}
\alias{sectionview3d}
\alias{sectionview3d,km-method}
\alias{sectionview3d,list-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a 3-D (using RGL) view of a kriging or modelPredict model, including design points }
\description{
  Plot a 3-D view of a kriging or modelPredict model. It is useful for a better understanding of a model behaviour.
}

\usage{
sectionview3d(model, ...)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{an object of class \code{"km"} or a list that can be used
    in a \code{"modelPredict"} call.}
  \item{\dots}{other arguments of the \code{sectionview3d.km} or
    \code{sectionview3d.list} function }
}

%%\value{ No value.}

\author{Yann Richet, IRSN }

\seealso{\code{\link{sectionview}}}

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

sectionview3d(m1)

}

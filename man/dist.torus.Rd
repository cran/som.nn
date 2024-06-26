% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/som.nn.distance.matrix.R
\name{dist.torus}
\alias{dist.torus}
\title{Torus distance matrix}
\usage{
dist.torus(coors)
}
\arguments{
\item{coors}{\code{data.frame} or \code{matrix} with two
columns with x- and y-coordinates.}
}
\value{
\if{html}{\out{<div class="sourceCode">}}\preformatted{ Complete distance matrix with diagonal and upper triangle values.
}\if{html}{\out{</div>}}
}
\description{
Calculates the distance matrix of points on the surface of a torus.
}
\details{
A rectangular plane is considered as torus (i.e. on an endless plane that contimues on the left,
when leaving at the right side, and in the same way connects top and bottom
border). Distances between two points on the plane are calculated as the shortest distance
between the points on the torus surface.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/som.nn.distance.functions.R
\name{dist.fun.linear}
\alias{dist.fun.linear}
\title{Linear distance functions for topological k-NN classifier}
\usage{
dist.fun.linear(x, sigma = 1.1)
}
\arguments{
\item{x}{Distance or \code{numeric} vector of distances.}

\item{sigma}{Maximum distance to be considered. Default is 1.1.}
}
\value{
\if{html}{\out{<div class="sourceCode">}}\preformatted{  Distance-dependent weight.
}\if{html}{\out{</div>}}
}
\description{
The function is used as distance-dependent weight \eqn{w} for k-NN voting.
}
\details{
The function returns 1.0 for \eqn{x = 0}, 0.0 for \eqn{x \ge \sigma} and

\deqn{1 - x / \sigma}

for \eqn{0 < x < \sigma}.
}

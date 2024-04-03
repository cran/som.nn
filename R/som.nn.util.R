#    SOMnn topology-based classifier
#    Copyright (C) 2017  Andreas Dominik
#                        THM University of Applied Sciences
#                        Gießen, Germany
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#' Softmax normalisation
#'
#' Calculates a softmax-like normalisation for the class frequencies.
#'
#' Softmax function is applied to a vector to squeeze the values in a way that they sum up 
#' to 1.0:
#' 
#' \code{som.nn.softmax(x) = exp(x/T) / sum(exp(x/T))}
#'
#' Low values for \code{T} result in a
#' strong separation of output values. High values for \code{T}
#' make output values more equal.
#' 
#' 
#' @param x  vector of votes for classes
#' @param t  temperature parameter. 
#'
#' @return   Vector of softmax normalised values.
#' 
#' @export
norm.softmax <- function(x, t = 0.2){

  x <- x / t
  denominator.sum <- sum( exp(x))

  return( exp(x)/denominator.sum)
}


#' Linear normalisation
#'
#' Calculates a linear normalisation for the class frequencies.
#'
#' The function is applied to a vector to squeeze the values in a way that they sum up 
#' to 1.0:
#' 
#' \code{som.nn.linnorm(x) = x / sum(x)}
#'
#' Linear normalisation is used to normalise class distrubution during 
#' prediction. Results seems often more reasonable, compared to softmax. The 
#' S4 \code{predict} function for Class \code{SOMnn} allows to specify
#' the normalisation function as parameter.
#' 
#' @param x  vector of votes for classes
#'
#' @return   Vector of normalised values.
#' @export
norm.linear <- function(x){
  
  return( x / sum(x))
}


#' Special version of maximum finder for SOMnn
#' 
#' Returns the index of the column with the maximum value for each row of a
#' data.frame.
#' 
#' A class is only assigned, if the vote for one class is higher than for
#' all others.
#' If more than one element has the same maximum value, 0 is returned.
#' 
#'
#' @param x  data.frame or matrix
#' @param strict minimum for max vote
#'
#' @return   index of max value for each row or 0, if more
#'           than one element has the same maximum value.
#'           
#' @keywords internal
som.nn.max.row <- function(x, strict = 0.8){

  max.x <- apply( x, 1, function(x){
                          m <- which.max(x)
                          if (x[m] < strict) {m <- 0}
                          return( m)}
                )
  return( max.x)
}


#' Makes a data.frame with codes coordinates
#' 
#' Coordinates of neurons of a som are calculated by 
#' calling \code{\link[class]{somgrid}} to be consistent with
#' other som/kohonen packages.
#'
#' @keywords internal
make.codes.grid <- function(xdim, ydim, topo = "hexagonal"){
 
  codes.grid <- class::somgrid(xdim, ydim, topo)

  codes.coors <- data.frame(i  = seq_len(xdim * ydim),
                            ix = rep(seq_len(xdim), ydim),
                            iy = rep(seq_len(ydim), each = xdim),
                            x  = codes.grid$pts[,1],
                            y  = codes.grid$pts[,2])
  return(codes.coors)
}

#' Advanced rounding of vectors
#' 
#' Rounds a vector of probabilities preserving their sum.
#' 
#' In general, if a vector of floating point values is rounded, 
#' the sum is not preserverd.
#' For a vector of probabilities (which sum up to 1.0), this may lead to 
#' strange results.
#' This function rounds all values of the vector and takes care, that 
#' the sum ist not changed (with a precision given in \code{digits}).
#' 
#' @param x      \code{numeric} vector of values.
#' @param digits demanded precision
#' @export
round.probabilities <- function(x, digits = 2){
  
  prec <- 10 ^ digits                  # 0-1 to 0-100
  x.round   <- round(x * prec)         # round off with precision
  x.too.much <- sum(x.round) - prec

  # make vector of corrections for each probablility and order witth
  # biggest probability first:
  x.correction <- c(rep(1,abs(x.too.much)), rep(0, length(x)-abs(x.too.much)))
  x.correction <- x.correction * sign(x.too.much)
  
  x.round[order(x.round, decreasing = TRUE)] <- x.round[order(x.round, decreasing = TRUE)] - x.correction 
  
  return(x.round / prec)  # restore original percent values
} 



#' Rounds a dataframe with vectors of votes for SOMnn
#' 
#' Each row of the \code{data.frame} may sum up to 1.0
#' before and after rounding.
#' Rounding is performed with \code{round.probabilities}.
#' 
#' @param  votes    \code{data.frame} with rows of class probabilities.
#' @param  classes  \code{character} vector with name of categories.
#'                  Names must match the column names of probabilities to be
#'                  rounded.
#' @param digits    precision; default = 2.
#' 
#' @return \code{data.frame} with roundes rows of class probabilities.
#'         other columns are not affected.
#' 
#' @keywords internal
som.nn.round.votes <- function(votes, classes, digits = 2){
  
  # get indices of classes columns and extract:
  i <- match(classes, names(votes))
  v <- as.matrix(votes[,i])

  r <- matrix(unlist(apply(v, 1, round.probabilities, digits = digits)), ncol = ncol(v), byrow = TRUE)
  r <- as.data.frame(r)
  names(r) <- colnames(v)
  
  votes[classes] <- r[classes]
  return(votes)
}

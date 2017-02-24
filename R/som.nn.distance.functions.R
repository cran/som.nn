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
#' Tricubic distance functions for topological k-NN classifier
#'
#' The tricubic function is used as distance-dependent weight \eqn{w} for
#' k-NN voting.
#'
#' The function returns 1.0 for \eqn{x = 0}, 0.0 for \eqn{x \ge \sigma} and
#' 
#' \deqn{w(x) = (1 - x^3 / \sigma^3)^3}
#'
#' for \eqn{0 < x < \sigma}. 
#' 
#'
#'
#' @param x      Distance or \code{numeric} vector or matrix of distances.
#' @param sigma  Maximum distance to be considered.
#'
#' @return       Distance-dependent weight.
#'
#' @export 
dist.fun.tricubic <- function(x, sigma=1){

  result <- (1 - x^3/sigma^3)^3
  result[x>sigma] <- 0

  return( result)
}


#' Inverse exponential distance functions for topological k-NN classifier
#' 
#' The function is used as distance-dependent weight \eqn{w} for k-NN voting.
#' 
#' The function returns 1.0 for \eqn{x = 0}, 0.0 for \eqn{x \ge \sigma} and
#' 
#' \deqn{1 / (x+1)^(1/sigma)}
#' 
#' for \eqn{0 < x < \sigma}.
#' 
#' 
#' @param x      Distance or \code{numeric} vector or matrix of distances.
#' @param sigma  Maximum distance to be considered. Default is 1.1.
#'   
#' @return       Distance-dependent weight.
#'   
#' @export 
dist.fun.inverse <- function(x, sigma=1.1){

  result <- 1 / (x+1)^(1/sigma)
  result[x>sigma] <- 0

  return( result)
}


#' Linear distance functions for topological k-NN classifier
#' 
#' The function is used as distance-dependent weight \eqn{w} for k-NN voting.
#' 
#' The function returns 1.0 for \eqn{x = 0}, 0.0 for \eqn{x \ge \sigma} and
#' 
#' \deqn{1 - x / \sigma}
#' 
#' for \eqn{0 < x < \sigma}.
#' 
#' 
#' @param x      Distance or \code{numeric} vector of distances.
#' @param sigma  Maximum distance to be considered. Default is 1.1.
#'   
#' @return       Distance-dependent weight.
#'   
#' @export 
dist.fun.linear <- function(x, sigma=1.1){
  
  result <- 1 - x/sigma
  result[x>sigma] <- 0
  
  return( result)
}



#' Bubble distance functions for topological k-NN classifier
#' 
#' The function is used as distance-dependent weight \eqn{w} for k-NN voting.
#' 
#' The function returns 1.0 for \eqn{0 < x \le \sigma} and 0.0 for \eqn{x > \sigma}.
#' 
#' 
#' @param x      Distance or \code{numeric} vector or matrix of distances.
#' @param sigma  Maximum distance to be considered. Default is 1.1.
#'   
#' @return       Distance-dependent weight.
#'   
#' @export 
dist.fun.bubble <- function(x, sigma=1.1){
  
  result <- ifelse(x > sigma, 0, 1)
  return( result)
}
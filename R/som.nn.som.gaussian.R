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
#' Gaussian kernel for som training.
#' 
#' Function is the kernel \code{gaussian} for som training, implemented
#' in pure R.
#'
#'
#' @param data     matrix with training data. 
#' @param grid     somgrid object
#' @param len      number of steps to be trained (steps - not epochs!).
#' @param alpha    learning rate.
#' @param radius   radius.
#' @param init     codes for initialisation.
#' @param toroidal true if doughnut-shaped som.
#'
#' @return         S3 object of type \code{kohonen} with the trained som.
#'
#' @keywords internal
som.nn.som.gaussian <- function(data, grid,
                                len = 100, alpha = 0.05,
                                radius,
                                init, toroidal = FALSE) {
  
  codes <- as.matrix(init)
  data <- as.matrix(data)
  
  # deltas for radius and alpha
  # (radius should never fall under 1.0):
  if (radius < 1.5) {radius.delta <- 0}
  else {             radius.delta <- (radius-1) / len}
  
  alpha.delta <- alpha / len
  
  # distances with topology:
  if (!toroidal) {
    distances <- as.matrix(stats::dist(grid$pts, diag = TRUE, upper = TRUE))
  } else {
    distances <- as.matrix(dist.torus(grid$pts))
  }
  
  # train rlen times:
  samples <- sample(nrow(data), len, replace=TRUE)
  
    for ( train.i in samples) {
    
    # select random sample:
    train <- data[train.i,]
    
    # find winner:
    ## winner.i <- som.nn.visual.one(data[train.i,], codes)[1]
    ## code from visual pasted for performance:
      # calc deltas for all dimensions:
      taxi <- t(codes) - train
    
      # make distances:
      dist.all <- colSums(taxi^2)
    
      # find minumum = winner:
      winner.i <- which.min(dist.all)
    
    winner <- codes[winner.i,]
    
    # update codes within radius:
    neighbours <- which(distances[winner.i,] <= radius)
    
    codes <- codes + t(train - t(codes)) * alpha *
                     stats::dnorm(distances[winner.i,], sd = radius)
    
    
    # adapt radius and alpha (from max to 0.0):
    radius <- radius - radius.delta
    alpha <- alpha - alpha.delta
  }
  
  # make SOM-like object:
  result <- list(grid = grid, codes = codes)
  class(result) <- c("SOM", class(result))
  return(result)
}

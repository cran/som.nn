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
#' calls the specified kernel for som training.
#'
#'
#' @param data     \code{numeric} matrix or data.frame with training data. 
#'                 Only numeric columns of data.frame are used for training.
#' @param classes  \code{character} vector with class labels (only necessary for
#'                 supervised training kernels).
#' @param kernel   kernel to be used
#' @param xdim     number of neurons in x
#' @param ydim     number of neurons in y
#' @param len      number of steps to be trained (steps - not epochs!).
#' @param alpha    initial learning rate (decreased to 0).
#' @param radius   initial radius (decreased to 1).
#' @param init     \code{numeric} matrix or data.frame with codes for initialisation.
#' @param toroidal true if doughnut-shaped som.
#'
#' @return         list with elements \code{codes} and \code{grid}.
#'
#' @keywords internal 
som.nn.run.kernel <- function(data, classes = "no classes",
                              kernel = c("internal", "SOM"),
                              xdim, ydim,
                              len = 100, alpha = 0.05, radius = 1,
                              init, toroidal = FALSE) {
  
  init <- as.matrix(init)
  data <- as.matrix(data)
  
  grid <- make.codes.grid(xdim, ydim, topo = "hexagonal")       # fully fletched grid
  som.grid <- class::somgrid(xdim, ydim, topo = "hexagonal")    # grid for class::som
  
  # select kernel for som training:
  # 1st: predefined kernels:
  if (typeof(kernel) == "character"){
    if (kernel == "SOM") {              # run class::SOM
      
      cat("Training som with kernel \"SOM\". Function class::SOM is used.\n")
      # create alpha and radius fo each step:
      alphas <- seq(from = alpha, to = 0, len = len)
      radii <- seq(from = radius, to = 1.1, len = len)
      som <- class::SOM(data = data, grid = som.grid,
                        alpha = alphas, radii = radii, 
                        init = init)
      codes <- som$codes
      
    } else if (kernel == "som") {  # run som::som.train
      
      cat("Training som with kernel \"som\". Function som::som is used.\n")
      som <- som::som.train(data = data, code = init,
                            xdim = xdim, ydim = ydim,
                            alpha = c(alpha, alpha), alphaType = "linear",
                            neigh = "gaussian", topol = "hexa",
                            radius = c(radius, radius),
                            rlen = c(len, 0)) 
      codes <- som$code
      
    } else if (kernel == "kohonen") {  # run kohonen::som
      
      cat("Training som with kernel \"kohonen\". Function kohonen::som is used.\n")
      som <- kohonen::som(data = data, grid = som.grid, 
                          rlen = len, 
                          alpha = c(alpha, 0.0),
                          radius = c(radius, 1.1),
                          toroidal = toroidal, n.hood = "circular",
                          keep.data = FALSE)
      codes <- som$codes
      
    } else if ((kernel == "internal") || (kernel == "bubble")){  # run internal R-implementation
      
      cat("Training som with kernel \"internal\".\n")
      som <- som.nn.som.internal(data, som.grid,
                                 len = len, alpha = alpha,
                                 radius = radius,
                                 init = init, toroidal = toroidal)
      codes <- som$codes
      
    } else if (kernel == "gaussian") {  # run internal R-implementation with gauss kernel
      
      cat("Training som with kernel \"gaussian\".\n")
      
      ## smaller radius (== r/3), for gaussian distance
      som <- som.nn.som.gaussian(data, som.grid,
                                 len = len, alpha = alpha,
                                 radius = radius/3,         
                                 init = init, toroidal = toroidal)
      codes <- som$codes
      
    } else if (kernel == "experimental") {  # run internal R-implementation
      
      cat("Training som with kernel \"experimental\".\n")
      som <- som.nn.som.experimental(data, som.grid,
                                 len = len, alpha = alpha,
                                 radius = radius,
                                 init = init, toroidal = toroidal)
      codes <- som$codes
      
    } else {
      
      cat("Error: Unsupported predefined kernel specified!\n")
      cat("Use one of: \"SOM\", \"internal\".\n")
    }
    
  } else {             # run custom som kernel
    
    cat("Training som with custom kernel.\n")
    som <- kernel(data, classes = classes, som.grid,
                  len = len, 
                  alpha = alpha, radius = radius,
                  init = init, toroidal = toroidal)
    codes <- som$codes
  }
  
  # make SOM-like object:
  codes <- as.matrix(codes)
  
  result <- list(codes = codes, grid = grid)
  class(result) <- c("SOM", class(result))
  
  return(result)
}

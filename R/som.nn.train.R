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
#' Hexagonal som training
#'
#' A self-organising map with hexagonal tolology is trained and
#' a model of Type SOMnn created for prediction of unknown samples.
#' In contrast to a "normal" som, class-labels for all samples of
#' the training set are required to build the topological model after SOM training.
#' 
#' Besides of the predefined kernels 
#' \code{"internal", "gaussian", "SOM", "kohonen" or "som"},  
#' any specified custom kernel function can be used for som training. The function must match the 
#' signature \code{kernel(data, grid, rlen, alpha, radius, init, toroidal)}, with 
#' arguments:
#' \itemize{
#'   \item \code{data:} \code{numeric} matrix of training data; one sample per row
#'   \item \code{classes:} optional \code{charater} vector of classes for training data
#'   \item \code{grid:} somgrid, generated with \code{\link[class]{somgrid}}
#'   \item \code{rlen:} number of training steps
#'   \item \code{alpha:} training rate
#'   \item \code{radius:} training radius
#'   \item \code{init:} \code{numeric} matrix of initial codebook vectors; one code per row
#'   \item \code{toroidal:} \code{logical}; TRUE, if the topology of grid is toroidal
#' }
#' The returned value must be a list with at minimum one element 
#' \itemize{
#'   \item \code{codes:} \code{numeric} matrix of result codebook vectors; one code per row
#' }
#' 
#'
#' @param x        data.fame with training data. Samples are requested as rows and taken randomly for the
#'                 training steps. All
#'                 columns except of the class lables are considered to be attributes and parts of
#'                 the training vector.
#'                 One column is needed as class labels. The column with class
#'                 lables is selected by the argument \code{class.col}.
#'                
#' @param class.col  single string or number. If class is a string, it is considered to be the
#'                 name of the column with class labels.
#'                 If class is a number, the respective column will be used as class labels
#'                 (after beeing coerced to character).
#'                 Default is 1.
#' @param kernel   kernel for som training. One of the predefined kernels
#'                 \code{"internal"}: train with the R-implementation or 
#'                 \code{"gaussian"}: train with the R-implementation of the Gaussian kernel or 
#'                 \code{"SOM"}: train with \code{\link[class]{SOM}} (\code{class::SOM}) or 
#'                 \code{"kohonen"}: train with \code{\link[kohonen]{som}} (\code{kohonen::som}) or 
#'                 \code{"som"}: train with \code{\link[som]{som}} (\code{som::som}). 
#'                 If a function is specified (as closure, not as character)
#'                 the specified custom function is used for training.
#' @param xdim     dimension in x-direction.
#' @param ydim     dimension in y-direction.
#' @param toroidal \code{logical}; if TRUE an endless som is trained as on the
#'                 surface of a torus. default: FALSE.
#' @param len      number of steps to be trained (steps - not epochs!).
#' @param alpha    initial training rate; the learning rate is decreased linearly to 0.0 for the laset training step.
#'                 Default: 0.02.
#' @param radius   inital radius for SOM training.
#'                 If Gaussian distance function is used, radius corresponds to sigma. 
#'                 The distance is decreased linearly to 1.0 for the last training step.
#'                 If \code{radius = 0} (default), the diameter of the SOM is used as initial
#'                 radius.
#'
#' @param norm     logical; if TRUE, input data is normalised by \code{scale(x, TRUE, TRUE)}.
#'
#' @param dist.fun parameter for k-NN prediction: Function used to calculate
#'                 distance-dependent weights. Any distance function must accept the two parameters
#'                 \code{x} (distance) and \code{sigma} (maximum distance to give a weight > 0.0).
#'                 Default is \code{dist.fun.inverse}.
#' @param max.dist parameter for k-NN prediction: Parameter \code{sigma} for dist.fun.
#'                 Default is 2.1. In order to avoid rounding issues, it is recommended not to 
#'                 use exact integers as limit, but values like 1.1 to make sure, that all 
#'                 neurons within distance 1 are included.
#'
#' @param name     optional name for the model. Name will be stored as slot \code{model@name} in the
#'                 trained model.
#'
#' @return         S4 object of type \code{\link{SOMnn}} with the trained model
#' 
#' @example examples/example.train.R
#'
#' @export 
som.nn.train <- function( 
                        x, class.col = 1, kernel = "internal",
                        xdim = 7, ydim = 5, toroidal = FALSE,
                        len = 0, alpha = 0.2, radius = 0,
                        norm = TRUE,                                     # som parameters
                        dist.fun = dist.fun.inverse, max.dist = 1.1,     # predictor parameter
                        name = "som.nn job"
                      ){

  # find class column:
  if (typeof(class.col) == "character") {
    class.idx <- which(names(x) == class.col)
  } else {
    class.idx <- class.col
  }

  len.total <- 0
  
  # init and run som:
  return(som.nn.do.train( x = x, class.idx = class.idx,
                          kernel = kernel,
                          xdim = xdim, ydim = ydim, toroidal = toroidal,
                          len = len, alpha = alpha, radius = radius,
                          norm = norm, norm.center = 0, norm.scale = 1,
                          dist.fun = dist.fun, max.dist = max.dist,                      
                          name = name,
                          continue = FALSE, len.total = 0, codes = NULL))
}


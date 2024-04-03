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
#' Continue hexagonal som training
#'
#' An existing self-organising map with hexagonal tolology is further trained and
#' a model created for prediction of unknown samples.
#' In contrast to a "normal" som, class-labels for all samples of
#' the training set are required to build the model.
#' 
#' Any specified custom kernel function is used for som training. The function must match the 
#' signature \code{kernel(data, grid, rlen, alpha, radius, init, toroidal)}, with 
#' arguments:
#' \itemize{
#'   \item \code{data} \code{numeric} matrix of training data; one sample per row
#'   \item \code{classes:} optional \code{charater} vector of classes for training data
#'   \item \code{grid} somgrid, generated with \code{\link[class]{somgrid}}
#'   \item \code{rlen} number of training steps
#'   \item \code{alpha} training rate
#'   \item \code{radius} training radius
#'   \item \code{init} \code{numeric} matrix of initial codebook vectors; one code per row
#'   \item \code{toroidal} \code{logical}; TRUE, if the topology of grid is toroidal
#' }
#' The returned value must be a list with at minimum one element 
#' \itemize{
#'   \item \code{codes:} \code{numeric} matrix of result codebook vectors; one code per row
#' }
#' 
#'
#' @param model    model of type \code{SOMnn}.
#' @param x        data.fame with training data. Samples are requested as rows and taken randomly for the
#'                 training steps. All
#'                 columns except of the class lables are considered to be attributes and parts of
#'                 the training vector.
#'                 \code{x} must include the same columns as the data.frame with which the model
#'                 have been trained originally.
#'                 One column is needed as class labels. The column with class
#'                 lables is selected by the slot \code{class.idx} of the model.
#' @param kernel   Kernel for som training. One of the predefined kernels
#'                 \code{"bubble"} and \code{"gaussian"} == train with the R-implementation or 
#'                 \code{"SOM"} == train with \code{\link[class]{SOM}} or 
#'                 \code{"kohonen"} == train with \code{\link[kohonen]{som}} (\code{kohonen::som}) or 
#'                 \code{"som"} == train with \code{\link[som]{som}} (\code{som::som}). 
#'                 If a function is specified (as closure, not as character)
#'                 the specified custom function is used for training.
#' @param len      number of steps to be trained (steps - not epochs!).
#' @param alpha    initial training rate; default 0.02.
#' @param radius   inital radius for SOM training.
#'                 If Gaussian distance function is used, radius corresponds to sigma.
#'
#' @return         S4 object of type \code{\link{SOMnn}} with the trained model
#'
#' @example man/examples/example.train.R
#' 
#' @export 
som.nn.continue <- function( 
                        model, x, kernel = "internal",
                        len = 0, alpha = 0.2, radius = 0
                      ){


  class.idx <- model@class.idx

  len.total <- model@len.total
  xdim <- model@xdim
  ydim <- model@ydim
  toroidal <- model@toroidal
  
  norm <- model@norm
  norm.center <- model@norm.center
  norm.scale <- model@norm.scale
  
  dist.fun <- model@dist.fun
  max.dist <- model@max.dist
  strict   <- model@strict
  name <- model@name
  
  codes <- model@codes
  
  # run som:
  return(som.nn.do.train( x = x, class.idx = class.idx, kernel = kernel,
                          xdim = xdim, ydim = ydim, toroidal = toroidal,
                          len = len, alpha = alpha, radius = radius,
                          norm = norm, norm.center = norm.center, norm.scale = norm.scale,
                          dist.fun = dist.fun, max.dist = max.dist, strict = strict,                    
                          name = name,
                          continue = TRUE, len.total = len.total, codes = codes))
}


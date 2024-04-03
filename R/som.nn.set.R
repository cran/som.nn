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
#' Set parameters for k-NN-like classifier in som.nn model
#'
#' Parameters for the k-NN-like classification can be set for an existing model of type SOMnn
#' after training.
#' 
#' The distance function defines the behaviour of the k-nearest-neighbour algorithm.
#' Choices for the distance function include \code{dist.fun.inverse} or \code{dist.fun.tricubic}, 
#' as defined in this package, or any other function that accepts exactly two arguments \code{x} 
#' (the distance) and \code{sigma} (a parameter defined by max.distance).
#' 
#' A data set must be presented to calculate the accuracy statistics of the
#' modified predictor. 
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
#' @param dist.fun distance function for weighting distances between codebook
#'                 vectors on the som (kernel for k-NN classifier).
#' @param max.dist maximum distance to be considered by the nearest-neighbour counting.
#' @param strict   strictness for class label assignment. Default = 0.8.
#' @param name     new name of the model.               
#'
#' @return         S4 object of type \code{\link{SOMnn}} with the updated model.
#' 
#' @seealso \code{\link{dist.fun.bubble}}, \code{\link{dist.fun.linear}}, 
#'          \code{\link{dist.fun.inverse}}, \code{\link{dist.fun.tricubic}}.
#'          
#' @example man/examples/example.predict.R
#'
#' @export 
som.nn.set <- function( model, x, 
                        dist.fun=NULL, max.dist=NULL, strict=NULL, name=NULL){
  
  new.model <- model
  if (!is.null(dist.fun)) new.model@dist.fun <- dist.fun
  if (!is.null(max.dist)) new.model@max.dist <- max.dist
  if (!is.null(name)) new.model@name <- name

  # run som:
  #
  new.model <- som.nn.validate(new.model, x)
  return(new.model)
}

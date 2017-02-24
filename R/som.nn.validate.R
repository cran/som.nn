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
#' Predict class labels for a validation dataset
#'
#' A model of type \code{SOMnn} is tested with a validation dataset. The dataset must
#' include a column with correct class labels.
#' The model is used to predict class labels. Confusion table, 
#' specificity, sensitivity and accuracy for each class are calculated.
#' 
#' Parameters stored in the model are applied for k-NN-like prediction. If necessary 
#' the parameters can be changed by \code{\link{som.nn.set}} before testing.
#' 
#' The funcion is only a wrapper and actually calls \code{som.nn.continue} with the test data and 
#' without training (i.e. \code{len = 0}).
#'
#' @param model    model of type \code{SOMnn}.
#' @param x        data.fame with validation data. Samples are requested as rows.
#'                 \code{x} must include the same columns as the data.frame with which the model
#'                 have been trained originally.
#'                 A column with correct class labels is needed. The column with class
#'                 lables is selected by the slot \code{class.idx} of the model.
#'
#' @return         S4 object of type \code{\link{SOMnn}} with the unchanged model and the
#'                 test statistics for the test data.
#'
#' @example examples/example.train.R
#' 
#' @export 
som.nn.validate <- function( model, x){


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
  name <- paste("Test of model:", model@name)
  
  codes <- model@codes
  
  # run som:
  return(som.nn.do.train( x = x, class.idx = class.idx, kernel = "internal",
                          xdim = xdim, ydim = ydim, toroidal = toroidal,
                          len = 0, alpha = 0.1, radius = 1.0,
                          norm = norm, norm.center = norm.center, norm.scale = norm.scale,
                          dist.fun = dist.fun, max.dist = max.dist,                      
                          name = name,
                          continue = TRUE, len.total = len.total, codes = codes))
}


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
#' predict method for S4 class \code{SOMnn}
#'
#' Predicts categories for a table of data, based on the hexagonal som in the model.
#' This S4 method is a wrapper for the predict method stored in the slot \code{predict}
#' of a model of type SOMnn.
#' 
#' The function returns the winner neuron in \code{codes} for
#' each test vector in \code{x}.
#' \code{x} is organised as one vector per row and must have
#' the same number of columns (i.e. dimensions) and the identical column names 
#' as stored in the SOMnn object.
#' 
#' If data have been normalised during training, the same normalisation is applied
#' to the unknown data to be predicted.
#' 
#' Probablilities are softmax normalised by default.

#'  
#' @rdname  predict-methods
#' @aliases predict,SOMnn-method
#' 
#' @param  object     object of type \code{SOMnn}.
#' @param  x          \code{data.frame} with rows of data to be predicted.
#' 
#' @return            \code{data.frame} with columns: 
#'                    \code{winner}, \code{x}, \code{y}, the predicted probabilities
#'                    for all categories and the prediction 
#'                    as category index (column name \code{prediction}) and
#'                    class label (column name \code{pred.class}).
#'                    
#' @example examples/example.predict.R
#'
#' @export 
setMethod( f = "predict", signature = "SOMnn",
           definition = function(object, x){

  som <- object
  return( som@predict(x))
})

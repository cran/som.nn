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
#' @example man/examples/example.predict.R
#'
#' @export 
setMethod( f = "predict", signature = "SOMnn",
           definition = function(object, x){

  som <- object
  norm.fun <- norm.softmax
  
  # remove unused colums:
  x <- x[names(x) %in% colnames(som@codes)]
  
  # scale, if called with new data:
  if (som@norm) { x <- scale( x, center = som@norm.center, scale = som@norm.scale)}
  vis <- som.nn.visual(som@codes, x)
  
  # distances with dist function:
  codes.coors <- make.codes.grid(som@xdim, som@ydim, topo = "hexagonal")
  if (!som@toroidal) {
    distances <- as.matrix(stats::dist(codes.coors[,c("x","y")], diag = TRUE, upper = TRUE))
  } else {
    distances <- as.matrix(dist.torus(codes.coors[,c("x","y")]))
  }
  
  weights <- som@dist.fun(distances, sigma = som@max.dist)
  votes <- lapply(vis$winner, function(winner){
    vote <- colSums( som@class.freqs * weights[winner,])  
  })
  
  # softmax or linear normalisation:
  votes <- lapply(votes, norm.fun)
  votes <- as.data.frame(matrix(unlist(votes), ncol = length(som@classes), byrow = TRUE))
  names(votes) <- som@classes
  
  # report only %-values:
  votes$prediction <- som.nn.max.row(votes, som@strict)
  votes$pred.class <- as.character(c("unknown",som@classes)[votes$prediction + 1])
  votes <- som.nn.round.votes(votes, som@classes, digits = 2)
  
  # add mapped neuron to result:
  winners <- data.frame(winner = vis$winner, 
                        x = (vis$winner -1) %% som@xdim + 1,
                        y = (vis$winner -1) %/% som@xdim +1)
  prediction <- as.data.frame(c(winners,votes))
  
  return( prediction)
})

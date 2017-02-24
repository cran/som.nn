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
#' Mapping function for SOMnn
#' 
#' Maps a sample of unknown category to a self-organising map (SOM)
#' stored in a object of type SOMnn. 
#' 
#' The function returns the winner neuron in \code{codes} for
#' each test vector in \code{x}.
#' \code{codes} and \code{x} are one vector per row and must have
#' the same number of columns (i.e. dimensions) and the identical column names.
#' 
#' \code{som.nn.visual} is the work horse for the k-NN-like classifier and normally used
#' by calling \code{predict}.
#'
#' @param codes   \code{data.frame} with codebook vectors.
#' @param data    \code{data.frame} with data to be mapped. Columns of \code{x}
#'                must have the same names as columns of \code{codes}.
#'
#' @return        \code{data.frame} with 2 columns:
#'                \itemize{
#'                \item Index of the winner neuron for each row (index starting at 1).
#'                \item Distance between winner and row.
#'                }
#'
#' @export 
som.nn.visual <- function(codes, data){

  # apply will not work on vectors and not on data.frames (i.e. matrix with one entry):
  if (is.data.frame(data)){
    data <- as.matrix(data)
  }
  
  if (is.matrix(data)){
    
    winners <- t(apply(data, 1, som.nn.visual.one, codes=codes))
    result <- data.frame(winner = winners[,1], distance = winners[,2])
  
  } else {
    
    winners <- som.nn.visual.one(data, codes)
    result <- data.frame(winner = winners[1], distance = winners[2])
  }

  return(result)
}


#' Maps one vector to the SOM
#' 
#' Working hourse function for visual.
#' 
#' @param codes \code{numeric matrix} of codebook vectors with
#'              one code per row
#' @param one   \code{numeric} vector to be mapped
#' 
#' @return vector with 2 elements: indes of winner and qerror
#' 
#' @keywords internal
som.nn.visual.one <- function(one, codes){
  
  # calc deltas for all dimensions:
  taxi <- t(t(codes) - one)
  
  # make distances:
  dist.all <- sqrt( rowSums(taxi^2))

  # find minumum = winner:
  winner <- which.min(dist.all)
  qerror <- dist.all[winner]
  
  result <- c(winner, qerror)
  names(result) <- c("winner", "qerror")
  
  return(result)
}

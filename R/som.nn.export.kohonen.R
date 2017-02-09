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
#' Export a som.nn model as object of type \code{kohonen}
#'
#' An existing model of type \code{SOMnn} is exported as
#' object of type \code{kohonen} for use with the tools of the 
#' package \code{kohonen}.
#' 
#' Training data is necessary to generate the kohonen object.
#' 
#'
#' @param model    model of type \code{SOMnn}.
#' @param train    training data
#' 
#' @return         list of type \code{kohonen} with the trained som.
#'                 See \code{\link[kohonen]{som}} for details.
#'
#' @export 
som.nn.export.kohonen <- function( model, train){
  
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
  name <- model@name
  
  # make all data numeric matrices:
  codes <- as.matrix(model@codes)
  
  train <- train[,-class.idx]
  train <- as.matrix(train)
    

  # run som 0 steps to create kohonen object:
  kohonen <- kohonen::som(data = train, grid = class::somgrid(xdim = xdim, ydim = ydim, topo = "hexagonal"),
                           rlen = 0, n.hood = "circular", toroidal = toroidal,
                           init = codes, keep.data = TRUE)
  return(kohonen)
}


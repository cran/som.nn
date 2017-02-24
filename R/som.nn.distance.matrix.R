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
#' Torus distance matrix
#' 
#' Calculates the distance matrix of points on the surface of a torus.
#' 
#' A rectangular plane is considered as torus (i.e. on an endless plane that contimues on the left, 
#' when leaving at the right side, and in the same way connects top and bottom
#' border). Distances between two points on the plane are calculated as the shortest distance
#' between the points on the torus surface.
#' 
#' 
#' @param coors \code{data.frame} or \code{matrix} with two
#'              columns with x- and y-coordinates.
#'              
#' @return      Complete distance matrix with diagonal and upper triangle values.
#' 
#' @export
dist.torus <- function(coors){
  
  # calc deltaX / Y in flatlands:
  x <- coors[,1]
  dx <- stats::dist(x, diag = TRUE, upper = TRUE)
  
  y <- coors[,2]
  dy <- stats::dist(y, diag = TRUE, upper = TRUE)
  
  
  # calc  maxX - deltaX (this is smaller, if deltaX is bigger than maxX/2):
  # + 1, because the last element still has a dist of 1 to the first
  max.x <- max(dx) + 1
  max.y <- max(dy) + 1
  
  mdx <- max.x - dx
  mdy <- max.y - dy
  
  # take the smaller value:
  dx <- pmin(dx, mdx)
  dy <- pmin(dy, mdy)
  
  # distances:
  d <- sqrt(dx^2 + dy^2)

  return(d)
}
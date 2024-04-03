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


#' Multi-step hexagonal som training
#'
#' A self-organising map with hexagonal tolology is trained 
#' in several steps and
#' a model of Type SOMnn created for prediction of unknown samples.
#' In contrast to a "normal" som, class-labels for all samples of
#' the training set are required to build the topological model after SOM training.
#' 
#' Besides of the predefined kernels 
#' \code{"bubble", "gaussian", "SOM", "kohonen" or "som"},  
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
#' If \code{focus > 1} enhancement of dirty samples is activated:
#' Training samples, mapped to neuron with >1 classes, are preferred in the next training step.
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
#'                 \code{"bubble"}: train with the R-implementation or 
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
#' @param len      \code{vector} of numberis of steps to be trained (steps - not epochs!).
#'                 the length of len defines the number of training rounds tobe performed.
#' @param alpha    initial training rate; the learning rate is decreased linearly to 0.0 for the laset training step.
#'                 Default: 0.02. 
#'                 If length(\code{alpha}) > 1, the length must be tha same as for \code{len}
#'                 and defines different alphas for each training round.
#' @param radius   inital radius for SOM training.
#'                 If Gaussian distance function is used, radius corresponds to sigma. 
#'                 The distance is decreased linearly to 1.0 for the last training step.
#'                 If \code{radius = 0} (default), the diameter of the SOM is used as initial
#'                 radius.
#'                 If length(\code{radius}) > 1, the length must be tha same as for \code{len}
#'                 and defines different radii for each training round.
#'                 
#' @param focus    Enhancement factor for focussing of training of "dirty" samples.
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
#' @example man/examples/example.train.R
#'
#' @export 
som.nn.multitrain <- function( 
                        x, class.col = 1, kernel = "internal",
                        xdim = 7, ydim = 5, toroidal = FALSE,
                        len = c(0), alpha = c(0.2), radius = c(0),
                        focus = 1,
                        norm = TRUE,                                     # som parameters
                        dist.fun = dist.fun.inverse, max.dist = 1.1,     # predictor parameter
                        name = "som.nn job"
                      ){

  # check for number of trainings:
  num.train <- length(len)
  
  if (length(alpha) > num.train) {   # ignore unneeded alphas:
    alpha <- alpha[seq_along(len)]
  }
  if (length(alpha) < num.train) {   # append needed alphas:
    last.alpha <- alpha[length(alpha)]
    alpha <- c(alpha, rep(last.alpha, length(len)-length(alpha)))
  }
    
  if (length(radius) > num.train) {   # ignore unneeded radii:
    radius <- radius[seq_along(len)]
  }
  if (length(radius) < num.train) {   # append needed radii:
    last.radius <- radius[length(radius)]
    radius <- c(radius, rep(last.radius, length(len)-length(radius)))
  }

  cat(paste("Training 1: ", len[1], " steps, alpha = ", alpha[1], " , radius = ", radius[1], "\n", sep = ""))

  model <- som.nn.train( x = x, class.col = class.col, kernel = kernel,
                         xdim = xdim, ydim = ydim, toroidal = toroidal,
                         len = len[1], alpha = alpha[1], radius = radius[1],
                         norm = norm,                                    
                         dist.fun = dist.fun, max.dist = max.dist,     
                         name = name)
  
  i <- 2
  while (i <= length(len)){
    plot(model)
    # enrich tarining set with unclean neurons:
    if (focus > 1) {
      x.focus <- enrich.dirty(x, model, multiple = round(focus))
    } else {
      x.focus <- x
    }
    
    cat(paste("\n\nTraining ", i, ": ", len[i], " steps, alpha = ", alpha[i], " , radius = ", radius[i], sep = ""))
    if (focus > 1) {
      cat(paste(", focus on dirty neurons (factor = ", focus, ")\n", sep = ""))
    } else {
      cat(paste("\n"))
    }
    
    model <- som.nn.continue( model = model, x = x.focus,
                              kernel = kernel,
                              len = len[i], alpha = alpha[i], radius = radius[i])
    i <- i+1
  }

  # restore original training set:
  model <- som.nn.validate( model = model, x = x)
  return(model)
}


#' enrich training set with dirty mapped samples
#' 
#' Maps x to the SOM defined in model and makes a list of dirty neurons
#' (i.e. neurons with more then one class label mapped).
#' All training samples in these neurons are added to the training set
#' to enhance their training.
#' 
#' @param x training data
#' 
#' @param model SOMnn model
#'
#' @param multiple enhancement factor for dirty samples
#' 
#' @keywords internal
#' 
enrich.dirty <- function(x, model, multiple){
  
  multiple <- multiple - 1

  p <- predict(model, x[-model@class.idx])
  classes <- x[,model@class.idx]
  border.neurons <- get.border.neurons(p, classes, model, distance = 1.1)
  
  p$rownum <- seq_len(nrow(p))
  
  border.samples <- p$rownum[p$winner %in% border.neurons]
  x.add <- x[rep(border.samples, multiple),]
  
  x.focus <- rbind(x, x.add)
  return(x.focus)
}

#' Get border neurons.
#' 
#' Returns a list of neurons which are on the border between 2 or more classes.
#' 
#' The function analyses all pairs of neurons with distance <= \code{distance}.
#' If samples represented by the pair belong to more than one class, both neurons
#' are added to the list.
#' 
#' @param p         prediction for training data set
#' @param classes   vector of true class lables for prediction
#' @param model     Object of class type \code{SOMnn}
#' @param distance  maximum distance of 2 neurons to be the border. Default 1.1:
#'                  only direct neighbours.
#'                  
#' @return numeric vector with the indices of all border neurons.
#' 
#' @keywords internal
#' 
get.border.neurons <- function(p, classes, model, distance = 1.1){
  
  p$class <- classes
  p$rownum <- 1:nrow(p)
  
  p <- p[c("rownum", "class", "winner")]
  
  xdim <- model@xdim
  ydim <- model@ydim
  
  coors <- make.codes.grid(xdim, ydim)
  
  # loop all pairs of neurons:
  border.neurons <- numeric()
  for (n.1 in seq_len(xdim*ydim)){
    for (n.2 in seq_len(xdim*ydim)){
      
      n1.n2 <- sqrt((coors$x[n.1]-coors$x[n.2])^2 + (coors$y[n.1]-coors$y[n.2])^2)
      if (n1.n2 <= distance) {
        
        content <- p$rownum[p$winner %in% c(n.1, n.2)]
        
        if (length(unique( p$class[content])) > 1){
          border.neurons <- c(border.neurons, n.1, n.2)
        }
      }
    }
  }
  border.neurons <- unique(border.neurons)
  return(border.neurons)
}


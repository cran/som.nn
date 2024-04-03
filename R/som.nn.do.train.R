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
#' Work hourse for hexagonal som training
#'
#' The function is called by \code{\link{som.nn.train}} and \code{\link{som.nn.continue}} 
#' to train self-organising map with hexagonal tolology.
#'
#' @param x        data.fame with training data. Samples are requested as rows and taken randomly for the
#'                 training steps. All
#'                 columns except of the class lables are considered to be attributes and parts of
#'                 the training vector.
#'                 One column is needed as class labels. The column with class
#'                 lables is selected by the argument \code{class.col}.
#'                 If class is not given, the first column is used as class labels.
#' @param class.idx  index of the column with as class labels
#'                 (after beeing coerced to character).
#' @param kernel   kernel to be used for training.
#' @param xdim     dimension in x-direction.
#' @param ydim     dimension in y-direction.
#' @param toroidal \code{logical}; if TRUE an endless som is trained as on the
#'                 surface of a torus.
#' @param len      number of steps to be trained (steps - not epochs!).
#' @param alpha    initial training rate.
#' @param radius   inital radius for SOM training.
#'                 Gaussian distance function is used, radius corresponds to sigma.
#'
#' @param norm     logical; if TRUE, input data is normalised with \code{scale(x, TRUE, TRUE)}.
#'
#' @param dist.fun parameter for k-NN prediction. Function is used to calculate
#'                 distance-dependent weights. Any distance function must accept the two parameters
#'                 \code{x} (distance) and \code{sigma} (maximum distance to give a weight > 0.0).
#' @param max.dist parameter for k-NN prediction. Parameter \code{sigma} for dist.fun.
#'                 In order to avoid rounding issues, it is recommended not to 
#'                 use exact integers as limit, but values like 1.1 to make sure, that all 
#'                 neurons with distance 1 are included.
#' @param strict   difference of maximum votes to assign class label
#'                 (if the difference between the to two votes is smaller or equal to 
#'                 strict, unknown is predicted). \code{default = 0.3.}
#'
#' @param name     name for the model. Name will be stored as slot \code{model@name} in the
#'                 trained model.
#' @param continue logical; if TRUE, the codebook vectors of the model, given in argument \code{model} will be used
#'                 as initial codes.
#' @param len.total number of previuos training steps.
#' @param codes    codes of a model to be used for initialisation.
#'
#' @return         S4 object of type \code{\link{SOMnn}} with the trained model
#'
#' @keywords internal 
som.nn.do.train <- function( x, class.idx, kernel = "internal",
                             xdim, ydim, toroidal,
                             len, alpha, radius = 0,
                             norm, norm.center, norm.scale,
                             dist.fun, max.dist, strict,        
                             name,
                             continue, len.total, codes=NULL   
                           ){

  x <- as.data.frame(x)
  x[[class.idx]] <- as.character(x[[class.idx]])
  # if number of neurons higher then number of samples, increase number of samples:
  # (necessary for kohonen::som)
  if ((typeof(kernel) == "character") && (kernel == "kohonen") && (nrow(x) < xdim * ydim)) {
    x.addl <- x[sample(nrow(x), xdim * ydim - nrow(x) + 10, replace = TRUE),]
    x <- rbind(x, x.addl)
  }
  x <- x[sample(nrow(x), nrow(x)),]
 
  # find class column:
  train.cl <- x[[class.idx]]
  train.cl <- as.character( train.cl)
  classes  <- sort(as.character( unique(train.cl)))

  # extract data matrix:
  train.x <- x[-class.idx]

  # set radius to default value:
  if (radius == 0) { radius <- sqrt(xdim*xdim + ydim*ydim)}

  if (max.dist == 0){ max.dist <- 1.1}
  if (is.null(dist.fun)){ dist.fun <- dist.fun.inverse}
  len.total <- len.total + len

  # init and run som:
  #
  if (!continue) {
    
    if (norm) {
      train.x <- scale(train.x, center = TRUE, scale = TRUE)
      norm.center <-attr(train.x, "scaled:center")
      norm.scale  <- attr(train.x, "scaled:scale")
    }
    
    # init codes with samples from train:
    #
    codes <- train.x[sample(1:nrow(train.x), xdim*ydim, replace = TRUE), , drop = FALSE]
    
  } else {    # continue:
    
    if (norm) {
      train.x <- scale(train.x, center = norm.center, scale = norm.scale)
    }
    # use codes from existing model:
    #
    codes <- codes
  }
    
  # skip training if len == 0:
  if (len > 0 ) {
    
    # train SOM:
    som <- som.nn.run.kernel( data = train.x, classes = train.cl,
                              kernel,
                              xdim = xdim, ydim = ydim,
                              len = len, alpha = alpha, 
                              radius = radius, toroidal = toroidal,
                              init = codes)
  
    codes <- as.data.frame(som$codes)
  }

  codes.coors <- make.codes.grid(xdim, ydim, topo = "hexagonal")
  colnames(codes) <- colnames(train.x)

 
  # count class hits and normalise to 1:
  #
  vis <- som.nn.visual(codes, train.x)
  vis$true.class <- as.character(train.cl)
  
  qerror <- sum(vis$distance) / length(vis$distance)
  
  class.counts <- lapply( seq_len(xdim*ydim),
                    function(code){
                      vis.code <- vis[vis$winner==code,]
                      unlist(lapply( classes, function(class){
                                               return( nrow(vis.code[vis.code$true.class==class,]))
                                              }))
                    })
  class.counts <- as.data.frame(t(as.data.frame(class.counts)))
  rownames( class.counts) <- seq_len(xdim*ydim)
  colnames( class.counts) <- classes

  sums <- rowSums(class.counts)
  sums[sums == 0] <- 1          # no div by 0
  class.freqs <- class.counts / sums

  # result assessment:
  cat("Calculate predictions for training data ...")


  # predict raw data (nm is done, in predict):
  # (1st make prelim. model for predict)
  new.model <- methods::new("SOMnn", name = name,
                            codes = codes,
                            qerror = qerror,
                            classes = classes, class.idx= class.idx,
                            class.counts = class.counts, class.freqs = class.freqs,
                            confusion = data.frame(), measures = data.frame(), accuracy = 0.0,
                            xdim = xdim, ydim = ydim, len.total = len.total, toroidal = toroidal,
                            norm = norm, norm.center = norm.center, norm.scale = norm.scale,
                            strict = strict,
                            dist.fun = dist.fun, max.dist = max.dist)
  
  prediction <- predict(new.model, x[-class.idx])
  prediction$pred.class <- as.character(prediction$pred.class)

  # create result class:
  cat("Calculate accuracy for training data ...")
  
  confusion <- som.nn.confusion(prediction, x[[class.idx]])
  measures <- som.nn.accuracy(prediction, x[[class.idx]])
  accuracy <- som.nn.all.accuracy(prediction, x[[class.idx]])
  
  # create model object:
  new.model <- methods::new("SOMnn", name = name,
                   codes = codes,
                   qerror = qerror,
                   classes = classes, class.idx= class.idx,
                   class.counts = class.counts, class.freqs = class.freqs,
                   confusion = confusion, measures = measures, accuracy = accuracy,
                   xdim = xdim, ydim = ydim, len.total = len.total, toroidal = toroidal,
                   norm = norm, norm.center = norm.center, norm.scale = norm.scale,
                   strict = strict,
                   dist.fun = dist.fun, max.dist = max.dist)

  return( new.model)
}


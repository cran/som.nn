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
#' @param class.col  index of the column with as class labels
#'                 (after beeing coerced to character).
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
#'
#' @param name     name for the model. Name will be stored as slot \code{model@name} in the
#'                 trained model.
#' @param continue logical; if TRUE, the codebook vectors of the model, given in argument \code{model} will be used
#'                 as initial codes.
#' @param len.total number of previuos training steps.
#' @param model    model of type \code{SOMnn}.
#'
#' @return         S4 object of type \code{\link{SOMnn}} with the trained model
#'
#' @keywords internal 
som.nn.do.train <- function( x, class.idx, kernel,
                             xdim, ydim, toroidal,
                             len, alpha, radius = 0,
                             norm, norm.center, norm.scale,
                             dist.fun, max.dist,          
                             name,
                             continue, len.total, codes   
                           ){

  # if number of neurons higher then number of samples, increase number of samples:
  # (necessary for kohonen::som)
  if (nrow(x) < xdim * ydim){
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
  if (!continue) {
    
    if (norm) {
      train.x <- scale(train.x, center = TRUE, scale = TRUE)
      norm.center <-attr(train.x, "scaled:center")
      norm.scale  <- attr(train.x, "scaled:scale")
    }
    
    # init codes with samples from train:
    codes <- train.x[sample(1:nrow(train.x), xdim*ydim, replace = FALSE), , drop = FALSE]
    
  } else {
    
    if (norm) {
      train.x <- scale(train.x, center = norm.center, scale = norm.scale)
    }
  }
    
  # skip training if rlen == 0:
  if (len > 0 ) {
    
    # train SOM:
    som <- som.nn.run.kernel( data = train.x, classes = train.cl,
                              kernel,
                              xdim = xdim, ydim = ydim,
                              rlen = len, alpha = alpha, 
                              radius = radius, toroidal = toroidal,
                              init = codes)
  
    codes <- as.data.frame(som$codes)
  }

  codes.coors <- make.codes.grid(xdim, ydim, topo = "hexagonal")
  colnames(codes) <- colnames(train.x)

 
  # count class hits and normalise to 1:
  #
  vis <- som.nn.visual(codes, train.x)
  vis$true.class <- train.cl
  
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


  # create predict function:
  som.nn.predict <- function(unk, norm.fun = norm.softmax, internal = FALSE){

    # scale, if called with new data:
    if (!internal && norm) { unk <- scale( unk, center = norm.center, scale = norm.scale)}
    vis <- som.nn.visual(codes, unk)

    # distances with dist function:
    if (!toroidal) {
      distances <- as.matrix(stats::dist(codes.coors[,c("x","y")], diag = TRUE, upper = TRUE))
    } else {
      distances <- as.matrix(dist.torus(codes.coors[,c("x","y")]))
    }

    weights <- dist.fun(distances, sigma = max.dist)
    votes <- lapply(vis$winner, function(winner){
                             vote <- colSums( class.freqs * weights[winner,])  
                           })
   
    # softmax or linear normalisation:
    votes <- lapply(votes, norm.fun)
    votes <- as.data.frame(matrix(unlist(votes), ncol = length(classes), byrow = TRUE))
    names(votes) <- classes

    # report only %-values:
    votes$prediction <- som.nn.max.row(votes)
    votes$pred.class <- c("unknown",classes)[votes$prediction + 1]
    votes <- som.nn.round.votes(votes, classes, digits = 2)
    
    # add mapped neuron to result:
    winners <- data.frame(winner = vis$winner, 
                          x = (vis$winner -1) %% xdim + 1,
                          y = (vis$winner -1) %/% xdim +1)
    return( as.data.frame(c(winners,votes)))
  }

  # result assessment:
  tr <- som.nn.predict(train.x, internal = TRUE)
  tr$true.class <- train.cl

  # create result class:
  # (1) confusion matrix:
  confusion <-
    lapply(seq_along(classes),
         function(true){
           unlist( lapply(seq_along(classes),
                     function(pred)
                       return( nrow(tr[tr$true.class==classes[true] & tr$pred.class==classes[pred],]))
           ))
         })

  confusion <- as.data.frame(matrix(unlist( confusion), ncol = length(classes), byrow = FALSE))
  colnames( confusion) <- paste("true.", classes, sep="")
  rownames( confusion) <- paste("pred.", classes, sep="")

  # (2) Sensistivity (True Positive)
  sens <- unlist( lapply(classes, function(cl){
                           true.pos <- nrow(tr[tr$true.class==cl & tr$pred.class==cl,])
                           act.pos  <- nrow(tr[tr$true.class==cl,])
                           return( true.pos / act.pos)
                           # confusion[paste("pred.", cl, sep=""),paste("true.", cl, sep="")] / nrow(tr[tr$true.class==cl,])
                        }))
  names(sens) <- classes

  # (3) Specificity (True Negative, 1- FP)
  spec <- unlist( lapply(classes, function(cl){
                            true.neg <- nrow(tr[tr$true.class!=cl & tr$pred.class!=cl,])
                            act.neg  <- nrow(tr[tr$true.class!=cl,])
                            return( true.neg / act.neg)
                        }))
  names(spec) <- classes

  # (4) Accuracy
  acc <- unlist( lapply(classes, function(cl){
                            true.pos <- nrow(tr[tr$true.class==cl & tr$pred.class==cl,])
                            true.neg <- nrow(tr[tr$true.class!=cl & tr$pred.class!=cl,])
                            return( (true.pos + true.neg) / nrow(tr))
                        }))
  names(acc) <- classes

  measures <- data.frame(sensitivity = sens, specificity = spec, accuracy = acc)



  # create model object:
  new.model <- methods::new("SOMnn", name = name,
                   codes = codes,
                   qerror = qerror,
                   classes = classes, class.idx= class.idx,
                   class.counts = class.counts, class.freqs = class.freqs,
                   confusion = confusion, measures = measures,
                   predict = som.nn.predict,
                   xdim = xdim, ydim = ydim, len.total = len.total, toroidal = toroidal,
                   norm = norm, norm.center = norm.center, norm.scale = norm.scale,
                   dist.fun = dist.fun, max.dist = max.dist)

  return( new.model)
}


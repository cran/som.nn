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
#'
#' An S4 class to hold a model for the topological classifier som.nn
#' 
#' Objects of type \code{SOMnn} can be created by training a self-organising map
#' with \link{som.nn.train}.
#' 
#' 
#' @slot name          optional name of the model.
#' @slot date          time and date of creation.
#' @slot codes         \code{data.frame} with codebook vectors of the som.
#'
#' @slot qerror        sum of the mapping errors of the training data.
#'
#' @slot class.idx     column index of column with class labels in input data.
#' @slot classes       \code{character} vector with names of categories.
#' @slot class.counts  \code{data.frame} with class hits for each neuron.
#' @slot class.freqs   \code{data.frame} with class frequencies for each neuron
#'                     (freqs sum up to 1).
#
#' @slot norm          \code{logical}; if TRUE, data is normalised before training and mapping.
#'                     Parameters for normalisation of training data is stored in the model and
#'                     applied before mapping of test data.
#' @slot norm.center   vector of centers for each column of training
#'                     data.
#' @slot norm.scale    vector of scale factors for each column of training
#'                     data.
#'
#' @slot confusion     \code{data.frame} with confusion matrix for training data.
#' @slot measures      \code{data.frame} with classes as rows and the
#'                     columns sensitivity, specificity and accuracy for each class.
#' @slot accuracy      The overall accuracy calculated based on the confusion matrix cmat:
#'                     \eqn{acc = sum(diag(cmat)) / sum(cmat)}.
#' @slot xdim          number of neurons in x-direction of the som.
#' @slot ydim          number of neurons in y-direction of the som.
#' @slot len.total     total number of training steps, performed to create the model.
#' @slot toroidal      \code{logical}; if TRUE, the map is toroidal (i.e. borderless).
#'
#' @slot dist.fun      \code{function}; kernel for the kNN classifier.
#' @slot max.dist      maximum distance for the kNN classifier.
#' @slot strict       Minimum vote for the winner (if the winner's vote is smaller than strict,
#'                     "unknown" is reported as class label (\code{default = 0.8}).

#'
#'
#' @name    SOMnn-class
#' @rdname  SOMnn-class
#' 
#' @import methods
#' @export
#' 
SOMnn <- setClass(
  Class="SOMnn",
  slots = c(

    name = "character",             # sample name
    date = "character",             # time and date of creation
    codes = "data.frame",           # with codebook vectors as rows and
                                    # dimensions as columns
    qerror = "numeric",             # total qerror
    
    class.idx = "numeric",          # col with categories
    classes = "character",          # alphabetically ordered list of classes
    class.counts = "data.frame",    # number of class hits per neuron
    class.freqs = "data.frame",     # frequencies of class hits per neuron
    
    norm = "logical",               # parameters for scaling
    norm.center = "numeric",
    norm.scale  = "numeric",
    
    confusion = "data.frame",       # confusion matrix
    measures  = "data.frame",       # with classes as rows and columns:
                                    # sensitivity, specificity, accuracy
    accuracy  = "numeric",

    xdim = "numeric",
    ydim = "numeric",
    len.total = "numeric",          # cummulative total of training steps
    toroidal = "logical",
    
    dist.fun = "function",         # for kNN
    max.dist = "numeric",          # for kNN
    strict = "numeric"             # for voting
  )
)


setMethod( "show", "SOMnn",
           function(object){
             cat("Object of type SOMnn, created with package som.nn\n")
             cat(paste("  Name:         ", object@name, "\n", sep=""))
             cat(paste("  Created at:   ", object@date, "\n", sep=""))
             cat(paste("  Dimensions:   ", object@xdim, "x", object@ydim, " hexagonal ", 
                       ifelse(object@toroidal, "and toroidal\n", "\n"), sep=""))
             cat(paste("  Training:     ", object@len.total, " steps",
                       ifelse(object@norm, " with", " without"), " normalised input.\n", sep=""))
             cat(paste("  qError:       ", format(object@qerror, digits=6), "\n", sep=""))

             cat(paste("  Attributes:  ", ncol(object@codes), " columns: \n"))
             print( colnames(object@codes))

                       cat(paste("\n    Confusion map:\n"))
             print(object@confusion)
             cat(paste("\n"))
             cat(paste("    Quality mesures:\n"))
             print(round(object@measures, digits = 2), digits = 2)
             cat(paste("Accuracy (overall):   ", format(object@accuracy, digits=2), "\n"))
           }
)

#' Constructor of SOMnn Class
#'
#' The constructor creates a new object of type SOMnn.
#' 
#' The constructor needs not to be called directly, because the normal
#' way to create a SOMnn object is to use \code{\link{som.nn.train}}.
#'
#' @param .Object      SOMnn object
#' @param name         optional name of the model.
#' @param codes        \code{data.frame} with codebook vectors of the som.
#' @param qerror       sum of the mapping errors of the training data.
#'
#' @param class.idx    \code{numeric} index of column with categories.
#' @param classes      \code{character} vector with names of categories.
#' @param class.counts \code{data.frame} with class hits for each neuron.
#' @param class.freqs  \code{data.frame} with class frequencies for each neuron
#'                      (freqs sum up to 1).
#'
#' @param confusion    \code{data.frame} with confusion matrix for training data.
#' @param measures     \code{data.frame} with classes as rows and the
#'                     columns sensitivity, specificity and accuracy for each class.
#' @param accuracy     Overall accuracy.
#' @param xdim         number of neurons in x-direction of the som.
#' @param ydim         number of neurons in y-direction of the som.
#' @param len.total    total number of training steps, performed to create the model.
#' @param toroidal     \code{logical}; if TRUE, the map is toroidal (i.e. borderless).
#'
#' @param norm         \code{logical}; if TRUE, data is normalised before training and mapping.
#'                     Parameters for normalisation of training data is stored in the model and
#'                     applied before mapping of test data.
#' @param norm.center  vector of centers for each column of training
#'                     data.
#' @param norm.scale   vector of scale factors for each column of training
#'                     data.
#' @param dist.fun     \code{function}; kernel for the kNN classifier.
#' @param max.dist     maximum distance \eqn{\sigma} for the kNN classifier.
#' @param strict   Minimum vote for the winner (if the winner's vote is smaller than strict,
#'                 "unknown" is reported as class label (\code{default = 0.8}).
#' 
#' @examples 
#' \dontrun{
#'new.som <- new("SOMnn", name = name,
#'               codes = codes,
#'               qerror = qerror,
#'               classes = classes, 
#'               class.idx = class.idx,
#'               class.counts = class.counts, 
#'               class.freqs = class.freqs,
#'               confusion = confusion, 
#'               measures = measures,
#'               accuracy = accuracy,
#'               xdim = xdim, 
#'               ydim = ydim, 
#'               len.total = len.total, 
#'               toroidal = toroidal,
#'               norm = norm, 
#'               norm.center = norm.center, 
#'               norm.scale = norm.scale,
#'               dist.fun = dist.fun, 
#'               max.dist = max.dist.
#'               strict = strict)
#'}
#'
#' @rdname initialize-methods
#' @aliases initialize,ANY,ANY-method
#' @export
#' 
setMethod("initialize", "SOMnn",
          function(.Object,
                   name,
                   codes, qerror, class.idx,
                   classes, class.counts, class.freqs,
                   confusion, measures, accuracy,
                   xdim, ydim, len.total, toroidal,
                   norm, norm.center, norm.scale,
                   dist.fun, max.dist, strict) {

              .Object@name <- name
              .Object@codes <- codes
              .Object@confusion <- confusion
              .Object@qerror <- qerror
              .Object@class.idx <- class.idx
              .Object@classes <- classes
              .Object@class.counts <- class.counts
              .Object@class.freqs <- class.freqs
              .Object@measures <- measures
              .Object@accuracy <- accuracy
              .Object@xdim <- xdim
              .Object@ydim <- ydim
              .Object@len.total <- len.total
              .Object@toroidal <- toroidal
              .Object@date <- date()
              .Object@norm <- norm
              .Object@norm.center <- norm.center
              .Object@norm.scale <- norm.scale
              .Object@dist.fun <- dist.fun
              .Object@max.dist <- max.dist
              .Object@strict <- strict

            return(.Object)
          }
)



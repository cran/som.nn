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
#' Calculate confusion matrix
#'
#' Calculates the confusion matrix for a prediction result
#' if the corresponding vector of true class labels 
#' is provided.
#'
#' The confusion matrix (also called table of confusion) displays the number 
#' of predicted class labels for each actual class. Example:
#' 
#' \tabular{rrrrr}{
#'                 \tab pred. cat \tab pred. dog  \tab pred. rabbit \tab unknown \cr
#'  actual cat     \tab   5       \tab   3        \tab   0          \tab  0      \cr   
#'  actual dog     \tab   2       \tab   3        \tab   1          \tab  0      \cr
#'  actual rabbit  \tab   0       \tab   2        \tab   9          \tab  2 
#'               }
#'               
#' The confusion matrix includes a column \code{unknown} displaying the samples 
#' for which no unambiguous prediction is possible.
#' 
#' 
#' @param x  \code{data.frame} with the predictions as returned by the 
#'           SOM.nn predict method.
#' @param class.labels \code{vector} of correct class labels for the predictions.
#'
#' @return   \code{data.frame} containing the confusion matrix.
#' 
#' @export
som.nn.confusion <- function(x, class.labels){
  
  x$true.class <- class.labels
  classes <- sort(unique(class.labels))
  classes.plus.unk <- c(classes,"unknown")
  
  confusion <-
    lapply(seq_along(classes),
           function(true){
             unlist( lapply(seq_along(classes.plus.unk),
                            function(pred)
                              return( nrow(x[x$true.class==classes[true] & x$pred.class==classes.plus.unk[pred],]))
             ))
           })
  
  confusion <- as.data.frame(t(matrix(unlist( confusion), ncol = length(classes), byrow = FALSE)))
  rownames( confusion) <- paste("true.", classes, sep="")
  colnames( confusion) <- paste("pred.", classes.plus.unk, sep="")
  
  return( confusion)
}

  
#' Calculate accuracy measures
#'
#' Calculates the sensitivity, specificity and overall accuracy for a prediction result
#' if the corresponding vector of true class labels 
#' is provided.
#'
#' \strong{Sensitivity} is the classifier's ability to correctly identify samples of a specific class A.
#' It is defined as
#' 
#' \deqn{sens_{A} = TP_{A} / (TP_{A} + FN_{A})}
#' 
#' with  TP = true positives and FN = false negatives. This is equivalent to the 
#' ratio of (correctly identified samples of class A) / (total number of samples of class A).
#' 
#' \strong{Specificity} is the classifier's ability to correctly identify samples not
#' of a specific class A.
#' It is defined as
#' 
#' \deqn{spec_{A} = TN_{A} / (TN_{A} + FP_{A})}
#' 
#' with  TN = true negatives and FP = false positives. This is equivalent to the 
#' ratio of (correctly identified samples not in class A) / (total number of samples not in class A).
#' 
#' \strong{Accuracy} is the classifier's ability to correctly classify samples of
#' a specific class A.
#' It is defined as
#' 
#' \deqn{acc_{A} = (TP_{A} + TN_{A}) / total}
#' 
#' with  TP = true positives, TN = true negatives and total = total number of samples of a class. 
#' This is equivalent to the 
#' ratio of (correctly classified samples) / (total number of samples).
#' 
#' @param x  \code{data.frame} with the predictions as returned by the 
#'           SOM.nn predict method.
#' @param class.labels \code{vector} of correct class labels for the predictions.
#'
#' @return   \code{data.frame} containing sensitivity, specificity and accuracy for all
#'           class labels in the data set.
#' 
#' @export
som.nn.accuracy <- function(x, class.labels){
  
  x$true.class <- class.labels
  classes <- sort(unique(class.labels))
  
  
  # (1) Sensistivity (True Positive)
  sens <- unlist( lapply(classes, function(cl){
    true.pos <- nrow(x[x$true.class==cl & x$pred.class==cl,])
    act.pos  <- nrow(x[x$true.class==cl,])
    return( true.pos / act.pos)
  }))
  names(sens) <- classes
  
  # (3) Specificity (True Negative, 1- FP)
  spec <- unlist( lapply(classes, function(cl){
    true.neg <- nrow(x[x$true.class!=cl & x$pred.class!=cl,])
    act.neg  <- nrow(x[x$true.class!=cl,])
    return( true.neg / act.neg)
  }))
  names(spec) <- classes
  
  # (4) Accuracy
  acc <- unlist( lapply(classes, function(cl){
    true.pos <- nrow(x[x$true.class==cl & x$pred.class==cl,])
    true.neg <- nrow(x[x$true.class!=cl & x$pred.class!=cl,])
    return( (true.pos + true.neg) / nrow(x))
  }))
  names(acc) <- classes
  
  measures <- data.frame(sensitivity = sens, specificity = spec, accuracy = acc)
  return(measures)
}

#' Calculate overall accuracy
#'
#' Calculates the accuracy over all class lables for a prediction result
#' if the corresponding vector of true class labels 
#' is provided.
#'
#' It is defined as
#' 
#' \deqn{acc = (TP + TN) / total = sum(diag(cmat)) / sum(cmat)}
#' 
#' with  TP = true positives, TN = true negatives and total = total number of samples of a class. 
#' This is equivalent to the 
#' ratio of (correctly classified samples) / (total number of samples).
#' 
#' @param x  \code{data.frame} with the predictions as returned by the 
#'           SOM.nn predict method.
#' @param class.labels \code{vector} of correct class labels for the predictions.
#'
#' @return   \code{one value} overall accuracy.
#' 
#' @export
som.nn.all.accuracy <- function(x, class.labels){

  confusion.unk <- som.nn.confusion(x, class.labels)
  confusion <- as.matrix(confusion.unk[!(names(confusion.unk) %in% "pred.unknown")])
  
  return( sum(diag(confusion)) / sum(confusion.unk))
}

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
#' Plot method for S4 class \code{SOMnn}
#'
#' Creates a plot of the hexagonal som in the model of type \code{SOMnn}.
#' 
#' In addition to the required parameters, many options can be 
#' specified to plot predicted samples and to modify colours, legend and scaling.
#' 
#'     
#' @rdname  plot-methods
#' @aliases plot,SOMnn-method
#' 
#' @param  x           trained som of type \code{SOMnn}.
#' @param  title       \code{logical}; if TRUE, slots name and date are used as main title. 
#' @param  col         defines colours for the classes of the dataset. Possible values include:
#'                     \code{NA}: default value; colours are generated with \code{rainbow},  
#'                     a \code{vector} of colour definitions or a
#'                     \code{data.frame} with categories in the first and respective colours in the second column.
#' @param  onlyDefCols \code{logical}; if TRUE, only categories are plotted, for which colours are defined. 
#'                     Default: FALSE.
#' @param  edit.cols   \code{logical}; if TRUE, colour definitions can be edited interactively before plotting. 
#'                     Default: FALSE.
#' @param  show.legend \code{logical}; if TRUE, a legend is displayed,. Default: TRUE.
#' @param  legend.loc  Legend position as specified for \code{\link{legend}}. Default is \code{"bottomright"}.
#' @param  legend.width  size of the legend.
#' @param  window.width  Manual setting of window width. Default is NA.
#' @param  window.height Manual setting of window height. Default is NA.
#' @param  show.box    Show frame around the plot . Default is TRUE.
#' @param  show.counter.border Percentile as limit for the display of labels in the pie charts. Default is 0.98.
#'                             Higher counts are displayed as numbers in the neuron.
#' @param  predict    \code{data.frame} as returned by the \code{som.nn::predict} function
#'                    or a \code{data.frame} or matrix that follows the specification: 
#'                    If columns \code{x} and \code{y} exist, these are used as coordinates
#'                    for the traget neuron; otherwise the first two columns are used.
#'                    Default: NULL.
#' @param  add        \code{logical}; if TRUE, points are plotted on an existing plot. This can be used to 
#'                    stepwise plot
#'                    points of different classes with different colours or symbols.
#' @param  pch.col    Colour of the markers for predicted samples.
#' @param  pch        Symbol of the markers for predicted samples.
#' @param  ...        More parameters as well as general 
#'                    plot parameters are allowed; see \code{\link{par}}.
#'              
#'
#' @import hexbin
#' 
#' @example man/examples/example.train.R
#' 
#' @export 
setMethod( f = "plot", signature = "SOMnn",
           definition = function(x, title = TRUE, 
                                 col = NA, onlyDefCols = FALSE, edit.cols = FALSE,
                                 show.legend = TRUE, legend.loc = "bottomright", legend.width = 4,
                                 window.width = NA, window.height = NA, show.box = TRUE,  
                                 show.counter.border = 0.98,
                                 predict=NULL, add = FALSE, pch.col = "black", pch = 19,
                                  ...){
       
  # make vis from prediction (cave: in somplot, indices start at 0):
  som <- x
  classes <- som@classes
  
  grid <- make.codes.grid(som@xdim, som@ydim, topo = "hexagonal")
  
  counts <- som@class.counts
  counts$i <- grid$i - 1
  counts$x <- grid$ix - 1
  counts$y <- grid$iy -1

  # count class matches:
  vis <- data.frame(x=numeric(0), y=numeric(0), kat=character(0), stringsAsFactors = FALSE)
  for (code in seq_along(counts[,1])) {

    for (class in classes) {

      for (i.count in seq_len( counts[code,class])){

        vis <- rbind(vis, data.frame(x=counts[code,"x"], y=counts[code,"y"], kat=class,
                                     stringsAsFactors = FALSE))
      }
    }
  }
  # print(vis)
  if (!add) {
    makehexbinplot(data = vis, col = col, 
                   show.legend = show.legend, legend.loc = legend.loc, legend.width = legend.width, 
                   window.width = window.width, window.height = window.height, 
                   onlyDefCols = onlyDefCols, 
                   show.box = show.box, edit.cols = edit.cols, show.counter.border = show.counter.border, 
                   ...)
  
  if (title) {title(paste(som@name, "-", som@date))}
  }
  
  # plot samples, if arg predict is given:
  if (!is.null(predict)){
    
    # make data.frame with columns i, x, y:
    if (("x" %in% names(predict) && ("y" %in% names(predict)))){
      predict <- data.frame(x = predict[,"x"], y = predict[,"y"])
    } else {
      predict <-  data.frame(x = predict[,1], y = predict[,2])
    }
    predict$i <- (predict$y-1) * som@xdim + predict$x
    predict <- data.frame(i = predict$i, x = predict$x, y = predict$y)
    
    plot_predictions(grid, predict, pch.col = pch.col, pch = pch, ...)
  }
})


#' Plots the hexagonals and pi charts.
#' Adapted code from package somplot.
#' 
#' @keywords internal
hexbinpie <- function(x, y, kat, xbnds=range(x), ybnds=range(y), hbc = NA, pal = NA, hex = "gray", circ = "gray50", cnt = "black", show.counter.border, ...)
{       
  hb  <- hexbin(shape = (diff(ybnds) + 1) / (diff(xbnds) + 1),  x, y, xbnds = xbnds, ybnds = ybnds, IDs = TRUE, xbins = diff(xbnds)*2)
  rx <- 0.5 -> ry
  hexC <- hexcoords(dx = rx, dy = ry / sqrt(3), n = 1)			
  nl <- length(levels(as.factor(kat)))					
  zbnds <- stats::quantile(hb@count, prob = c(0.05, 0.98, show.counter.border), na.rm = TRUE )	# quantile borders for circle diameter and display counter
  zz <- pmax(pmin(sqrt(hb@count / zbnds[2]), 0.85), 0.2)					# circle diameter from 20 to 85% of hexgon diameter
  tt <- unclass(table(kat, hb@cID))							
  
  for (i in seq(along=zz))    # loop neurons
  {
    if (!is.na(hex)) 
    {
      graphics::polygon(hbc$x[i] + hexC$x, hbc$y[i] + hexC$y, col = NA, border = hex)
    }
    tp <- pi / 2 - 2 * pi * c(0, cumsum(tt[,i]) / sum(tt[,i]))
    used = FALSE
    for (j in 1:nl)     # loop categories
    {
      if (tp[j+1] == tp[j]) 
      {
        next
      }
      if (j >= 2)
      {
        used = TRUE
        pp <- seq(tp[j], tp[j+1], length = floor((tp[j] - tp[j + 1]) * 4) + 2)
        xi <- hbc$x[i] + c(0, zz[i] * rx * cos(pp))
        yi <- hbc$y[i] + c(0, zz[i] * ry * sin(pp))
        graphics::polygon(xi, yi, col = pal[j], border = NA, ...)
      }
      #print(j)
    }
    if (!is.na(circ) & used)  
    {
      graphics::polygon(hbc$x[i] + rx * zz[i] * cos((1:18) * pi / 9), hbc$y[i] + ry * zz[i] * sin((1:18) * pi / 9), col = NA, border = circ)
    }
  }
  for (i in seq(along = zz)) 
  {
    if ((!is.na(cnt)) & (hb@count[i] > zbnds[3]))
    {
      graphics::text(hbc$x[i], hbc$y[i], hb@count[i], col = cnt, cex = 0.5)
    }
  }
}

#' makes the actual heagonal plot.
#' Adapted code from package somplot.
#' 
#' @keywords internal
makehexbinplot <-function(data, col = NA, show.legend = TRUE, legend.loc = "bottomright", legend.width = 4, window.width = NA, window.height = NA, 
                          onlyDefCols = FALSE, 
                          show.box = TRUE, edit.cols = FALSE, show.counter.border = 0.98, ...)
{
  if (!show.legend) 
  {
    legend.width = 0
  }
  
  # calc hbc an fill up empty coordinates with an "empty" element
  pos = 1
  range.x = max(data$x) - min(data$x) + 1
  range.y = max(data$y) - min(data$y) + 1
  
  hbc = data.frame(x = seq(1,(range.x) * (range.y),1), y = NA)
  for (y in c(min(data$y) : max(data$y)))
  {
    for (x in c(min(data$x):max(data$x)))
    {
      hbc$x[pos] = ifelse(y %% 2 == 1, x - 0.5, x)
      hbc$y[pos] = y * 0.866
      pos = pos + 1
      if (nrow(data[data$x == x & data$y == y,]) == 0)
      {
        data = rbind(data, data.frame(x = x, y = y, kat = ""))
      }
    }
  }
  
  lvls = levels(as.factor(data$kat))
  lvls = lvls[lvls != ""]
  
  pal = grDevices::rainbow(length(lvls))
  if (!is.na(col[1]))
  {
    if (onlyDefCols)
    {
      tmp.pal = rep("white", length(lvls))
    }
    else
    {
      tmp.pal = vector("character", length = length(lvls))
    }
    if (is.data.frame(col))
    {	
      for (i in c(1 : nrow(col)))
      {
        tmp.pal[lvls == col[i,1]] = as.character(col[i,2])
      }
    }
    else
    {
      tmp.pal[c(1:length(col))] = col
    }
    
    # convert color names into hex values and fill up colors
    if(!onlyDefCols)
    {
      dbl.pal = sprintf("#%02X%02X%02XFF", 
                        grDevices::col2rgb(tmp.pal[tmp.pal != ""])[1,], 
                        grDevices::col2rgb(tmp.pal[tmp.pal != ""])[2,], 
                        grDevices::col2rgb(tmp.pal[tmp.pal != ""])[3,])
      pal = setdiff(pal, dbl.pal)
      
      for (i in c(1 : length(lvls)))
      {		
        if (is.na(tmp.pal[i]) | tmp.pal[i] == "")
        {
          tmp.pal[i] = pal[1]
          pal = pal[-1]
        }
      }
    }
    pal = tmp.pal
  }
  
  if(edit.cols)
  {
    pal = as.vector(utils::edit(data.frame(kat = lvls, col = pal))[,2])	
  }
  lvls = append("empty", lvls)	
  pal = c("white", pal)	
  
  if(!is.na(window.width))
  {
    window.height = ifelse(is.na(window.height), window.width * (max(hbc$y) - min(hbc$y) - 1 + (range.x / range.y * 2)) / (max(hbc$x) - min(hbc$x) + legend.width), window.height)
    grDevices::dev.new(width = window.width, height = window.height)
  }
  graphics::plot.new()
  graphics::plot.window(c(min(hbc$x) - 0.5, max(hbc$x) + 0.5 + legend.width), c(min(hbc$y) - 0.5, max(hbc$y) + 1), asp=0.866)
  if(show.box)
  {
    graphics::box()	
  }
    if (show.legend)
  {	
    graphics::legend(legend.loc, lvls[-1], fill=pal[-1], x.intersp=0.2)
  }
  hexbinpie(data$x, data$y, kat=data$kat, hbc = hbc, pal = pal, show.counter.border = show.counter.border, ...) 
}


#' Plots predicted samples as points into a plotted som.
#' 
#' @keywords internal
plot_predictions <- function(grid, predict, pch.col, pch, ...){
  
  # fit grid to plot coordinates (left-bootom is (0,0) in plot, but (1,5,0.8660254) in somgrid:
  grid$x <- grid$x - grid$x[1]
  grid$y <- grid$y - grid$y[1]
  
  # map indices to coors:
  coors <- grid[predict$i,c("x","y")]
  
  # function get.pattern
  # returns a pattern of points with relative coors-
  # n   : number of points to be organised
  # sep : separation between points
  #
  get.pattern <- function(n, sep = 0.2){
    sml <- sep * 0.65
    big <- sep * 1.2
    
    if (n == 1){
      return(data.frame(x=0, y=0))
      
    } else if (n == 2) {
      return(data.frame(x=c(-sml, sml), y=c(sml, -sml)))
      
    } else if (n == 3) {
      return(data.frame(x=c(-sml, 0, sml), y=c(-sml*0.87*2/3, sml*0.87*4/3, -sml*0.87*2/3)))
      
    } else if (n == 4) {
      return(data.frame(x=c(-sml, -sml, sml, sml), y=c(sml,-sml, -sml, sml)))
      
    } else if (n == 5) {
      return(data.frame(x=c(-sep, -sep, sep, sep, 0), y=c(sep,-sep, -sep, sep, 0)))
      
    } else if (n == 6) {
      return(data.frame(x=c(-sep, -sep, -sep, sep, sep, sep), y=c(sep, 0, -sep, -sep, 0, sep)))
      
    } else if (n == 7) {
      return(data.frame(x=c(-sep, -sep, -sep, sep, sep, sep, 0), y=c(sep, 0, -sep, -sep, 0, sep, 0)))
      
    } else if (n == 8) {
      return(data.frame(x=c(-sep, -sep, -sep, sep, sep, sep, 0, 0), y=c(sep, 0, -sep, -sep, 0, sep, sep, -sep)))
      
    } else if (n == 9) {
      return(data.frame(x=c(-sep, -sep, -sep, sep, sep, sep, 0, 0, 0), y=c(sep, 0, -sep, -sep, 0, sep, sep, -sep, 0)))
    } else {
      return(data.frame(x=stats::runif(n, min=-big, max=big), y=stats::runif(n, min=-big, max=big)))
    } 
  }
 
  # group points in the same neuron:
  nums <- by(predict, predict$i, function(x){   # process all points in same neuron as one group
                           n <- nrow(x)
                           coors <- grid[x$i,c("x","y")] + get.pattern(n)
                           graphics::points(coors, pch = pch, col = pch.col)
                        })
}

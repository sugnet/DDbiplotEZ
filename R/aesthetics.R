#' Format aesthetics for units
#'
#' @description
#' This function allows the user to format the aesthetics for the units.
#'
#' @param bp an object of class \code{ddbiplot}.
#' @param which a vector containing the groups or classes for which the intervals should be
#'              displayed, with default \code{bp$g}.
#' @param col the colour(s) for the intervals, with default \code{blue}.
#' @param label a logical value indicating whether the intervals should be labelled, with default
#'              \code{FALSE}.
#' @param label.name a vector of the same length as \code{which} with label names for the intervals,
#'                   with default \code{NULL}. If \code{NULL}, the rownames of the intervals are
#'                   used. Alternatively, a custom vector of length \code{n} should be used.
#' @param label.col a vector of the same length as \code{which} with label colours for the intervals,
#'                  with default as the same colour of the intervals.
#' @param label.cex a vector of the same length as \code{which} with label text expansions for the
#'                  intervals, with default \code{0.75}.
#' @param label.side the side at which the label of the interval appears, with default
#'                   \code{bottom}.  Note that unlike the argument \code{pos} in \code{text()},
#'                   options are not \code{1}, \code{2}, \code{3}, \code{4}, but "\code{bottom}",
#'                   "\code{left}", "\code{top}", "\code{right}" and in addition "\code{mid.bottom}",
#'                   "\code{mid.left}", "\code{mid.right}", "\code{mid.top}", "\code{bottom.left}",
#'                   "\code{bottom.right}", "\code{top.left}", and "\code{top.right}" are also allowed.
#'                   The latter four options only take effect when the \code{type} argument in \code{plot}
#'                   is "\code{diagonal}".
#' @param label.offset the offset of the label from the interval. See \code{?text} for a
#'                     detailed explanation of the argument \code{offset} when \code{label.side} is one of
#'                     "\code{bottom}", "\code{left}", "\code{top}", "\code{right}". For the other options
#'                     of \code{label.side}, the \code{label.offset} values is applied as \code{offset}-0.5
#'                     units in the plot.
#' @return The object of class \code{ddbiplot} will be appended with a list called \code{intervals}
#'         containing the following elements:
#' \item{which}{a vector containing the groups or classes for which the samples (and means) are
#'              displayed.}
#' \item{col}{the colour(s) of the intervals.}
#' \item{label}{a logical value indicating whether intervals are labelled.}
#' \item{label.name}{the label names of the samples.}
#' \item{label.col}{the label colours of the samples.}
#' \item{label.cex}{the label text expansions of the samples.}
#' \item{label.side}{the side at which the label of the interval appears.}
#' \item{label.offset}{the offset of the label from the plotted intervals.}
#'
#' @usage
#' units (bp,  which = 1:bp$g, col = biplotEZ:::ez.col, label = FALSE,
#' label.name = rownames(bp$X[[1]]$values), label.col=NULL, label.cex = 0.75,
#' label.side = "bottom", label.offset = 0.5)
#' @aliases units
#'
#' @export
#'
#' @import biplotEZ
#'
#' @examples
#' ddbiplot(data = Oils.data) |> PCA() |> units(col="purple", label = TRUE) |> plot()
#'
units <- function (bp,  which = 1:bp$g, col = biplotEZ:::ez.col, label = FALSE,
                   label.name = rownames(bp$X[[1]]$values), label.col=NULL,
                   label.cex = 0.75, label.side = "bottom", label.offset = 0.5)
{
  g <- bp$g
  n <- bp$n
  p <- bp$p

  if(is.null(which) & length(col)==0) col <- biplotEZ:::ez.col

  if(!is.null(label.col) | any(label.side!="bottom") | any(label.offset !=0.5) | any(label.cex!=0.75))
    label<-TRUE

  unit.names <- switch(bp$X[[1]]$type,
                       numeric = names(bp$X[[1]]$values),
                       interval = rownames(bp$X[[1]]$values),
                       histogram = rownames(bp$X[[1]]$intervals))
  if(is.null(label.name)) label.name <- unit.names

  #This piece of code is just to ensure which arguments in samples() and alpha.bag() lines up
  # to plot only the specified alpha bags and points
  if(!is.null(bp$alpha.bag.aes$which) & !is.null(bp$alpha.bag.outside)){
    if(length(which) != length(bp$alpha.bag.aes$which)){
      message("NOTE in intervals(): 'which' argument overwritten in alpha.bags()")
      which<-bp$alpha.bag.aes$which
    }
    else if(all(sort(which) != sort(bp$alpha.bag.aes$which))){
      message("NOTE in intervals(): 'which' argument overwritten in alpha.bags()")
      which<-bp$alpha.bag.aes$which
    }
  }

  if (!is.null(which))
  {
    if (!all(is.numeric(which))) which <- match(which, bp$g.names, nomatch = 0)
    which <- which[which <= g]
    which <- which[which > 0]
  }
  if (is.null(which))
    sample.group.num <- g
  else
    sample.group.num <- length(which)

  # Expand col to length g
  col.len <- length(col)
  col <- col[ifelse(1:g%%col.len==0,col.len,1:g%%col.len)]
  if(is.null(col)){col <- rep(NA, g)}

    while (length(label) < n) label <- c(label, label)
    label <- as.vector(label[1:n])
    for (i in 1:g) if (is.na(match(i, which))) label[bp$group.aes==bp$g.names[i]] <- NA

    while (length(label.side) < n) label.side <- c(label.side, label.side)
    label.side <- as.vector(label.side[1:n])
    for (i in 1:g) if (is.na(match(i, which))) label.side[bp$group.aes==bp$g.names[i]] <- NA

    while (length(label.offset) < n) label.offset <- c(label.offset, label.offset)
    label.offset <- as.vector(label.offset[1:n])
    for (i in 1:g) if (is.na(match(i, which))) label.offset[bp$group.aes==bp$g.names[i]] <- NA

  while (length(label.cex) < n) label.cex <- c(label.cex, label.cex)
  label.cex <- as.vector(label.cex[1:n])
  for (i in 1:g) if (is.na(match(i, which))) label.cex[bp$group.aes==bp$g.names[i]] <- NA

  if (is.null(label.col))
  {
    label.col <- rep(NA, n)
    for (j in 1:g)
      if (!is.na(match(j, which))) label.col[bp$group.aes==bp$g.names[j]] <- col[which==j][1]
  }
  else
  {
    while (length(label.col) < n) label.col <- c(label.col, label.col)
    label.col <- as.vector(label.col[1:n])
  }

  bp$units = list(which = which, col = col, label = label, label.name = label.name,
                      label.col = label.col, label.cex = label.cex, label.side = label.side,
                      label.offset = label.offset)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Format aesthetics for intervals
#'
#' @description
#' This function allows the user to format the aesthetics for the entities.
#'
#' @param bp an object of class \code{ddbiplot}.
#' @param which a vector containing the groups or classes for which the intervals should be
#'              displayed, with default \code{bp$g}.
#' @param col the colour(s) for the intervals, with default \code{blue}.
#' @param lwd the line width for the intervals, with default \code{1}.
#' @param lty the line type for the intervals, with default \code{1}.
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
#'                   \code{bottom}. Note that unlike the argument \code{pos} in \code{text()},
#'                   options are "\code{bottom}", "\code{left}", "\code{top}", "\code{right}" and
#'                   not \code{1}, \code{2}, \code{3}, \code{4}.
#' @param label.offset the offset of the label from the interval. See \code{?text} for a
#'                     detailed explanation of the argument \code{offset}.
#'
#' @return The object of class \code{ddbiplot} will be appended with a list called \code{intervals}
#'         containing the following elements:
#' \item{which}{a vector containing the groups or classes for which the samples (and means) are
#'              displayed.}
#' \item{col}{the colour(s) of the intervals.}
#' \item{lwd}{the line width of the intervals.}
#' \item{lty}{the line type of the intervals.}
#' \item{label}{a logical value indicating whether intervals are labelled.}
#' \item{label.name}{the label names of the samples.}
#' \item{label.col}{the label colours of the samples.}
#' \item{label.cex}{the label text expansions of the samples.}
#' \item{label.side}{the side at which the label of the interval appears.}
#' \item{label.offset}{the offset of the label from the plotted intervals.}
#'
#' @usage
#' intervals (bp,  which = 1:bp$g, col = biplotEZ:::ez.col, lwd = 1, lty = 1,
#' label = FALSE, label.name = rownames(bp$X[[1]]$values), label.col=NULL, label.cex = 0.75,
#' label.side = "bottom", label.offset = 0.5)
#' @aliases intervals
#'
#' @export
#'
#' @import biplotEZ
#'
#' @examples
#' ddbiplot(data = Oils.data) |> PCA() |> intervals(col="purple",lwd=2, label = TRUE) |> plot()
#'
intervals <- function (bp,  which = 1:bp$g, col = biplotEZ:::ez.col, lwd = 1, lty = 1,
                       label = FALSE, label.name = rownames(bp$X[[1]]$values), label.col=NULL,
                       label.cex = 0.75, label.side = "bottom", label.offset = 0.5)
{
  g <- bp$g
  n <- bp$n
  p <- bp$p

  if(is.null(which) & length(col)==0) col <- biplotEZ:::ez.col

  if(!is.null(label.col) | any(label.side!="bottom") | any(label.offset !=0.5) | any(label.cex!=0.75))
    label<-TRUE

  if(is.null(label.name)) label.name <- rownames(bp[[1]]$Z$values)
  if(is.null(label.name)) label.name <- names(bp[[1]]$Z$values)

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

  # Expand lwd to length g
  lwd.len <- length(lwd)
  lwd <- lwd[ifelse(1:g%%lwd.len==0,lwd.len,1:g%%lwd.len)]
  if(is.null(lwd)){lwd <- rep(0, g)}

  # Expand lty to length g
  lty.len <- length(lty)
  lty <- lty[ifelse(1:g%%lty.len==0,lty.len,1:g%%lty.len)]
  if(is.null(lty)){lty <- rep(0, g)}

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

  bp$intervals = list(which = which, col = col, lwd = lwd, lty = lty,
                      label = label, label.name = label.name,
                      label.col = label.col, label.cex = label.cex, label.side = label.side,
                      label.offset = label.offset)
  bp
}

# ----------------------------------------------------------------------------------------------
#' Format aesthetics for vertices representation
#'
#' @description
#' This function allows the user to format the aesthetics for the entities.
#'
#' @param bp an object of class \code{ddbiplot}.
#' @param which a vector containing the groups or classes for which the vertices should be
#'              displayed, with default \code{bp$g}.
#' @param col the colour(s) for the vertices, with default \code{blue}.
#' @param pch the plotting character(s) for the vertices, with default \code{1}.
#' @param cex the character expansion of the plotting character(s), with default \code{1}.
#' @param lwd the line width for the vertices, with default \code{1}.
#' @param lty the line type for the vertices, with default \code{1}.
#' @param label a logical value indicating whether the vertices should be labelled, with default
#'              \code{FALSE}.
#' @param label.name a vector of the same length as \code{which} with label names for the vertices,
#'                   with default \code{NULL}. If \code{NULL}, the rownames of the entities are
#'                   used. Alternatively, a custom vector of length \code{n} should be used.
#' @param label.col a vector of the same length as \code{which} with label colours for the vertices,
#'                  with default as the same colour of the vertices.
#' @param label.cex a vector of the same length as \code{which} with label text expansions for the
#'                  vertices, with default \code{0.3}.
#' @param label.side the side at which the label of the entity appears, with default
#'                   \code{bottom}. Note that unlike the argument \code{pos} in \code{text()},
#'                   options are "\code{bottom}", "\code{left}", "\code{top}", "\code{right}" and
#'                   not \code{1}, \code{2}, \code{3}, \code{4}.
#' @param label.offset the offset of the label from the plotted vertices. See \code{?text} for a
#'                     detailed explanation of the argument \code{offset}.
#' @param type either \code{"connect"} to connect vertices in full space such as a cube in 3D or
#'             \code{"convexhull"} to construct a convex hull around the vertices.
#'
#' @return The object of class \code{ddbiplot} will be appended with a list called \code{vertices}
#'         containing the following elements:
#' \item{which}{a vector containing the groups or classes for which the samples (and means) are
#'              displayed.}
#' \item{col}{the colour(s) of the vertices.}
#' \item{pch}{the plotting character(s) of the vertices.}
#' \item{cex}{the character expansion of the plotting character(s).}
#' \item{lwd}{the line width for connecting vertices or a convex hull.}
#' \item{lty}{the line type for connecting vertices or a convex hull.}
#' \item{label}{a logical value indicating whether vertices are labelled.}
#' \item{label.name}{the label names of the vertices.}
#' \item{label.col}{the label colours of the vertices.}
#' \item{label.cex}{the label text expansions of the vertices.}
#' \item{label.side}{the side at which the label of the vertices appears..}
#' \item{label.offset}{the offset of the label from the vertices.}
#' \item{type}{either \code{"convexhull"} or \code{"connect"}}
#'
#' @usage
#' vertices (bp,  which = 1:bp$g, col = biplotEZ:::ez.col, pch = 1, cex = 0.3,
#'           lwd = 1, lty = 1, label = FALSE, label.name = rownames(bp$X[[1]]$values),
#'           label.col=NULL, label.cex = 0.75, label.side = "bottom", label.offset = 0.5,
#'           type = "convexhull")
#' @aliases vertices
#'
#' @export
#'
#' @import biplotEZ
#'
#' @examples
#' ddbiplot(data = Oils.data) |> PCA() |> vertices(col="purple",lwd=2, label = TRUE) |> plot()
#'
vertices <- function (bp,  which = 1:bp$g, col = biplotEZ:::ez.col, pch = 1, cex = 0.3, lwd = 1, lty = 1,
                       label = FALSE, label.name = rownames(bp$X[[1]]$values), label.col=NULL,
                       label.cex = 0.75, label.side = "bottom", label.offset = 0.5,
                       type = "convexhull")
{
  g <- bp$g
  n <- bp$n
  p <- bp$p

  if(is.null(which) & length(col)==0) col <- biplotEZ:::ez.col
  if(!is.null(label.col) | any(label.side!="bottom") | any(label.offset !=0.5) | any(label.cex!=0.75))
    label<-TRUE

  if(is.null(label.name)) label.name <- rownames(bp[[1]]$Z$values)
  if(is.null(label.name)) label.name <- names(bp[[1]]$Z$values)

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

  # Expand pch to length g
  pch.len <- length(pch)
  pch <- pch[ifelse(1:g%%pch.len==0,pch.len,1:g%%pch.len)]
  if(is.null(pch)){pch <- rep(0, g)}

  # Expand pch to length g
  cex.len <- length(cex)
  cex <- cex[ifelse(1:g%%cex.len==0,cex.len,1:g%%cex.len)]
  if(is.null(cex)){cex <- rep(0, g)}

  # Expand lwd to length g
  lwd.len <- length(lwd)
  lwd <- lwd[ifelse(1:g%%lwd.len==0,lwd.len,1:g%%lwd.len)]
  if(is.null(lwd)){lwd <- rep(0, g)}

  # Expand lty to length g
  lty.len <- length(lty)
  lty <- lty[ifelse(1:g%%lty.len==0,lty.len,1:g%%lty.len)]
  if(is.null(lty)){lty <- rep(0, g)}

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

  if (is.na(match(type, c("connect","convexhull"))))
  {  warning ("type should be either 'connect' or 'convexhull'; reset to 'connect'")
     type <- "connect"
  }

  bp$vertices = list(which = which, col = col, pch = pch, cex = cex, lwd = lwd, lty = lty, label = label,
                     label.name = label.name, label.col = label.col, label.cex = label.cex,
                     label.side = label.side, label.offset = label.offset, type = type)
  bp
}

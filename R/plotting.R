# ----------------------------------------------------------------------------------------------
#' Generic Plotting function of objects of class ddPCA_intervals
#'
#' @param x An object of class \code{ddPCA_intervals}.
#' @param type either "vertices" or "intervals" with default \code{"vertices"}
#' @param exp.factor a numeric value with default axes of the biplot. Larger values are specified
#'                   for zooming out with respect to sample points in the biplot display and smaller
#'                   values are specified for zooming in with respect to sample points in the biplot
#'                   display.
#' @param axis.predictivity either a logical or a numeric value between \code{0} and \code{1}. If
#'                          it is a numeric value, this value is used as threshold so that only axes
#'                          with axis predictivity larger than the threshold is displayed. If
#'                          \code{axis.predictivity = TRUE}, the axis colour is 'diluted' in
#'                          proportion with the axis predictivity.
#' @param sample.predictivity either a logical or a numeric value between 0 and 1. If it is a
#'                            numeric value, this value is used as threshold so that only samples
#'                            with sample predictivity larger than the threshold is displayed. If
#'                            \code{sample.predictivity = TRUE}, the sample size is shrinked in
#'                            proportion with the sample predictivity.
#' @param zoom a logical value allowing the user to select an area to zoom into.
#' @param add a logical value allowing the user to add the biplot to a current plot. If
#'            \code{add = TRUE} the argument \code{zoom} is inactive.
#' @param xlim the horizontal limits of the plot.
#' @param ylim the vertical limits of the plot.
#' @param ... additional arguments.
#'
#' @importFrom biplotEZ axes legend.type
#'
#' @return An object of class \code{ddPCA_intervals}.
#'
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (mtcars, entities = "cyl", interval=c("mpg","disp","hp"))
#' ddbiplot(data = obj) |> PCA() |> plot()
plot.ddPCA_intervals <- function(x, type = "vertices", exp.factor=1.2, axis.predictivity=NULL,
                                 sample.predictivity=NULL, zoom = FALSE, add = FALSE, xlim = NULL,
                                 ylim = NULL, ...)
{
  if (is.null(x$Z)) stop ("Add a biplot method before generating a plot")
  else Z <- x$Z
  allZ <- rbind (Z$lo, Z$up)

  #aesthetics for intervals
  if (is.null(x$intervals)) x <- intervals(x)

  if (add) zoom <- FALSE
  if(zoom)
    grDevices::dev.new()

  # Predict samples
  if (!is.null(x$predict$samples)) stop ("predict samples not yet implemented")
#    predict.mat <- Z[x$predict$samples, , drop = F]
  else predict.mat <- NULL

  # Predict means
  if (!is.null(x$predict$means)) stop ("predict means not yet implemented")
#    predict.mat <- rbind(predict.mat, x$Zmeans[x$predict$means, , drop = F])

  if (x$dim.biplot == 3) stop ("3D not yet implemented") #plot3D(bp=x, exp.factor=exp.factor, ...)
  else
  {
    old.par <- graphics::par(pty = "s", ...)
    withr::defer(graphics::par(old.par))

    if(x$dim.biplot == 1) stop ("1D not yet implemented")
#      { plot1D (bp=x, exp.factor=exp.factor)
#      }

    else # Plot 2D biplot
    {
      if(is.null(xlim) & is.null(ylim)){
        xlim <- range(allZ[, 1] * exp.factor)
        ylim <- range(allZ[, 2] * exp.factor)
      }

      # Start with empty plot
      if (!add)
        plot(allZ[, 1] * exp.factor, allZ[, 2] * exp.factor, xlim = xlim, ylim = ylim,
             xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", xaxs = "i", yaxs = "i", asp = 1)

      usr <- graphics::par("usr")

      # Density
      if(!is.null(x$z.density)) stop ("density not yet implemented") #.density.plot(x$z.density, x$density.style)

      # Axes
      if (is.null(x$axes)) x <- biplotEZ::axes(x)
      ax.aes <- x$axes
      if (all(ax.aes$names=="")) ax.aes$names <- names(x$X)

      # Axis predictivity
      too.small <- NULL
      if (!is.null(axis.predictivity)) stop ("axis predictivity not yet implemented")

      if (length(ax.aes$which) > 0)
        {
          Xhat <- allZ %*% solve(x$Lmat)[x$e.vects,]
          if (x$scaled) Xhat <- scale(Xhat, center=FALSE, scale=1/x$sd)
          if (x$center) Xhat <- scale(Xhat, center=-1*x$means, scale=FALSE)

          if(!is.null(x$PCOaxes))
          { if (x$PCOaxes == "splines")
            {
              allX <- sapply(x$X, function(z) return (z$values))
              z.axes <- lapply(1:length(ax.aes$which), biplotEZ:::biplot.spline.axis, allZ, allX,
                             means=x$means, sd=x$sd, n.int=ax.aes$ticks,
                             spline.control=x$spline.control)
              biplotEZ:::.nonlin.axes.plot(z.axes, ax.aes, predict.mat, too.small, usr=usr, x=x)
            }
            else if(x$PCOaxes == "regression")
                   {
                     z.axes <- lapply(1:length(ax.aes$which), biplotEZ:::.calibrate.axis, Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                               ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
                     biplotEZ:::.lin.axes.plot(z.axes, ax.aes, predict.mat, too.small,usr=usr,predict_which=x$predict$which)
                   }
          }
          else
          { # Otherwise calibrate linear axes
            z.axes <- lapply(1:length(ax.aes$which), biplotEZ:::.calibrate.axis, Xhat, x$means, x$sd, x$ax.one.unit, ax.aes$which,
                             ax.aes$ticks, ax.aes$orthogx, ax.aes$orthogy)
            biplotEZ:::.lin.axes.plot(z.axes, ax.aes, predict.mat, too.small,usr=usr,predict_which=x$predict$which)
          }

         # Interpolate new axes
        if(!is.null(x$newvariable)) stop ("new variables not yet implemented")

#        # Fit measures
#        too.small <- NULL
#        cex.vec <- rep(1, x$n)
#        if (!is.null(sample.predictivity) & !inherits(x, "CVA"))
#        {
#          if(is.null(x$sample.predictivity)) x <- fit.measures(x)
#          if(is.numeric(sample.predictivity))
#            too.small <- (1:x$n)[x$sample.predictivity<sample.predictivity]
#          if(sample.predictivity)
#            cex.vec <- x$sample.predictivity
#        }

        # Samples
        type <- c("intervals","vertices")[pmatch (type, c("intervals","vertices"))]
        if (is.null(x$intervals)) x <- intervals(x)
        if (is.null(x$vertices)) x <- vertices(x)
        if (type == "vertices")
          if  (!is.null(x$intervals$which))
            .interval.asvertices (x$X, x$Vr, x$group.aes, x$vertices, x$n, x$g.names, NULL,
                                  rep(1,x$n), usr, x$alpha.bag.outside, x$alpha.bag.aes)
        if (type == "intervals")
          if  (!is.null(x$vertices$which))
            .interval.asint (x$X, x$Vr, x$group.aes, x$intervals, x$n, x$p, x$g.names, NULL,
                             rep(1,x$n), usr, x$alpha.bag.outside, x$alpha.bag.aes)

        # New samples
        if (!is.null(x$Znew)) warning ("new sample not yet implememnted") #if (is.null(x$newsamples)) x <- newsamples(x)
        #if (!is.null(x$Znew)) .newsamples.plot (x$Znew, x$newsamples, ggrepel.new, usr=usr)

        # Means
#        if (!is.null(x$class.means)) warning ("class means not yet implemented") #if (x$class.means)
#        {
#          if (is.null(x$means.aes)) x <- means(x)
#          .means.plot (x$Zmeans, x$means.aes, x$g.names, ggrepel.means,usr=usr)
#        }

        # Alpha bags
        if (!is.null(x$alpha.bags)) warning ("alpha bags not yet implemented") #.bags.plot (x$alpha.bags, x$alpha.bag.aes)

        # Ellipse
        if (!is.null(x$conc.ellipses)) warning ("ellipses not yet implemented") # .conc.ellipse.plot (x$conc.ellipses, x$conc.ellipse.aes)

        # Title
        if (!is.null(x$Title)) graphics::title(main=x$Title)

        # Legends
        if (!is.null(x$legend))
        {
          x$samples$col <- x$intervals$col
          if (is.null(x$samples$col)) x$samples$col <- x$vertices$col
          x$samples$pch <- rep(15,length(x$samples$col))
          x$samples$which <- x$intervals$which
          if (is.null(x$samples$which)) x$samples$which <- x$vertices$which
          do.call(biplotEZ:::biplot.legend, list(bp=x, x$legend.arglist))
        }
      }

    }

  }

  if(zoom){
    cat("Choose upper left hand corner:\n")
    a <- graphics::locator(1)
    cat("Choose lower right hand corner:\n")
    b <- graphics::locator(1)
    arguments <- as.list(match.call())
    arguments[[1]] <- NULL
    arguments$x <- x
    arguments$zoom <- FALSE
    arguments$xlim <- c(a$x,b$x)[order(c(a$x,b$x))]
    arguments$ylim <- c(a$y,b$y)[order(c(a$y,b$y))]
    grDevices::dev.off()
    do.call(plot.ddPCA_intervals,arguments)
  }

  invisible(x)
}

#' Plot PCA of interval scaled data as vertices
#'
#' @param X an object of class \code{ddPCA_intervals}.
#' @param Vr the matrix to transform the data to principal components.
#' @param group.aes a vector identifying groups of aesthetic formatting.
#' @param vertices.aes a list returned as the \code{vertices} component from the
#'                       function \code{vertices()}.
#' @param n the number of entities.
#' @param g.names a vector identifying groups for aesthetic formatting.
#' @param too.small a cut-off value for minimum predictivity to show on the biplot
#' @param lwd.vec a factor to illustrate predictivity with line widths.
#' @param usr the current plotting region.
#' @param alpha.bag.outside entities to plot outside an alpha-bag.
#' @param alpha.bag.aes the aesthetic formatting for alpha-bags.
#'
#' @noRd
#'
.interval.asvertices <- function (X, Vr, group.aes, vertices.aes, n, g.names, too.small,
                             lwd.vec, usr = usr, alpha.bag.outside, alpha.bag.aes)
{
  # - only performed once for all variables:
  #This is to plot the points outside the alphabag
  which.vertices <- rep(FALSE, n)
  if(!is.null(alpha.bag.outside)){
    for (j in 1:length(alpha.bag.aes$which))
      which.vertices[group.aes == g.names[vertices.aes$which[j]]] <- alpha.bag.outside[[j]]
  }
  else {
    for (j in 1:length(vertices.aes$which))
      which.vertices[group.aes == g.names[vertices.aes$which[j]]] <- TRUE
  }
  groups <- levels(group.aes)

  # - done for each of the n entities:
  connection.mat <- NULL
  for (i in 1:n)
  {
    if (!which.vertices[i]) next
    out <- create.vertices(X, i)
    Xmat <- out$vertices
    Zmat <- Xmat %*% Vr

    x.vals <- Zmat[, 1]
    y.vals <- Zmat[, 2]
    invals <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    if(!any(invals)) next

    plot.col <- vertices.aes$col[group.aes[i]]
    plot.pch <- vertices.aes$pch[group.aes[i]]
    plot.cex <- vertices.aes$cex[group.aes[i]]
    plot.lwd <- vertices.aes$lwd[group.aes[i]]
    plot.lty <- vertices.aes$lty[group.aes[i]]

    if (!is.null(too.small)) warning ("predictivities to be implemented")
    graphics::points (Zmat, col = plot.col, pch = plot.pch, cex=plot.cex)
    if (vertices.aes$type == "convexhull")
      graphics::polygon(Zmat[grDevices::chull(Zmat),], border = plot.col, lwd = plot.lwd, lty = plot.lty)

    if (vertices.aes$type == "connect")
    {
      if (is.null(connection.mat))
      {
        out <- create.vertices (X, i, connect = TRUE)
        connection.mat <- out$connections
        if (nrow(connection.mat)==0) connection.mat <- matrix (1:2, nrow=1)
      }
      for (k in 1:nrow(connection.mat))
        graphics::lines (x = Zmat[connection.mat[k,],1], y = Zmat[connection.mat[k,],2],
                         col = plot.col, lwd = plot.lwd, lty = plot.lty)
    }
    if (vertices.aes$label[i])
    {
      text.pos <- match(vertices.aes$label.side[j], c("bottom", "left", "top", "right"))
      if (vertices.aes$label.side[j] == "bottom") { Zx <- mean(range(Zmat[,1]))
                                                    Zy <- min(Zmat[,2])
      }
      if (vertices.aes$label.side[j] == "left") { Zx <- min(Zmat[,1])
                                               Zy <- mean(range(Zmat[,2]))
      }
      if (vertices.aes$label.side[j] == "top") { Zx <- mean(range(Zmat[,1]))
                                              Zy <- max(Zmat[,2])
      }
      if (vertices.aes$label.side[j] == "right") { Zx <- max(Zmat[,1])
                                                Zy <- mean(range(Zmat[,2]))
      }
      graphics::text(Zx, Zy, labels = vertices.aes$label.name[i],
                     cex = vertices.aes$label.cex[i], col = vertices.aes$label.col[i],
                     pos = text.pos, offset = vertices.aes$label.offset[i])
    }
  }
}

#' Plot PCA of interval scaled data as intervals
#'
#' @param X an object of class \code{ddPCA_intervals}.
#' @param Vr the matrix to transform the data to principal components.
#' @param group.aes a vector identifying groups of aesthetic formatting.
#' @param interval.aes a list returned as the \code{interval} component from the
#'                       function \code{intervals()}.
#' @param n the number of entities.
#' @param p the number of interval variables.
#' @param g.names a vector identifying groups for aesthetic formatting.
#' @param too.small a cut-off value for minimum predictivity to show on the biplot
#' @param lwd.vec a factor to illustrate predictivity with line widths.
#' @param usr the current plotting region.
#' @param alpha.bag.outside entities to plot outside an alpha-bag.
#' @param alpha.bag.aes the aesthetic formatting for alpha-bags.
#'
#' @noRd
#'
.interval.asint <- function (X, Vr, group.aes, interval.aes, n, p, g.names, too.small,
                             lwd.vec, usr = usr, alpha.bag.outside, alpha.bag.aes)
{
  # - only performed once for all variables:
  #This is to plot the points outside the alphabag
  which.intervals <- rep(FALSE, n)
  if(!is.null(alpha.bag.outside)){
    for (j in 1:length(alpha.bag.aes$which))
      which.intervals[group.aes == g.names[interval.aes$which[j]]] <- alpha.bag.outside[[j]]
  }
  else {
    for (j in 1:length(interval.aes$which))
      which.intervals[group.aes == g.names[interval.aes$which[j]]] <- TRUE
  }
  groups <- levels(group.aes)

  label.aes <- data.frame (no=1:n, shown=rep(FALSE, n), names=interval.aes$label.name,
                           label=interval.aes$label, label.side = interval.aes$label.side,
                           label.cex = interval.aes$label.cex,
                           label.col = interval.aes$label.col,
                           label.offset = interval.aes$label.offset)

  centres <- sapply (X, function(x) apply(x$value, 1, mean))
  ZZ <- cbind(centres%*%Vr,centres%*%Vr) # x-min, y-min, x-max, y-max for each entity

  # - done for each of the p intervals:
  for (j in 1:p)
  {
    Xlo <- Xup <- centres
    Xlo[,j] <- X[[j]]$values[,1]
    Xup[,j] <- X[[j]]$values[,2]
    Zlo <- Xlo %*% Vr
    Zup <- Xup %*% Vr

    x.vals <- Zlo[, 1]
    y.vals <- Zlo[, 2]
    invals.lo <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    x.vals <- Zup[, 1]
    y.vals <- Zup[, 2]
    invals.up <- x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]
    invals <- invals.lo | invals.up
    if(!any(invals)) next

    plot.data <- data.frame (no=1:n, group.aes = group.aes, col = rep(NA,n),
                             lwd = rep(NA,n), lwd.vec, lty = rep(NA,n), Zlo, Zup)
    for(i in 1:length(interval.aes$which))
    {
      plot.data$col[group.aes == g.names[interval.aes$which[i]]] <- interval.aes$col[i]
      plot.data$lwd[group.aes == g.names[interval.aes$which[i]]] <- interval.aes$lwd[i]
      plot.data$lty[group.aes == g.names[interval.aes$which[i]]] <- interval.aes$lty[i]
    }

    plot.data <- plot.data[which.intervals,]
    plot.data <- plot.data[invals[which.intervals],]
    if (!is.null(too.small))
      plot.data <- plot.data[-stats::na.omit(match(too.small, plot.data[,1])),]
    label.aes$shown [match(plot.data$no, label.aes$no)] <- TRUE
    min.x <- apply (plot.data[,c(7,9)],1,min)
    ZZ[ZZ[,1]>min.x,1] <- min.x[ZZ[,1]>min.x]
    max.x <- apply (plot.data[,c(7,9)],1,max)
    ZZ[ZZ[,3]<max.x,3] <- max.x[ZZ[,3]<max.x]
    min.y <- apply (plot.data[,c(8,10)],1,min)
    ZZ[ZZ[,2]>min.y,2] <- min.y[ZZ[,2]>min.y]
    max.y <- apply (plot.data[,c(8,10)],1,max)
    ZZ[ZZ[,4]<max.y,4] <- max.y[ZZ[,4]<max.y]

    plot.data <- plot.data[,-1]
    entity.aes <- plot.data[,2:5]
    plot.data <- plot.data[,-(1:5)]

    for (i in 1:nrow(plot.data))
      graphics::lines (x = plot.data[i,c(1,3)], y = plot.data[i,c(2,4)], col = entity.aes$col[i],
                       lwd = entity.aes$lwd.vec[i] * entity.aes$lwd[i], lty = entity.aes$lty[i])
  }
  show.labels <- interval.aes$label & label.aes$shown
  ZZ <- ZZ[show.labels,]
  label.aes <- label.aes[show.labels,]
  if (nrow(label.aes) > 0)
  for (i in 1:nrow(label.aes))
    {  text.pos <- match(label.aes$label.side[i], c("bottom", "left", "top", "right"))
       if (label.aes$label.side[i] == "bottom") { Zx <- mean(ZZ[i,c(1,3)])
                                                 Zy <- ZZ[i,2]
                                               }
       if (label.aes$label.side[i] == "left") { Zx <- ZZ[i,1]
                                                Zy <- mean(ZZ[i,c(2,4)])
                                              }
       if (label.aes$label.side[i] == "top") { Zx <- mean(ZZ[i,c(1,3)])
                                               Zy <- ZZ[i,4]
                                             }
       if (label.aes$label.side[i] == "right") { Zx <- ZZ[i,3]
                                                 Zy <- mean(ZZ[i,c(2,4)])
                                               }
       if (label.aes$label[i])
         graphics::text(Zx, Zy, labels = label.aes$names[i],
                        cex = label.aes$label.cex[i], col = label.aes$label.col[i],
                        pos = text.pos, offset = label.aes$label.offset[i])
    }
}

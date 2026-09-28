# ----------------------------------------------------------------------------------------------
#' Generic Plotting function of objects of class ddPCA
#'
#' @param x An object of class \code{ddPCA_intervals}.
#' @param exp.factor a numeric value with default axes of the biplot. Larger values are specified
#'                   for zooming out with respect to sample points in the biplot display and smaller
#'                   values are specified for zooming in with respect to sample points in the biplot
#'                   display.
#' @param simulation.n the number of simulated values to obtain a non-parametric density
#'                     estimate of the distribution of a unit in the biplot space
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
#' @return An object of class \code{ddPCA}.
#'
#' @export
#'
#' @examples
#' obj <- suminto_ddobj (mtcars, units = "cyl", interval=c("mpg","disp","hp"))
#' ddbiplot(data = obj) |> PCA() |> plot()
plot.ddPCA <- function(x, exp.factor=1.2, simulation.n = 1000, axis.predictivity=NULL,
                                 sample.predictivity=NULL, zoom = FALSE, add = FALSE, xlim = NULL,
                                 ylim = NULL, ...)
{
  if (is.null(x$Z)) stop ("Add a biplot method before generating a plot")
  Z <- do.call(rbind, x$Z)

  #aesthetics for units
  if (is.null(x$units)) x <- units(x)

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
        xlim <- range(Z[, 1] * exp.factor)
        ylim <- range(Z[, 2] * exp.factor)
      }

      # Start with empty plot
      if (!add)
        plot(Z[, 1] * exp.factor, Z[, 2] * exp.factor, xlim = xlim, ylim = ylim,
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

      Xmat <- NULL
      for (j in x$numeric.vars)
      {
        if (x$raw.X[[j]]$type != "histogram") Xmat <- cbind (Xmat, range(x$raw.X[[j]]$values, na.rm=TRUE))
        else Xmat <- cbind (Xmat, range(x$raw.X[[j]]$intervals, na.rm=TRUE))
      }
      Xmat[2,] <- ifelse (Xmat[2,]-Xmat[1,] < .Machine$double.eps^0.4, Xmat[2,]+0.1, Xmat[2,])
      if (length(ax.aes$which) > 0)
        {
            z.axes <- lapply(1:length(ax.aes$which), biplotEZ:::.calibrate.axis, Xmat,
                             x$means, x$sd, x$ax.one.unit, ax.aes$which,
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

        # Units
          if  (!is.null(x$units$which))
            .unitsplot (x$X, x$Vr, x$Z, simulation.n, x$group.aes, x$units,
                        x$n, x$p, x$g.names, NULL,
                        usr, x$alpha.bag.outside, x$alpha.bag.aes)

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
          x$samples$col <- x$units$col
          x$samples$pch <- rep(15,length(x$samples$col))
          x$samples$which <- x$units$which
          do.call(biplotEZ:::biplot.legend, list(bp=x, x$legend.arglist))
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
    do.call(plot.ddPCA,arguments)
  }

  invisible(x)
}

#' Plot units in the biplot
#'
#' @param X an object of class \code{ddPCA_intervals}.
#' @param Vr the matrix to transform the data to principal components.
#' @param ZZ a list of coordinates of the vertices of the units
#' @param simulation.n the number of simulated values to obtain a non-parametric density
#'                     estimate of the distribution of a unit in the biplot space
#' @param group.aes a vector identifying groups of aesthetic formatting.
#' @param unit.aes a list returned as the \code{interval} component from the
#'                       function \code{intervals()}.
#' @param n the number of units.
#' @param p the number of interval variables.
#' @param g.names a vector identifying groups for aesthetic formatting.
#' @param too.small a cut-off value for minimum predictivity to show on the biplot
#' @param usr the current plotting region.
#' @param alpha.bag.outside units to plot outside an alpha-bag.
#' @param alpha.bag.aes the aesthetic formatting for alpha-bags.
#'
#' @noRd
#'
.unitsplot <- function (X, Vr, ZZ, simulation.n, group.aes, unit.aes, n, p, g.names, too.small,
                             usr = usr, alpha.bag.outside, alpha.bag.aes)
{
  which.units <- rep (FALSE, n)
  for (j in 1:length(unit.aes$which))
    which.units[group.aes == g.names[unit.aes$which[j]]] <- TRUE
  groups <- levels(group.aes)

  for (i in 1:n)
  {
    if (which.units[i])
    {
      X0mat <- matrix (nrow=simulation.n, ncol = p)
      tval.mat <- matrix (stats::runif (simulation.n*p), ncol = p)
      for (j in 1:p)
        X0mat[,j] <- quantij(X, i=i, j=j)(tval.mat[,j])
      Z <- X0mat %*% Vr

      this.col <- grDevices::col2rgb(unit.aes$col[(1:nlevels(group.aes))[group.aes[i]==groups]], alpha=TRUE)
      col0 <- grDevices::rgb (this.col[1], this.col[2], this.col[3], alpha=50, maxColorValue=255)
      col1 <- grDevices::rgb (this.col[1], this.col[2], this.col[3], alpha=200, maxColorValue=255)
      col.vec <- grDevices::colorRampPalette(c(col0, col1), alpha = TRUE)(100)

      bw_x <- MASS::bandwidth.nrd(Z[, 1])
      bw_y <- MASS::bandwidth.nrd(Z[, 2])
      if (!(bw_x > 0) | !(bw_y > 0))
      {
        graphics::points (Z, col=col.vec[100], pch=16)
      }
      else
      {
        Z.kde <- MASS::kde2d(Z[,1], Z[,2], h = c (bw_x, bw_y), n=200)
        vertices.hull <- ZZ[[i]][grDevices::chull(ZZ[[i]]),]
        hull <- rbind(vertices.hull, vertices.hull[1, ])
        kde.grid <- expand.grid (Z.kde$x, Z.kde$y)
        inside <- sp::point.in.polygon(kde.grid[,1], kde.grid[,2],
                                       vertices.hull[,1], vertices.hull[,2])
        Z.kde$z[!matrix(inside,nrow=length(Z.kde$x))] <- NA
        if (all(is.na(Z.kde$z))) graphics::points (Z, col=col.vec[100], pch=16, cex=0.25)
        else graphics::image (Z.kde, col=col.vec, add = TRUE)
      }
      if (unit.aes$label[i])
      {
        text.pos <- match(unit.aes$label.side[i],
                          c("bottom", "left", "top", "right"))

        if (unit.aes$label.side[i] == "bottom") { Zx <- mean(Z[,1], na.rm=TRUE)
                                                   Zy <- min(Z[,2], na.rm=TRUE)
                                                   pos <- 1
        }
        if (unit.aes$label.side[i] == "left") { Zx <- min(Z[,1], na.rm=TRUE)
                                                 Zy <- mean(Z[,2], na.rm=TRUE)
                                                 pos <- 2
        }
        if (unit.aes$label.side[i] == "top") { Zx <- mean(Z[i,1], na.rm=TRUE)
                                                Zy <- max(Z[i,2], na.rm=TRUE)
                                                pos <- 3
        }
        if (unit.aes$label.side[i] == "right") { Zx <- max(Z[i,1], na.rm=TRUE)
                                                  Zy <- mean(Z[i,2], na.rm=TRUE)
                                                  pos <- 4
        }
        graphics::text(Zx, Zy, labels = unit.aes$label.name[i],
                       cex = unit.aes$label.cex[i], col = unit.aes$label.col[i],
                       pos = pos, offset = unit.aes$label.offset[i])
      }
    }
  }
}

#.calibrate.axis <-  function (j, Xhat, means, sd, axes.rows, ax.which, ax.tickvec,
#            ax.orthogxvec, ax.orthogyvec)
#{
#  ax.num <- ax.which[j]
#  tick <- ax.tickvec[j]
#  ax.direction <- axes.rows[ax.num, ]
#  r <- ncol(axes.rows)
#  ax.orthog <- rbind(ax.orthogxvec, ax.orthogyvec)
#  if (nrow(ax.orthog) < r)
#    ax.orthog <- rbind(ax.orthog, 0)
#  if (nrow(axes.rows) > 1)
#    phi.vec <- diag(1/diag(axes.rows %*% t(axes.rows))) %*%
#    axes.rows %*% ax.orthog[, ax.num]
#  else phi.vec <- (1/(axes.rows %*% t(axes.rows))) %*% axes.rows %*%
#    ax.orthog[, ax.num]
#
#  std.ax.tick.label <- if (X[[ax.num]]$type == "interval")
#                         pretty (X[[ax.num]]$values, n = tick)
#                       else pretty (X[[ax.num]]$intervals, n = tick)
#  std.range <- range(std.ax.tick.label)
#  std.ax.tick.label.min <- std.ax.tick.label - (std.range[2] - std.range[1])
#  std.ax.tick.label.max <- std.ax.tick.label + (std.range[2] - std.range[1])
#  std.ax.tick.label <- c(std.ax.tick.label, std.ax.tick.label.min,
#                         std.ax.tick.label.max)
#
#  interval <- (std.ax.tick.label - ddQmean (X, ax.num)(0.5))/sd[j]
#
#  axis.vals <- sort(unique(interval))
#  number.points <- length(axis.vals)
#  axis.points <- matrix(0, nrow = number.points, ncol = r)
#  for (i in 1:r) axis.points[, i] <- ax.orthog[i, ax.num] +
#    (axis.vals - phi.vec[ax.num]) * ax.direction[i]
#  axis.points <- cbind(axis.points, axis.vals * sd[j] + ddQmean (X, ax.num)(0.5))
#  slope <- (axis.points[1, 2] - axis.points[2, 2])/(axis.points[1,
#                                                                1] - axis.points[2, 1])
#  v <- NULL
#  if (is.na(slope)) {
#    v <- axis.points[1, 1]
#    slope = NULL
#  }
#  else if (abs(slope) == Inf) {
#    v <- axis.points[1, 1]
#    slope = NULL
#  }
#  intercept <- axis.points[1, 2] - slope * axis.points[1, 1]
#  details <- list(a = intercept, b = slope, v = v)
#  retvals <- list(coords = axis.points, a = intercept, b = slope,
#                  v = v)
#  return(retvals)
#}


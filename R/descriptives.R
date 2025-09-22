#' Computes the mean of a distributional data object
#'
#' @param x a list, typically a single component of an object of class \code{ddojb}
#'
#' @description
#' This function only works for an interval scaled or histogram scaled variable. For an interval
#' scaled variable \code{x} should be a list with component \code{$type = "interval"} and
#' component \code{values} a two-column matrix of interval endpoints. If \code{x} is a histogram
#' scaled variable, it should be a list with component \code{$type = "histogram"} and components
#' \code{intervals} and \code{proportions}.
#'
#' @references
#' Billard, L., 2008. Sample covariance functions for complex quantitative
#'  data. In Proceedings of IASC 2008, Yokohama, Japan, pp 157-163.
#'
#' @returns numeric mean value
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddmean (obj$ncases)
#' ddmean (obj$ncontrols)
#'
ddmean <- function (x)
{
  mean.val <- NA

  # --- interval scaled variable
  if (x$type == "interval")
  {
    mean.val <- sum(apply(x$values, 1, sum))/(2*nrow(x$values))
  }

  # --- histogram scaled variable
  if (x$type == "histogram")
  {
    nn <- ncol(x$intervals)
    bin.a.plus.b <- x$intervals[,-nn] + x$intervals[,-1]
    temp <- bin.a.plus.b * x$proportions
    mean.val <- sum(apply(temp, 1, sum, na.rm = TRUE))/(2*nrow(x$intervals))
  }

  mean.val
}

# ----------------------------------------------------------------------------------------------

#' Computes the variance or covariance of (a) distributional data object(s)
#'
#' @param x a list, typically a single component of an object of class \code{ddojb}
#' @param y optional, a list, typically a single component of an object of class \code{ddojb}. If
#'           \code{y} is specified, the covariance is computed.
#'
#' @description
#' This function only works for an interval scaled or histogram scaled variable. For an interval
#' scaled variable \code{x} should be a list with component \code{$type = "interval"} and
#' component \code{values} a two-column matrix of interval endpoints. If \code{x} is a histogram
#' scaled variable, it should be a list with component \code{$type = "histogram"} and components
#' \code{intervals} and \code{proportions}.
#'
#' @references
#' Billard, L., 2008. Sample covariance functions for complex quantitative data. In Proceedings
#' of IASC 2008, Yokohama, Japan, pp 157-163.
#'
#' @returns numeric variance or covariance value
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddvar (obj$ncases) # variance
#' ddvar (obj$ncases, obj$ncontrols) # covariance
#'
ddvar <- function (x, y)
{
  # --- variance
  if (missing(y))
  {
    var.val <- NA

    # --- interval scaled variable
    if (x$type == "interval")
    {
      var.val <- sum(apply(x$values, 1, function(int) int[1]^2 + int[1]*int[2] + int[2]^2))/
        (3*nrow(x$values)) - ddmean(x)^2
    }

    # --- histogram scaled variable
    if (x$type == "histogram")
    {
      nn <- ncol(x$intervals)
      temp <- (x$intervals[,-nn]^2 + x$intervals[,-nn]*x$intervals[,-1] + x$intervals[,-1]^2) *
                 x$proportions
      var.val <- sum(apply(temp, 1, sum, na.rm = TRUE))/(3*nrow(x$intervals)) - ddmean(x)^2
    }

    var.val
  }
  # --- covariance
  else
  {
    ddcov (x,y)
  }
}

# ----------------------------------------------------------------------------------------------

#' Computes the covariance of two distributional data objects
#'
#' @param x a list, typically a single component of an object of class \code{ddobj}
#' @param y a list, typically a single component of an object of class \code{ddobj}
#'
#' @description
#' This function only works for an interval scaled or histogram scaled variable. For an interval
#' scaled variable \code{x} should be a list with component \code{$type = "interval"} and
#' component \code{values} a two-column matrix of interval endpoints. If \code{x} is a histogram
#' scaled variable, it should be a list with component \code{$type = "histogram"} and components
#' \code{intervals} and \code{proportions}.
#'
#' @references
#' Billard, L., 2008. Sample covariance functions for complex quantitative data. In Proceedings
#' of IASC 2008, Yokohama, Japan, pp 157-163.
#'
#' @returns numeric variance or covariance value
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddcov (obj$ncases, obj$ncontrols)
#'
ddcov <- function (x, y)
{
  cov.val <- NA

  # --- interval scaled - histogram scaled variables
  if (x$type == "interval" & y$type == "histogram")
  {  x <- int.to.hist (x)  }
  if (x$type == "histogram" & y$type == "interval")
  { y <- int.to.hist (y)  }

  # --- interval scaled - interval scaled variables
  if (x$type == "interval" & y$type == "interval")
  {
    tot <- 0
    if (nrow(x$values) != nrow(y$values)) stop ("Differing number of samples for covariance.")
    mean.j <- ddmean(x)
    mean.k <- ddmean(y)
    for (i in 1:nrow(x$values))
      tot <- tot + (2 * (x$values[i,1] - mean.j) * (y$values[i,1] - mean.k) +
                    (x$values[i,1] - mean.j) * (y$values[i,2] - mean.k) +
                    (x$values[i,2] - mean.j) * (y$values[i,1] - mean.k) +
                    2* (x$values[i,2] - mean.j) * (y$values[i,2] - mean.k))
    cov.val <- tot / (6*nrow(x$values))
  }

  # --- histogram scaled - histogram scaled variables
  if (x$type == "histogram" & y$type == "histogram")
  {
    if (nrow(x$intervals) != nrow(y$intervals)) stop ("Differing number of samples for covariance.")
    nx <- ncol(x$intervals)
    ny <- ncol(y$intervals)
    mean.j <- ddmean(x)
    mean.k <- ddmean(y)

    mat.Lj <- as.matrix((x$intervals[,-nx] - mean.j) * x$proportions)
    mat.Uj <- as.matrix((x$intervals[,-1] - mean.j) * x$proportions)
    mat.Lk <- as.matrix((y$intervals[,-ny] - mean.k) * y$proportions)
    mat.Uk <- as.matrix((y$intervals[,-1] - mean.k) * y$proportions)
    mat.Lj[is.na(mat.Lj)] <- 0
    mat.Uj[is.na(mat.Uj)] <- 0
    mat.Lk[is.na(mat.Lk)] <- 0
    mat.Uk[is.na(mat.Uk)] <- 0

    cov.val <- sum(2 * t(mat.Lj) %*% mat.Lk +
                 t(mat.Lj) %*% mat.Uk +
                 t(mat.Uj) %*% mat.Lk +
                 2 * t(mat.Uj) %*% mat.Uk)/(6*nrow(x$intervals))
  }
  cov.val
}

# ----------------------------------------------------------------------------------------------

#' Computes the correlation of two distributional data objects
#'
#' @param x a list, typically a single component of an object of class \code{ddojb}
#' @param y a list, typically a single component of an object of class \code{ddojb}
#'
#' @returns numeric correlation value
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddcor (obj$ncases, obj$ncontrols)
#
ddcor <- function (x, y)
{
  ddvar(x,y)/sqrt(ddvar(x)*ddvar(y))
}

# ----------------------------------------------------------------------------------------------

#' Computes the covariance matrix of distributional data variables
#'
#' @param obj an object of class \code{ddobj}
#'
#' @returns a covariance matrix of size length of obj by length of obj
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddcovmat (obj)

ddcovmat <- function (obj)
{
  p <- length(obj)
  mat <- matrix (0, nrow=p, ncol=p)
  for (j in 1:p)
    for (k in j:p)
      if (j==k) mat[j,k] <- ddvar(obj[[j]])
      else mat[j,k] <- ddcov(obj[[j]], obj[[k]])
  var.vec <- diag(mat)
  mat <- mat + t(mat)
  diag(mat) <- var.vec
  mat
}

# ----------------------------------------------------------------------------------------------

#' Computes the correlation matrix of distributional data variables
#'
#' @param obj an object of class \code{ddobj}
#'
#' @returns a correlation matrix of size length of obj by length of obj
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddcormat (obj)

ddcormat <- function (obj)
{
  p <- length(obj)
  mat <- matrix (0, nrow=p, ncol=p)
  for (j in 1:(p-1))
    for (k in (j+1):p)
      mat[j,k] <- ddcor(obj[[j]], obj[[k]])
  mat <- mat + t(mat)
  diag(mat) <- 1
  mat
}

# ===============================================================================================

#' Computes the L2 Wasserstein variance or covariance of (a) distributional data object(s)
#'
#' @param x an object of class \code{ddojb}
#'
#' @description
#' This function uses the \code{WH.var.covar} method from the \code{HistDAWass} package to
#' compute variances and covariances. Interval scaled data are converted to histogram
#' scaled with a single bin and proportion = 1.
#'
#' @references
#' Irpino, A. and Verde, R. 2015. Basic statistics for distributional symbolic variables:
#' a new metric-based approach. Advances in Data Analysis and Classification, 9(2), pp.143-157.
#'
#' @returns variance-covariance matrix
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' WassL2var (obj)
#'
WassL2var <- function (x)
{
  hist.to.distrH <- function (a, i)
  {
    int <- as.numeric(a$intervals[i,])
    int <- int[!is.na(int)]
    prop <- as.numeric(a$proportions[i,])
    prop <- prop[!is.na(prop)]
    HistDAWass::distributionH (x = int, p = c(0,cumsum(prop)))
  }

  n <- switch(x[[1]]$type,
              numeric = length(x[[1]]$values),
              interval = nrow(x[[1]]$values),
              histogram = nrow(x[[1]]$intervals))
  unit.names <- switch(x[[1]]$type,
                         numeric = names(x[[1]]$values),
                         interval = rownames(x[[1]]$values),
                         histogram = rownames(x[[1]]$intervals))
  p <- length (x)
  distrH.list <- vector ("list", n*p)
  i <- 0
  for (j in 1:p)
  {
    if (x[[j]]$type == "numeric") x[[j]] <- num.to.int (x[[j]])
    if (x[[j]]$type == "interval") x[[j]] <- int.to.hist (x[[j]])
    for (h in 1:n)
      { i <- i + 1
        distrH.list[[i]] <- hist.to.distrH(x[[j]], i=h)
      }
  }
  MatHobj <- HistDAWass::MatH (distrH.list, nrow=n, ncol=p)
  HistDAWass::WH.var.covar (MatHobj)
}

#' Computes the L2 Wasserstein correlation matrix of distributional data object(s)
#'
#' @param x an object of class \code{ddojb}
#'
#' @description
#' This function uses the \code{WH.var.covar} method from the \code{HistDAWass} package to
#' compute correlations. Interval scaled data are converted to histogram
#' scaled with a single bin and proportion = 1.
#'
#' @references
#' Irpino, A. and Verde, R. 2015. Basic statistics for distributional symbolic variables:
#' a new metric-based approach. Advances in Data Analysis and Classification, 9(2), pp.143-157.
#'
#' @returns correlation matrix
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' WassL2cor (obj)
#'
WassL2cor <- function (x)
{
  covmat <- WassL2var (x)
  SDs <- diag(sqrt(diag(covmat)))
  solve(SDs) %*% covmat %*% solve(SDs)
}

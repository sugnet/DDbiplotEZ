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
#' Billard, L. and Diday, E. (2003) From the Statistics of Data to the Statistics of Knowledge.
#' \emph{Journal of the American Statistical Association.} 98:470-487.
#'
#' @returns numeric mean value
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, entities = "agegp", interval="ncases", histogram="ncontrols")
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
#' Billard, L. and Diday, E. (2003) From the Statistics of Data to the Statistics of Knowledge.
#' \emph{Journal of the American Statistical Association.} 98:470-487.
#'
#' @returns numeric variance or covariance value
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, entities = "agegp", interval="ncases", histogram="ncontrols")
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
#' Billard, L. and Diday, E. (2003) From the Statistics of Data to the Statistics of Knowledge.
#' \emph{Journal of the American Statistical Association.} 98:470-487.
#'
#' @returns numeric variance or covariance value
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, entities = "agegp", interval="ncases", histogram="ncontrols")
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
      tot <- tot + (x$values[i,2] + x$values[i,1]) * (y$values[i,2] + y$values[i,1])
    cov.val <- tot / (4*nrow(x$values)) - mean.j*mean.k
  }

  # --- histogram scaled - histogram scaled variables
  if (x$type == "histogram" & y$type == "histogram")
  {
    if (nrow(x$intervals) != nrow(y$intervals)) stop ("Differing number of samples for covariance.")
    nx <- ncol(x$intervals)
    ny <- ncol(y$intervals)
    mean.j <- ddmean(x)
    mean.k <- ddmean(y)

    mat.j <- (x$intervals[,-nx] + x$intervals[, -1]) * x$proportions
    mat.k <- (y$intervals[,-ny] + y$intervals[, -1]) * y$proportions
    mat.j[is.na(mat.j)] <- 0
    mat.k[is.na(mat.k)] <- 0
    temp <- as.matrix(t(mat.j)) %*% as.matrix(mat.k)
    cov.val <- sum(temp)/(4*nrow(x$intervals)) - mean.j*mean.k
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
#' obj <- suminto.ddobj (esoph, entities = "agegp", interval="ncases", histogram="ncontrols")
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
#' obj <- suminto.ddobj (esoph, entities = "agegp", interval="ncases", histogram="ncontrols")
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
#' obj <- suminto.ddobj (esoph, entities = "agegp", interval="ncases", histogram="ncontrols")
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

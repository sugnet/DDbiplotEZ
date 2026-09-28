#' Computes the Frechet mean's quantile function
#'
#' @param x an object of class \code{ddobj}
#' @param j the number(s) of the variable(s) for which the quantile function is computed.
#'          If \code{j} is null, the computation is performed for all variables.
#'
#' @returns a the quantile function
#' @export
#'
#' @examples
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddQmean (obj, j=1)(seq(from=0, to=1, len=20))
#'
ddQmean <- function (x, j)
{
  n <- switch(x[[1]]$type,
              numeric   = length(x[[1]]$values),
              interval  = nrow(x[[1]]$values),
              histogram = nrow(x[[1]]$intervals))

  # Precompute quantile functions for this j
  Qfuns <- lapply(seq_len(n), function(i)
    quantij(x, i = i, j = j)
  )

  # Return a single function
  function(tvec) {
    tvec <- as.vector(tvec)
    Qmat <- sapply(Qfuns, function(f) f(tvec))
    if (is.null(dim(Qmat))) mean(Qmat) else rowMeans(Qmat)
  }
}

# ------------------------------------------------------------------------------

#' Computes the variance or covariance based on Wasserstein distance of
#' (a) distributional data object(s)
#'
#' @param x an object of class \code{ddojb}
#'
#' @description
#' This function computes variances and covariances based on the \eqn{L_2} Wasserstein
#' distance as defined in Irpino and Verde (2015).
#'
#' @references
#' Irpino, A. and Verde, R. 2015. Basic statistics for distributional symbolic variables:
#' a new metric-based approach. Advances in Data Analysis and Classification, 9(2), pp.143-157.
#'
#' @returns variance-covariance matrix
#' @export
#'
#' @examples
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddvarW (obj)
#'
ddvarW <- function (x)
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.",
         call. = FALSE)
  }

  get.c.and.r <- function (intervals)
  {
    s <- length(intervals)
    c.vec <- (intervals[-1] + intervals[-s])/2
    r.vec <- diff(intervals)/2
    list (c=c.vec, r=r.vec)
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
    if (x[[j]]$type == "numeric") x[[j]] <- num_to_int (x[[j]])
    if (x[[j]]$type == "interval") x[[j]] <- int_to_hist (x[[j]])
    for (h in 1:n)
    {
      i <- i + 1
      distrH.list[[i]] <- hist_to_distrH(x[[j]], i=h)
    }
  }
#  x <- uniformly.dense.intervals(x)
#  s <- dim(x$dens.int)[3] - 1
#  c.list <- r.list <- vector("list", p)

#  for (j in 1:p)
#  {
#    cj.mat <- rj.mat <- matrix(0, nrow=n, ncol=s)
#    intervals.j <- x$dens.int[,j,]

#    for (i in 1:n) {
#      out <- get.c.and.r(intervals.j[i, ])
#      cj.mat[i, ] <- out$c
#      rj.mat[i, ] <- out$r
#    }
#    c.list[[j]] <- cj.mat
#    r.list[[j]] <- rj.mat
#  }

#  Cmean <- lapply(c.list, colMeans)
#  Rmean <- lapply(r.list, colMeans)
#  pvec <- x$proportions

#  mat <- matrix(0, nrow=p, ncol=p)
#  for (j in 1:p) {
#    cj.min.mean <- sweep(c.list[[j]], 2, Cmean[[j]])
#    rj.min.mean <- sweep(r.list[[j]], 2, Rmean[[j]])
#    for (k in j:p) {
#      ck.min.mean <- sweep(c.list[[k]], 2, Cmean[[k]])
#      rk.min.mean <- sweep(r.list[[k]], 2, Rmean[[k]])

#      sum.1.to.n <- ((cj.min.mean * ck.min.mean) + (rj.min.mean * rk.min.mean)/3)
#      sum.1.to.n <- apply (sum.1.to.n, 1, function (x) x * pvec)
#      mat[j, k] <- sum(sum.1.to.n) / n
#    }
#  }

#  diag.val <- diag(mat)
#  mat <- mat + t(mat)
#  diag(mat) <- diag.val

#  mat
  MatHobj <- HistDAWass::MatH (distrH.list, nrow=n, ncol=p)
  out <- HistDAWass::WH.var.covar(MatHobj)
  rownames (out) <- colnames (out) <- names(x)
  out
}

# -------------------------------------------------------------------------------------

#' Computes the correlation based on Wasserstein distance of distributional data objects
#'
#' @param x an object of class \code{ddojb}
#'
#' @description
#' This function computes correlations from the variances and covariances based on the
#' \eqn{L_2} Wasserstein distance as defined in Irpino and Verde (2015).
#'
#' @references
#' Irpino, A. and Verde, R. 2015. Basic statistics for distributional symbolic variables:
#' a new metric-based approach. Advances in Data Analysis and Classification, 9(2), pp.143-157.
#'
#' @returns correlation matrix
#' @export
#'
#' @examples
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddcorW (obj)
#'
ddcorW <- function (x)
{
  covmat <- ddvarW (x)
  SDs <- diag(sqrt(diag(covmat)))
  out <- solve(SDs) %*% covmat %*% solve(SDs)
  rownames (out) <- colnames (out) <- names(x)
  out
}


# ===============================================================================================


#' Computes the mean of a distributional data object as defined by Billard (2008)
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
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
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

#' Computes the variance or covariance of (a) distributional data object(s) as defined by Billard (2008)
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
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddvarB (obj$ncases) # variance
#' ddvarB (obj$ncases, obj$ncontrols) # covariance
#'
ddvarB <- function (x, y)
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
    ddcovB (x,y)
  }
}

# ----------------------------------------------------------------------------------------------

#' Computes the covariance of two distributional data objects as defined by Billard (2008)
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
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddcovB (obj$ncases, obj$ncontrols)
#'
ddcovB <- function (x, y)
{
  cov.val <- NA

  # --- interval scaled - histogram scaled variables
  if (x$type == "interval" & y$type == "histogram")
  {  x <- int_to_hist (x)  }
  if (x$type == "histogram" & y$type == "interval")
  { y <- int_to_hist (y)  }

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

#' Computes the correlation of two distributional data objects as defined by Billard (2008)
#'
#' @param x a list, typically a single component of an object of class \code{ddojb}
#' @param y a list, typically a single component of an object of class \code{ddojb}
#'
#' @returns numeric correlation value
#' @export
#'
#' @examples
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddcorB (obj$ncases, obj$ncontrols)
#
ddcorB <- function (x, y)
{
  ddvarB(x,y)/sqrt(ddvarB(x)*ddvarB(y))
}

# ----------------------------------------------------------------------------------------------

#' Computes the covariance matrix of distributional data variables as defined by Billard (2008)
#'
#' @param obj an object of class \code{ddobj}
#'
#' @returns a covariance matrix of size length of obj by length of obj
#' @export
#'
#' @examples
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddcovmatB (obj)

ddcovmatB <- function (obj)
{
  p <- length(obj)
  mat <- matrix (0, nrow=p, ncol=p)
  for (j in 1:p)
    for (k in j:p)
      if (j==k) mat[j,k] <- ddvarB(obj[[j]])
  else mat[j,k] <- ddcovB(obj[[j]], obj[[k]])
  var.vec <- diag(mat)
  mat <- mat + t(mat)
  diag(mat) <- var.vec
  rownames (mat) <- colnames (mat) <- names(obj)
  mat
}

# ----------------------------------------------------------------------------------------------

#' Computes the correlation matrix of distributional data variables as defined by Billard (2008)
#'
#' @param obj an object of class \code{ddobj}
#'
#' @returns a correlation matrix of size length of obj by length of obj
#' @export
#'
#' @examples
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' ddcormatB (obj)

ddcormatB <- function (obj)
{
  p <- length(obj)
  mat <- matrix (0, nrow=p, ncol=p)
  for (j in 1:(p-1))
    for (k in (j+1):p)
      mat[j,k] <- ddcorB(obj[[j]], obj[[k]])
  mat <- mat + t(mat)
  diag(mat) <- 1
  rownames (mat) <- colnames (mat) <- names(obj)
  mat
}

#' @importFrom biplotEZ PCA
#' @export
biplotEZ::PCA

#' Calculate elements for the PCA biplot for distributional data
#'
#' @description This function performs calculations for the construction of a PCA biplot for
#'              distributional data.
#'
#' @param bp an object of class \code{ddbiplot} obtained from preceding function \code{ddbiplot()}.
#' @param dim.biplot the dimension of the biplot. Only values \code{1}, \code{2} and \code{3} are accepted, with default \code{2}.
#' @param e.vects the vector indicating which eigenvectors (principal components) should be plotted in the biplot, with default \code{1:dim.biplot}.
#' @param group.aes a vector of the same length as the number of rows in the data matrix
#'                  for differentiated aesthetics for samples.
#' @param show.class.means a logical value indicating whether group means should be plotted in the
#'                         biplot.
#' @param correlation.biplot a logical value. If \code{FALSE}, the distances between sample points are
#'                           optimally approximated in the biplot. If \code{TRUE}, the correlations between
#'                           variables are optimally approximated by the cosine of the angles between
#'                           axes. Default is \code{FALSE}.
#'
#' @return an object of class \code{ddPCA}, inherits from class \code{ddbiplot}.
#' @export
#'
#' @examples
#' ddbiplot(data = Oils.data) |> PCA() |> plot()
#'
PCA.ddbiplot <- function (bp, dim.biplot = c(2, 1, 3), e.vects = 1:bp$p, group.aes=NULL,
                        show.class.means = FALSE, correlation.biplot=FALSE)
{

  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
  bp$g.names <-levels(factor(group.aes))
  bp$g <- length(bp$g.names)
  }

  if (!bp$center)
  {  warning("PCA requires a centred datamatrix. Your data was centred before computation. Use center = TRUE in the call to biplot()")
     bp <- ddbiplot (bp$X, center = TRUE, scaled=bp$scaled, bp$classes, bp$group.aes)
  }

  X <- bp$X
  n <- bp$n
  p <- bp$p
  Smat <- ddcovmat(X)

  svd.out <- svd(Smat)
  V.mat <- svd.out$v
  Lambda.mat <- diag(svd.out$d)
  Lmat <- svd.out$v
  Vr <- svd.out$v[, e.vects, drop = FALSE]

  type.vec <- sapply (X, function (x)x$type)
  if (any(type.vec == "histogram")) stop ("histograms to follow")
  else
  {
    if (correlation.biplot)
    { stop ("not yet implemented")
    }
    else
    {
      X.lo <- sapply (X, function (x)x$value[,1])
      X.up <- sapply (X, function (x)x$value[,2])
      Z.lo <- X.lo %*% Vr
      Z.up <- X.up %*% Vr
      for (k in 1:dim.biplot)
      {
        if (all(Z.lo[,k] > Z.up[,k]))
        {
          temp <- Z.lo[,k]
          Z.lo[,k] <- Z.up[,k]
          Z.up[,k] <- temp
          Vr[,k] <- -1*Vr[,k]
          Lmat[,e.vects[k]] <- -1*Lmat[,e.vects[k]]
        }
      }
      rownames(Z.lo) <- rownames(Z.up) <- rownames(X[[1]]$values)
      Z <- list (Z.lo, Z.up)
      names(Z) <- c("lo","up")
    }
  }


  if (correlation.biplot)
    ax.one.unit <- NULL
  else
    ax.one.unit <- 1/(diag(Vr %*% t(Vr))) * Vr

  bp$Z <- Z
  bp$Lmat <- Lmat
  bp$eigenvalues <- svd.out$d
  bp$ax.one.unit <- ax.one.unit
  bp$e.vects <- e.vects
  bp$Vr <- Vr
  bp$dim.biplot <- dim.biplot
  if (bp$g == 1) bp$class.means <- FALSE else bp$class.means <- show.class.means
  if (bp$class.means)
  {
    warning ("class means not yet implemented")
#    G <- indmat(bp$group.aes)
#    Xmeans <- solve(t(G)%*%G) %*% t(G) %*% X
#    Zmeans <- Xmeans %*% Lmat[,e.vects]
#    bp$Zmeans <- Zmeans
  }

  class(bp)<-append(class(bp),"ddPCA_intervals")
  bp
}

PCA2 <- function (bp, dim.biplot = c(2, 1, 3), e.vects = 1:bp$p, group.aes=NULL,
                          show.class.means = FALSE, correlation.biplot=FALSE)
{

  dim.biplot <- dim.biplot[1]
  if (dim.biplot != 1 & dim.biplot != 2 & dim.biplot != 3) stop("Only 1D, 2D and 3D biplots")
  e.vects <- e.vects[1:dim.biplot]
  if (!is.null(group.aes)) { bp$group.aes <- factor(group.aes)
  bp$g.names <-levels(factor(group.aes))
  bp$g <- length(bp$g.names)
  }

  if (!bp$center)
  {  warning("PCA requires a centred datamatrix. Your data was centred before computation. Use center = TRUE in the call to biplot()")
    bp <- ddbiplot (bp$X, center = TRUE, scaled=bp$scaled, bp$classes, bp$group.aes)
  }

  X <- bp$X
  n <- bp$n
  p <- bp$p

  D.sq <- sq.L2.Wass_dist(X)
  I.min.n <- diag(n) - matrix(1,nrow=n,ncol=n)/n
  B <- I.min.n %*% (-0.5*D.sq) %*% I.min.n
  svd.out <- svd(B)
  Zmat <- svd.out$v[,1:2] %*% diag(sqrt(svd.out$d[1:2]))
#  Vr <- solve(t(Zmat)%*%Zmat)%*%t(Zmat)%*%Xmat
  return (Zmat)
}


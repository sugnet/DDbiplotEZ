#' First step to create a new biplot with \pkg{biplotEZ}
#'
#' @description
#' This function produces a \code{ddbiplot} object.
#'
#' @param data an object of class \code{ddobj}.
#' @param classes a vector identifying class membership.
#' @param group.aes a vector identifying groups for aesthetic formatting.
#' @param center a logical value indicating whether \code{data} should be variable centered,
#'               with default \code{TRUE}.
#' @param scaled default \code{NULL} for no scaling of the data. Other options are
#'               \code{Billard} or \code{Wasserstein} to divide centred quantile functions
#'               by the squareroot of the variance computed with \code{ddvarB} or
#'               \code{ddvarW} respectively. Partial matching allowed.
#' @param Title the title of the biplot to be rendered, enter text in "  ".
#'
#' @import biplotEZ
#'
#' @details
#'This function is the entry-level function in \code{DDbiplotEZ} to construct a biplot display.
#'It initialises an object of class \code{ddbiplot} which can then be piped to various other functions
#'to build up the biplot display.
#'
#' @return A list with the following components is available:
#' \item{X}{numeric part of the original \code{ddobj}, centred and scaled according to the
#'          arguments \code{center} and \code{scaled}.}
#' \item{Xcat}{categorical and modal part of the original \code{ddobj}.}
#' \item{raw.X}{original \code{ddobj}.}
#' \item{numeric.vars}{which of the variables in raw.X are contained in X.}
#' \item{classes}{the vector of category levels for the class variable. This is to be used for
#'                \code{colour}, \code{pch} and \code{cex} specifications.}
#' \item{na.action}{the observations that have been removed.}
#' \item{center}{a logical value indicating whether centering was applied.}
#' \item{scaled}{one of \code{NULL} or \code{Billard} or \code{Wasserstein}.}
#' \item{means}{the vector of means for each numeric, interval or histogram variable.}
#' \item{sd}{the vector of standard deviations computed according to the specification in\code{scaled}.}
#' \item{n}{the number of observations.}
#' \item{p}{the number of variables.}
#' \item{group.aes}{the vector of category levels for the grouping variable. This is to be used
#'                  for \code{colour}, \code{pch} and \code{cex} specifications.}
#' \item{g.names}{the descriptive names to be used for group labels.}
#' \item{g}{the number of groups.}
#' \item{Title}{the title of the biplot rendered}
#'
#' @usage ddbiplot(data, classes = NULL, group.aes = NULL, center = TRUE,
#'                 scaled = c(NULL, "Wasserstein", "Billard"), Title = NULL)
#' @aliases ddbiplot
#'
#' @export
#'
#' @examples
#' ddbiplot(data = Oils.data)
#' # create a PCA biplot
#' ddbiplot(data = Oils.data) |> PCA() |> plot()
#'
ddbiplot <- function(data, classes = NULL, group.aes = NULL, center = TRUE,
                   scaled = c(NULL, "Wasserstein", "Billard"), Title = NULL)
{
    scaled <- match.arg(scaled)
    pp <- length(data)
    if(pp < 2) stop("Not enough variables to construct a biplot \n Consider using data with more columns")
    n <- switch(data[[1]]$type,
                numeric = length(data[[1]]$values),
                interval = nrow(data[[1]]$values),
                histogram = nrow(data[[1]]$intervals),
                categorical = length(data[[1]]$values),
                modal = nrow(data[[1]]$categories))
    unit.names <- switch(data[[1]]$type,
                         numeric = names(data[[1]]$values),
                         interval = rownames(data[[1]]$values),
                         histogram = rownames(data[[1]]$intervals),
                         categorical = rownames(data[[1]]$values),
                         modal = rownames(data[[1]]$categories))

    na.vec <- rep (FALSE, n)
    p <- 0
    for (j in 1:pp)
    {
      if (data[[j]]$type == "numeric") data[[j]] <- num_to_int (data[[j]])
      if (data[[j]]$type == "interval")
      {
         na.vec.j <- stats::na.action(stats::na.omit(data[[j]]$values))
         if (length(na.vec.j) == n) stop(paste("No observations left after deleting missing observations for variable", j))
         else if (!is.null(na.vec.j))  warning(paste(length(na.vec.j), "rows deleted due to missing values for variable", j))
         na.vec[na.vec.j] <- TRUE
         p <- p + 1
      }
      if (data[[j]]$type == "histogram")
      {
        my.check <- (abs(apply (data[[j]]$proportions, 1, sum, na.rm = TRUE) - 1)<1e-13)
        if (any(!my.check)) na.vec[!my.check] <- TRUE
        if (any(!my.check)) warning(paste(sum(!my.check), "rows deleted due to proportions not adding to 1 for variable", j))
        for (i in 1:nrow(data[[j]]$proportions))
          if (abs(sum(data[[j]]$proportions[i,1:(length(data[[j]]$intervals[i,!is.na(data[[j]]$intervals[i,])])-1)]) - 1)>1e-13)
          {
            na.vec[i] <- TRUE
            warning (paste("Histogram proportions for", unit.names[i],
                           "corresponding with NA intervals removed.\n"))
          }
        p <- p + 1
      }
    }
    X <- vector ("list", p)
    Xcat <- vector ("list", pp - p)
    j.num <- j.cat <- 1
    num <- NULL
    for (j in 1:pp)
    {
      if (data[[j]]$type == "interval")
      {
        new.var <- list ("interval", data[[j]]$values[!na.vec,])
        new.var.names <- c("type","values")
        X[[j.num]] <- new.var
        names(X[[j.num]]) <- new.var.names
        j.num <- j.num + 1
        num <- c(num, j)
      }
      if (data[[j]]$type == "histogram")
      {
        new.var <- list ("histogram", data[[j]]$intervals[!na.vec,], data[[j]]$proportions[!na.vec,])
        new.var.names <- c("type","intervals", "proportions")
        X[[j.num]] <- new.var
        names(X[[j.num]]) <- new.var.names
        j.num <- j.num + 1
        num <- c(num, j)
      }
      if (data[[j]]$type != "interval" & data[[j]]$type != "histogram")
      {
        if (data[[j]]$type == "categorical")
          new.var <- list (data[[j]]$type, data[[j]]$categories)
        else
          new.var <- list (data[[j]]$type, data[[j]]$categories, data[[j]]$proportions)
        new.var.names <- names(data[[j]])
        Xcat[[j.cat]] <- new.var
        names(Xcat[[j.cat]]) <- new.var.names
        j.cat <- j.cat + 1
      }
    }
    names(X) <- names(data)[num]
    if (p>0) class(X) <- "ddobj" else X <- NULL
    if (is.null(X))
    {
      means <- sd <- n <- p <- NULL
    }
    else
    {
      if (center) means <- sapply (X, ddmean) else means <- rep(0,p)
      if (is.null(scaled)) sd <- rep(1,p)
      else
      {
        if (!is.na(pmatch(scaled,"Billard"))) sd <- sqrt(sapply(X, ddvarB))
        if (!is.na(pmatch(scaled,"Wasserstein"))) sd <- sqrt(diag(ddvarW(X)))
      }

      for (j in 1:p)
      {
        if (X[[j]]$type == "numeric") X[[j]]$values <- (X[[j]]$values - means[j])/sd[j]
        if (X[[j]]$type == "interval") X[[j]]$values <- (X[[j]]$values - means[j])/sd[j]
        if (X[[j]]$type == "histogram") X[[j]]$intervals <- (X[[j]]$intervals - means[j])/sd[j]
      }
    }

    p2 <- pp - p
    if (p2>0) class(Xcat) <- "ddobj" else Xcat <- NULL

    if (!is.null(group.aes) & length(na.vec) > 0) group.aes <- group.aes[!na.vec]

    if (p > 0)
    {
      pdfs <- vector ("list", p)
      for (j in 1:p)
      {
        pdfs[[j]] <- vector ("list", n)
        if (data[[j]]$type == "interval") for (i in 1:n) pdfs[[j]][[i]] <- intpdf (X, i=i, j= j)
        if (data[[j]]$type == "histogram") for (i in 1:n) pdfs[[j]][[i]] <- histpdf (X, i=i, j=j)
      }
    }

#    if(!is.null(Xcat))
#    {
#      if (is.null(n)) n <- nrow(Xcat)
#      p2 <- ncol(Xcat)
#      if (is.null(rownames(Xcat))) rownames(Xcat) <- paste(1:nrow(Xcat))
#      if (is.null(colnames(Xcat))) colnames(Xcat) <- paste("F", 1:ncol(Xcat), sep = "")
#    }
#    else p2 <- NULL

    if(!is.null(classes))
      classes <- factor(classes)

    if(is.null(group.aes)) { if (!is.null(classes)) group.aes <- classes else group.aes <- factor(rep(1,n)) }
    else group.aes <- factor(group.aes)

    g.names <-levels(group.aes)
    g <- length(g.names)

    object <- list(X = X, Xcat = Xcat, raw.X = data, numeric.vars = num, classes=classes,
                   na.action=(1:n)[na.vec],
                   center=center, scaled=scaled, means=means, sd=sd,
                   n=n, p=p, p2=p2,
                   group.aes = group.aes,g.names = g.names,g = g,
                   Title = Title)
    class(object) <- "ddbiplot"
  object
}

# -----------------------------------------------------------------------

#' Predict biplot samples
#'
#' @param bp an object of class \code{ddbiplot} obtained from preceding function \code{ddbiplot()}.
#' @param samples logical TRUE to predict samples; which sample numbers to be implemented
#' @param type either default \code{intervals} or \code{vertices}. Which unit
#'             representation to predict
#'
#' @returns an object of class \code{ddbiplot} with additional component
#'          \code{predict$samples}
#' @export
#'
#' @examples
#' ddbiplot(data = Oils.data) |> PCA() |>
#'   prediction (type = "intervals") |> plot(type = "intervals")
#'
prediction <- function (bp, samples = TRUE, type = "intervals")
{
  p <- bp$p
  n <- bp$n
  Vr <- bp$Vr
  Xhat <- vector("list", p)
  for (j in 1:p)
    Xhat[[j]] <- list (type = "interval",
                       values = matrix (NA, nrow=bp$n, ncol=2))
  if (type == "intervals")
  {
    mat <- vector("list", p)
    centres <- sapply (bp$X, function(x) apply(x$value, 1, mean))
    ZZ <- cbind(centres %*% Vr, centres %*% Vr)
    for (j in 1:p)
    {
      Xlo <- Xup <- centres
      Xlo[,j] <- bp$X[[j]]$values[,1]
      Xup[,j] <- bp$X[[j]]$values[,2]
      Zlo <- Xlo %*% Vr
      Zup <- Xup %*% Vr
      mat[[j]] <- list (Zlo, Zup)
    }
    mat.hat <- lapply (mat, function(x) list(x[[1]] %*% t(bp$Vr),
                                             x[[2]] %*% t(bp$Vr)))
    for (i in 1:n)
    {
      pred.i <-lapply (mat.hat, function(x)rbind(x[[1]][i,],x[[2]][i,]))
      pred.mat <- NULL
      for (k in 1:length(pred.i))
        pred.mat <- rbind (pred.mat, pred.i[[k]])
      for (j in 1:p)
        Xhat[[j]]$values[i,] <- range(pred.mat[,j])
    }
  }
  if (type == "vertices")
  {
    vertices.mat <- vector("list", n)
    for (i in 1:n)
      vertices.mat[[i]] <- create_vertices (bp$X, i)

    vert.mat.hat <- lapply (vertices.mat, function(x)
                                          x$vertices %*% Vr %*% t(Vr))
    vert.hat <- lapply (vert.mat.hat, function(x)apply(x, 2, range))

    for (j in 1:p)
      for (i in 1:n)
        Xhat[[j]]$values[i,] <- vert.hat[[i]][,j]
  }
  if (type == "diagonal")
  {
    X.lo <- X.up <- matrix (NA, nrow=n, ncol=p)
    for (j in 1:p)
      for (i in 1:n)
      {
        X.lo[i,j] <- bp$X[[j]]$values[i,1]
        X.up[i,j] <- bp$X[[j]]$values[i,2]
      }
    Xstar.lo <- X.lo %*% Vr %*% t(Vr)
    Xstar.up <- X.up %*% Vr %*% t(Vr)

    for (j in 1:p)
      for (i in 1:n)
      {
        Xhat[[j]]$values[i,1] <- min(Xstar.lo[i,j], Xstar.up[i,j])
        Xhat[[j]]$values[i,2] <- max(Xstar.lo[i,j], Xstar.up[i,j])
      }
  }
  for (j in 1:p)
    Xhat[[j]]$values <- Xhat[[j]]$values*bp$sd[j] + bp$means[j]
  bp$Xhat <- Xhat
  bp
}

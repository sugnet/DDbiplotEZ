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
#' @param scaled a logical value indicating whether \code{data} should be standardised to unit
#'               variable variances, with default \code{FALSE}.
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
#' \item{classes}{the vector of category levels for the class variable. This is to be used for
#'                \code{colour}, \code{pch} and \code{cex} specifications.}
#' \item{na.action}{the observations that have been removed.}
#' \item{center}{a logical value indicating whether centering was applied.}
#' \item{scaled}{a logical value indicating whether scaling was applied.}
#' \item{means}{the means for each variable.}
#' \item{sd}{the standard deviations for each variable.}
#' \item{n}{the number of observations.}
#' \item{p}{the number of variables.}
#' \item{group.aes}{the vector of category levels for the grouping variable. This is to be used
#'                  for \code{colour}, \code{pch} and \code{cex} specifications.}
#' \item{g.names}{the descriptive names to be used for group labels.}
#' \item{g}{the number of groups.}
#' \item{Title}{the title of the biplot rendered}
#'
#' @usage ddbiplot(data, classes = NULL, group.aes = NULL, center = TRUE, scaled = FALSE,
#' Title = NULL)
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
                   scaled = FALSE, Title = NULL)
{
    pp <- length(data)
    if(pp < 2) stop("Not enough variables to construct a biplot \n Consider using data with more columns")
    n <- switch(data[[1]]$type,
                numeric = length(data[[1]]$values),
                interval = nrow(data[[1]]$values),
                histogram = nrow(data[[1]]$intervals),
                categorical = length(data[[1]]$values),
                modal = length(data[[1]]$cats))

    na.vec <- rep (FALSE, n)
    p <- 0
    for (j in 1:pp)
    {
      if (data[[j]]$type == "numeric") data[[j]] <- num.to.int (data[[j]])
      if (data[[j]]$type == "interval")
      {
         na.vec.j <- stats::na.action(stats::na.omit(data[[j]]$values))
         if (length(na.vec.j) == n) stop(paste("No observations left after deleting missing observations for variable", j))
         else if (!is.null(na.vec.j))  warning(paste(length(na.vec.j), "rows deleted due to missing values for variable", j))
         na.vec[na.vec.j] <- TRUE
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
      else
      {
        Xcat[[j.cat]] <- data[[j]]
        j.cat <- j.cat + 1
      }
    }
    names(X) <- names(data)[num]
    if (p>0) class(X) <- "ddobj" else X <- NULL
    p2 <- pp - p
    if (p2>0) class(Xcat) <- "ddobj" else Xcat <- NULL

    if (!is.null(group.aes) & length(na.vec) > 0) group.aes <- group.aes[!na.vec]

    # scaling of numeric data
    if(p==0)
    {  means <- NULL
       sd <- NULL
       p <- NULL
       n <- NULL
    }
    else
    {
      means <- sapply(X, ddmean)
      sd <- sapply(lapply(X, ddvar), sqrt)
      if (!center) {  X <- X
                      means <- rep(0, p)
                      sd <- rep(1, p)
      }
      else if (scaled)
           { for (k in 1:p) X[[k]]$values <- (X[[k]]$values-means[k])/sd[k]
           }
           else
           {  for (k in 1:p) X[[k]]$values <- X[[k]]$values-means[k]
              sd <- rep(1, p)
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

    object <- list(X = X, Xcat = Xcat, raw.X = data, classes=classes, na.action=(1:n)[na.vec], center=center, scaled=scaled,
                   means = means, sd = sd, n=n, p=p, p2=p2, group.aes = group.aes,g.names = g.names,g = g,
                   Title = Title)
    class(object) <- "ddbiplot"
  object
}

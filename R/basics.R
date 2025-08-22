#' Creates a distributional data object
#'
#' @param df data frame from which the \code{ddobj} object is created
#' @param types types of distributional variables provided as a vector. Possible types are
#'        \code{numeric}, \code{interval}, \code{histogram}, \code{categorical}, \code{modal}
#' @param cols the first column number in \code{df} corresponding to each of the `types`
#' @param n.int number of intervals for each histogram variable
#' @param n.cat number of categories for each modal variable
#'
#' @returns an object of class \code{ddobj}
#' @export
#' @usage create.ddobj(df, types=NULL, cols=NULL, n.int=NULL, n.cat=NULL)
#'
#' @examples
#' create.ddobj (toy.data, type=c("numeric","interval","histogram","categorical", "modal"),
#'               cols = c(1, 2, 4, 13, 14), n.int=4, n.cat=3)
#'
create.ddobj <- function(df, types=NULL, cols=NULL, n.int=NULL, n.cat=NULL)
{
  df <- as.data.frame(df)
  entity.names <- rownames(df)
  if (is.null(types))
  {  types <- c("categorical","numeric")[as.numeric(sapply (df, is.numeric))+1]
     cols <- 1:ncol(df)
  }
  p <- length(types)

  if (is.null(n.int))
    if (any(types=="histogram"))
      n.int <- rep (3, sum(types=="histogram"))
  hist.n.int <- rep(NA, p)
  which.hist <- (1:p)[types=="histogram"]
  if (length(which.hist)>0)
    for (j in 1:length(which.hist)) hist.n.int[which.hist[j]] <- n.int[j]

  if (is.null(n.cat))
    if (any(types=="modal"))
      n.cat <- rep (2, sum(types=="modal"))
  modal.n.cat <- rep(NA, p)
  which.modal <- (1:p)[types=="modal"]
  if (length(which.modal)>0)
    for (j in 1:length(which.modal)) modal.n.cat[which.modal[j]] <- n.cat[j]

  if (is.null(cols))
  {
    next.col <- 1
    for (j in 1:p)
    {
      cols <- c(cols, next.col)
      next.col <- next.col + switch(types[j], numeric = 1,
                                              interval = 2,
                                              histogram = hist.n.int[j]*2+1,
                                              categorical = 1)
    }
  }
  var.names <- colnames(df)[cols]
  obj <- vector ("list", length(types))

  used <- NULL
  for (j in 1:p)
  {
    current <- cols[j]

    if (types[j] == "numeric" | types[j] == "categorical")
    {
      new.var <- df[,current]
      names(new.var) <- entity.names
      new.var <- list (c("categorical","numeric")[is.numeric(new.var)+1], new.var)
      new.var.names <- c("type","values")

      if (!is.na(match(current, used)))
        warning (paste("In constructing", var.names[j], "column", current,
                       "already forms part of another variable.\n"))
      used <- c(used, current)
    }

    if (types[j] == "interval")
    {
      mat <- df[,(0:1)+current]
      mat <- t(apply (mat, 1, sort))
      rownames(mat) <- entity.names
      colnames(mat) <- c("lower","upper")
      new.var <- list ("interval", mat)
      new.var.names <- c("type","values")

      my.check <- match(c(0:1)+current, used)
      if (!any(is.na(my.check)))
        warning (paste("In constructing", var.names[j], "column(s)", (c(0:1)+current)[stats::na.omit(my.check)],
                       "already forms part of another variable.\n"))
      used <- c(used, c(0:1)+current)
    }

    if (types[j] == "histogram")
    {
      intervals <- df[,(0:hist.n.int[j])+current]
      probs <- df[,(1:hist.n.int[j])+hist.n.int[j]+current]

      my.check <- apply (intervals, 1, function(x)all(order(x) == 1:length(x)))
      if (any(!my.check)) stop (paste("Histogram intervals for", entity.names[!my.check],
                                      "not in increasing order.\n"))
      my.check <- (apply (probs, 1, sum, na.rm = TRUE) == 1)
      if (any(!my.check)) stop (paste("Histogram proportions for", entity.names[!my.check],
                                      "do not add up to 1.\n"))
      for (i in 1:nrow(probs))
        if (sum(probs[i,1:(length(intervals[i,!is.na(intervals[i,])])-1)]) != 1)
          stop (paste("Histogram proportions for", entity.names[i],
                      "correspond with NA intervals.\n"))

      rownames(probs) <- entity.names
      new.var <- list ("histogram", intervals, probs)
      new.var.names <- c("type","intervals","proportions")

      my.check <- match(c(0:(hist.n.int[j]*2))+current, used)
      if (!any(is.na(my.check)))
        warning (paste("In constructing", var.names[j], "column(s)", (c(0:(hist.n.int[j]*2+1))+current)[stats::na.omit(my.check)],
                       "already forms part of another variable.\n"))
      used <- c(used, c(0:(hist.n.int[j]*2))+current)
    }


    if (types[j] == "modal")
    {
      cats <- df[,(0:(modal.n.cat[j]-1))+current]
      probs <- df[,(0:(modal.n.cat[j]-1))+modal.n.cat[j]+current]

      my.check <- (apply (probs, 1, sum, na.rm = TRUE) == 1)
      if (any(!my.check)) stop (paste("Modal proportions for", entity.names[!my.check],
                                      "do not add up to 1.\n"))
      for (i in 1:nrow(probs))
        if (sum(probs[i,1:(length(cats[i,!is.na(cats[i,])]))]) != 1)
          stop (paste("Modal proportions for", entity.names[i],
                      "correspond with NA categories.\n"))

      rownames(probs) <- entity.names
      new.var <- list ("modal", cats, probs)
      new.var.names <- c("type","categories","proportions")

      my.check <- match(c(0:(modal.n.cat[j]*2+1))+current, used)
      if (!any(is.na(my.check)))
        warning (paste("In constructing", var.names[j], "column(s)", (c(0:(modal.n.cat[j]*2+1))+current)[stats::na.omit(my.check)],
                       "already forms part of another variable.\n"))
      used <- c(used, c(0:(modal.n.cat[j]*2+1))+current)
    }

    obj[[j]] <- new.var
    names(obj[[j]]) <- new.var.names
  }

  names(obj) <- var.names
  class (obj) <- "ddobj"
  obj
}

# ----------------------------------------------------------------------------------------------
#' Generic print function for objects of class ddobj
#'
#' @description
#' This function is used to print output when the biplot object is created.
#'
#' @param x an object of class \code{ddobj}.
#' @param ... additional arguments.
#'
#' @return This function will not produce a return value, it is called for side effects.
#'
#' @export
#' @examples
#' my.obj <- create.ddobj (toy.data, type=c("numeric","interval","histogram","categorical", "modal"),
#'           cols = c(1, 2, 4, 13, 14), n.int=4, n.cat=3)
#' print (my.obj)
#'
print.ddobj <- function (x, ...)
{
  n <- switch(x[[1]]$type,
              numeric = length(x[[1]]$values),
              interval = nrow(x[[1]]$values),
              histogram = nrow(x[[1]]$intervals),
              categorical = length(x[[1]]$values),
              modal = length(x[[1]]$cats))
  cat ("An object of class ddobj with", n, "entities containing", length(x),
       "distributional data variables.\n")

  for (j in 1:length(x))
  {
    this.list <- x[[j]]
    cat ("\n", names(x)[j], ":", this.list$type, "\n")
    if (this.list$type=="numeric")
    {
      print (stats::quantile(this.list$values, (0:4)/4))
    }
    if (this.list$type=="interval")
    {
      cat (paste("An interval with smallest lower bound:",
                  this.list$values[which.min(this.list$values[,1]),1], "-" ,
                  this.list$values[which.min(this.list$values[,1]),2], "\n"))
      cat (paste("An interval with largest upper bound:",
                  this.list$values[which.max(this.list$values[,2]),1], "-",
                  this.list$values[which.max(this.list$values[,2]),2], "\n"))
    }
    if (this.list$type=="histogram")
    {
      breaks <- paste(this.list$intervals[which.min(this.list$intervals[,1]),], collapse=", ")
      cat (paste("Intervals of a histogram with smallest lower bound:", breaks, "\n"))
      find.max <- apply (this.list$intervals, 1, max, na.rm = TRUE)
      breaks <- paste(this.list$intervals[which.max(find.max),], collapse=", ")
      cat (paste("Intervals of a histogram with largest upper bound:", breaks, "\n"))
    }
    if (this.list$type=="categorical")
    {
      print (table(this.list$values))
    }
    if (this.list$type=="modal")
    {
      all.cats <- NULL
      for (k in 1:ncol(this.list$categories))
        all.cats <- c(all.cats, this.list$categories[,k])
      all.cats <- paste (levels(factor(all.cats)), collapse=", ")
      cat ("Categories are:", all.cats, "\n")
    }
  }

#  invisible (x)
}

# ----------------------------------------------------------------------------------------------
#' Summarise a data set into a distributional data object
#'
#' @param df data frame from which the \code{ddobj} object is created
#' @param entities the column names that will form the entities
#' @param interval the column names that will be summarised into interval scaled data
#' @param histogram the column names that will be summarised into histogram scaled data
#' @param modal the column names that will be summarised into modal data
#'
#' @returns and object of class \code{ddobj}
#' @export
#'
#' @usage suminto.ddobj(df, entities = names(df)[1],
#'                      interval=NULL, histogram=NULL, modal=NULL)
#'
#' @examples
#' suminto.ddobj (esoph, entities = "agegp", interval="ncases",
#'                histogram="ncontrols", modal=c("alcgp","tobgp"))
#'
suminto.ddobj <- function (df, entities = names(df)[1],
                           interval=NULL, histogram=NULL, modal=NULL)
{
  df <- as.data.frame(df)
  obj <- vector("list", 0)

  which.cols <- stats::na.omit(match(entities, colnames(df)))
  if (length(which.cols) > 1) entities.vals <- apply(df[,which.cols],1,paste,collapse="_")
  else entities.vals <- df[,which.cols]
  if (length(entities.vals) == 0) stop ("entities misspecified")

  entities <- levels(factor(entities.vals))

  if (!is.null(interval))
    for (j in 1:length(interval))
    {
      this.col <- match(interval[j], colnames(df))
      mat <- t(sapply(tapply(df[,this.col], entities.vals, range), function(x)x))
      colnames(mat) <- c("lower","upper")
      new.var <- list ("interval", mat)
      new.var.names <- c("type","values")
      obj[[length(obj)+1]] <- new.var
      names(obj[[length(obj)]]) <- new.var.names
    }

  if (!is.null(histogram))
    for (j in 1:length(histogram))
    {
      this.col <- match(histogram[j], colnames(df))
      out <- tapply(df[,this.col], entities.vals, graphics::hist, breaks = 5, plot = FALSE)
      nums <- sapply(out, function(x)length(x$counts))
      num <- max(nums)
      intervals <- t(sapply(out, function(x)c(x$breaks, rep(NA,num-length(x$breaks)+1))))
      probs <- t(sapply(out, function(x)c(x$counts/sum(x$counts), rep(NA,num-length(x$counts)))))
      new.var <- list ("histogram", intervals, probs)
      new.var.names <- c("type","intervals","proportions")
      obj[[length(obj)+1]] <- new.var
      names(obj[[length(obj)]]) <- new.var.names
    }

  if (!is.null(modal))
    for (j in 1:length(modal))
    {
      this.col <- match(modal[j], colnames(df))
      out <- tapply(df[,this.col], entities.vals, table)
      nums <- sapply(out, function(x)length(x))
      num <- max(nums)
      cats <- t(sapply(out, function(x)c(names(x), rep(NA,num-length(x)))))
      probs <- t(sapply(out, function(x)c(x/sum(x), rep(NA,num-length(x)))))
      new.var <- list ("modal", cats, probs)
      new.var.names <- c("type","categories","proportions")
      obj[[length(obj)+1]] <- new.var
      names(obj[[length(obj)]]) <- new.var.names
    }

  names(obj) <- c(interval, histogram, modal)
  class (obj) <- "ddobj"
  obj
}

#' Converts an interval scaled variable to a histogram scaled variable with a single bin and
#' proportion one
#'
#' @param x a list, typically a single component of an object of class \code{ddobj}
#'
#' @returns a list, a single histogram scaled variable component for an object of class \code{ddobj}
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, entities = "agegp", interval="ncases", histogram="ncontrols")
#' int.to.hist (obj$ncases)
#'
int.to.hist <- function (x)
{
  entity.names <- rownames(x$values)
  x <- list(type = "histogram",
            intervals = x$values,
            proportions = matrix(1,nrow=nrow(x$values), ncol=1))
  rownames(x$intervals) <- rownames(x$proportions) <- entity.names
  x
}

#' Converts a numeric variable to an interval scaled variable with zero interval width
#'
#' @param x a list, typically a single component of an object of class \code{ddobj}
#'
#' @returns a list, a single interval scaled variable component for an object of class \code{ddobj}
#' @export
#'
#' @examples
#' num.to.int (Oils.data$SO.LDP)
#'
num.to.int <- function (x)
{
  entity.names <- names(x$values)
  x <- list(type = "interval",
            values = cbind(x$values, x$values))
  rownames(x$values) <- entity.names
  x
}

#' Computes the squared l2 Wasserstein distance
#'
#' @param obj an object of class \code{ddobj}
#'
#' @returns a symmetric matrix of squared distances
#'
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, entities = "agegp", interval="ncases", histogram="ncontrols")
#' sq.L2.Wass_dist(obj)
#'
sq.L2.Wass_dist <- function (obj)
{
  hist.to.distrH <- function (a, i)
  {
    int <- as.numeric(a$intervals[i,])
    int <- int[!is.na(int)]
    prop <- as.numeric(a$proportions[i,])
    prop <- prop[!is.na(prop)]
    HistDAWass::distributionH (x = int, p = c(0,cumsum(prop)))
  }

  n <- switch(obj[[1]]$type,
          numeric = length(obj[[1]]$values),
          interval = nrow(obj[[1]]$values),
          histogram = nrow(obj[[1]]$intervals))
  entity.names <- switch(obj[[1]]$type,
                         numeric = names(obj[[1]]$values),
                         interval = rownames(obj[[1]]$values),
                         histogram = rownames(obj[[1]]$intervals))
  p <- length (obj)

  Dmat <- matrix (0, nrow=n, ncol=n)
  for (j in 1:p)
  {
    if (obj[[j]]$type == "numeric") obj[[j]] <- num.to.int (obj[[j]])
    if (obj[[j]]$type == "interval") obj[[j]] <- int.to.hist (obj[[j]])

    distrH.list <- vector ("list", n)
    for (i in 1:n) distrH.list[[i]] <- hist.to.distrH(obj[[j]], i=i)

    for (i in 1:(n-1))
      for (h in (i+1):n)
        Dmat[i,h] <- Dmat[i,h] + HistDAWass::WassSqDistH(distrH.list[[i]], distrH.list[[h]])
  }

  Dmat <- Dmat + t(Dmat)
  rownames(Dmat) <- colnames(Dmat) <- entity.names
  Dmat
}

#' Create a vertices matrix from interval scaled data
#'
#' @param obj an object of class \code{ddobj}
#' @param i number of the entity for which the vertices matrix is computed
#' @param connect logical argument indicating whether connections between vertices should be
#'                computed. Note this requires evaluating (2^p) chose 2 possible connections
#'
#' @importFrom utils combn
#' @return a list with two components:
#' \item{vertices}{a matrix of size \eqn{2^p \times p} where \eqn{p} is the number of intervals.}
#' \item{connections}{a two-column matrix with each row indicating the
#'                    numbers of two rows of \code{vertices} to be
#'                    connected such as a cube in 3D.}
#' @export
#'
#' @examples
#' obj <- suminto.ddobj (esoph, entities = "agegp", interval=c("ncases","ncontrols"))
#' create.vertices (obj, 1)
#'
create.vertices <- function (obj, i, connect=FALSE)
{
  temp.list <- vector("list", length(obj))
  for (j in 1:length(obj)) temp.list[[j]] <- obj[[j]]$values[i,]
  mat <- as.matrix(expand.grid (temp.list))
  colnames(mat) <- names(obj)

  connections <- NULL
  if (connect)
  {
    p <- ncol(mat)
    n <- nrow(mat)
    # Generate all unique row pairs (i < k)
    pairs <- t(utils::combn(n, 2))
    # Compute differences for each pair
    diffs <- abs(mat[pairs[,1], ] - mat[pairs[,2], ])
    # Count how many elements are within tolerance for each pair
    close_counts <- rowSums(diffs < 1e-14)
    # Keep only those with at least (p - 1) close elements
    connections <- pairs[close_counts == (p - 1), , drop = FALSE]
  }
  list (vertices = mat, connections = connections)
}


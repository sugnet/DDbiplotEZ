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
#' @usage create_ddobj(df, types=NULL, cols=NULL, n.int=NULL, n.cat=NULL)
#'
#' @examples
#' create_ddobj (toy.data, types=c("numeric","interval","histogram","categorical", "modal"),
#'               cols = c(1, 2, 4, 13, 14), n.int=4, n.cat=3)
#'
create_ddobj <- function(df, types=NULL, cols=NULL, n.int=NULL, n.cat=NULL)
{
  if (any(!(types %in% c("numeric","interval","histogram","categorical","modal"))))
    stop (paste (paste(types[!(types %in% c("numeric","interval","histogram","categorical","modal"))],collapse=","),
                 "is not one of numeric, interval, histogram, categorical, modal"))
  df <- as.data.frame(df)
  unit.names <- rownames(df)
  if (is.null(unit.names)) unit.names <- 1:nrow(df)
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
                                              categorical = 1,
                                              modal = modal.n.cat[j]+2+1)
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
      if (types[j]=="numeric") new.var <- as.numeric(new.var)
      names(new.var) <- unit.names
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
      mat <- t(apply (mat, 1, as.numeric))
      mat <- t(apply (mat, 1, function(x) if (any(is.na(x))) x else sort(x)))
      rownames(mat) <- unit.names
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
      intervals <- t(apply (intervals, 1, as.numeric))
      probs <- df[,(1:hist.n.int[j])+hist.n.int[j]+current]
      probs <- t(apply (probs, 1, as.numeric))

      all.missing <- apply (intervals, 1, function(x)all(is.na(x)))
      intervals2 <- intervals[!all.missing,]
      probs2 <- probs[!all.missing,]

      my.check <- apply (intervals2, 1, function(x)all(order(x) == 1:length(x)))
      if (any(!my.check)) stop (paste("Histogram intervals for", (unit.names[!all.missing])[!my.check],
                                      "not in increasing order.\n"))

      my.check <- (abs(apply (probs2, 1, sum, na.rm = TRUE) - 1)<1e-13)
      if (any(!my.check)) stop (paste("Histogram proportions for", (unit.names[!all.missing])[!my.check],
                                      "do not add up to 1.\n"))

      for (i in 1:nrow(probs2))
        if (abs(sum(probs2[i,1:(length(intervals2[i,!is.na(intervals2[i,])])-1)]) - 1)>1e-13)
          stop (paste("Histogram proportions for", (unit.names[!all.missing])[i],
                      "correspond with NA intervals.\n"))

      rownames(intervals) <- unit.names
      rownames(probs) <- unit.names
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
      probs <- t(apply (probs, 1, as.numeric))
      my.check <- (abs(apply (probs, 1, sum, na.rm = TRUE)-1) < 1e-13)

      if (any(!my.check)) stop (paste("Modal proportions for variable", j, "unit(s)", unit.names[!my.check],
                                      "do not add up to 1.\n"))
      for (i in 1:nrow(probs))
        if (abs(sum(probs[i,1:(length(cats[i,!is.na(cats[i,])]))]) - 1) > 1e-13)
          stop (paste("Modal proportions for", unit.names[i],
                      "correspond with NA categories.\n"))

      rownames(cats) <- rownames(probs) <- unit.names
      new.var <- list ("modal", cats, probs)
      new.var.names <- c("type","categories","proportions")

      my.check <- match(c(0:(modal.n.cat[j]*2+1))+current, used)
      if (!any(is.na(my.check)))
        warning (paste("In constructing", var.names[j], "column(s)", (c(0:(modal.n.cat[j]*2+1))+current)[stats::na.omit(my.check)],
                       "already forms part of another variable.\n"))
      used <- c(used, c(0:(modal.n.cat[j]*2)-1)+current)
    }

    obj[[j]] <- new.var
    names(obj[[j]]) <- new.var.names
  }
  names(obj) <- var.names
  class (obj) <- "ddobj"
  obj
}

# ----------------------------------------------------------------------------------------------
#' Generic summary function for objects of class ddobj
#'
#' @description
#' This function is used to summarise a distributional data object.
#'
#' @param object an object of class \code{ddobj}.
#' @param ... additional arguments.
#'
#' @return This function will not produce a return value, it is called for side effects.
#'
#' @export
#' @examples
#' my.obj <- create_ddobj (toy.data, type=c("numeric","interval","histogram","categorical", "modal"),
#'           cols = c(1, 2, 4, 13, 14), n.int=4, n.cat=3)
#' summary (my.obj)
#'
summary.ddobj <- function (object, ...)
{
  unit.names <- switch(object[[1]]$type,
                         numeric = names(object[[1]]$values),
                         interval = rownames(object[[1]]$values),
                         histogram = rownames(object[[1]]$intervals),
                         categorical = names(object[[1]]$values),
                         modal = rownames(object[[1]]$categories))
  n <- length(unit.names)

  cat ("An object of class ddobj with", n, "units\n")
  print (unit.names)
  cat ("\n  containing", length(object), "distributional data variables.\n")

  for (j in 1:length(object))
  {
    this.list <- object[[j]]
    cat ("\n", names(object)[j], ":", this.list$type, "\n")
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
#' Generic print function for objects of class ddobj
#'
#' @description
#' This function is used to print a distributional data object.
#'
#' @param x an object of class \code{ddobj}.
#' @param ... additional arguments.
#'
#' @return This function will not produce a return value, it is called for side effects.
#'
#' @export
#' @examples
#' my.obj <- create_ddobj (toy.data, type=c("numeric","interval","histogram","categorical", "modal"),
#'           cols = c(1, 2, 4, 13, 14), n.int=4, n.cat=3)
#' print (my.obj)
#'
print.ddobj <- function (x, ...)
{
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required for this function. Please install it.", call. = FALSE)
  }
  unit.names <- switch(x[[1]]$type,
                       numeric = names(x[[1]]$values),
                       interval = rownames(x[[1]]$values),
                       histogram = rownames(x[[1]]$intervals),
                       categorical = names(x[[1]]$values),
                       modal = rownames(x[[1]]$categories))
  n <- length(unit.names)
  tb <- NULL

  for (j in 1:length(x))
  {
    this.list <- x[[j]]
    if (this.list$type=="numeric" | this.list$type =="categorical")
      this.str <- this.list$values

    if (this.list$type=="interval")
      this.str <- paste (paste0("[",format(this.list$values[,1],digits = 3),","),
                         paste0(format(this.list$values[,2],digits = 3),"]"))

    if (this.list$type=="histogram")
    {
      this.str <- NULL
      for (k in 1:ncol(this.list$proportions))
        this.str <- paste (this.str,
                           paste0("[",format(this.list$intervals[,k],digits = 3),","),
                           paste0(format(this.list$intervals[,k+1],digits = 3),"],"),
                           format(this.list$proportions[,k],digits=3))
    }

    if (this.list$type=="modal")
    {
      this.str <- NULL
      for (k in 1:ncol(this.list$categories))
        this.str <- paste (this.str,
                           this.list$categories[,k],
                           paste0("(", format(this.list$proportions[,k],digits=3),")"))
    }
    tb <- tibble::tibble (tb, this.str)
    colnames(tb)[ncol(tb)] <- names(x)[j]
  }
  tb <- tibble::tibble (units = unit.names, tb)
  print(tb)
}
# ----------------------------------------------------------------------------------------------
#' Summarise a data set into a distributional data object
#'
#' @param df data frame from which the \code{ddobj} object is created
#' @param units the column names that will form the units
#' @param interval the column names that will be summarised into interval scaled data
#' @param histogram the column names that will be summarised into histogram scaled data
#' @param modal the column names that will be summarised into modal data
#'
#' @returns and object of class \code{ddobj}
#' @export
#'
#' @usage suminto_ddobj(df, units = names(df)[1],
#'                      interval=NULL, histogram=NULL, modal=NULL)
#'
#' @examples
#' suminto_ddobj (esoph, units = "agegp", interval="ncases",
#'                histogram="ncontrols", modal=c("alcgp","tobgp"))
#'
suminto_ddobj <- function (df, units = names(df)[1],
                           interval=NULL, histogram=NULL, modal=NULL)
{
  df <- as.data.frame(df)
  obj <- vector("list", 0)

  which.cols <- stats::na.omit(match(units, colnames(df)))
  if (length(which.cols) > 1) units.vals <- apply(df[,which.cols],1,paste,collapse="_")
  else units.vals <- df[,which.cols]
  if (length(units.vals) == 0) stop ("units misspecified")

  units <- levels(factor(units.vals))

  if (!is.null(interval))
    for (j in 1:length(interval))
    {
      this.col <- match(interval[j], colnames(df))
      mat <- t(sapply(tapply(df[,this.col], units.vals, range), function(x)x))
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
      out <- tapply(df[,this.col], units.vals, graphics::hist, breaks = 5, plot = FALSE)
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
      out <- tapply(df[,this.col], units.vals, table)
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

#' Extract some variables from a \code{ddobj}
#'
#' @param obj an object of class \code{ddobj}
#' @param j a vector of the numbers of the variables to extract
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @examples
#' my.obj <- create_ddobj (toy.data, type=c("numeric","interval","histogram","categorical", "modal"),
#'           cols = c(1, 2, 4, 13, 14), n.int=4, n.cat=3)
#' extractDDvars (my.obj, j=c(1,3))
#'
extractDDvars <- function (obj, j)
{
  if (any (j>length(obj))) stop (paste("Values in j should only be between 1 and",length(obj)))
  newobj <- vector ("list", length(j))
  for (k in 1:length(j))
  {
    this.var <- obj[[j[[k]]]]
    if (this.var$type == "numeric" | this.var$type == "categorical" |
        this.var$type == "interval")
      newobj[[k]] <- list (type = this.var$type, values = this.var$values)
    if (this.var$type == "histogram")
      newobj[[k]] <- list (type = this.var$type, intervals = this.var$intervals,
                           proportions = this.var$proportions)
    if (this.var$type == "modal")
      newobj[[k]] <- list (type = this.var$type, categories = this.var$categories,
                           proportions = this.var$proportions)
  }
  names (newobj) <- names(obj)[j]
  class(newobj) <- "ddobj"
  newobj
}

#' Extract some units from a \code{ddobj}
#'
#' @param obj an object of class \code{ddobj}
#' @param i a vector of the numbers of the units to extract
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @examples
#' my.obj <- create_ddobj (toy.data, type=c("numeric","interval","histogram","categorical", "modal"),
#'           cols = c(1, 2, 4, 13, 14), n.int=4, n.cat=3)
#' extractDDunits (my.obj, i=c(1,3:4))
#'
extractDDunits <- function (obj, i)
{
  n <- switch(obj[[1]]$type,
              numeric = length(obj[[1]]$values),
              interval = nrow(obj[[1]]$values),
              histogram = nrow(obj[[1]]$intervals),
              categorical = length(obj[[1]]$values),
              modal = nrow(obj[[1]]$categories))
  unit.names <- switch(obj[[1]]$type,
                       numeric = names(obj[[1]]$values),
                       interval = rownames(obj[[1]]$values),
                       histogram = rownames(obj[[1]]$intervals),
                       categorical = rownames(obj[[1]]$values),
                       modal = rownames(obj[[1]]$categories))

  if (any (i>n)) stop (paste("Values in i should only be between 1 and",n))
  newobj <- obj

  for (j in 1:length(obj))
  {
    this.var <- obj[[j]]
    if (this.var$type == "numeric" | this.var$type == "categorical")
    {
      newobj[[j]] <- list (type = this.var$type, values = this.var$values[i])
      names(newobj[[j]]$values) <- names(obj[[j]]$values)[i]
    }
    if (this.var$type == "interval")
    {
      newobj[[j]] <- list (type = this.var$type, values = this.var$values[i,])
      names(newobj[[j]]$values) <- names(obj[[j]]$values)[i]
    }
    if (this.var$type == "histogram")
    {
      newobj[[j]] <- list (type = this.var$type, intervals = this.var$intervals[i,],
                           proportions = this.var$proportions[i,])
      rownames(newobj[[j]]$intervals) <- rownames(newobj[[j]]$proportions) <-
        rownames(obj[[j]]$intervals)[i]
    }
    if (this.var$type == "modal")
    {
      newobj[[j]] <- list (type = this.var$type, categories = this.var$categories[i,],
                           proportions = this.var$proportions[i,])
#      newobj[[j]]$categories <- newobj[[j]]$categories[,!apply (newobj[[j]]$categories, 2,
#                                                                function(x)all(is.na(x)))]
#      newobj[[j]]$proportions <- newobj[[j]]$proportions[,!apply (newobj[[j]]$proportions, 2,
#                                                                function(x)all(is.na(x)))]
      rownames(newobj[[j]]$categories) <- rownames(newobj[[j]]$proportions) <-
        rownames(obj[[j]]$categories)[i]
    }
  }
  names (newobj) <- names(obj)
  class(newobj) <- "ddobj"
  newobj
}

# =======================================================================================

#' Converts an interval scaled variable to a histogram scaled variable with a single bin and
#' proportion one
#'
#' @param x a list, typically a single component of an object of class \code{ddobj}
#'
#' @returns a list, a single histogram scaled variable component for an object of class \code{ddobj}
#' @export
#'
#' @examples
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' int_to_hist (obj$ncases)
#'
int_to_hist <- function (x)
{
  unit.names <- rownames(x$values)
  x <- list(type = "histogram",
            intervals = x$values,
            proportions = matrix(1,nrow=nrow(x$values), ncol=1))
  rownames(x$intervals) <- rownames(x$proportions) <- unit.names
  x
}

# ------------------------------------------------------------------------------------

#' Converts a numeric variable to an interval scaled variable with zero interval width
#'
#' @param x a list, typically a single component of an object of class \code{ddobj}
#'
#' @returns a list, a single interval scaled variable component for an object of class \code{ddobj}
#' @export
#'
#' @examples
#' num_to_int (Oils.data$SO.LDP)
#'
num_to_int <- function (x)
{
  unit.names <- names(x$values)
  x <- list(type = "interval",
            values = cbind(x$values, x$values))
  rownames(x$values) <- unit.names
  x
}

# =======================================================================================

#' Probability density function of a interval scaled data unit
#'
#' @param obj an object of class \code{ddojb}
#' @param i unit number
#' @param j variable number
#'
#' @details
#' This function also operates on numeric data, viewed as zero length intervals.
#' In theory the density is infinite at the numeric value. For computational
#' purposes, the density value is replaced by twice the largest density value
#' of the other units on the same variable.
#'
#' @returns
#' a function(x) that will evaluate the p.d.f. at the values in x
#'
intpdf <- function(obj, i, j)
{
  function(x) {
    eps <- 1e-15
    a <- obj[[j]]$values[, 1]
    b <- obj[[j]]$values[, 2]
    widths <- b - a
    if (all(widths<=1e-15)) min.val <- 1 else min.val <- min(widths[widths>1e-15])/2
    if ((b[i]-a[i])<eps) val <- min.val else val <- widths[i]
    ifelse(x >= a[i] & x <= b[i], 1 / val, 0)
  }
}

# -------------------------------------------------------------------------------------

#' Probability density function of a histogram scaled data unit
#'
#' @param obj an object of class \code{ddojb}
#' @param i unit number
#' @param j variable number
#'
#' @returns
#' a function(x) that will evaluate the p.d.f. at the values in x
#'
histpdf <- function(obj, i, j)
{
  intervals <- stats::na.omit(obj[[j]]$intervals[i,])
  proportions <- stats::na.omit(obj[[j]]$proportions[i,])
  a <- intervals[-length(intervals)]
  b <- intervals[-1]
  widths <- b - a
  density <- proportions / widths

  function(x) {
    k <- findInterval(x, intervals, rightmost.closed = TRUE)
    res <- numeric(length(x))
    inside <- k > 0 & k <= length(density)
    res[inside] <- density[k[inside]]
    res
  }
}

# --------------------------------------------------------------------------------------

#' Cumulative distribution function of a distributional data unit
#'
#' @param obj an object of class \code{ddojb}
#' @param i unit number
#' @param j variable number
#'
#' @returns
#' a function(x) that will evaluate the c.d.f. at the values in x
#'
cdfij <- function (obj, i, j)
{
  if (obj[[j]]$type == "numeric") obj[[j]] <- num_to_int(obj[[j]])
  if (obj[[j]]$type == "interval") obj[[j]] <- int_to_hist(obj[[j]])

  intervals <- stats::na.omit(obj[[j]]$intervals[i,])
  proportions <- stats::na.omit(obj[[j]]$proportions[i,])
  a <- intervals[-length(intervals)]
  b <- intervals[-1]
  widths <- b - a
  cumprop <- c(0, cumsum(proportions))

  function(x) {
    res <- numeric(length(x))
    k <- findInterval(x, intervals, rightmost.closed = TRUE)
    res[k == 0] <- 0
    res[k >= length(intervals)] <- 1
    inside <- k > 0 & k < length(intervals)
    kk <- k[inside]
    res[inside] <- cumprop[kk] +
                     (x[inside] - a[kk]) * proportions[kk] / widths[kk]
    res[is.na(res)] <- 1
    res
  }
}

# ------------------------------------------------------------------------------

#' Computes the quantile function
#'
#' @param obj an object of class \code{ddojb}
#' @param i unit number
#' @param j variable number
#'
#' @returns
#' a function(tvec) that will evaluate the inverse of the c.d.f. at the values
#' in tvec, where 0 <= tvec <= 1
#'
quantij <- function (obj, i, j)
{

  if (obj[[j]]$type == "numeric") obj[[j]] <- DDbiplotEZ::num_to_int (obj[[j]])
  if (obj[[j]]$type == "interval") obj[[j]] <- DDbiplotEZ::int_to_hist (obj[[j]])

  intervals <- stats::na.omit(obj[[j]]$intervals[i,])
  proportions <- stats::na.omit(obj[[j]]$proportions[i,])
  a <- intervals[-length(intervals)]
  b <- intervals[-1]
  widths <- b - a

  function(tvec) {
    res <- numeric(length(tvec))

    if (any(tvec<0)) stop ("Quantile function not defined for values less than 0.")
    if (any(tvec>1)) stop ("Quantile function not defined for values larger than 1.")

    Phi.vec <- cdfij (obj, i, j)(intervals)
    k <- findInterval(tvec, Phi.vec, rightmost.closed = TRUE)
    k[k==0] <- 1
    res[tvec == 0] <- a[1]
    res[tvec == 1] <- b[length(b)]

    inside <- tvec > 0 & tvec < 1
    kk <- k[inside]

    res[inside] <- a[kk] +
      (tvec[inside] - Phi.vec[kk]) / proportions[kk] * widths[kk]
    res
  }
}

# ------------------------------------------------------------------------------

#' Computes uniformly dense intervals
#'
#' @param x an object of class \code{ddobj}
#'
#' @returns a list with the following components
#' \item{dens.int}{an nxpxs array with the s values defining the (s-1) uniformly dense
#' intervals for each of the nxp unit-variable combinations}
#' \item{proportions}{the vector of uniform proportions}
#'
uniformly_dense_intervals <- function (x)
{
  n <- switch(x[[1]]$type,
              numeric = length(x[[1]]$values),
              interval = nrow(x[[1]]$values),
              histogram = nrow(x[[1]]$intervals))
  unit.names <- switch(x[[1]]$type,
                       numeric = names(x[[1]]$values),
                       interval = rownames(x[[1]]$values),
                       histogram = rownames(x[[1]]$intervals))
  p <- length (x)
  for (j in 1:p)
  {
    if (x[[j]]$type == "numeric") x[[j]] <- num_to_int (x[[j]])
    if (x[[j]]$type == "interval") x[[j]] <- int_to_hist (x[[j]])
  }

  weights <- NULL
  for (j in 1:p)
    for (i in 1:n)
      weights <- c(weights, cumsum(c(0,x[[j]]$proportions[i,])))

  weights <- stats::na.omit(weights)
  weights <- sort(unique(weights))
  s <- length(weights)

  out <- array (0, dim=c(n,p,s), dimnames=list(unit.names, names(x), paste0("I",(1:s)-1)))
  p.vec <- diff(weights)

  for (i in 1:n)
    for (j in 1:p)
    {
      out[i,j,] <- quantij (obj = x, i = i, j = j)(weights)
    }
  list (dens.int = out, proportions = p.vec)
}

# ==============================================================================

#' Converts the histogram scaled component of a ddobj to a distrH object for use in HistDAWass
#'
#' @param a a histogram scaled component of a ddobj
#' @param i unit number
#'
#' @noRd
#'
hist_to_distrH <- function (a, i)
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.",
         call. = FALSE)
  }
  int <- as.numeric(a$intervals[i,])
  int <- int[!is.na(int)]
  prop <- as.numeric(a$proportions[i,])
  prop <- prop[!is.na(prop)]
  HistDAWass::distributionH (x = int, p = c(0,cumsum(prop)))
}

#' Computes the squared L2 Wasserstein distance
#'
#' @param obj an object of class \code{ddobj}
#'
#' @returns a symmetric matrix of squared distances
#'
#' @export
#'
#' @examples
#' obj <- suminto_ddobj (esoph, units = "agegp", interval="ncases", histogram="ncontrols")
#' sqL2Wass_dist(obj)
#'
sqL2Wass_dist <- function (obj)
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.",
         call. = FALSE)
  }
  n <- switch(obj[[1]]$type,
          numeric = length(obj[[1]]$values),
          interval = nrow(obj[[1]]$values),
          histogram = nrow(obj[[1]]$intervals))
  unit.names <- switch(obj[[1]]$type,
                         numeric = names(obj[[1]]$values),
                         interval = rownames(obj[[1]]$values),
                         histogram = rownames(obj[[1]]$intervals))
  p <- length (obj)

  Dmat <- matrix (0, nrow=n, ncol=n)
  for (j in 1:p)
  {
    if (obj[[j]]$type == "numeric") obj[[j]] <- num_to_int (obj[[j]])
    if (obj[[j]]$type == "interval") obj[[j]] <- int_to_hist (obj[[j]])

    distrH.list <- vector ("list", n)
    for (i in 1:n) distrH.list[[i]] <- hist_to_distrH(obj[[j]], i=i)

    for (i in 1:(n-1))
      for (h in (i+1):n)
        Dmat[i,h] <- Dmat[i,h] + HistDAWass::WassSqDistH(distrH.list[[i]], distrH.list[[h]])
  }

  Dmat <- Dmat + t(Dmat)
  rownames(Dmat) <- colnames(Dmat) <- unit.names
  Dmat
}

#' Product of two quantile functions using integrate()
#'
#' @param x an object of class ddobj
#' @param i unit number
#' @param j variable number
#'
#' @returns
#' a scalar value
#'
int_prod <- function (x, i, j)
{
  if (length(j) != 2) j <- c(j[1],j[1])
  ff <- function (tvec)
  {
    f1 <- quantij (x, i=i, j=j[1])
    f2 <- quantij (x, i=i, j=j[2])
    f1(tvec)*f2(tvec)
  }

  out <- try(stats::integrate(ff, lower = 0, upper = 1), silent = TRUE)

  if (inherits(out, "try-error") || out$message != "OK") {

    out <- try(stats::integrate(ff, lower = 0, upper = 1, subdivisions = 1000), silent = TRUE)
    if (inherits(out, "try-error") || out$message != "OK")
      out <- try(stats::integrate(ff, lower = 0, upper = 1,
                                  subdivisions = 1000, rel.tol = 1e-5), silent = TRUE)
    if (inherits(out, "try-error") || out$message != "OK")
    {
      warning("integration error")
      return (0)
    }
  }
  out$value
}

mat_prod_sym <- function (x)
{
  n <- switch(x[[1]]$type,
              numeric   = length(x[[1]]$values),
              interval  = nrow(x[[1]]$values),
              histogram = nrow(x[[1]]$intervals))
  p <- length(x)

  outmat <- matrix (0, nrow=p, ncol=p)
  for (rr in 1:p)
    for (cc in (rr:p))
      outmat[rr,cc] <- sum(sapply (1:n, function(i) int_prod(x, i, j=c(rr,cc))))
  dd <- diag(outmat)
  outmat <- outmat + t(outmat)
  diag(outmat) <- dd
  outmat
}
# ==============================================================================

#' Create a vertices matrix from interval scaled data
#'
#' @param obj an object of class \code{ddobj}
#' @param i number of the unit for which the vertices matrix is computed
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
#' obj <- suminto_ddobj (esoph, units = "agegp", interval=c("ncases","ncontrols"))
#' create_vertices (obj, 1)
#'
create_vertices <- function (obj, i, connect=FALSE)
{
  temp.list <- vector("list", length(obj))
  for (j in 1:length(obj))
    if (obj[[j]]$type != "histogram")
      temp.list[[j]] <- obj[[j]]$values[i,]
    else
      temp.list[[j]] <- range(obj[[j]]$intervals[i,], na.rm=TRUE)

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


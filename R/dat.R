#' Toy data of illustrating different types of distributional and 'ordinary' data.
#'
#' A data set containing a numeric, an interval scaled, a histogram scaled, a categorical
#' and a modal variable.
#'
#' @format A data frame with 5 rows and 19 columns:
#' \describe{
#'   \item{X1}{numeric variable}
#'   \item{X2}{first endpoint of the interval scaled variable}
#'   \item{X2.up}{second endpoint of the interval scaled variable. Typically the first endpoint is
#'                the lower endpoint and the second the upper endpoint, however, if specified in
#'                reverse, the function \code{create_ddobj} will switch the endpoints}
#'   \item{X3}{lower end of the first interval for the histogram scaled variable}
#'   \item{X3.I1}{upper end of the first interval for the histogram scaled variable}
#'   \item{X3.I2}{upper end of the second interval for the histogram scaled variable}
#'   \item{X3.I3}{upper end of the third interval for the histogram scaled variable}
#'   \item{X3.I4}{upper end of the fourth interval for the histogram scaled variable}
#'   \item{X3.p1}{proportion of observations in the first interval for the histogram scaled variable}
#'   \item{X3.p2}{proportion of observations in the second interval for the histogram scaled variable}
#'   \item{X3.p3}{proportion of observations in the third interval for the histogram scaled variable}
#'   \item{X3.p4}{proportion of observations in the fourth interval for the histogram scaled variable}
#'   \item{X4}{categorical variable}
#'   \item{X5}{first category for the modal variable}
#'   \item{X5.c2}{second category for the modal variable}
#'   \item{X5.c3}{third category for the modal variable}
#'   \item{X5.p1}{proportion of observations in the first category for the modal variable}
#'   \item{X5.p2}{proportion of observations in the second category for the modal variable}
#'   \item{X5.p3}{proportion of observations in the third category for the modal variable}
#'   }
#'
#'   "toy.data"
#'
toy.data <- data.frame(
  X1 = c(3, 6, 4, 8, 2),
  X2    = c(2, 5, 4, 7, 6),
  X2.up = c(3, 7, 7, 1, 2),
  X3    = c(1,   0,  2,     1,  0),
  X3.I1 = c(2,   1,  2.5,   2,  0.25),
  X3.I2 = c(3,   2,  3,     3,  0.5),
  X3.I3 = c(4,   NA, 3.5,   4,  0.75),
  X3.I4 = c(5,   NA, 4,    NA,  1),
  X3.p1 = c(0.1, 0.7, 0.1, 1/3, 0.2),
  X3.p2 = c(0.2, 0.3, 0.2, 1/3, 0.2),
  X3.p3 = c(0.3, NA,  0.5, 1/3, 0.4),
  X3.p4 = c(0.4, NA,  0.2, NA,  0.2),
  X4 = c("Yes","No","Yes","Unsure","Yes"),
  X5    = c("blue",    "cyan","blue",   "cyan","blue"),
  X5.c2 = c( "red",  "magenta","cyan","magenta", "red"),
  X5.c3 = c("green",       NA, "red",      NA,     NA),
  X5.p1 = c(    0.1,      0.5,   0.3,     0.4,    0.8),
  X5.p2 = c(    0.4,      0.5,   0.3,     0.6,    0.2),
  X5.p3 = c(    0.5,       NA,   0.4,      NA,     NA)
)
rownames(toy.data) <- paste0 ("sample", 1:5)

sample.names <- c("Linseed", "Perilla", "Cotton", "Sesame", "Camellia", "Olive", "Beef", "Hog")
Spec.gravity <- list(c(0.93, 0.94),
                     c(0.93, 0.94),
                     c(0.92, 0.92),
                     c(0.92, 0.93),
                     c(0.92, 0.92),
                     c(0.91, 0.92),
                     c(0.86, 0.87),
                     c(0.86, 0.86))
names(Spec.gravity) <- sample.names
Freezing.point <- list(c(-27, -18),
                       c(-5, -4),
                       c(-6, -1),
                       c(-6, -4),
                       c(-21, -15),
                       c(0, 6),
                       c(30, 38),
                       c(22, 32))
names(Freezing.point) <- sample.names
Iodine.value <- list(c(170, 204),
                     c(192, 208),
                     c(99, 113),
                     c(104, 106),
                     c(80, 82),
                     c(79, 90),
                     c(40, 48),
                     c(53, 77))
names(Iodine.value) <- sample.names
Saponification <- list(c(118, 196),
                       c(188, 197),
                       c(189, 198),
                       c(187, 193),
                       c(189, 193),
                       c(187, 196),
                       c(190, 199),
                       c(190, 202))
names(Saponification) <- sample.names
SO.LDP <- c(1.394, 0.343, 0.289, 0.299, 0.277, 0.390, 0.403, 0.452)
names(SO.LDP) <- sample.names

#'  Oils data of Ichino. 1988. General metrics for mixed features the Cartesian space theory for
#'               pattern recognition. In Proceedings of the 1988 IEEE International Conference on
#'               Systems, Man, and Cybernetics (Vol. 1, pp. 494-497). IEEE.
#'
#' A data set containing 8 units with four interval scaled variables and one numeric variable.
#'
#' @format An object of class \code{ddobj}. A list of length 5:
#' \describe{
#'   \item{Spec.gravity}{interval scaled variable}
#'   \item{Freezing.point}{interval scaled variable}
#'   \item{Iodine.value}{interval scaled variable}
#'   \item{Saponification}{interval scaled variable}
#'   \item{Fatty.acids}{numeric variable}
#'   }
#'
#'   "Oils.data"
#'
Oils.data <- list (Spec.gravity = list (type = "interval",
                                        values = t(sapply(Spec.gravity, function(x) return (x)))),
                   Freezing.point = list (type = "interval",
                                          values = t(sapply(Freezing.point, function(x) return (x)))),
                   Iodine.value = list (type = "interval",
                                        values = t(sapply(Iodine.value, function(x) return (x)))),
                   Saponification = list (type = "interval",
                                          values = t(sapply(Saponification, function(x) return (x)))),
                   Fatty.acids = list (type = "numeric",
                                  values = SO.LDP))
class(Oils.data) <- "ddobj"

# ----- Credit card data

#'  Credit card data.
#'
#' A data set containing 10 units with five interval scaled variables.
#'
#' @format An object of class \code{ddobj}. A list of length 5:
#' \describe{
#'   \item{Food}{interval scaled variable}
#'   \item{Social}{interval scaled variable}
#'   \item{Travel}{interval scaled variable}
#'   \item{Gas}{interval scaled variable}
#'   \item{Clothes}{numeric variable}
#'   }
#'
   "Creditcard.data"

#tmp <- new.env()
#load("G:\\My Drive\\My Documents\\Navorsing\\Projekte\\Symbolic Data Analysis\\credicard_dataset.RDATA", envir = tmp)
#tmpdata <- tmp$CreditCard_symbDF
#colnames (tmpdata) <- gsub (" min", "", colnames (tmpdata))
#Creditcard.data <- create_ddobj (tmpdata,
#                                 types = rep("interval", 5),
#                                  cols = c(1, 3, 5, 7, 9))
#rm(tmpdata)
#rm(tmp)

### ===================================================================
### Data sets from MAINT.Data

#' Converts the Cars data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{MAINT.Data} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' Cars.data <- get.Cars()
#'
get.Cars <- function ()
{
  if (!requireNamespace("MAINT.Data", quietly = TRUE)) {
    stop("Package 'MAINT.Data' is required for this function. Please install it.", call. = FALSE)
  }
  df <- MAINT.Data::Cars
  colnames (df) <- gsub ("LB_", "", colnames (df))
  create_ddobj (df,
                types = c(rep("interval", 4), "categorical"),
                cols = c(1, 3, 5, 7, 9))
}

#' Converts the ChinaTemp data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{MAINT.Data} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' China.data <- get.ChinaTemp()
#'
get.ChinaTemp <- function ()
{
  if (!requireNamespace("MAINT.Data", quietly = TRUE)) {
    stop("Package 'MAINT.Data' is required for this function. Please install it.", call. = FALSE)
  }
  df <- MAINT.Data::ChinaTemp
  colnames (df) <- gsub ("LB_", "", colnames (df))
  create_ddobj (df,
                types = c(rep("interval", 4), "categorical"),
                cols = c(1, 3, 5, 7, 9))
}

#' Converts the FlightsIdt data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{MAINT.Data} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' Flights.data <- get.FlightsIdt()
#'
get.FlightsIdt <- function ()
{
  if (!requireNamespace("MAINT.Data", quietly = TRUE)) {
    stop("Package 'MAINT.Data' is required for this function. Please install it.", call. = FALSE)
  }
  half.ranges <- exp(MAINT.Data::FlightsIdt@LogR)/2
  midpoints <- MAINT.Data::FlightsIdt@MidP
  p <- ncol(midpoints)
  mat <- NULL
  for (j in 1:p)
    mat <- cbind (mat, midpoints[,j]-half.ranges[,j], midpoints[,j]+half.ranges[,j])
  colnames (mat) <- paste0 (rep(gsub (".MidP", "", colnames (midpoints)), each=2), c("","1"))
  rownames (mat) <- rownames(MAINT.Data::FlightsIdt@MidP)
  create_ddobj (mat,
                types = rep("interval", p),
                cols = c((1:p)*2-1))
}

#' Converts the LoansbyPurpose_minmaxDt data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{MAINT.Data} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' Loan.data <- get.LoansbyPurpose_minmaxDt()
#'
get.LoansbyPurpose_minmaxDt <- function ()
{
  if (!requireNamespace("MAINT.Data", quietly = TRUE)) {
    stop("Package 'MAINT.Data' is required for this function. Please install it.", call. = FALSE)
  }
  df <- MAINT.Data::LoansbyPurpose_minmaxDt
  colnames (df) <- gsub ("_min", "", colnames (df))
  create_ddobj (df,
                types = rep("interval", 4),
                cols = c(1, 3, 5, 7))
}

#' Converts the LoansbyRiskLvs_minmaxDt data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{MAINT.Data} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' Loan.data <- get.LoansbyRiskLvs_minmaxDt()
#'
get.LoansbyRiskLvs_minmaxDt <- function ()
{
  if (!requireNamespace("MAINT.Data", quietly = TRUE)) {
    stop("Package 'MAINT.Data' is required for this function. Please install it.", call. = FALSE)
  }
  df <- MAINT.Data::LoansbyRiskLvs_minmaxDt
  colnames (df) <- gsub ("_min", "", colnames (df))
  create_ddobj (df,
                types = rep("interval", 4),
                cols = c(1, 3, 5, 7))
}


#' Converts the LoansbyRiskLvs_qntlDt data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{MAINT.Data} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' Loan.data <- get.LoansbyRiskLvs_qntlDt()
#'
get.LoansbyRiskLvs_qntlDt <- function ()
{
  if (!requireNamespace("MAINT.Data", quietly = TRUE)) {
    stop("Package 'MAINT.Data' is required for this function. Please install it.", call. = FALSE)
  }
  df <- MAINT.Data::LoansbyRiskLvs_qntlDt
  colnames (df) <- gsub ("_q0.10", "", colnames (df))
  create_ddobj (df,
                types = rep("interval", 4),
                cols = c(1, 3, 5, 7))
}

### ===================================================================
### Data sets from dataSDA

#' Convert histogram-valued data from dataSDA to data.frame
#'
#' @param hist_mat output of the \code{hist_extract()} function
#'
#' @returns a data frame for input to create_ddobj
#'
dataSDAhist_to_cols <- function(hist_mat)
{
   concepts <- tapply(hist_mat$concept,hist_mat$observation,length)
   col.max <- max(concepts)+1
   mat <- matrix (nrow=length(concepts), ncol=col.max*2-1)
   for (i in 1:length(concepts))
   {
     submat <- hist_mat[hist_mat$concept == names(concepts)[i],]
     mat[i,1:(concepts[i]+1)] <- c(submat$lower[1],submat$upper)
     mat[i,col.max+(1:concepts[i])] <- submat$proportion
   }
   rownames(mat) <- names(concepts)
   mat
}

#' Convert modal-valued data from dataSDA to data.frame
#'
#' @param modal_mat output of the \code{hist_extract()} function
#'
#' @returns a data frame for input to create_ddobj
#'
dataSDAmodal_to_cols <- function(modal_mat)
{
  concepts <- tapply(modal_mat$concept, modal_mat$observation, length)
  col.max <- max(concepts)
  mat <- matrix (nrow=length(concepts), ncol=col.max*2)
  for (i in 1:length(concepts))
  {
    submat <- modal_mat[modal_mat$concept == names(concepts)[i],]
    mat[i,1:(concepts[i])] <- submat$label
    mat[i,col.max+(1:concepts[i])] <- submat$proportion
  }
  rownames(mat) <- names(concepts)
  mat
}

#' Copy of dataSDA::hist_extract
#'
#' @details
#' Added code with ChatGPT to allow for modal variables
#' Changed default of normalize = TRUE
#'
#' @noRd
#'
hist_extract <- function (x, variables = NULL, normalize = TRUE)
{
  if (!is.logical(normalize) || length(normalize) != 1L ||
      is.na(normalize)) {
    stop("'normalize' must be TRUE or FALSE.", call. = FALSE)
  }
  number <- "[+-]?(?:(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?|Inf)"
  interval <- paste0("^([[(])\\s*(", number, ")\\s*,\\s*(",
                     number, ")\\s*([])])$")
  one_sided <- paste0("^(<=|>=|<|>)\\s*(", number, ")$")
  point <- paste0("^(", number, ")$")
  matches <- function(pattern, value) {
    regmatches(value, regexec(pattern, value, perl = TRUE))[[1L]]
  }
  endpoints <- function(label) {
    s <- trimws(label)
    if (grepl("^[^[(<>]+\\(.*\\)$", s, perl = TRUE)) {
      s <- sub("^[^[(<>]+\\((.*)\\)$", "\\1", s, perl = TRUE)
      s <- trimws(s)
    }
    m <- matches(interval, s)
    if (length(m)) {
      return(list(lower = as.numeric(m[3L]), upper = as.numeric(m[4L]),
                  lower_closed = m[2L] == "[", upper_closed = m[5L] ==
                    "]"))
    }
    m <- matches(one_sided, s)
    if (length(m)) {
      v <- as.numeric(m[3L])
      if (startsWith(m[2L], "<")) {
        return(list(lower = -Inf, upper = v, lower_closed = FALSE,
                    upper_closed = m[2L] == "<="))
      }
      return(list(lower = v, upper = Inf, lower_closed = m[2L] ==
                    ">=", upper_closed = FALSE))
    }
    if (length(matches(point, s))) {
      v <- as.numeric(s)
      return(list(lower = v, upper = v, lower_closed = TRUE,
                  upper_closed = TRUE))
    }
    NULL
  }

  candidate <- function(col) {
    if (inherits(col, "symbolic_modal")) {
      cells <- unclass(col)
      return(any(vapply(cells, function(cell) {
        is.list(cell) && !is.null(cell$var) && !is.null(cell$prop) && length(cell$var) > 0L
      }, logical(1))))
    }
    if (!is.character(col))
      return(FALSE)
    any(grepl(paste0("^\\s*\\{\\s*(?:[[(<>]|", number, "\\s*,)"),
              col[!is.na(col)], perl = TRUE))
  }

  empty <- data.frame(observation = integer(), concept = character(),
                      variable = character(), bin = integer(), lower = double(),
                      upper = double(), proportion = double(), lower_closed = logical(),
                      upper_closed = logical(), label = character(), stringsAsFactors = FALSE)
  if (is.data.frame(x)) {
    nr <- nrow(x)
    concepts <- attr(x, "concept", exact = TRUE)
    if (is.null(concepts))
      concepts <- rownames(x)
    cols <- unclass(x)
    if (is.null(variables)) {
      variables <- names(cols)[vapply(cols, candidate,
                                      logical(1))]
      if (!length(variables) && nr > 0L) {
        stop("No numeric histogram columns detected; select 'variables' explicitly if needed.",
             call. = FALSE)
      }
    }
    else if (!is.character(variables) || anyNA(variables) ||
             anyDuplicated(variables) || !all(variables %in% names(cols))) {
      stop("'variables' must contain distinct existing column names.",
           call. = FALSE)
    }
    cols <- cols[variables]
  }
  else if (is.character(x) || inherits(x, "symbolic_modal")) {
    if (!is.null(variables))
      stop("'variables' requires a data frame.", call. = FALSE)
    nr <- length(x)
    concepts <- names(x)
    cols <- list(histogram = x)
  }
  else {
    stop("'x' must be a character vector, symbolic_modal column, or data frame.",
         call. = FALSE)
  }
  if (is.null(concepts))
    concepts <- as.character(seq_len(nr))
  if (length(concepts) != nr)
    stop("The 'concept' attribute must match the row count.",
         call. = FALSE)
  output <- vector("list", nr * length(cols))
  k <- 0L
  for (j in seq_along(cols)) {
    col <- cols[[j]]
    modal <- inherits(col, "symbolic_modal")
    if (modal)
      col <- unclass(col)
    if (!modal && !is.character(col)) {
      stop("Unsupported selected column: ", names(cols)[j],
           call. = FALSE)
    }
    for (i in seq_len(nr)) {
      fail <- function(message) stop("Row ", i, ", variable '",
                                     names(cols)[j], "': ", message, call. = FALSE)
      cell <- col[[i]]
      missing <- is.null(cell) || (!modal && (is.na(cell) ||
                                                !nzchar(trimws(cell))))
      if (missing) {
        bins <- data.frame(bin = NA_integer_, lower = NA_real_,
                           upper = NA_real_, proportion = NA_real_, lower_closed = NA,
                           upper_closed = NA, label = NA_character_)
      }
      else {
        if (modal) {
          if (!is.list(cell) || is.null(cell$var) ||
              is.null(cell$prop)) {
            fail("Expected modal 'var' and 'prop' components.")
          }
          labels <- as.character(cell$var)
          props <- cell$prop
          if (!is.numeric(props) || !length(labels) ||
              length(labels) != length(props)) {
            fail("Modal labels and numeric proportions must have equal positive lengths.")
          }
        }
        else {
          s <- trimws(cell)
          if (!grepl("^\\{.+\\}$", s))
            fail("Expected a histogram enclosed in braces.")
          s <- substr(s, 2L, nchar(s) - 1L)
          s <- gsub(paste0("([])])\\s*;\\s*(", number,
                           ")\\s*(?=;|$)"), "\\1, \\2", s, perl = TRUE)
          if (grepl(";\\s*$", s))
            fail("Empty trailing bin.")
          parts <- strsplit(s, ";", fixed = TRUE)[[1L]]
          pattern <- paste0("^\\s*(.+)\\s*,\\s*(", number,
                            ")\\s*$")
          parsed <- lapply(parts, function(s) matches(pattern,
                                                      s))
          if (any(lengths(parsed) != 3L))
            fail("Malformed bin or proportion.")
          labels <- vapply(parsed, function(m) trimws(m[2L]),
                           character(1))
          props <- vapply(parsed, function(m) as.numeric(m[3L]),
                          numeric(1))
        }
        if (anyNA(labels))
          fail("Missing bin label.")
        ep <- lapply(labels, endpoints)
        numeric_labels <- !vapply (ep, is.null, logical(1))
        if (any(!is.finite(props) | props < 0 | props > 1)) {
          fail("Proportions must be finite numbers between zero and one.")
        }

# --- ChaptGPT extension for modal variables
        if (modal && !all(numeric_labels))
        {
          lower <- vapply (ep, function(b) {if (is.null(b)) NA_real_ else b$lower}, numeric(1))
          upper <- vapply (ep, function(b) {if (is.null(b)) NA_real_ else b$upper}, numeric(1))
          lower_closed <- vapply (ep, function(b) {if (is.null(b)) NA else b$lower_closed}, logical(1))
          upper_closed <- vapply (ep, function(b) {if (is.null(b)) NA else b$upper_closed}, logical(1))
        }
        else
        {
          lower <- vapply (ep, function(b) b$lower, numeric(1))
          upper <- vapply (ep, function(b) b$upper, numeric(1))
          lower_closed <- vapply (ep, function(b) b$lower_closed, logical(1))
          upper_closed <- vapply (ep, function(b) b$upper_closed, logical(1))
        }
# ---
        bins <- data.frame(lower, upper, lower_closed, upper_closed)
        if (normalize) {
          if (sum(props) <= 0)
            fail("Cannot normalize a histogram with zero total mass.")
          props <- props/sum(props)
        }
        bins$bin <- seq_along(labels)
        bins$proportion <- props
        bins$label <- labels
      }
      k <- k + 1L
      output[[k]] <- data.frame(observation = i, concept = as.character(concepts[i]),
                                variable = names(cols)[j], bins, stringsAsFactors = FALSE)
    }
  }
  if (!k)
    return(empty)
  result <- do.call(rbind, output)
  rownames(result) <- NULL
  reversed <- which(result$lower > result$upper)
  if (length(reversed)) {
    first <- reversed[1L]
    warning(length(reversed), " bin(s) have reversed endpoints; preserved as stored. First: row ",
            result$observation[first], ", variable '", result$variable[first],
            "', bin ", result$bin[first], ".", call. = FALSE)
  }
  result[names(empty)]
}

#' Extract modal variables from the dataSDA package
#'
#' @param x modal data from a data set in dataSDA
#' @param normalize logical value to ensure probabilities add up to 1
#'
#' @returns a data frame for \code{create_ddobj()}
#'
modal_extract <- function(x, normalize = TRUE)
{
  if (!is.character(x)) { stop("'x' must be a character vector.", call. = FALSE)  }

  parse_cell <- function(cell)
    {
      if (is.na(cell) || !nzchar(trimws(cell)))
        { return(list(category = character(0), proportion = numeric(0))) }
      s <- trimws(cell)
      if (!grepl("^\\{.*\\}$", s))
        { stop("Invalid modal cell: ", cell, call. = FALSE) }
      s <- substr(s, 2L, nchar(s) - 1L)
      parts <- strsplit(s, ";", fixed = TRUE)[[1L]]

      parsed <- lapply(parts, function(z)
                              {  m <- regmatches(trimws(z),
                                                 regexec("^(.+?)\\s*,\\s*([0-9.eE+-]+)\\s*$", trimws(z)))[[1L]]
                                 if (length(m) != 3L) { stop("Malformed category/proportion: ", z, call. = FALSE) }
                                 list(category = trimws(m[2L]), proportion = as.numeric(m[3L]))   })
      categories <- vapply(parsed, `[[`, character(1), "category")
      proportions <- vapply(parsed, `[[`, numeric(1), "proportion")

      if (anyNA(proportions) || any(proportions < 0) || any(proportions > 1))
        { stop("Proportions must be between 0 and 1.", call. = FALSE) }

      if (normalize)
        {  total <- sum(proportions)
           if (total <= 0)
             stop("Cannot normalize zero total proportion.", call. = FALSE)
           proportions <- proportions / total
        }
      list(category = categories, proportion = proportions)
  }

  parsed <- lapply(x, parse_cell)
  max_categories <- max(vapply(parsed, function(z) length(z$category), integer(1)), 0L)
  if (max_categories == 0L) return(data.frame())

  # Categories first
  category_names <- paste0("category", seq_len(max_categories))
  proportion_names <- paste0("proportion", seq_len(max_categories))

  result <- data.frame(matrix(NA_character_, nrow = length(x), ncol = max_categories),
                       stringsAsFactors = FALSE)
  names(result) <- category_names
  proportions <- data.frame(matrix(NA_real_, nrow = length(x), ncol = max_categories),
                            stringsAsFactors = FALSE)
  names(proportions) <- proportion_names

  for (i in seq_along(parsed))
    {
       n <- length(parsed[[i]]$category)
       if (n > 0) {  result[i, seq_len(n)] <- parsed[[i]]$category
                     proportions[i, seq_len(n)] <- parsed[[i]]$proportion
                  }
    }
  cbind(result, proportions)
}

#' Converts the abalone.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' abalone.data <- get.abalone.int()
#'
get.abalone.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::abalone.int
  colnames (df) <- gsub ("_min", "", colnames (df))
  create_ddobj (df,
                types = rep("interval", 7),
                cols = c(1, 3, 5, 7, 9, 11, 13))
}

#' Converts the acid_rain.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' acidRain.data <- get.acid_rain.int()
#'
get.acid_rain.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::acid_rain.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  create_ddobj (df,
                types = c("categorical", rep("interval", 2)),
                cols = c(1, 2, 4))
}

#' Converts the age_choloesterol_weight.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' age.chol.wt.data <- get.age_cholesterol_weight.int()
#'
get.age_cholesterol_weight.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::age_cholesterol_weight.int)
  var.names <- colnames(dataSDA::age_cholesterol_weight.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::age_cholesterol_weight.int[[j]], function(x)
                     { complex <- x[1]
                       cbind (Re(complex),Im(complex))
                     })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  create_ddobj (mat,
                types = c("interval","interval","interval","numeric"),
                cols = c(1, 3, 5, 7))
}

#' Converts the age_pyramids.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' pyramids.data <- get.age_pyramids.hist()
#'
get.age_pyramids.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::age_pyramids.hist)
  var.names <- colnames(dataSDA::age_pyramids.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::age_pyramids.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::age_pyramids.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the airline_flights.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' flights.data <- get.airline_flights.hist()
#'
get.airline_flights.hist <- function ()
{
  #  [1] "Flight Time(<120)"        "Flight Time([120, 220])"  "Flight Time(>220)"        "Taxi In(<4)"
  #  [5] "Taxi In([4, 10])"         "Taxi In(>10)"             "Arrival Delay(<0)"        "Arrival Delay([0, 60])"
  #  [9] "Arrival Delay(>60)"       "Taxi Out(<16)"            "Taxi Out([16, 30])"       "Taxi Out(>30)"
  #  [13] "Departure Delay(<0)"      "Departure Delay([0, 60])" "Departure Delay(>60)"     "Weather Delay(No)"
  #  [17] "Weather Delay(Yes)"

  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  Flight.Time <- cbind (Flight.Time=0, FT1=120, FT2=220, FT3=220+120,
                        FT4 = dataSDA::airline_flights.hist$"Flight Time(<120)",
                        FT5 = dataSDA::airline_flights.hist$"Flight Time([120, 220])",
                        FT6 = dataSDA::airline_flights.hist$"Flight Time(>220)")
  Taxi.In <- cbind (Taxi.In=0, TI1=4, TI2=10, TI3=20,
                    TI4 = dataSDA::airline_flights.hist$"Taxi In(<4)",
                    TI5 = dataSDA::airline_flights.hist$"Taxi In([4, 10])",
                    TI6 = dataSDA::airline_flights.hist$"Taxi In(>10)")
  Arrival.Delay <- cbind (Arrival.Delay=-60, AD1=0, AD2=60, AD3=120,
                          AD4 = dataSDA::airline_flights.hist$"Arrival Delay(<0)",
                          AD5 = dataSDA::airline_flights.hist$"Arrival Delay([0, 60])",
                          AD6 = dataSDA::airline_flights.hist$"Arrival Delay(>60)")
  Taxi.Out <- cbind (Taxi.Out=0, TO1=16, TO2=30, TO3=60,
                     TO4 = dataSDA::airline_flights.hist$"Taxi Out(<16)",
                     TO5 = dataSDA::airline_flights.hist$"Taxi Out([16, 30])",
                     TO6 = dataSDA::airline_flights.hist$"Taxi Out(>30)")
  Departure.Delay <- cbind (Departure.Delay=-60, DD1=0, DD2=60, DD3=120,
                            DD4 = dataSDA::airline_flights.hist$"Departure Delay(<0)",
                            DD5 = dataSDA::airline_flights.hist$"Departure Delay([0, 60])",
                            DD6 = dataSDA::airline_flights.hist$"Departure Delay(>60)")
  Weather.Delay <- data.frame (Weather.Delay="Yes", WD1="No",
                               WD3 = dataSDA::airline_flights.hist$"Weather Delay(Yes)",
                               WD4 = dataSDA::airline_flights.hist$"Weather Delay(No)")
  df <- data.frame (Flight.Time, Taxi.In, Arrival.Delay, Taxi.Out, Departure.Delay, Weather.Delay)
  create_ddobj (df,
                types = c(rep("histogram", 5), "modal"),
                cols = c(1,8,15,22,29,36),
                n.int = rep(3,5),2)
}

#' Converts the airline_flights2.modal data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' flights.data <- get.airline_flights2.modal()
#'
get.airline_flights2.modal <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::airline_flights2.modal)
  var.names <- colnames(dataSDA::airline_flights2.modal)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAmodal_to_cols(hist_extract(dataSDA::airline_flights2.modal[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, ncol(dat)/2)
  }
  rownames(mat) <- rownames(dataSDA::airline_flights2.modal)
  create_ddobj (mat,
                types = rep("modal",p),
                cols = 1+c(0, cumsum(n.int*2)[-length(n.int)]),
                n.cat = n.int)
}

#' Converts the baseball.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' baseball.data <- get.baseball.int()
#'
get.baseball.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::baseball.int)
  var.names <- colnames(dataSDA::baseball.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::baseball.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (mat, sapply(dataSDA::baseball.int[[3]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:2], each=2), c("","1")), var.names[3])
  create_ddobj (mat,
                types = c("interval","interval","categorical"),
                cols = c(1, 3, 5))
}

#' Converts the bats.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' bats.data <- get.bats.int()
#'
get.bats.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::bats.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  create_ddobj (df,
                types = c("categorical", rep("interval", 4)),
                cols = c(1, 2, 4, 6, 8))
}

#' Converts the bird.mix data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' bird.data <- get.bird.mix()
#'
get.bird.mix <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::bird.mix)
  var.names <- colnames(dataSDA::bird.mix)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::bird.mix[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:2], each=2), c("","1")))
  create_ddobj (mat,
                types = c("interval","interval"),
                cols = c(1, 3))
}

#' Converts the bird_color_taxonomy.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' bird.data <- get.bird_color_taxonomy.hist()
#'
get.bird_color_taxonomy.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::bird_color_taxonomy.hist)
  var.names <- colnames(dataSDA::bird_color_taxonomy.hist)
  mat <- n.int <- NULL
  for (j in 1:2)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::bird_color_taxonomy.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  mat <- cbind (mat, dataSDA::bird_color_taxonomy.hist[[3]])
  colnames(mat)[ncol(mat)] <- var.names[3]
  dat <- modal_extract(dataSDA::bird_color_taxonomy.hist[[4]])
  colnames(dat) <- c(var.names[4], 2:ncol(dat))
  mat <- cbind (mat, dat)
  n.cat <- ncol(dat)/2

  rownames(mat) <- rownames(dataSDA::bird_color_taxonomy.hist)
  create_ddobj (mat,
                types = c(rep("histogram",2),"categorical","modal"),
                cols = 1+c(0, cumsum(n.int*2+1),sum(n.int*2+1)+1),
                n.cat = n.cat, n.int=n.int)
}

#' Converts the bird_species_extended.mix data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' bird.data <- get.bird_species_extended.mix()
#'
get.bird_species_extended.mix <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::bird_species_extended.mix)
  var.names <- colnames(dataSDA::bird_species_extended.mix)

  mat <- cbind (dataSDA::bird_species_extended.mix[[1]], dataSDA::bird_species_extended.mix[[2]])
  colnames(mat) <- var.names[1:2]

  df <- dataSDA::bird_species_extended.mix[,3:4]
  colnames (df) <- gsub ("_l", "", colnames (df))
  mat <- cbind (mat, df)

  dat <- modal_extract(dataSDA::bird_species_extended.mix[[5]])
  colnames(dat) <- c(var.names[5], 2:ncol(dat))
  mat <- cbind (mat, dat)
  n.cat <- ncol(dat)/2

  mat <- cbind (mat, dataSDA::bird_species_extended.mix[[6]])
  colnames (mat)[ncol(mat)] <- var.names[6]

  rownames(mat) <- rownames(dataSDA::bird_species_extended.mix)
  create_ddobj (mat,
                types = c(rep("categorical",2),"interval","modal","categorical"),
                cols = c(1,2,3,5,5+n.cat*2),
                n.cat = n.cat)
}

#' Converts the blood.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' blood.data <- get.blood.hist()
#'
get.blood.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::blood.hist)
  var.names <- colnames(dataSDA::blood.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::blood.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::blood.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the blood_pressure.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' bp.data <- get.blood_pressure.int()
#'
get.blood_pressure.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::blood_pressure.int)
  var.names <- colnames(dataSDA::blood_pressure.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::blood_pressure.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the car.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' car.data <- get.car.int()
#'
get.car.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::car.int)
  var.names <- colnames(dataSDA::car.int)
  mat <- NULL
  for (j in 2:p)
  {
    dat <- sapply(dataSDA::car.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (sapply(dataSDA::car.int[[1]], function(x) x[1]), mat)
  colnames(mat) <- c(var.names[1], paste0 (rep(var.names[2:p], each=2), c("","1")))
  create_ddobj (mat,
                types = c("categorical", rep("interval",4)),
                cols = c(1, 2, 4, 6, 8))
}

#' Converts the car_models.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' car_models.data <- get.car_models.int()
#'
get.car_models.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::car_models.int)
  var.names <- colnames(dataSDA::car_models.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::car_models.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }

  mat <- data.frame (mat, sapply(dataSDA::car_models.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:(p-1)], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval",p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the cardiological.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#'
#' @examples
#' cardiological.data <- get.cardiological.int()
#'
get.cardiological.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::cardiological.int)
  var.names <- colnames(dataSDA::cardiological.int)
  mat <- NULL
  for (j in (1:p))
  {
    dat <- sapply(dataSDA::cardiological.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p*2-1))
}

#' Converts the cars.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' cars.data <- get.cars.int()
#'
get.cars.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::cars.int)
  var.names <- colnames(dataSDA::cars.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::cars.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }

  mat <- data.frame (mat, sapply(dataSDA::cars.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:(p-1)], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval",p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the census.mix data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' census.data <- get.census.mix()
#'
get.census.mix <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::census.mix)
  var.names <- colnames(dataSDA::census.mix)

  mat <- n.int <- NULL
  for (j in 1:2)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::census.mix[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }

  n.cat <- NULL
  for (j in c(3,5))
  {
    dat <- modal_extract(dataSDA::census.mix[[j]])
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.cat <- n.cat <- c(n.cat, ncol(dat)/2)
  }

  mat <- cbind (mat, dataSDA::census.mix[[4]])
  colnames(mat)[ncol(mat)] <- var.names[4]

  dat <- sapply(dataSDA::census.mix[[6]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
  a <- ncol(mat)
  mat <- cbind (mat, t(dat))
  colnames(mat)[a+1] <- var.names[6]

  rownames(mat) <- rownames(dataSDA::census.mix)
  create_ddobj (mat,
                types = c(rep("histogram",2),rep("modal",2),"categorical","interval"),
                cols = c(1, (n.int[1]+1)*2, sum((n.int+1)*2)-1, sum((n.int+1)*2)+n.cat[1]*2-1,
                         sum(c(n.int,n.cat)*2)+3, sum(c(n.int,n.cat)*2)+4),
                n.cat = n.cat, n.int = n.int)
}

#' Converts the china_climate_month.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' China.data <- get.china_climate_month.hist()
#'
get.china_climate_month.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::china_climate_month.hist)
  var.names <- colnames(dataSDA::china_climate_month.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::china_climate_month.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::china_climate_month.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the china_climate_season.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' China.data <- get.china_climate_season.hist()
#'
get.china_climate_season.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::china_climate_season.hist)
  var.names <- colnames(dataSDA::china_climate_season.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::china_climate_season.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::china_climate_season.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the china_temp.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' ChinaTemp.data <- get.china_temp.int()
#'
get.china_temp.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::china_temp.int)
  var.names <- colnames(dataSDA::china_temp.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::china_temp.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }

  mat <- data.frame (mat, sapply(dataSDA::china_temp.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:(p-1)], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval",p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the china_temp_monthly.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' ChinaTemp.data <- get.china_temp_monthly.int()
#'
get.china_temp_monthly.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::china_temp_monthly.int)
  var.names <- colnames(dataSDA::china_temp_monthly.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::china_temp_monthly.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }

  mat <- data.frame (mat, sapply(dataSDA::china_temp_monthly.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:(p-1)], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval",p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the cholesterol.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' cholesterol.data <- get.cholesterol.hist()
#'
get.cholesterol.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::cholesterol.hist)
  var.names <- colnames(dataSDA::cholesterol.hist)
  mat <- dataSDA::cholesterol.hist[,1:2]
  colnames(mat) <- var.names[1:2]

  n.int <- NULL
  dat <- dataSDAhist_to_cols(hist_extract(dataSDA::cholesterol.hist[[3]]))
  colnames(dat) <- c(var.names[3], 2:ncol(dat))
  mat <- cbind (mat, dat)
  n.int <- c(n.int, (ncol(dat)-1)/2)

  rownames(mat) <- rownames(dataSDA::cholesterol.hist)
  create_ddobj (mat,
                types = c(rep("categorical",2),"histogram"),
                cols = 1:3,
                n.int = n.int)
}

#' Converts the county_income_gender.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' county.data <- get.county_income_gender.hist()
#'
get.county_income_gender.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::county_income_gender.hist)
  var.names <- colnames(dataSDA::county_income_gender.hist)
  mat <- n.int <- NULL
  for (j in 1:2)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::county_income_gender.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  mat <- cbind (mat, dataSDA::county_income_gender.hist[[3]])
  colnames(mat)[ncol(mat)] <- var.names[3]
  mat <- cbind (mat, dataSDA::county_income_gender.hist[[4]])
  colnames(mat)[ncol(mat)] <- var.names[4]


  rownames(mat) <- rownames(dataSDA::county_income_gender.hist)
  create_ddobj (mat,
                types = c(rep("histogram",2),rep("numeric",2)),
                cols = 1+c(0, cumsum(n.int*2+1), sum(n.int*2+1)+1),
                n.int = n.int)
}

#' Converts the cover_types.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' cover.data <- get.cover_types.hist()
#'
get.cover_types.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::cover_types.hist)
  var.names <- colnames(dataSDA::cover_types.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::cover_types.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::cover_types.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the credit_card.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' creditCard.data <- get.credit_card.int()
#'
get.credit_card.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::credit_card.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  create_ddobj (df,
                types = c("categorical", rep("interval", 5)),
                cols = c(1, 2, 4, 6, 8, 10))
}

#' Converts the crime.modal data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' crime.data <- get.crime.modal()
#'
get.crime.modal <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::crime.modal
  p <- ncol(dataSDA::crime.modal)
  n <- nrow(dataSDA::crime.modal)
  var.names <- colnames(dataSDA::crime.modal)
  mat <- NULL

  mat <- cbind (mat, Crime = rep("violent",n),
                     Crime2 = rep("non-violent", n),
                     Crime3 = rep("none", n))
  df[10,1:3] <- df[10,1:3]/sum(df[10,1:3])
  mat <- cbind (mat, df[,1:3])

  mat <- cbind (mat, Gender = rep("male",n),
                     Gender2 = rep("female",n))
  df[14,4:5] <- df[14,4:5]/sum(df[14,4:5])
  mat <- cbind (mat, df[,4:5])

  mat <- cbind (mat, Age = rep("<20",n),
                Age2 = rep(">=20",n))
  mat <- cbind (mat, df[,6:7])

  rownames (mat) <- rownames(df)
  create_ddobj (mat,
                types = rep("modal",3),
                cols = c(1, 7, 11),
                n.cat = c(3,2,2))
}

#' Converts the ecoli_routes.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' ecoliRoutes.data <- get.ecoli_routes.int()
#'
get.ecoli_routes.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::ecoli_routes.int)
  var.names <- colnames(dataSDA::ecoli_routes.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::ecoli_routes.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }

  mat <- data.frame (mat, sapply(dataSDA::ecoli_routes.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:(p-1)], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the employment.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' employment.data <- get.employment.int()
#'
get.employment.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::employment.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  rownames(df) <-  dataSDA::employment.int[,1]
  create_ddobj (df,
                types = c(rep("interval", 9), "categorical"),
                cols = c((1:9)*2,20))
}

#' Converts the environment.mix data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' environment.data <- get.environment.mix()
#'
get.environment.mix <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::environment.mix)
  var.names <- colnames(dataSDA::environment.mix)
  df.list <- vector ("list", 4)

  for (j in 1:4)   # modal variables
  {
    x <- unclass(dataSDA::environment.mix[[j]])
    ncat <-max(sapply(x, function(y)length(y$var)))
    df <- data.frame(matrix (NA, nrow=length(x), ncol=ncat*2))
    for (i in 1:length(x))
    {
      df[i,1:length(x[[i]]$var)] <- x[[i]]$var
      df[i,(1:length(x[[i]]$prop))+ncat] <- x[[i]]$prop/sum(x[[i]]$prop)
    }
    colnames(df)[1] <- var.names[j]
    df.list[[j]] <- df
  }

  mat <- NULL
  for (j in 5:p)
  {
    dat <- sapply(dataSDA::environment.mix[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }

  colnames(mat) <- c(paste0 (rep(var.names[5:p], each=2), c("","1")))
  mat <- data.frame (df.list[[1]], df.list[[2]], df.list[[3]], df.list[[4]], mat)
  rownames(mat) <- rownames(dataSDA::environment.mix)
  nn <- sapply (df.list, function(x)ncol(x)/2)
  create_ddobj (mat,
                types = c("modal","modal","modal","modal", rep("interval",p-4)),
                cols = c(1,cumsum(nn*2)+1,sum(nn*2)+(2:(p-4))*2-1),
                n.cat = nn)
}

#' Converts the exchange_rate_returns.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' returns.data <- get.exchange_rate_returns.hist()
#'
get.exchange_rate_returns.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::exchange_rate_returns.hist)
  var.names <- colnames(dataSDA::exchange_rate_returns.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::exchange_rate_returns.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::exchange_rate_returns.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the face.iGAP data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' face.data <- get.face.iGAP()
#'
get.face.iGAP <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::face.iGAP)
  var.names <- colnames(dataSDA::face.iGAP)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::face.iGAP[[j]], function(x)
    { complex <- x

      lbound <- substring(complex,2,match(",",substring (complex,1:nchar(complex),1:nchar(complex)))-1)
      ubound <- substring(complex,match(",",substring (complex,1:nchar(complex),1:nchar(complex)))+1,nchar(complex))
      cbind (as.numeric(lbound),as.numeric(ubound))
    })
    mat <- cbind (mat, t(dat))
  }
  rownames(mat) <- rownames(dataSDA::face.iGAP)
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the finance.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' fin.data <- get.finance.int()
#'
get.finance.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::finance.int)-1
  var.names <- colnames(dataSDA::finance.int)[-1]
  mat <- NULL
  for (j in (1:p)+1)
  {
    dat <- sapply(dataSDA::finance.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  rownames(mat) <- dataSDA::finance.int[[1]]
  create_ddobj (mat,
                types = c(rep("interval",p-1),"numeric"),
                cols = (1:p*2-1))
}

#' Converts the flights_detail.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' flights.data <- get.flights_detail.hist()
#'
get.flights_detail.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::flights_detail.hist)
  var.names <- colnames(dataSDA::flights_detail.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::flights_detail.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::flights_detail.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the french_agriculture.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' agri.data <- get.french_agriculture.hist()
#'
get.french_agriculture.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::french_agriculture.hist)
  var.names <- colnames(dataSDA::french_agriculture.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::french_agriculture.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::french_agriculture.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the freshwater_fish.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' fish.data <- get.freshwater_fish.int()
#'
get.freshwater_fish.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::freshwater_fish.int)
  var.names <- colnames(dataSDA::freshwater_fish.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::freshwater_fish.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (mat, sapply(dataSDA::freshwater_fish.int[[p]], function(x) x[1]))

  colnames(mat) <- c(paste0 (rep(var.names[1:(p-1)], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval",p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the fuel_consumption.modal data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' fuel.data <- get.fuel_consumption.modal()
#'
get.fuel_consumption.modal <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::fuel_consumption.modal)
  var.names <- colnames(dataSDA::fuel_consumption.modal)
  mat <- dataSDA::fuel_consumption.modal[,1:2]
  colnames(mat) <- var.names[1:2]

  n.int <- NULL
  dat <- dataSDAmodal_to_cols(hist_extract(dataSDA::fuel_consumption.modal[[3]]))
  colnames(dat) <- c(var.names[3], 2:ncol(dat))
  mat <- cbind (mat, dat)
  n.cat <- ncol(dat)/2

  rownames(mat) <- rownames(dataSDA::fuel_consumption.modal)
  create_ddobj (mat,
                types = c("categorical","numeric","modal"),
                cols = 1:3,
                n.cat = n.cat)
}

#' Converts the fungi.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' fungi.data <- get.fungi.int()
#'
get.fungi.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::fungi.int)
  var.names <- colnames(dataSDA::fungi.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::fungi.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- data.frame (mat, sapply(dataSDA::fungi.int[[p]], function(x) x[1]))

  colnames(mat) <- c(paste0 (rep(var.names[1:(p-1)], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval",p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the genome_abundances.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' genome.data <- get.genome_abundances.int()
#'
get.genome_abundances.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::genome_abundances.int)
  var.names <- colnames(dataSDA::genome_abundances.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::genome_abundances.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- data.frame (mat, sapply(dataSDA::genome_abundances.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:(p-1)], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval",p-1),"numeric"),
                cols = (1:p)*2-1)
}

#' Converts the glucose.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' glucose.data <- get.glucose.hist()
#'
get.glucose.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::glucose.hist)
  var.names <- colnames(dataSDA::glucose.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::glucose.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::glucose.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the hardwood.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' hardwood.data <- get.hardwood.hist()
#'
get.hardwood.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::hardwood.hist)
  var.names <- colnames(dataSDA::hardwood.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::hardwood.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  mat[3,4] <- 24.4
  rownames(mat) <- rownames(dataSDA::hardwood.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the hdi_gender.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' HDIgender.data <- get.hdi_gender.int()
#'
get.hdi_gender.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::hdi_gender.int)
  var.names <- colnames(dataSDA::hdi_gender.int)
  mat <- NULL
  for (j in 4:(p-1))
  {
    dat <- sapply(dataSDA::hdi_gender.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- data.frame (sapply(dataSDA::hdi_gender.int[[1]], function(x) x[1]),
                     sapply(dataSDA::hdi_gender.int[[2]], function(x) x[1]),
                     sapply(dataSDA::hdi_gender.int[[3]], function(x) x[1]),
                     mat,
                     sapply(dataSDA::hdi_gender.int[[p]], function(x) x[1]))
  colnames(mat) <- c(var.names[1:3], paste0 (rep(var.names[4:(p-1)], each=2), c("","1")), var.names[p])
  rownames(mat) <- rownames(dataSDA::hdi_gender.int)
  create_ddobj (mat,
                types = c("categorical","categorical","numeric",rep("interval",2),"categorical"),
                cols = c(1:4,6,8))
}

#' Converts the hematocrit.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' hematocrit.data <- get.hematocrit.hist()
#'
get.hematocrit.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::hematocrit.hist)
  var.names <- colnames(dataSDA::hematocrit.hist)
  mat <- dataSDA::cholesterol.hist[,1:2]
  colnames(mat) <- var.names[1:2]

  n.int <- NULL
  dat <- dataSDAhist_to_cols(hist_extract(dataSDA::hematocrit.hist[[3]]))
  colnames(dat) <- c(var.names[3], 2:ncol(dat))
  mat <- cbind (mat, dat)
  n.int <- c(n.int, (ncol(dat)-1)/2)

  rownames(mat) <- rownames(dataSDA::hematocrit.hist)
  create_ddobj (mat,
                types = c(rep("categorical",2),"histogram"),
                cols = 1:3,
                n.int = n.int)
}

#' Converts the hematocrit_hemoglobin.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' hematocrit_hemoglobin.data <- get.hematocrit_hemoglobin.hist()
#'
get.hematocrit_hemoglobin.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::hematocrit_hemoglobin.hist)
  var.names <- colnames(dataSDA::hematocrit_hemoglobin.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::hematocrit_hemoglobin.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::hematocrit_hemoglobin.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the hierarchy.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' hierarchy.data <- get.hierarchy.hist()
#'
get.hierarchy.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::hierarchy.hist)
  var.names <- colnames(dataSDA::hierarchy.hist)
  mat <- n.int <- NULL

  dat <- dataSDAhist_to_cols(hist_extract(dataSDA::hierarchy.hist[[1]]))
  colnames(dat) <- c(var.names[1], 2:ncol(dat))
  mat <- cbind (mat, dat)
  n.int <- c(n.int, (ncol(dat)-1)/2)
  cols <- c(1,ncol(mat)+1)

  mat <- data.frame (mat,
                     sapply(dataSDA::hierarchy.hist[[2]], function(x) x[1]),
                     sapply(dataSDA::hierarchy.hist[[3]], function(x) x[1]),
                     sapply(dataSDA::hierarchy.hist[[4]], function(x) x[1]))
  colnames(mat)[cols[2]+(0:2)] <- var.names[2:4]
  cols <- c(cols, max(cols)+1:3)

  dat <- dataSDAhist_to_cols(hist_extract(dataSDA::hierarchy.hist[[5]]))
  colnames(dat) <- c(var.names[5], 2:ncol(dat))
  mat <- cbind (mat, dat)
  n.int <- c(n.int, (ncol(dat)-1)/2)
  cols <- c(cols,ncol(mat)+1)

  dat <- dataSDAhist_to_cols(hist_extract(dataSDA::hierarchy.hist[[6]]))
  colnames(dat) <- c(var.names[6], 2:ncol(dat))
  mat <- cbind (mat, dat)
  n.int <- c(n.int, (ncol(dat)-1)/2)
  cols <- c(cols,ncol(mat)+1)

  dat <- sapply(dataSDA::hierarchy.hist[[7]], function(x)
  { complex <- x[1]
    cbind (Re(complex),Im(complex))
  })
  mat <- cbind (mat, t(dat))
  colnames(mat)[cols[length(cols)]] <- var.names[7]

  rownames(mat) <- rownames(dataSDA::hierarchy.hist)
  create_ddobj (mat,
                types = c("histogram",rep("categorical",3),rep("histogram",2),"interval"),
               n.int = n.int, cols = cols)
}

#' Converts the hierarchy.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' hierarchy.data <- get.hierarchy.int()
#'
get.hierarchy.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::hierarchy.int)
  var.names <- colnames(dataSDA::hierarchy.int)
  mat <- NULL
  for (j in c(1,5,6))
  {
    dat <- sapply(dataSDA::hierarchy.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- data.frame (mat[,1:2],
                     sapply(dataSDA::hierarchy.int[[2]], function(x) x[1]),
                     sapply(dataSDA::hierarchy.int[[3]], function(x) x[1]),
                     sapply(dataSDA::hierarchy.int[[4]], function(x) x[1]),
                     mat[,-(1:2)])

  colnames(mat) <- c(var.names[1], paste0(var.names[1],"1"), var.names[2:4],
                     paste0 (rep(var.names[5:6], each=2), c("","1")))
  rownames(mat) <- rownames(dataSDA::hierarchy.int)
  create_ddobj (mat,
                types = c("interval","categorical","categorical","categorical",rep("interval",2)),
                cols = c(1,3:6,8))
}

#' Converts the horses.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' horse.data <- get.horses.int()
#'
get.horses.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::horses.int)-1
  var.names <- colnames(dataSDA::horses.int)[-1]
  mat <- NULL
  for (j in (1:p)+1)
  {
    dat <- sapply(dataSDA::horses.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  rownames(mat) <- dataSDA::horses.int[[1]]
  create_ddobj (mat,
                types = c(rep("interval",p-1),"numeric"),
                cols = (1:p*2-1))
}

#' Converts the hospital.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' hospital.data <- get.hospital.hist()
#'
get.hospital.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::hospital.hist)
  var.names <- colnames(dataSDA::hospital.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::hospital.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::hospital.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the iris.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' iris.data <- get.iris.int()
#'
get.iris.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::iris.int)
  var.names <- colnames(dataSDA::iris.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::iris.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (mat, sapply(dataSDA::iris.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:p-1], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval", p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the iris_species.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' iris.data <- get.iris_species.hist()
#'
get.iris_species.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::iris_species.hist)
  var.names <- colnames(dataSDA::iris_species.hist)

  mat <- data.frame(dataSDA::iris_species.hist[[1]])
  colnames(mat) <- var.names[1]

  n.int <- NULL
  for (j in 2:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::iris_species.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::iris_species.hist)
  create_ddobj (mat,
                types = c("categorical",rep("histogram",p-1)),
                cols = c(1,2+c(0, cumsum(n.int*2+1)[-length(n.int)])),
                n.int = n.int)
}

#' Converts the joggers.mix data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' joggers.data <- get.joggers.mix()
#'
get.joggers.mix <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::joggers.mix)
  var.names <- colnames(dataSDA::joggers.mix)
  mat <- NULL
  for (j in 1:1)
  {
    dat <- sapply(dataSDA::joggers.mix[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:1], each=2), c("","1")))

  hist.obj <- dataSDAhist_to_cols(hist_extract(dataSDA::joggers.mix[[2]]))
  colnames(hist.obj) <- c(var.names[2], 2:ncol(hist.obj))
  mat <- cbind (mat, hist.obj)
  create_ddobj (mat,
                types = c("interval","histogram"),
                cols = c(1,3),
                n.int = (ncol(hist.obj)-1)/2)
}

#' Converts the judge1.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' judge.data <- get.judge1.int()
#'
get.judge1.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::judge1.int)
  var.names <- colnames(dataSDA::judge1.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::judge1.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the judge2.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' judge.data <- get.judge2.int()
#'
get.judge2.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::judge2.int)
  var.names <- colnames(dataSDA::judge2.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::judge2.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the judge3.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' judge.data <- get.judge3.int()
#'
get.judge3.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::judge3.int)
  var.names <- colnames(dataSDA::judge3.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::judge3.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the lackinfo.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' lackinfo.data <- get.lackinfo.int()
#'
get.lackinfo.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::lackinfo.int)-2
  var.names <- colnames(dataSDA::lackinfo.int)[-(1:2)]
  mat <- NULL
  for (j in (1:p)+2)
  {
    dat <- sapply(dataSDA::lackinfo.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  rownames(mat) <- dataSDA::lackinfo.int[[1]]
  mat <- data.frame (sex=dataSDA::lackinfo.int$sex, mat)
  create_ddobj (mat,
                types = c("categorical","numeric", rep("interval",p-1)),
                cols = c(1,(1:p)*2))
}

#' Converts the lisbon_air_quality.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' Lisbon.data <- get.lisbon_air_quality.int()
#'
get.lisbon_air_quality.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::lisbon_air_quality.int)
  var.names <- colnames(dataSDA::lisbon_air_quality.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::lisbon_air_quality.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the loans_by_purpose.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' loans.data <- get.loans_by_purpose.int()
#'
get.loans_by_purpose.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::loans_by_purpose.int)
  var.names <- colnames(dataSDA::loans_by_purpose.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::loans_by_purpose.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the loans_by_risk.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' loans.data <- get.loans_by_risk.int()
#'
get.loans_by_risk.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::loans_by_risk.int)
  var.names <- colnames(dataSDA::loans_by_risk.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::loans_by_risk.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (mat, sapply(dataSDA::loans_by_risk.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:p-1], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval", p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the loans_by_risk_quantile.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' loans.data <- get.loans_by_risk_quantile.int()
#'
get.loans_by_risk_quantile.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::loans_by_risk_quantile.int)
  var.names <- colnames(dataSDA::loans_by_risk_quantile.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::loans_by_risk_quantile.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the lung_cancer.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' lung_cancer.data <- get.lung_cancer.hist()
#'
get.lung_cancer.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::lung_cancer.hist)
  var.names <- colnames(dataSDA::lung_cancer.hist)

  mat <- data.frame(dataSDA::lung_cancer.hist[[1]])
  colnames(mat) <- var.names[1]

  dat <- dataSDAhist_to_cols(hist_extract(dataSDA::lung_cancer.hist[[2]]))
  colnames(dat) <- c(var.names[2], 2:ncol(dat))
  mat <- cbind (mat, dat)
  n.int <- (ncol(dat)-1)/2

  rownames(mat) <- rownames(dataSDA::lung_cancer.hist)
  create_ddobj (mat,
                types = c("categorical","histogram"),
                cols = 1:2,
                n.int = n.int)
}

#' Converts the lynne1.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' Lynne.data <- get.lynne1.int()
#'
get.lynne1.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::lynne1.int)
  var.names <- colnames(dataSDA::lynne1.int)
  mat <- NULL
  for (j in 2:p)
  {
    dat <- sapply(dataSDA::lynne1.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (sapply(dataSDA::lynne1.int[[1]], function(x) x[1]), mat)
  colnames(mat) <- c(var.names[1], paste0 (rep(var.names[2:p], each=2), c("","1")))
  create_ddobj (mat,
                types = c("categorical",rep("interval", p-1)),
                cols = c(1,(1:(p-1))*2))
}

#' Converts the mtcars.mix data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' mtcars.data <- get.mtars.mix()
#'
get.mtcars.mix <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::mtcars.mix)
  var.names <- colnames(dataSDA::mtcars.mix)
  df.list <- vector ("list", 4)

  for (j in c(2,8:11))   # modal variables
  {
    x <- unclass(dataSDA::mtcars.mix[[j]])
    ncat <-max(sapply(x, function(y)length(y$var)))
    df <- data.frame(matrix (NA, nrow=length(x), ncol=ncat*2))
    for (i in 1:length(x))
    {
      df[i,1:length(x[[i]]$var)] <- x[[i]]$var
      df[i,(1:length(x[[i]]$prop))+ncat] <- x[[i]]$prop
    }
    colnames(df)[1] <- var.names[j]
    df.list[[j]] <- df
  }

  mat <- NULL
  for (j in c(1,3:7))
  {
    dat <- sapply(dataSDA::mtcars.mix[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }

  colnames(mat) <- c(paste0 (rep(var.names[c(1,3:7)], each=2), c("","1")))
  mat <- data.frame (df.list[[2]], mat, df.list[[8]], df.list[[9]], df.list[[10]], df.list[[11]])
  nn <- sapply (df.list, function(x)ncol(x)/2)
  nn <- unlist(nn[c(2,8:11)])
  create_ddobj (mat,
                types = c("modal", rep("interval",6), "modal","modal","modal", "modal"),
                cols = c(1, 2*nn[1]+(1:6)*2-1, 2*nn[1]+13, 2*nn[1]+13+cumsum(2*nn[-c(1,length(nn))])),
                n.cat = nn)
}

#' Converts the mushroom.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' mushroom.data <- get.mushroom.int()
#'
get.mushroom.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::mushroom.int.mm[,-1]
  colnames (df) <- gsub ("_min", "", colnames (df))
  rownames(df) <- dataSDA::mushroom.int.mm[,1]
  create_ddobj (df,
                types = c(rep("interval", 3), "categorical"),
                cols = c(1, 3, 5, 7))
}

#' Converts the mushroom_fuzzy.mix data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' mushroom.data <- get.mushroom_fuzzy.mix()
#'
get.mushroom_fuzzy.mix <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  n <- nrow(dataSDA::mushroom_fuzzy.mix)
  p <- ncol(dataSDA::mushroom_fuzzy.mix)
  var.names <- colnames(dataSDA::mushroom_fuzzy.mix)
  mat <- data.frame (specimen = dataSDA::mushroom_fuzzy.mix[[1]],
                     species = dataSDA::mushroom_fuzzy.mix[[2]],
                     stripe_thickness = dataSDA::mushroom_fuzzy.mix[[3]],
                     fuzzy_stripe_thickness = rep("small", n),
                     fuzzy2 = rep ("average", n),
                     fuzzy3 = rep ("large", n),
                     fuzzy4 = dataSDA::mushroom_fuzzy.mix[[4]],
                     fuzzy5 = dataSDA::mushroom_fuzzy.mix[[5]],
                     fuzzy6 = dataSDA::mushroom_fuzzy.mix[[6]],
                     strip_length = dataSDA::mushroom_fuzzy.mix[[7]],
                     cap_colour = dataSDA::mushroom_fuzzy.mix[[9]])

    dat <- dataSDA::mushroom_fuzzy.mix[[8]]
    split_dat <- strsplit(dat, " \\+/- ")
    matrix_data <- matrix(as.numeric(unlist(split_dat)), ncol = 2, byrow = TRUE)
    lower <- matrix_data[, 1] - matrix_data[, 2]
    upper  <- matrix_data[, 1] + matrix_data[, 2]
    mat <- data.frame (mat, cap_size = lower, upper)

  create_ddobj (mat,
                types = c("categorical","categorical","numeric","modal","numeric","categorical","interval"),
                cols = c(1:4,10:12), n.cat = 3)
}

#' Converts the nycflights.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' nycflights.data <- get.nycflights.int()
#'
get.nycflights.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::nycflights.int)
  var.names <- colnames(dataSDA::nycflights.int)
  mat <- NULL
  for (j in 2:p)
  {
    dat <- sapply(dataSDA::nycflights.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (sapply(dataSDA::nycflights.int[[1]], function(x) x[1]), mat)
  colnames(mat) <- c(var.names[1], paste0 (rep(var.names[2:p], each=2), c("","1")))
  create_ddobj (mat,
                types = c("categorical", rep("interval", p-1)),
                cols = c(1,(1:(p-1))*2))
}

#' Converts the occupations.modal data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' occupations.data <- get.occupations.modal()
#'
get.occupations.modal <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::occupations.modal
  p <- ncol(dataSDA::occupations.modal)
  n <- nrow(dataSDA::occupations.modal)
  var.names <- colnames(dataSDA::occupations.modal)

  mat <- data.frame(dataSDA::occupations.modal[,1])
  colnames(mat) <- var.names[1]

  mat <- cbind (mat, Gender = rep("M",n),
                Gender2 = rep("F",n))
  mat <- cbind (mat, df[,2:3])

  mat <- cbind (mat, Salary = rep("1",n),
                Salary2 = rep("2", n),
                Salary3 = rep("3", n),
                Salary4 = rep("4", n),
                Salary5 = rep("5", n),
                Salary6 = rep("6", n),
                Salary7 = rep("7", n))
  propmat <- df[,4:10]
  propmat <- t(apply (propmat, 1, function(x)x/sum(x)))
  mat <- cbind (mat, propmat)

  mat <- cbind (mat, df[,11])
  colnames(mat)[ncol(mat)] <- var.names[11]

  rownames (mat) <- rownames(df)
  create_ddobj (mat,
                types = c("categorical","modal","modal", "numeric"),
                cols = c(1, 2, 6, ncol(mat)),
                n.cat = c(2,7))
}

#' Converts the ohtemp.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' ohtemp.data <- get.ohtemp.int()
#'
get.ohtemp.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  n <- nrow(dataSDA::ohtemp.int)
  p <- ncol(dataSDA::ohtemp.int)
  var.names <- colnames(dataSDA::ohtemp.int)
  mat <- data.frame (ID = dataSDA::ohtemp.int[[1]],
                     NAME = dataSDA::ohtemp.int[[2]],
                     STATE = dataSDA::ohtemp.int[[3]],
                     LONGITUDE = dataSDA::ohtemp.int[[4]],
                     LATITUDE = dataSDA::ohtemp.int[[5]],
                     ELEVATION = dataSDA::ohtemp.int[[6]])

  dat <- sapply(dataSDA::ohtemp.int[[7]], function(x)
  { complex <- x[1]
    cbind (Re(complex),Im(complex))
  })
  mat <- cbind (mat, t(dat))
  colnames(mat)[7:8] <- c("TEMPERATURE","1")

  create_ddobj (mat,
                types = c(rep("categorical",3),rep("numeric",3), "interval"),
                cols = 1:7)
}

#' Converts the oils.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' oils.data <- get.oils.int()
#'
get.oils.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::oils.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  create_ddobj (df,
                types = c("categorical", rep("interval", 4)),
                cols = c(1, 2, 4, 6, 8))
}

#' Converts the ozone.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' ozone.data <- get.ozone.hist()
#'
get.ozone.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::ozone.hist)
  var.names <- colnames(dataSDA::ozone.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::ozone.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::ozone.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the polish_cars.mix data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' cars.data <- get.polish_cars.mix()
#'
get.polish_cars.mix <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::polish_cars.mix)
  var.names <- colnames(dataSDA::polish_cars.mix)
  mat <- NULL
  for (j in c(1,3:6,8:10,12))
  {
    dat <- sapply(dataSDA::polish_cars.mix[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names[c(1,3:6,8:10,12)], each=2), c("","1"))

  mat <- cbind (mat[,1:2], body = dataSDA::polish_cars.mix[[2]],
                mat[,3:10], engine_capacity = dataSDA::polish_cars.mix[[7]],
                mat[,11:16], fuel_type = dataSDA::polish_cars.mix[[11]],
                mat[,17:18])

  create_ddobj (mat,
                types = c("interval", "categorical", rep("interval", 4),
                          "categorical", rep ("interval",3),
                          "categorical", "interval"),
                cols = c(1,3,4,6,8,10,12,13,15,17,19,20))
}

#' Converts the polish_voivodships.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' Polish.data <- get.polish_voivodships.int()
#'
get.polish_voivodships.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::polish_voivodships.int)
  var.names <- colnames(dataSDA::polish_voivodships.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::polish_voivodships.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the profession.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' profession.data <- get.profession.int()
#'
get.profession.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::profession.int)-2
  var.names <- colnames(dataSDA::profession.int)[-(1:2)]
  mat <- NULL
  for (j in (1:p)+2)
  {
    dat <- sapply(dataSDA::profession.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  df <- data.frame (Type_of_Work = dataSDA::profession.int[,1],
                    Profession = dataSDA::profession.int[,2], mat)
  create_ddobj (df,
                types = c("categorical", "categorical", "interval", "interval"),
                cols = c(1, 2, 3, 5))
}

#' Converts the prostate.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' prostate.data <- get.prostate.int()
#'
get.prostate.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::prostate.int)
  var.names <- colnames(dataSDA::prostate.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::prostate.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the simulated.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' simulated.data <- get.simulated.hist()
#'
get.simulated.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::simulated.hist)
  var.names <- colnames(dataSDA::simulated.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::simulated.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::simulated.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the soccer_bivar.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' soccer.data <- get.soccer_bivar.int()
#'
get.soccer_bivar.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::soccer_bivar.int)
  var.names <- colnames(dataSDA::soccer_bivar.int)
  mat <- NULL
  for (j in (1:p))
  {
    dat <- sapply(dataSDA::soccer_bivar.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  create_ddobj (mat,
                types = rep("interval", p),
                cols = c(1, 3, 5))
}

#' Converts the state_income.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' income.data <- get.state_income.hist()
#'
get.state_income.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::state_income.hist)
  var.names <- colnames(dataSDA::state_income.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::state_income.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::state_income.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the synthetic_clusters.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' cluster.data <- get.synthetic_clusters.int()
#'
get.synthetic_clusters.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::synthetic_clusters.int)
  var.names <- colnames(dataSDA::synthetic_clusters.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::synthetic_clusters.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (mat, sapply(dataSDA::synthetic_clusters.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:p-1], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval", p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the teams.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' teams.data <- get.teams.int()
#'
get.teams.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::teams.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  create_ddobj (df,
                types = c("categorical", rep("interval", 3)),
                cols = c(1, 2, 4, 6))
}

#' Converts the temperature_city.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' temperature.data <- get.temperature_city.int()
#'
get.temperature_city.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::temperature_city.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  create_ddobj (df,
                types = c("categorical", rep("interval", 6)),
                cols = c(1, 2, 4, 6, 8, 10, 12))
}

#' Converts the tennis.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' tennis.data <- get.tennis.int()
#'
get.tennis.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::tennis.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  create_ddobj (df,
                types = c("categorical", rep("interval", 3)),
                cols = c(1, 2, 4, 6))
}

#' Converts the town_services.mix data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' services.data <- get.town_services.mix()
#'
get.town_services.mix <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::town_services.mix[,-4]
  colnames (df) <- gsub ("_l", "", colnames (df))

  dat <- dataSDA::town_services.mix[[4]]
  pos <- regexpr("\\%", dat)
  val1 <- as.numeric(substring (dat, 2, pos-1))/100
  dat  <- substring (dat, pos+2, nchar(dat))
  pos <- regexpr("\\%", dat)
  pos0 <- regexpr("\\(", dat)
  val2 <- ifelse (pos > 0, as.numeric(substring (dat, pos0+1, pos-1))/100, 0)

  df <- data.frame (df, type=rep("Public",nrow(df)), rep("Private",nrow(df)), val1, val2)

  create_ddobj (df,
                types = c("categorical","interval","categorical","interval","categorical","modal"),
                cols = c(1, 2, 4, 5, 7, 8),
                n.cat = 2)
}

#' Converts the trivial_intervals.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' interval.data <- get.trivial_intervals.int()
#'
get.trivial_intervals.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::trivial_intervals.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  create_ddobj (df,
                types = rep("interval", 3),
                cols = c(1, 3, 5))
}

#' Converts the uscrime.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' crime.data <- get.uscrime.int()
#'
get.uscrime.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::uscrime.int)
  var.names <- colnames(dataSDA::uscrime.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::uscrime.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the utsnow.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' snow.data <- get.utsnow.int()
#'
get.utsnow.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::utsnow.int[,-1]

  dat <- sapply(dataSDA::utsnow.int[[1]], function(x)
  { complex <- x[1]
    cbind (Re(complex),Im(complex))
  })
  df <- cbind (t(dat), df)
  colnames(df)[1:2] <- c("snow_load","1")

  create_ddobj (df,
                types = c("interval",rep("numeric",4)),
                cols = c(1, 3:6))
}

#' Converts the veterinary.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' vet.data <- get.veterinary.int()
#'
get.veterinary.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::veterinary.int)-1
  var.names <- colnames(dataSDA::veterinary.int)[-1]
  mat <- NULL
  for (j in (1:p)+1)
  {
    dat <- sapply(dataSDA::veterinary.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  df <- data.frame (Sex = substring(dataSDA::veterinary.int[[1]],
                                    nchar(dataSDA::veterinary.int[[1]]),
                                    nchar(dataSDA::veterinary.int[[1]])), mat)
  rownames(df) <- dataSDA::veterinary.int[[1]]
  create_ddobj (df,
                types = c("categorical","interval","interval"),
                cols = c(1, 2, 4))
}

#' Converts the video1.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' video.data <- get.video1.int()
#'
get.video1.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::video1.int)
  var.names <- colnames(dataSDA::video1.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::video1.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the video2.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' video.data <- get.video2.int()
#'
get.video2.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::video2.int)
  var.names <- colnames(dataSDA::video2.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::video2.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the video3.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' video.data <- get.video3.int()
#'
get.video3.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::video3.int)
  var.names <- colnames(dataSDA::video3.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::video3.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- c(paste0 (rep(var.names[1:p], each=2), c("","1")))
  create_ddobj (mat,
                types = rep("interval",p),
                cols = (1:p)*2-1)
}

#' Converts the water_flow.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' flow.data <- get.water_flow.int()
#'
get.water_flow.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::water_flow.int)
  var.names <- colnames(dataSDA::water_flow.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::water_flow.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (mat, sapply(dataSDA::water_flow.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:p-1], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval", p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the weight_age.hist data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' weight_age.data <- get.weight_age.hist()
#'
get.weight_age.hist <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  p <- ncol(dataSDA::weight_age.hist)
  var.names <- colnames(dataSDA::weight_age.hist)
  mat <- n.int <- NULL
  for (j in 1:p)
  {
    dat <- dataSDAhist_to_cols(hist_extract(dataSDA::weight_age.hist[[j]]))
    colnames(dat) <- c(var.names[j], 2:ncol(dat))
    mat <- cbind (mat, dat)
    n.int <- c(n.int, (ncol(dat)-1)/2)
  }
  rownames(mat) <- rownames(dataSDA::weight_age.hist)
  create_ddobj (mat,
                types = rep("histogram",p),
                cols = 1+c(0, cumsum(n.int*2+1)[-length(n.int)]),
                n.int = n.int)
}

#' Converts the wine.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' wine.data <- get.wine.int()
#'
get.wine.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::wine.int)
  var.names <- colnames(dataSDA::wine.int)
  mat <- NULL
  for (j in 1:(p-1))
  {
    dat <- sapply(dataSDA::wine.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  mat <- cbind (mat, sapply(dataSDA::wine.int[[p]], function(x) x[1]))
  colnames(mat) <- c(paste0 (rep(var.names[1:p-1], each=2), c("","1")), var.names[p])
  create_ddobj (mat,
                types = c(rep("interval", p-1),"categorical"),
                cols = (1:p)*2-1)
}

#' Converts the world_cup.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' world_cup.data <- get.world_cup.int()
#'
get.world_cup.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::world_cup.int
  colnames (df) <- gsub ("_l", "", colnames (df))
  create_ddobj (df,
                types = c("categorical", rep("interval", 3), "numeric"),
                cols = c(1, 2, 4, 6, 8))
}

### ===================================================================
### Data sets from HistDAWass

#' Converts a matH object (package HistDAWass) to a data.frame
#'
#' @param obj matH obj
#'
#' @returns A list with the following components is available:
#' \item{df}{the data.frame constructed from the matH object.}
#' \item{cols}{a vector with the first column number for each variable.}
#' \item{n.int}{a vector with the number of intervals for each variable.}
#'
matH.to.data.frame <- function (obj)
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.", call. = FALSE)
  }

  info <- HistDAWass::get.MatH.main.info(obj)
  df <- NULL
  cols <- NULL
  n.int <- NULL

  for (j in 1:info$ncols)
    {
      temp <- vector ("list", info$nrows)
      for (i in 1:info$nrows)
        temp[[i]] <- HistDAWass::get.cell.MatH (obj, i, j)
      m <- max(sapply (temp, function(obs) length(obs@x)))-1
      mat <- matrix (nrow=info$nrows, ncol=2*m+1)
      colnames(mat) <- c("X", paste0("I",1:m), paste0("p",1:m))
      for (i in 1:info$nrows)
        {
          mat[i,1:length(temp[[i]]@x)] <- temp[[i]]@x
          mat[i,m+1+1:(length(temp[[i]]@x)-1)] <- diff(temp[[i]]@p)
        }

      cols <- c(cols, ifelse (is.null(cols), 1, ncol(df)+1))
      n.int <- c(n.int, m)
      df <- cbind (df, mat)
  }
  colnames(df)[cols] <- info$varnames
  rownames(df) <- info$rownames
  list (df=as.data.frame(df), cols=cols, n.int=n.int)
}

#' Converts the Age_Pyramids_2014 data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{HistDAWass} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' pyramid.data <- get.Age_Pyramids_2014()
#'
get.Age_Pyramids_2014 <- function ()
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.", call. = FALSE)
  }

  out <- matH.to.data.frame(HistDAWass::Age_Pyramids_2014)
  create_ddobj (out$df,
                types = c(rep("histogram", length(out$cols))),
                cols = out$cols, n.int = out$n.int)
}

#' Converts the Agronomique data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{HistDAWass} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' pyramid.data <- get.Agronomique()
#'
get.Agronomique <- function ()
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.", call. = FALSE)
  }

  out <- matH.to.data.frame(HistDAWass::Agronomique)
  create_ddobj (out$df,
                types = c(rep("histogram", length(out$cols))),
                cols = out$cols, n.int = out$n.int)
}

#' Converts the BLOOD data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{HistDAWass} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' blood.data <- get.BLOOD()
#'
get.BLOOD <- function ()
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.", call. = FALSE)
  }

  out <- matH.to.data.frame(HistDAWass::BLOOD)
  create_ddobj (out$df,
                types = c(rep("histogram", length(out$cols))),
                cols = out$cols, n.int = out$n.int)
}

#' Converts the BloodBRITO data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{HistDAWass} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' blood.data <- get.BloodBRITO()
#'
get.BloodBRITO <- function ()
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.", call. = FALSE)
  }

  out <- matH.to.data.frame(HistDAWass::BloodBRITO)
  create_ddobj (out$df,
                types = c(rep("histogram", length(out$cols))),
                cols = out$cols, n.int = out$n.int)
}

#' Converts the China_Month data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{HistDAWass} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' blood.data <- get.China_Month()
#'
get.China_Month <- function ()
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.", call. = FALSE)
  }

  out <- matH.to.data.frame(HistDAWass::China_Month)
  create_ddobj (out$df,
                types = c(rep("histogram", length(out$cols))),
                cols = out$cols, n.int = out$n.int)
}

#' Converts the China_Seas data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{HistDAWass} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' blood.data <- get.China_Seas()
#'
get.China_Seas <- function ()
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.", call. = FALSE)
  }

  out <- matH.to.data.frame(HistDAWass::China_Seas)
  create_ddobj (out$df,
                types = c(rep("histogram", length(out$cols))),
                cols = out$cols, n.int = out$n.int)
}

#' Converts the OzoneFull data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{HistDAWass} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' blood.data <- get.OzoneFull()
#'
get.OzoneFull <- function ()
{
  if (!requireNamespace("HistDAWass", quietly = TRUE)) {
    stop("Package 'HistDAWass' is required for this function. Please install it.", call. = FALSE)
  }

  out <- matH.to.data.frame(HistDAWass::OzoneFull)
  create_ddobj (out$df,
                types = c(rep("histogram", length(out$cols))),
                cols = out$cols, n.int = out$n.int)
}



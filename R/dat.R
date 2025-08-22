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
#'                reverse, the function \code{create.ddobj} will switch the endpoints}
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

sample.names <- c("Linseed", "Perilla", "Cotton", "Sesame", "Cmellia", "Olive", "Beef", "Hog")
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
#' A data set containing 8 entities with four interval scaled variables and one numeric variable.
#'
#' @format An object of class \code{ddobj}. A list of length 5:
#' \describe{
#'   \item{Spec.gravity}{interval scaled variable}
#'   \item{Freezing.point}{interval scaled variable}
#'   \item{Iodine.value}{interval scaled variable}
#'   \item{Saponification}{interval scaled variable}
#'   \item{SO.LDP}{numeric variable}
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
                   SO.LDP = list (type = "numeric",
                                  values = SO.LDP))
class(Oils.data) <- "ddobj"


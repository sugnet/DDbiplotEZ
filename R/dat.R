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

#tmp <- new.env()
#load("G:\\My Drive\\My Documents\\Navorsing\\Projekte\\Symbolic Data Analysis\\credicard_dataset.RDATA", envir = tmp)
#tmpdata <- tmp$CreditCard_symbDF
#colnames (tmpdata) <- gsub (" min", "", colnames (tmpdata))
#Creditcard.data <- create.ddobj (tmpdata,
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
  create.ddobj (df,
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
  create.ddobj (df,
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
  create.ddobj (mat,
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
  create.ddobj (df,
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
  create.ddobj (df,
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
  create.ddobj (df,
                types = rep("interval", 4),
                cols = c(1, 3, 5, 7))
}

### ===================================================================
### Data sets from dataSDA

#' Converts the Abalone data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' Abalone.data <- get.Abalone()
#'
get.Abalone <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::Abalone
  colnames (df) <- gsub ("_min", "", colnames (df))
  create.ddobj (df,
                types = rep("interval", 7),
                cols = c(1, 3, 5, 7, 9, 11, 13))
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
#' age.chol.wt.data <- get.age_cholosterol_weight()
#'
get.age_cholesterol_weight.int <- function ()
{
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
  create.ddobj (mat,
                types = c("interval","interval","interval","numeric"),
                cols = c(1, 3, 5, 7))
}

#' Converts the airline_flights data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' flights.data <- get.airline_flights()
#'
get.airline_flights <- function ()
{
#  [1] "Flight Time(<120)"        "Flight Time([120, 220])"  "Flight Time(>220)"        "Taxi In(<4)"
#  [5] "Taxi In([4, 10])"         "Taxi In(>10)"             "Arrival Delay(<0)"        "Arrival Delay([0, 60])"
#  [9] "Arrival Delay(>60)"       "Taxi Out(<16)"            "Taxi Out([16, 30])"       "Taxi Out(>30)"
#  [13] "Departure Delay(<0)"      "Departure Delay([0, 60])" "Departure Delay(>60)"     "Weather Delay(No)"
#  [17] "Weather Delay(Yes)"

  Flight.Time <- cbind (Flight.Time=0, FT1=120, FT2=220, FT3=220+120,
                        FT4 = dataSDA::airline_flights$"Flight Time(<120)",
                        FT5 = dataSDA::airline_flights$"Flight Time([120, 220])",
                        FT6 = dataSDA::airline_flights$"Flight Time(>220)")
  Taxi.In <- cbind (Taxi.In=0, TI1=4, TI2=10, TI3=20,
                    TI4 = dataSDA::airline_flights$"Taxi In(<4)",
                    TI5 = dataSDA::airline_flights$"Taxi In([4, 10])",
                    TI6 = dataSDA::airline_flights$"Taxi In(>10)")
  Arrival.Delay <- cbind (Arrival.Delay=-60, AD1=0, AD2=60, AD3=120,
                    AD4 = dataSDA::airline_flights$"Arrival Delay(<0)",
                    AD5 = dataSDA::airline_flights$"Arrival Delay([0, 60])",
                    AD6 = dataSDA::airline_flights$"Arrival Delay(>60)")
  Taxi.Out <- cbind (Taxi.Out=0, TO1=16, TO2=30, TO3=60,
                    TO4 = dataSDA::airline_flights$"Taxi Out(<16)",
                    TO5 = dataSDA::airline_flights$"Taxi Out([16, 30])",
                    TO6 = dataSDA::airline_flights$"Taxi Out(>30)")
  Departure.Delay <- cbind (Departure.Delay=-60, DD1=0, DD2=60, DD3=120,
                          DD4 = dataSDA::airline_flights$"Departure Delay(<0)",
                          DD5 = dataSDA::airline_flights$"Departure Delay([0, 60])",
                          DD6 = dataSDA::airline_flights$"Departure Delay(>60)")
  Weather.Delay <- data.frame (Weather.Delay="Yes", WD1="No",
                            WD3 = dataSDA::airline_flights$"Weather Delay(Yes)",
                            WD4 = dataSDA::airline_flights$"Weather Delay(No)")
  df <- data.frame (Flight.Time, Taxi.In, Arrival.Delay, Taxi.Out, Departure.Delay, Weather.Delay)
  create.ddobj (df,
                types = c(rep("histogram", 5), "modal"),
                cols = c(1,8,15,22,29,36),
                n.int = rep(3,5),2)
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
  create.ddobj (mat,
                types = c("interval","interval","categorical"),
                cols = c(1, 3, 5))
}

#' Converts the bird.int data set to a ddobj
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
#' bird.data <- get.bird.int()
#'
get.bird.int <- function ()
{
  p <- ncol(dataSDA::bird.int)
  var.names <- colnames(dataSDA::bird.int)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::bird.int[[j]], function(x)
    { complex <- x[1]
      cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  create.ddobj (mat,
                types = c("interval","interval"),
                cols = c(1, 3))
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
#'
#' @examples
#' bp.data <- get.blood_pressure.int()
#'
get.blood_pressure.int <- function ()
{
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
  create.ddobj (mat,
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
#'
#' @examples
#' car.data <- get.car.int()
#'
get.car.int <- function ()
{
  p <- ncol(dataSDA::car.int)-1
  var.names <- colnames(dataSDA::car.int)[-1]
  mat <- NULL
  for (j in (1:p)+1)
  {
    dat <- sapply(dataSDA::car.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  rownames(mat) <- dataSDA::car.int[[1]]
  create.ddobj (mat,
                types = rep("interval",p),
                cols = (1:p*2-1))
}

#' Converts the Face.iGAP data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' face.data <- get.Face.iGAP()
#'
get.Face.iGAP <- function ()
{
  p <- ncol(dataSDA::Face.iGAP)
  var.names <- colnames(dataSDA::Face.iGAP)
  mat <- NULL
  for (j in 1:p)
  {
    dat <- sapply(dataSDA::Face.iGAP[[j]], function(x)
    { complex <- x

      lbound <- substring(complex,2,match(",",substring (complex,1:nchar(complex),1:nchar(complex)))-1)
      ubound <- substring(complex,match(",",substring (complex,1:nchar(complex),1:nchar(complex)))+1,nchar(complex))
      cbind (as.numeric(lbound),as.numeric(ubound))
    })
    mat <- cbind (mat, t(dat))
  }
  rownames(mat) <- rownames(dataSDA::Face.iGAP)
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  create.ddobj (mat,
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
  create.ddobj (mat,
                types = c(rep("interval",p-1),"numeric"),
                cols = (1:p*2-1))
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
  create.ddobj (mat,
                types = c(rep("interval",p-1),"numeric"),
                cols = (1:p*2-1))
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
  create.ddobj (mat,
                types = c("categorical","numeric", rep("interval",p-1)),
                cols = c(1,(1:p)*2))
}

#' Converts the mushroom data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' mushroom.data <- get.mushroom()
#'
get.mushroom <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }
  df <- dataSDA::mushroom[,-1]
  colnames (df) <- gsub ("_min", "", colnames (df))
  rownames(df) <- dataSDA::mushroom[,1]
  create.ddobj (df,
                types = c(rep("interval", 3), "categorical"),
                cols = c(1, 3, 5, 7))
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
  create.ddobj (df,
                types = c("categorical", "categorical", "interval", "interval"),
                cols = c(1, 2, 3, 5))
}

#' Converts the soccer.bivar.int data set to a ddobj
#'
#' @returns an object of class \code{ddobj}
#' @export
#'
#' @description
#' Obtain the data set from the package \code{dataSDA} and convert is into a \code{ddobj}
#' for use in \code{ddbiplot()}.
#'
#' @examples
#' soccer.data <- get.soccer.bivar.int()
#'
get.soccer.bivar.int <- function ()
{
  if (!requireNamespace("dataSDA", quietly = TRUE)) {
    stop("Package 'dataSDA' is required for this function. Please install it.", call. = FALSE)
  }

  p <- ncol(dataSDA::soccer.bivar.int)
  var.names <- colnames(dataSDA::soccer.bivar.int)
  mat <- NULL
  for (j in (1:p))
  {
    dat <- sapply(dataSDA::soccer.bivar.int[[j]], function(x)
    { complex <- x[1]
    cbind (Re(complex),Im(complex))
    })
    mat <- cbind (mat, t(dat))
  }
  colnames(mat) <- paste0 (rep(var.names, each=2), c("","1"))
  create.ddobj (mat,
                types = rep("interval", p),
                cols = c(1, 3, 5))
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
  create.ddobj (df,
                types = c("categorical","interval","interval"),
                cols = c(1, 2, 4))
}

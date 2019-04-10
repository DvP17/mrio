#' Construct matrix for specific environmental indicator of EXIOBASE
#'
#' Load EXIOBASE data and return a matrix from an environmental indicator.
#'
#' @usage readExio(year, indicator)
#'
#' @param year Numeric for the respective year
#' @param indicator Numeric for the row number of the corresponding
#' indicator
#'
#' @return None
#'
#' @examples readExio(year = 1995, indicator = 200)
#'
#' @export
readExio <- function(year, indicator) {

  # define path
  path <- c(
    paste0("IOT_", year, "_ixi/A.txt"),
    paste0("IOT_", year, "_ixi/Y.txt"),
    paste0("IOT_", year, "_ixi/satellite/F.txt")
  )

  # read matrices
  A <- as.matrix(data.table::fread(path[1], select = 3:7989, skip = 3,
                                   header = F))

  FD <- as.matrix(data.table::fread(path[2], select = 3:345, skip = 3,
                                    header = F))

  Q <- as.matrix(data.table::fread(path[3], select = 2:7988, skip = 2,
                                   header = F))

  # satellite indicators
  if (indicator < 7988) {

    Q <- Q[indicator,]


  } else if (indicator == 7988) {

    # load characterization factors
    CF <- data.table::fread("QH_EXIOlabel_CF.csv", select = 6)
    Q <- Q[which(!is.na(CF)) + 23,] # +23 bc data starts 26 row minus head

    # weight by characterization factors
    CF <- unlist(CF[which(!is.na(CF)),])
    Q <- Q * matrix(rep(CF, ncol(Q)), ncol = ncol(Q))

    # add indicators
    Q <- colSums(Q)

    # clean Q
    Q[which(Q < 0)] <- 0


  }

  # cat("Matrices have been loaded.\n")

  # calculate emissionmatrix
  I <- diag(ncol(A))

  L <- solve(I - A)

  # cat("Leontief inverse has been generated.\n")

  X <- L %*% FD
  xout <- as.matrix(rowSums(X))
  totalinput <- t(xout)
  # cat("Total input and output have been generated.\n")

  E <- Q / totalinput
  E[which(is.nan(E))] <- 0 # remove NaNs
  E[which(is.infinite(E))] <- 0 # remove Infinites
  E[which(E < 0)] <- 0 # remove Negatives

  emissionmatrix <- matrix(rep(E, length(E)), nrow = length(E)) * L *
    matrix(rep(rowSums(FD), length(E)), nrow = length(E))
  # cat("Emission matrix has been generated.\n")

  # return emissionmatrixyoplay
  return(emissionmatrix)

}



#' Construct list of matrices for specific environmental indicator of EXIOBASE
#'
#' Load EXIOBASE data and return a list of matrices from an environmental
#' indicator ocer a period of time.
#'
#' @usage exioloop(years, indicator)
#'
#' @param years Numeric vector for the respective year
#' @param indicator Numeric for the row number of the corresponding
#' indicator
#'
#' @return Vector of numerics representing the chosen years
#' @return Numerics representing the chosen indicator
#'
#' @examples exioloop(years = 1995:2000, indicator = 200)
#'
#' @export
exioloop <- function(years, indicator) {

  # Test duration and ask for choice
  sysspeed <- system.time(for (i in 1:999999) {y <- i ^ i})
  if (sysspeed[3] > 0.7) {
    duration <- length(years) * sysspeed[3] * 217.648 # 217 is speed for loop=1s

  } else {
    sysspeed <- system.time(for (i in 1:9999999) {y <- i ^ i})
    duration <- length(years) * sysspeed[3] * 15.418 # 15 is speed for loop=1s
  }

  choice <- menu(c("Yes", "No"),
                 title = sprintf("This process will take about %.1f minutes. Do you want to proceed? \n",
                                 duration / 60))

  if (choice == 1) {

    # Core function
    emissionall <- list()
    for (i in years) {

      emissionall[[i]] <- readExio(i, indicator)

      # progress bar
      setTxtProgressBar(txtProgressBar(min = min(years) - 1,
                                       max = max(years), style = 3), i)

    }

    return(emissionall)

  } else {

    cat("The process was terminated.\n")

  }

}




# emissiondyads <- emissionmatrix %>% as.data.frame() %>%
#   gather(row.names, var, everything())
# reshape2::colsplit(emissiondyads$Var1, "_", c("1","2"))



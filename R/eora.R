#' Construct matrix for specific environmental indicator of Eora
#'
#' Load Eora data and return a matrix from an environmental indicator.
#'
#' @usage readEora(year, indicator)
#'
#' @param year Numeric for the respective year
#' @param indicator Numeric for the row number of the corresponding
#' indicator or character string for characterization factor
#' * "bl" for **b**iodiversity **l**oss
#' * "bw" for **b**lue **w**ater consumption
#' * "cc" for **c**limate **c**hange impacts
#' * "en" for **en**ergy demand
#' * "lu" for **l**and **u**se
#' * "mf" for **m**aterial **f**ootprint
#' * "ws" for **w**ater **s**tress
#' @param method Character string for method to calculate matrix
#' @md
#'
#' @return Matrix
#'
#' @examples readEora(year = 1995, indicator = 200)
#'
#' @export
readEora <- function(year, indicator, method, target) {

  # define path
  path <- c(
    paste0("Eora26_", year, "_bp/", "Eora26_", year, "_bp_T.txt"),
    paste0("Eora26_", year, "_bp/", "Eora26_", year, "_bp_FD.txt"),
    paste0("Eora26_", year, "_bp/", "Eora26_", year, "_bp_Q.txt"),
    paste0("Eora26_", year, "_bp/", "Eora26_", year, "_bp_QY.txt")
  )

  # load data
  T <- as.matrix(data.table::fread(path[1], header = F))

  FD <- as.matrix(data.table::fread(path[2], header = F))

  Q <- as.matrix(data.table::fread(path[3], header = F))

  Q_hh <- as.matrix(data.table::fread(path[4], header = F))

  # satellite indicators
  if (is.numeric(indicator)) {
    E <- Q[indicator,]

  } else if (indicator == "cc") { # climate change
    E <- t(mrio::cf_eora$cf_cc) %*% Q
    E_hh <- t(mrio::cf_eora$cf_cc) %*% Q_hh

  } else if (indicator == "ws") { # water stress
    E <- t(mrio::cf_eora$cf_ws) %*% Q
    E_hh <- t(mrio::cf_eora$cf_ws) %*% Q_hh

  } else if (indicator == "lu") { # land use
    E <- t(mrio::cf_eora$cf_lu) %*% Q
    E_hh <- t(mrio::cf_eora$cf_lu) %*% Q_hh

  } else if (indicator == "mf") { # material footprint
    E <- t(mrio::cf_eora$cf_mf) %*% Q
    E_hh <- t(mrio::cf_eora$cf_mf) %*% Q_hh

  } else if (indicator == "bw") { # blue water consumption
    E <- t(mrio::cf_eora$cf_bw) %*% Q
    E_hh <- t(mrio::cf_eora$cf_bw) %*% Q_hh

  } else if (indicator == "en") { # energy demand
    E <- t(mrio::cf_eora$cf_en) %*% Q
    E_hh <- t(mrio::cf_eora$cf_en) %*% Q_hh

  }

  # calculate emissionmatrix
  xout <- rowSums(T) + rowSums(FD)
  totalinput <- t(xout)

  E <- E / totalinput
  E[which(is.nan(E))] <- 0 # remove NaNs
  E[which(is.infinite(E))] <- 0 # remove Infinites
  E[which(E < 0)] <- 0 # remove Negatives

  A <- sweep(T, MARGIN = 2, totalinput, FUN = '/')
  A[which(is.nan(A))] <- 0 # remove NaNs
  E[which(is.infinite(A))] <- 0 # remove Infinites
  E[which(A < 0)] <- 0 # remove Negatives

  I <- diag(ncol(T))
  L <- solve(I - A)


  if (!hasArg(method)) {
    emissionmatrix <- matrix(rep(E, length(E)), nrow = length(E)) * L *
      matrix(rep(rowSums(FD), length(E)), nrow = length(E))
  } else if (method == "pd") {
    emissionmatrix <- (matrix(rep(E, length(E)), nrow = length(E)) * L) %*% FD
  }


  # return emissionmatrixyoplay
  return(emissionmatrix)

}



#' Construct list of matrices for specific environmental indicator of Eora
#'
#' Load Eora data and return a list of matrices from an environmental
#' indicator ocer a period of time.
#'
#' @usage eoraloop(years, indicator)
#'
#' @param years Numeric vector for the respective year
#' @param indicator Numeric for the row number of the corresponding
#' indicator or character string for characterization factor
#' * "bl" for **b**iodiversity **l**oss
#' * "bw" for **b**lue **w**ater consumption
#' * "cc" for **c**limate **c**hange impacts
#' * "en" for **en**ergy demand
#' * "lu" for **l**and **u**se
#' * "mf" for **m**aterial **f**ootprint
#' * "ws" for **w**ater **s**tress
#' @param method Character string for method to calculate matrix
#' @md
#'
#' @return Matrix or list
#'
#' @examples eoraloop(years = 1995:2000, indicator = 200)
#'
#' @export
eoraloop <- function(years, indicator, method) {

  # Test duration and ask for choice
  sysspeed <- system.time(for (i in 1:999999) {y <- i ^ i})
  if (sysspeed[3] > 0.7) {
    duration <- length(years) * sysspeed[3] * 60.791 # 60.79 is speed for loop=1s

  } else {
    sysspeed <- system.time(for (i in 1:9999999) {y <- i ^ i})
    duration <- length(years) * sysspeed[3] * 4.170115 # 4.17 is speed for loop=1s
  }

  choice <- menu(c("Yes", "No"),
                 title = sprintf("This process will take about %.1f minutes. Do you want to proceed? \n",
                                 duration / 60))

  if (choice == 1) {

    # Core function
    emissionall <- list()
    for (i in years) {

      emissionall[[i]] <- readEora(i, indicator, method)

      # progress bar
      setTxtProgressBar(txtProgressBar(min = min(years) - 1,
                                       max = max(years), style = 3), i)

    }

    return(emissionall)

  } else {

    cat("The process was terminated.\n")

  }

}

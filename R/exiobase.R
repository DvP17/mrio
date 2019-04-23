#' Construct matrix for specific environmental indicator of EXIOBASE
#'
#' Load EXIOBASE data and return a matrix from an environmental indicator.
#'
#' @usage readExio(year, indicator)
#'
#' @param year Numeric for the respective year
#' @param indicator Numeric for the row number of the corresponding
#' indicator or character string for characterization factor
#' * "cc" for **c**limate **c**hange impacts
#' * "ws" for **w**ater **s**tress
#' * "lu" for **l**and **u**se
#' @param method Character string for method to calculate matrix
#' * "pd" for **p**roduction to **d**emand matrix
#' * "no-double-pt" for **p**roduction to target **d**emand matrix
#' * "no-double-ts" for **t**arget to final **s**upply matrix
#' * "no-double-td" for **t**arget to final **d**emand matrix
#' * "no-double-pd" for **p**roduction to final **d**emand matrix
#' @param target Three letter country code for corresponding target
#' country if double counting is prevented
#' @md
#'
#' @return Produces a matrix
#'
#' @examples readExio(year = 2000, indicator = 200)
#' readExio(year = 1995, indicator = "cc", method = "no-double-pd",
#' target = "CHN")
#'
#' @export
readExio <- function(year, indicator, method, target) {

  # define path
  path <- c(
    paste0("IOT_", year, "_ixi/A.txt"),
    paste0("IOT_", year, "_ixi/Y.txt"),
    paste0("IOT_", year, "_ixi/satellite/F.txt"),
    paste0("IOT_", year, "_ixi/satellite/F_hh.txt")
  )

  # read matrices
  A <- as.matrix(data.table::fread(path[1], select = 3:7989, skip = 3,
                                   header = F))

  FD <- as.matrix(data.table::fread(path[2], select = 3:345, skip = 3,
                                    header = F))

  Q <- as.matrix(data.table::fread(path[3], select = 2:7988, skip = 2,
                                   header = F))

  Q_hh <- as.matrix(data.table::fread(path[4], select = 2:344, skip = 2,
                                      header = F))

  # satellite indicators
  if (is.numeric(indicator)) {
    E <- Q[indicator,]

  } else if (indicator == "cc") { # climate change
    E <- t(mrio::cf_exio$cf_cc) %*% Q
    E_hh <- t(mrio::cf_exio$cf_cc) %*% Q_hh

  } else if (indicator == "ws") { # water stress
    E <- t(mrio::cf_exio$cf_ws) %*% Q
    E_hh <- t(mrio::cf_exio$cf_ws) %*% Q_hh

  } else if (indicator == "lu") { # land use

  } else if (indicator == "mf") { # material footprint

  } else if (indicator == "bwc") { # blue water consumption

  } else if (indicator == "ed") { # energy demand

  }


  # calculate emissionmatrix
  I <- diag(ncol(A))

  L <- solve(I - A)

  X <- L %*% FD
  xout <- as.matrix(rowSums(X))
  totalinput <- t(xout)

  E <- E / totalinput
  E[which(is.nan(E))] <- 0 # remove NaNs
  E[which(is.infinite(E))] <- 0 # remove Infinites
  E[which(E < 0)] <- 0 # remove Negatives


  # For no double counting
  if (grepl("no-double", method)) {

    # Constructing Index and calculating Leontief
    collab <- read.delim(paste0("IOT_", year, "_ixi/unit.txt"))
    index_t <- which(collab$region == target)
    index_o <- which(collab$region != target)
    I_new <- diag(length(index_o))
    L_oo_dash <- solve(I_new - A[index_o, index_o])

    X_t_wdc_C <- FD[index_t,] + A[index_t, index_o] %*%
      L_oo_dash %*% FD[index_o,]

    X_t_wdc_O <- matrix(0, nrow = length(index_t), ncol = length(index_o) + 1)
    X_t_wdc_O[, 1] <- rowSums(FD[index_t,])
    X_t_wdc_O[, 2:(length(index_o) + 1)] <- A[index_t, index_o] %*%
      L_oo_dash %*% diag(rowSums(FD[index_o,]))

  }


  # Calculate final matrix
  if (!hasArg(method)) {
    e_matrix <- matrix(rep(E, length(E)), nrow = length(E)) * L *
      matrix(rep(rowSums(FD), length(E)), nrow = length(E))
  } else if (method == "pd") {
    e_matrix <- (matrix(rep(E, length(E)), nrow = length(E)) * L) %*% FD
    e_matrix <- rbind(e_matrix, as.vector(E_hh))
  } else if (method == "no-double-pt") { # between production (rows -> Labels_Production) and target (column --> Labels_Target)
    e_matrix <- diag(as.vector(E)) %*% L[, index_t] %*% diag(rowSums(X_t_wdc_C))
  } else if (method == "no-double-ts") { # between target (rows -> Labels_Target) and final supply (column --> Labels_FinalSupply)
    e_matrix <- diag(as.vector(E %*% L[, index_t])) %*% X_t_wdc_O
  } else if (method == "no-double-td") { # between target (rows -> Labels_Target) and final demand (column --> Labels_FinalDemand)
    e_matrix <- diag(as.vector(E %*% L[, index_t])) %*% X_t_wdc_C
  } else if (method == "no-double-pd") { # between production (rows -> Labels_Production) and final demand (column -->Labels_FinalDemand)
    e_matrix <- diag(as.vector(E)) %*% L[, index_t] %*% X_t_wdc_C
  }


  # return e_matrix
  return(e_matrix)

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
#' indicator or character string for characterization factor
#' * "cc" for **c**limate **c**hange impacts
#' * "ws" for **w**ater **s**tress
#' * "lu" for **l**and **u**se
#' @param method Character string for method to calculate matrix
#' * "pd" for **p**roduction to **d**emand matrix
#' * "no-double-pt" for **p**roduction to target **d**emand matrix
#' * "no-double-ts" for **t**arget to final **s**upply matrix
#' * "no-double-td" for **t**arget to final **d**emand matrix
#' * "no-double-pd" for **p**roduction to final **d**emand matrix
#' @param target Three letter country code for corresponding target
#' country if double counting is prevented
#' @md
#'
#' @return Matrix or list
#'
#' @examples exioloop(years = 1995:2000, indicator = 200)
#' exioloop(year = 1995:1997, indicator = "cc", method = "pd",
#' target = "USA")
#'
#' @export
exioloop <- function(years, indicator, method, target) {

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

      emissionall[[i]] <- readExio(i, indicator, method, target)

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



###########################################################################|
###########################   E X I O B A S E   ###########################|
###########################################################################|

# Function
library(data.table)
readExio <- function(arg1, arg2) { # arg1 should be a string from filenames

  # get year and define path
  year <- arg1

  path <- c(
    paste0("IOT_", year, "_ixi/A.txt"),
    paste0("IOT_", year, "_ixi/Y.txt"),
    paste0("IOT_", year, "_ixi/satellite/F.txt")
  )

  # read matrices
  A <- as.matrix(fread(path[1], select = 3:7989, skip = 3, header = F))

  FD <- as.matrix(fread(path[2], select = 3:345, skip = 3, header = F))

  Q <- as.matrix(fread(path[3], select = 2:7988, skip = 2, header = F))

  # satellite indicators
  if (arg2 < 7988) {

    Q <- Q[arg2,]


  } else if (arg2 == 7988) {

    # load characterization factors
    CF <- fread("QH_EXIOlabel_CF.csv", select = 6)
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



############################################################################
## For Loop for all matrices ###############################################
exioloop <- function(arg1, arg2) {

  # Test duration and ask for choice
  sysspeed <- system.time(for (i in 1:999999) {y <- i ^ i})
  duration <- length(arg1) * sysspeed * 217.648 # 217 is speed for loop=1s

  choice <- menu(c("Yes", "No"),
                 title = sprintf("This process will take about %.1f minutes. Do you want to proceed? \n",
                                 duration / 60))

  if (choice == 1) {

    # Core function
    emissionall <- list()
    for (i in arg1) {

      emissionall[[i]] <- readExio(i, arg2)

      # progress bar
      setTxtProgressBar(txtProgressBar(min = min(arg1) - 1,
                                       max = max(arg1), style = 3), i)

    }

    return(emissionall)

  } else {

    cat("The process was terminated.\n")

  }

}

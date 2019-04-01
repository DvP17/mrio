###########################################################################|
###############################   E O R A   ###############################|
###########################################################################|

# Function
library(data.table)
readEora <- function(arg1, arg2) { # arg1 should be a string from filenames

  # get year and define path
  year <- arg1

  path <- c(
    paste0("Eora26_", year, "_bp/", "Eora26_", year, "_bp_T.txt"),
    paste0("Eora26_", year, "_bp/", "Eora26_", year, "_bp_FD.txt"),
    paste0("Eora26_", year, "_bp/", "Eora26_", year, "_bp_Q.txt")
  )

  # load data
  T <- as.matrix(fread(path[1], header = F))

  FD <- as.matrix(fread(path[2], header = F))

  Q <- as.matrix(fread(path[3], header = F))

  # satellite indicators
  if (arg2 <= 4915) {

    Q <- Q[arg2,]


  } else if (arg2 > 4915) {

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

  # calculate emissionmatrix
  xout <- rowSums(T) + rowSums(FD)
  totalinput <- t(xout)

  E <- Q / totalinput
  E[which(is.nan(E))] <- 0 # remove NaNs
  E[which(is.infinite(E))] <- 0 # remove Infinites
  E[which(E < 0)] <- 0 # remove Negatives

  A <- sweep(T, MARGIN = 2, totalinput, FUN = '/')
  A[which(is.nan(A))] <- 0 # remove NaNs
  E[which(is.infinite(A))] <- 0 # remove Infinites
  E[which(A < 0)] <- 0 # remove Negatives

  I <- diag(ncol(T))
  L <- solve(I - A)


  emissionmatrix <- matrix(rep(E, length(E)), nrow = length(E)) * L *
    matrix(rep(rowSums(FD), length(E)), nrow = length(E))

  # return emissionmatrixyoplay
  return(emissionmatrix)

}

############################################################################
## For Loop for all matrices ###############################################
eoraloop <- function(arg1, arg2) {

  # Test duration and ask for choice
  sysspeed <- system.time(for (i in 1:999999) {y <- i ^ i})
  duration <- length(arg1) * sysspeed * 60.791 # 217 is speed for loop=1s

  choice <- menu(c("Yes", "No"),
                 title = sprintf("This process will take about %.1f minutes. Do you want to proceed? \n",
                                 duration / 60))

  if (choice == 1) {

    # Core function
    emissionall <- list()
    for (i in arg1) {

      emissionall[[i]] <- readEora(i, arg2)

      # progress bar
      setTxtProgressBar(txtProgressBar(min = min(arg1) - 1,
                                       max = max(arg1), style = 3), i)

    }

    return(emissionall)

  } else {

    cat("The process was terminated.\n")

  }

}

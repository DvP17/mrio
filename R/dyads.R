#' Transform matrices into dyadic data.frames
#'
#' Matrices and list with matrices will be transformed into
#' data.frames and labeled. The type of data input is recognized
#' automatically. Year input can be a vector of year or one single year.
#' Note that high RAM is required to execute this function for all
#' countries especially for Exiobase and larger time frames.
#'
#' @usage dyads(year, data, country)
#'
#' @param year Numeric vector or number for the respective year/s
#' @param data Data object/ matrix that was created before
#' @param input String or character vector for input country/ies
#' @param output String or character vector for output country/ies
#'
#' @return data.frame
#'
#' @examples dyads(year = 1995:1996, dataobject, output = c("DEU", "CHN"))
#'
#' @export
dyads <- function(year, data, input, output) {

  # Evaluate data input
  if (!is.list(data)) { # for matrices
    if (all(dim(data) == rep(4915, 2))) {type <- "eoramatrix"}
    else if (all(dim(data) == c(4915, 1140))) {type <- "eoramatrixECC"}
    else if (all(dim(data) == rep(7987, 2))) {type <- "exiomatrix"}
    else if (all(dim(data) == c(7987, 343))) {type <- "exiomatrixECC"}
    else {cat("Data input has wrong number of dimensions.")}
  } else if (is.list(data)) { # for lists
    if (all(dim(data[[min(year)]]) == rep(4915, 2))) {type <- "eoralist"}
    else if (all(dim(data[[min(year)]]) == c(4915, 1140))) {type <- "eoralistECC"}
    else if (all(dim(data[[min(year)]]) == rep(7987, 2))) {type <- "exiolist"}
    else if (all(dim(data[[min(year)]]) == c(7987, 343))) {type <- "exiolistECC"}
    else {cat("Data input has wrong number of dimensions.")}
  } else {cat("Data input must be matrix or list.")
  }

  # Function for label assignment on matrix input
  labelingmatrix <- function(a, b, input, output) {

    if (hasArg(input) & !hasArg(output)) {
      data <- data[which(rowlab$V2 %in% input),]
      rowlab <- rowlab[which(rowlab$V2 %in% input),]
      row.names(data) <- c(paste0(rowlab[[a]], "_", rowlab[[b]]))
      colnames(data) <- c(paste0(collab[[a]], "_", collab[[b]]))
    } else if (hasArg(output) & !hasArg(input)) {
      data <- data[,which(collab$V2 %in% output)]
      row.names(data) <- c(paste0(rowlab[[a]], "_", rowlab[[b]]))
      collab <- collab[which(collab$V2 %in% output),]
      colnames(data) <- c(paste0(collab[[a]], "_", collab[[b]]))
    } else if (hasArg(input) & hasArg(output)) {
      data <- data[which(rowlab$V2 %in% input), which(collab$V2 %in% output)]
      rowlab <- rowlab[which(rowlab$V2 %in% input),]
      row.names(data) <- c(paste0(rowlab[[a]], "_", rowlab[[b]]))
      collab <- collab[which(collab$V2 %in% output),]
      colnames(data) <- c(paste0(collab[[a]], "_", collab[[b]]))
    } else {
      row.names(data) <- c(paste0(rowlab[[a]], "_", rowlab[[b]]))
      colnames(data) <- c(paste0(collab[[a]], "_", collab[[b]]))
    }

    return(data)

  }

  # Function for label assignment on list input
  labelinglist <- function(a, b, input, output) {

    if (hasArg(input) & !hasArg(output)) {
      data <- lapply(data, function(x) {x[which(rowlab$V2 %in% input),]})
      rowlab <- rowlab[which(rowlab$V2 %in% input),]
      rowlab <- c(paste0(rowlab[[a]], "_", rowlab[[b]]))
      data <- lapply(data, function(x) {row.names(x) <- rowlab; x})
      collab <- c(paste0(collab[[a]], "_", collab[[b]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {colnames(x) <- collab; x})
    } else if (hasArg(output) & !hasArg(input)) {
      data <- lapply(data, function(x) {x[,which(collab$V2 %in% output)]})
      rowlab <- c(paste0(rowlab[[a]], "_", rowlab[[b]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {row.names(x) <- rowlab; x})
      collab <- collab[which(collab$V2 %in% output),]
      collab <- c(paste0(collab[[a]], "_", collab[[b]]))
      data <- lapply(data, function(x) {colnames(x) <- collab; x})
    } else if (hasArg(input) & hasArg(output)) {
      data <- lapply(data, function(x) {x[which(rowlab$V2 %in% input),
                                          which(collab$V2 %in% output)]})
      rowlab <- rowlab[which(rowlab$V2 %in% input),]
      rowlab <- c(paste0(rowlab[[a]], "_", rowlab[[b]]))
      collab <- collab[which(collab$V2 %in% output),]
      collab <- c(paste0(collab[[a]], "_", collab[[b]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {row.names(x) <- rowlab; x})
      data <- lapply(data, function(x) {colnames(x) <- collab; x})
    } else {
      collab <- c(paste0(collab[[a]], "_", collab[[b]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {colnames(x) <- collab; x})
      rowlab <- c(paste0(rowlab[[a]], "_", rowlab[[b]]))
      data <- lapply(data, function(x) {row.names(x) <- rowlab; x})
    }

    return(data)

  }


  # MAIN
  if (grepl("eoramatrix", type)) {

    if (type == "eoramatrix") {
      collab <- read.delim(paste0("Eora26_", min(year), "_bp/labels_T.txt"),
                           header = F)
      rowlab <- collab
    } else if (type == "eoramatrixECC") {
      collab <- read.delim(paste0("Eora26_", min(year), "_bp/labels_FD.txt"),
                           header = F)
      rowlab <- read.delim(paste0("Eora26_", min(year), "_bp/labels_T.txt"),
                           header = F)
    }

    # Call labeling matrix function
    data <- labelingmatrix(2, 4, input, output)

    # Form dyads
    data <- reshape2::melt(data, as.is = T) # as.is makes chr
    data <- tidyr::separate(data, Var1, into = c("C1", "S1"), sep = "_")
    data <- tidyr::separate(data, Var2, into = c("C2", "S2"), sep = "_")

    # data <- data[order(data$C1),]
    final <- cbind(year, data)

  }

  if (grepl("eoralist", type)) {

    if (type == "eoralist") {
      collab <- read.delim(paste0("Eora26_", min(year), "_bp/labels_T.txt"),
                           header = F)
      rowlab <- collab
    } else if (type == "eoralistECC") {
      collab <- read.delim(paste0("Eora26_", min(year), "_bp/labels_FD.txt"),
                           header = F)
      rowlab <- read.delim(paste0("Eora26_", min(year), "_bp/labels_T.txt"),
                           header = F)
    }

    # Call labeling list function
    data <- labelinglist(2, 4, input, output)

    # Form dyads
    data <- lapply(data, function(x) {reshape2::melt(x, as.is = T)})
    data <- lapply(data, function(x) {tidyr::separate(x, Var1,
                                                      into = c("C1", "S1"), sep = "_")})
    data <- lapply(data, function(x) {tidyr::separate(x, Var2,
                                                      into = c("C2", "S2"), sep = "_")})

    # data <- lapply(data, function(x) {x[order(x$C1),]})
    data <- Map(cbind, year = as.list(year), data)
    final <- data.table::rbindlist(data)

  }

  if (grepl("exiomatrix", type)) {

    if (type == "exiomatrix") {
      collab <- read.delim(paste0("IOT_", min(year), "_ixi/unit.txt"))
      colnames(collab)[1] <- "V2"
      rowlab <- collab
    } else if (type == "exiomatrixECC") {
      collab <- read.delim(paste0("IOT_", min(year), "_ixi/Y.txt"), nrows = 1)
      collab <- data.frame(V2 = substr(colnames(collab)[3:ncol(collab)], 1, 2),
                           V3 = unlist(collab[1, 3:ncol(collab)]))
      rowlab <- read.delim(paste0("IOT_", min(year), "_ixi/unit.txt"))
    }

    # Call labeling matrix
    data <- labelingmatrix(1, 2, input, output)

    # Form dyads
    data <- reshape2::melt(data, as.is = T) # as.is makes chr
    data <- tidyr::separate(data, Var1, into = c("C1", "S1"), sep = "_"); gc()
    data <- tidyr::separate(data, Var2, into = c("C2", "S2"), sep = "_"); gc()

    # data <- data[order(data$C1),]
    final <- cbind(min(year), data); rm(data); gc()

  }

  if (grepl("exiolist", type)) {

    if (type == "exiolist") {
      collab <- read.delim(paste0("IOT_", min(year), "_ixi/unit.txt"))
      colnames(collab)[1] <- "V2"
      rowlab <- collab
    } else if (type == "exiolistECC") {
      collab <- read.delim(paste0("IOT_", min(year), "_ixi/Y.txt"), nrows = 1)
      collab <- data.frame(V2 = substr(colnames(collab)[3:ncol(collab)], 1, 2),
                           V3 = unlist(collab[1, 3:ncol(collab)]))
      rowlab <- read.delim(paste0("IOT_", min(year), "_ixi/unit.txt"))
    }

    # Call labeling list function
    data <- labelinglist(1, 2, input, output)

    # Form dyads
    data <- lapply(data, function(x) {reshape2::melt(x, as.is = T)})
    data <- lapply(data, function(x) {tidyr::separate(x, Var1,
                                                      into = c("C1", "S1"), sep = "_")})
    data <- lapply(data, function(x) {tidyr::separate(x, Var2,
                                                      into = c("C2", "S2"), sep = "_")})

    # data <- lapply(data, function(x) {x[order(x$C1),]})
    data <- Map(cbind, year = as.list(year), data)
    final <- data.table::rbindlist(data); rm(data)

  }

  return(final); gc()

}


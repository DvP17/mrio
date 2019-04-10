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
#' @param country String or character vector for corresponding country/ies
#'
#' @return None
#'
#' @examples dyads(year = 1995:1996, dataobject, country = c("DEU", "CHN"))
#'
#' @export
dyads <- function(year, data, country) {

  # Evaluate data input
  if (!is.list(data)) {
    if (dim(data) == rep(4915, 2)) {type <- "eoramatrix"}
    else if (dim(data) == rep(7987, 2)) {type <- "exiomatrix"}
    else {cat("Data input has wrong number of dimensions.")}
  }
  else if (is.list(data)) {
    if (dim(data[[min(year)]]) == rep(4915, 2)) {type <- "eoralist"}
    else if (dim(data[[min(year)]]) == rep(7987, 2)) {type <- "exiolist"}
    else {cat("Data input has wrong number of dimensions.")}
  }
  else {cat("Data input must be matrix or list.")
  }

  # Main
  if (type == "eoramatrix") {
    labels <- read.delim(paste0("Eora26_", min(year), "_bp/labels_T.txt"),
                         header = F)

    if (hasArg(country)) {
      data <- data[which(labels$V2 %in% country), which(labels$V2 %in% country)]
      labels <- labels[which(labels$V2 %in% country),]
    }

    colnames(data) <- c(paste0(labels[[2]], "_", labels[[4]]))
    row.names(data) <- colnames(data)

    data <- reshape2::melt(data, as.is = T) # as.is makes chr
    data <- tidyr::separate(data, Var1, into = c("C1", "S1"), sep = "_")
    data <- tidyr::separate(data, Var2, into = c("C2", "S2"), sep = "_")

    # data <- data[order(data$C1),]
    final <- cbind(year, data)

  }

  if (type == "eoralist") {
    labels <- read.delim(paste0("Eora26_", min(year), "_bp/labels_T.txt"),
                         header = F)

    if (hasArg(country)) {
      data <- lapply(data, function(x) {x[which(labels$V2 %in% country),
                                          which(labels$V2 %in% country)]})
      labels <- labels[which(labels$V2 %in% country),]
    }

    colnames <- c(paste0(labels[[2]], "_", labels[[4]]))
    data <- lapply(data[min(year):max(year)],
                   function(x) {colnames(x) <- colnames; x})
    data <- lapply(data, function(x) {row.names(x) <- colnames; x})

    data <- lapply(data, function(x) {reshape2::melt(x, as.is = T)})
    data <- lapply(data, function(x) {tidyr::separate(x, Var1,
                                                      into = c("C1", "S1"), sep = "_")})
    data <- lapply(data, function(x) {tidyr::separate(x, Var2,
                                                      into = c("C2", "S2"), sep = "_")})

    # data <- lapply(data, function(x) {x[order(x$C1),]})
    data <- Map(cbind, year = as.list(year), data)
    final <- data.table::rbindlist(data)

  }

  if (type == "exiomatrix") {
    labels <- read.delim(paste0("IOT_", min(year), "_ixi/unit.txt"), header = F)
    colnames(labels)[1] <- "V2"

    if (hasArg(country)) {
      data <- data[which(labels$V2 %in% country), which(labels$V2 %in% country)]
      labels <- labels[which(labels$V2 %in% country),]
    }

    colnames(data) <- c(paste0(labels[[1]], "_", labels[[2]])); rm(labels)
    row.names(data) <- colnames(data)

    data <- reshape2::melt(data, as.is = T); gc() # as.is makes chr
    data <- tidyr::separate(data, Var1, into = c("C1", "S1"), sep = "_"); gc()
    data <- tidyr::separate(data, Var2, into = c("C2", "S2"), sep = "_"); gc()

    # data <- data[order(data$C1),]
    final <- cbind(min(year), data); rm(data); gc()

  }

  if (type == "exiolist") {
    labels <- read.delim(paste0("IOT_", min(year), "_ixi/unit.txt"))
    colnames(labels)[1] <- "V2"

    if (hasArg(country)) {
      data <- lapply(data, function(x) {x[which(labels$V2 %in% country),
                                          which(labels$V2 %in% country)]})
      labels <- labels[which(labels$V2 %in% country),]
    }

    colnames <- c(paste0(labels[[1]], "_", labels[[2]])); rm(labels)
    data <- lapply(data[min(year):max(year)],
                   function(x) {colnames(x) <- colnames; x})
    data <- lapply(data, function(x) {row.names(x) <- colnames; x}); rm(colnames); gc()

    data <- lapply(data, function(x) {reshape2::melt(x, as.is = T)}); gc()

    data <- lapply(data, function(x) {tidyr::separate(x, Var1,
                                                      into = c("C1", "S1"), sep = "_")}); gc()
    data <- lapply(data, function(x) {tidyr::separate(x, Var2,
                                                      into = c("C2", "S2"), sep = "_")}); gc()

    # data <- lapply(data, function(x) {x[order(x$C1),]})
    data <- Map(cbind, year = as.list(year), data); gc()
    final <- data.table::rbindlist(data); rm(data); gc()

  }

  return(final); gc()

}



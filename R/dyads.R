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

    if (hasArg(input) & !hasArg(output)) {
      data <- data[which(labels$V2 %in% input),]
      colnames(data) <- c(paste0(labels[[2]], "_", labels[[4]]))
      labels <- labels[which(labels$V2 %in% input),]
      row.names(data) <- c(paste0(labels[[2]], "_", labels[[4]]))
    } else if (hasArg(output) & !hasArg(input)) {
      data <- data[,which(labels$V2 %in% output)]
      row.names(data) <- c(paste0(labels[[2]], "_", labels[[4]]))
      labels <- labels[which(labels$V2 %in% output),]
      colnames(data) <- c(paste0(labels[[2]], "_", labels[[4]]))
    } else if (hasArg(input) & hasArg(output)) {
      data <- data[which(labels$V2 %in% input), which(labels$V2 %in% output)]
      labels1 <- labels[which(labels$V2 %in% input),]
      labels2 <- labels[which(labels$V2 %in% output),]
      row.names(data) <- c(paste0(labels1[[2]], "_", labels1[[4]]))
      colnames(data) <- c(paste0(labels2[[2]], "_", labels2[[4]]))
    } else {
      colnames(data) <- c(paste0(labels[[2]], "_", labels[[4]]))
      row.names(data) <- colnames(data)
    }


    data <- reshape2::melt(data, as.is = T) # as.is makes chr
    data <- tidyr::separate(data, Var1, into = c("C1", "S1"), sep = "_")
    data <- tidyr::separate(data, Var2, into = c("C2", "S2"), sep = "_")

    # data <- data[order(data$C1),]
    final <- cbind(year, data)

  }

  if (type == "eoralist") {
    labels <- read.delim(paste0("Eora26_", min(year), "_bp/labels_T.txt"),
                         header = F)


    if (hasArg(input) & !hasArg(output)) {
      data <- lapply(data, function(x) {x[which(labels$V2 %in% input),]})
      colnames <- c(paste0(labels[[2]], "_", labels[[4]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {colnames(x) <- colnames; x})
      labels <- labels[which(labels$V2 %in% input),]
      labels <- c(paste0(labels[[2]], "_", labels[[4]]))
      data <- lapply(data, function(x) {row.names(x) <- labels; x})
    } else if (hasArg(output) & !hasArg(input)) {
      data <- lapply(data, function(x) {x[,which(labels$V2 %in% output)]})
      rownames <- c(paste0(labels[[2]], "_", labels[[4]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {row.names(x) <- rownames; x})
      labels <- labels[which(labels$V2 %in% output),]
      labels <- c(paste0(labels[[2]], "_", labels[[4]]))
      data <- lapply(data, function(x) {colnames(x) <- labels; x})
    } else if (hasArg(input) & hasArg(output)) {
      data <- lapply(data, function(x) {x[which(labels$V2 %in% input),
                                          which(labels$V2 %in% output)]})
      labels1 <- labels[which(labels$V2 %in% input),]
      labels1 <- c(paste0(labels1[[2]], "_", labels1[[4]]))
      labels2 <- labels[which(labels$V2 %in% output),]
      labels2 <- c(paste0(labels2[[2]], "_", labels2[[4]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {row.names(x) <- labels1; x})
      data <- lapply(data, function(x) {colnames(x) <- labels2; x})
    } else {
      colnames <- c(paste0(labels[[2]], "_", labels[[4]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {colnames(x) <- colnames; x})
      data <- lapply(data, function(x) {row.names(x) <- colnames; x})
    }


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
    labels <- read.delim(paste0("IOT_", min(year), "_ixi/unit.txt"))
    colnames(labels)[1] <- "V2"

    if (hasArg(input) & !hasArg(output)) {
      data <- data[which(labels$V2 %in% input),]
      colnames(data) <- c(paste0(labels[[1]], "_", labels[[2]]))
      labels <- labels[which(labels$V2 %in% input),]
      row.names(data) <- c(paste0(labels[[1]], "_", labels[[2]]))
    } else if (hasArg(output) & !hasArg(input)) {
      data <- data[,which(labels$V2 %in% output)]
      row.names(data) <- c(paste0(labels[[1]], "_", labels[[2]]))
      labels <- labels[which(labels$V2 %in% output),]
      colnames(data) <- c(paste0(labels[[1]], "_", labels[[2]]))
    } else if (hasArg(input) & hasArg(output)) {
      data <- data[which(labels$V2 %in% input), which(labels$V2 %in% output)]
      labels1 <- labels[which(labels$V2 %in% input),]
      labels2 <- labels[which(labels$V2 %in% output),]
      row.names(data) <- c(paste0(labels1[[1]], "_", labels1[[2]]))
      colnames(data) <- c(paste0(labels2[[1]], "_", labels2[[2]]))
    } else {
      colnames(data) <- c(paste0(labels[[1]], "_", labels[[2]]))
      row.names(data) <- colnames(data)
    }


    data <- reshape2::melt(data, as.is = T) # as.is makes chr
    data <- tidyr::separate(data, Var1, into = c("C1", "S1"), sep = "_"); gc()
    data <- tidyr::separate(data, Var2, into = c("C2", "S2"), sep = "_"); gc()

    # data <- data[order(data$C1),]
    final <- cbind(min(year), data); rm(data); gc()

  }

  if (type == "exiolist") {
    labels <- read.delim(paste0("IOT_", min(year), "_ixi/unit.txt"))
    colnames(labels)[1] <- "V2"


    if (hasArg(input) & !hasArg(output)) {
      data <- lapply(data, function(x) {x[which(labels$V2 %in% input),]})
      colnames <- c(paste0(labels[[1]], "_", labels[[2]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {colnames(x) <- colnames; x})
      labels <- labels[which(labels$V2 %in% input),]
      labels <- c(paste0(labels[[1]], "_", labels[[2]]))
      data <- lapply(data, function(x) {row.names(x) <- labels; x})
    } else if (hasArg(output) & !hasArg(input)) {
      data <- lapply(data, function(x) {x[,which(labels$V2 %in% output)]})
      rownames <- c(paste0(labels[[1]], "_", labels[[2]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {row.names(x) <- rownames; x})
      labels <- labels[which(labels$V2 %in% output),]
      labels <- c(paste0(labels[[1]], "_", labels[[2]]))
      data <- lapply(data, function(x) {colnames(x) <- labels; x})
    } else if (hasArg(input) & hasArg(output)) {
      data <- lapply(data, function(x) {x[which(labels$V2 %in% input),
                                          which(labels$V2 %in% output)]})
      labels1 <- labels[which(labels$V2 %in% input),]
      labels1 <- c(paste0(labels1[[1]], "_", labels1[[2]]))
      labels2 <- labels[which(labels$V2 %in% output),]
      labels2 <- c(paste0(labels2[[1]], "_", labels2[[2]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {row.names(x) <- labels1; x})
      data <- lapply(data, function(x) {colnames(x) <- labels2; x})
    } else {
      colnames <- c(paste0(labels[[1]], "_", labels[[2]]))
      data <- lapply(data[min(year):max(year)],
                     function(x) {colnames(x) <- colnames; x})
      data <- lapply(data, function(x) {row.names(x) <- colnames; x})
    }


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


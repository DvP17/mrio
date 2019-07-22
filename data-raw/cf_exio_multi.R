# Characterization factors for EXIOBASE
cf_ws <- data.table::fread("C:/Data/Matlab Tool/Files/CF/CF_water.txt",
                           header = T)


# Characterization factors
cf_bl <- NULL

cf_ws <- cf_ws[2:nrow(cf_ws), c(2 + c(1:8, rep(9, 11), rep(10, 144)))]
cf_ws <- as.numeric(unlist(t(cf_ws)))



# Combine all CFs
cf_exio_multi <- list(cf_ws = cf_ws)
usethis::use_data(cf_exio_multi, overwrite = TRUE)

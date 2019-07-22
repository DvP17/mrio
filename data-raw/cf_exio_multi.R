# Characterization factors for EXIOBASE
cf_ws_or <- data.table::fread("C:/Data/Matlab Tool/Files/CF/CF_water.txt",
                              header = T)


# Characterization factors
cf_bl <- NULL


cf_ws <- cf_ws_or[2:nrow(cf_ws_or), c(2 + c(1:8, rep(9, 11), rep(10, 144)))]
cf_ws <- as.numeric(unlist(t(cf_ws)))

cf_ws_hh <- cf_ws_or[2:nrow(cf_ws_or), c(2 + rep(10, 7))]
cf_ws_hh <- as.numeric(unlist(t(cf_ws_hh)))



# Combine all CFs
cf_exio_multi <- list(cf_ws_or = cf_ws_or, cf_ws = cf_ws, cf_ws_hh = cf_ws_hh)
usethis::use_data(cf_exio_multi, overwrite = TRUE)

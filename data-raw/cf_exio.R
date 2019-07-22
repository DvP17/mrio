# Characterization factors for EXIOBASE
cf <- data.table::fread("C:/Data/Exiobase/IOT_1995_ixi/satellite/unit.txt",
                        header = T)
cf <- tidyr::separate(cf, V1, c("env impact", "type", "compartment"),
                      sep = " - ")
cf[1:23,] <- tidyr::separate(cf[1:23,], "env impact",
                             c("env impact", "type", "compartment"), sep = ": ")


# Characterization factors
cf_bw <- c(rep(0, 923), rep(1, 103), rep(0, 78))
cf_cc <- c(rep(0, 23), 1, 36.8, 298, 0, 0, 0, 1.57, rep(0, 7), 4.23, rep(0, 29),
           rep(36.8, 8), rep(1.57, 17), 1, 1, rep(0, 47), rep(4.23, 47),
           rep(0, 235), 26100, 1, 1, 36.8, 1, 0, 298, rep(0, 5), 36.8, 1.57, 0,
           1, rep(0, 665))
cf_en <- c(rep(0, 466), rep(1, 4), rep(0, 634))
cf_lu <- c(rep(0, 446), rep(1, 20), rep(0, 638))
cf_mf <- c(rep(0, 470), rep(1, 217), rep(0, 417))
cf_ws <- c(rep(0, 923), rep(1, 103), rep(0, 78))
cf_bl <- NULL

# Combine all CFs
cf_exio <- cbind(cf, cf_bw, cf_cc, cf_en, cf_lu, cf_mf)
usethis::use_data(cf_exio, overwrite = TRUE)

# Characterization factors for EXIOBASE
cf <- data.table::fread("C:/Data/Exiobase/IOT_1995_ixi/satellite/unit.txt",
                        header = T)
cf <- tidyr::separate(cf, V1, c("env impact", "type", "compartment"),
                      sep = " - ")
cf[1:23,] <- tidyr::separate(cf[1:23,], "env impact",
                             c("env impact", "type", "compartment"), sep = ": ")
cf_cc <- c(rep(0, 23), 1, 36.8, 298, 0, 0, 0, 1.57, rep(0, 7), 4.23, rep(0, 29),
           rep(36.8, 8), rep(1.57, 17), 1, 1, rep(0, 47), rep(4.23, 47),
           rep(0, 235), 26100, 1, 1, 36.8, 1, 0, 298, rep(0, 5), 36.8, 1.57, 0,
           1, rep(0, 665))
cf_exio <- cbind(cf, cf_cc)
usethis::use_data(cf_exio, overwrite = TRUE)

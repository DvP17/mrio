# Characterization factors for Eora
cf <- data.table::fread("C:/Data/EF/Eora/Eora26_1990_bp/labels_Q.txt",
                        header = F)
cf$V3[grep("[(]", cf$V1)] <- unlist(regmatches(cf$V1,
                                                gregexpr("(?<=\\().*?(?=\\))",
                                                         cf$V1, perl = T)))
cf$V1 <- gsub("\\s*\\([^\\)]+\\)","",cf$V1)
colnames(cf) <- c("env impact", "compartment", "unit")


# Characterization factors
cf_bw <- c(rep(0, 2475), c(1,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1),
           rep(0, 219))
cf_cc <- c(rep(0, 1498), rep(4.23, 55), rep(0, 949), 36.8, rep(0, 16), 1,
           rep(0, 50), 1, rep(0, 67), 298, rep(0, 16), 1, rep(0, 33), 26100,
           rep(0, 18), 1, rep(0, 11))
cf_en <- c(rep(1, 9), rep(0, 2711))
cf_lu <- c(rep(0, 2436), rep(1, 33), rep(0, 251))
cf_mf <- c(rep(0, 1867), rep(1, 40), rep(0,813))
cf_ws <- NULL
cf_bl <- NULL


# Combine all CFs
cf_exio <- cbind(cf, cf_bw, cf_cc, cf_en, cf_lu, cf_mf)
usethis::use_data(cf_eora, overwrite = TRUE)

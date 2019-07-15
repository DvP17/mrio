# Characterization factors for Eora
cf <- data.table::fread("C:/Data/EF/Eora/Eora26_1990_bp/labels_Q.txt",
                        header = F)
cf$V3[grep("[(]", cf$V1)] <- unlist(regmatches(cf$V1,
                                                gregexpr("(?<=\\().*?(?=\\))",
                                                         cf$V1, perl = T)))
cf$V1 <- gsub("\\s*\\([^\\)]+\\)","",cf$V1)
colnames(cf) <- c("env impact", "compartment", "unit")
cf_cc <- c(rep(0, 1498), rep(4.23, 55), rep(0, 949), 36.8, rep(0, 16), 1,
           rep(0, 50), 1, rep(0, 67), 298, rep(0, 16), 1, rep(0, 33), 26100,
           rep(0, 18), 1, rep(0, 11))
cf_ws <- NULL
cf_lu <- c(rep(0, 2436) , rep(1, 33), rep(0, 251))
cf_en <- c(rep(1, 9) , rep(0, 2711))
cf_bw <- c(rep(0, 2475), c(1,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1,0,0,1),
           rep(0, 219))
cf_mf <- c(rep(0, 1867), rep(1, 40), rep(0,813))
cf_eora <- cbind(cf, cf_cc, cf_lu, cf_en, cf_bw, cf_mf)
usethis::use_data(cf_eora, overwrite = TRUE)

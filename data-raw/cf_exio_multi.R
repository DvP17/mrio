# Characterization factors for EXIOBASE
cf_bl_or <- data.table::fread("C:/Data/Matlab Tool/Files/CF/CF_landuse.txt",
                              header = T)
cf_bl_q_2007 <- data.table::fread("C:/Data/Matlab Tool/Files/CF/F_2007.txt",
                              header = T)
cf_bl_qhh_2007 <- data.table::fread("C:/Data/Matlab Tool/Files/CF/F_hh_2007.txt",
                                    header = T)
cf_ws_or <- data.table::fread("C:/Data/Matlab Tool/Files/CF/CF_water.txt",
                              header = T)



############################################################################
## CFs Biodiversity Loss ###################################################
cf_landuse <- cf_bl_or
cf_landuse[, 13] <- 0 # set "other land use" = 0
cf_landuse <- cf_landuse[-1,-(1:2)]
cf_landuse <- cf_landuse[rep(1:nrow(cf_landuse), each = 163),]
cf_landuse <- t(cf_landuse)
cf_landuse <- data.frame(apply(cf_landuse, 2,
                               function(x) as.numeric(as.character(x))))


cf_bl_q_2007 <- cf_bl_q_2007[-1, -1]
cf_bl_q_2007 <- data.frame(sapply(cf_bl_q_2007,
                                  function(x) as.numeric(as.character(x))))
cf_bl_q_2007 <- rbind(cf_bl_q_2007[447:448,], colSums(cf_bl_q_2007[449:453,]),
                      cf_bl_q_2007[454:461,], colSums(cf_bl_q_2007[462:464,]),
                      cf_bl_q_2007[465:466,])

cf_bl_i <- matrix(rep(0, 111818), 14)
for (q in 1:9) {
  for (n in 0:48) {
    i = 1 + 163 * n
    j = i + 162
    for (z in i:j) {
      # print(q)
      # print(z)
      if (cf_bl_q_2007[q, z] > 0) {
        cf_bl_i[q, z] <- cf_landuse[q, z] / rowSums(cf_bl_q_2007[q, i:j]);
      }
      else {
        cf_bl_i[q, z] <- 0
      }
    }
  }
}

cf_bl_i <- as.data.frame(cf_bl_i)
cf_bl_i[10:14,] <- cf_landuse[10:14,] * 10 ^ 6
cf_bl <- cf_bl_i


  # Households
cf_landuse_hh <- cf_bl_or
cf_landuse_hh[, 13] <- 0 # set "other land use" = 0
cf_landuse_hh <- cf_landuse_hh[-1,-(1:2)]
cf_landuse_hh <- cf_landuse_hh[rep(1:nrow(cf_landuse_hh), each = 7),]
cf_landuse_hh <- t(cf_landuse_hh)
cf_landuse_hh <- data.frame(apply(cf_landuse_hh, 2,
                                  function(x) as.numeric(as.character(x))))


cf_bl_qhh_2007 <- cf_bl_qhh_2007[-1, -1]
cf_bl_qhh_2007 <- data.frame(sapply(cf_bl_qhh_2007,
                                  function(x) as.numeric(as.character(x))))
cf_bl_qhh_2007 <- rbind(cf_bl_qhh_2007[447:448,], colSums(cf_bl_qhh_2007[449:453,]),
                        cf_bl_qhh_2007[454:461,], colSums(cf_bl_qhh_2007[462:464,]),
                        cf_bl_qhh_2007[465:466,])

cf_bl_i <- matrix(rep(0, 4802), 14)
for (q in 1:9) {
  for (n in 0:48) {
    i = 1 + 7 * n
    j = i + 6
    for (z in i:j) {
      if (cf_bl_qhh_2007[q, z] > 0) {
        cf_bl_i[q, z] <- cf_landuse_hh[q, z] / rowSums(cf_bl_qhh_2007[q, i:j]);
      }
      else {
        cf_bl_i[q, z] <- 0
      }
    }
  }
}

cf_bl_i <- as.data.frame(cf_bl_i)
cf_bl_i[10:14,] <- cf_landuse_hh[10:14,] * 10 ^ 6
cf_bl_hh <- cf_bl_i



############################################################################
## CFs water stress ########################################################
cf_ws <- cf_ws_or[2:nrow(cf_ws_or), c(2 + c(1:8, rep(9, 11), rep(10, 144)))]
cf_ws <- as.numeric(unlist(t(cf_ws)))

cf_ws_hh <- cf_ws_or[2:nrow(cf_ws_or), c(2 + rep(10, 7))]
cf_ws_hh <- as.numeric(unlist(t(cf_ws_hh)))



############################################################################
## Combine all CFs #########################################################
cf_exio_multi <- list(cf_bl_or = cf_bl_or, cf_ws_or = cf_ws_or,
                      cf_bl = cf_bl, cf_bl_hh = cf_bl_hh,
                      cf_ws = cf_ws, cf_ws_hh = cf_ws_hh)
usethis::use_data(cf_exio_multi, overwrite = TRUE)

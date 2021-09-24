# Include cold pool index in coldpool R package
# To be run after generating cold pool index.

cpa_pre2021 <- read.csv(file = "./inst/extdata/cpa_areas2019.csv")
ebs_proj_crs <- "EPSG:3338"
cold_pool_index <- read.csv(file = "./output/estimated_cpa/cpa_out_ste.csv") %>%
  dplyr::rename(COLD_POOL_AREA_KM2 = ste_lte2, YEAR = year) %>%
  dplyr::mutate(LAST_UPDATE = Sys.Date())
save(cpa_pre2021, ebs_proj_crs, cold_pool_index, file = "./data/sysdata.rda")
rm(cpa_pre2021)
rm(ebs_proj_crs)
rm(cold_pool_index)
load("./data/sysdata.rda")

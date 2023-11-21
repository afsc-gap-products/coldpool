library(coldpool)
library(gapctd)
library(navmaps)
library(lubridate)
library(spmodel)

fit_2d_spmod <- function(data,
                         spcov_type,
                         model_formula,
                         anisotropy,
                         ...) {
    
    model_structure <- expand.grid(spcov_type = spcov_type,
                                   formula = model_formula,
                                   anisotropy = anisotropy)
    model_structure$spcov_type <- as.character(model_structure$spcov_type)
    model_structure$convergence <- as.numeric(NA)
    model_structure$AICc <- as.numeric(NA)
    model_structure$mspe <- as.numeric(NA)
    model_structure$anis_rotate <- 0
    model_structure$anis_scale <- 1
    model_structure$df <- as.numeric(NA)
    
    for(jj in 1:nrow(model_structure)) {
      
      mod_fit <- spmodel::splm(formula = model_structure$formula[[jj]], 
                            data = data, 
                            spcov_type = model_structure$spcov_type[jj],
                            anisotropy = model_structure$anisotropy[jj],
                            ...)
      
      model_structure$anis_rotate[jj] <- mod_fit$coefficients$spcov[['rotate']]
      model_structure$anis_scale[jj] <- mod_fit$coefficients$spcov[['scale']]
      
      model_structure$AICc[jj] <- spmodel::AICc(mod_fit)
      model_structure$convergence[jj] <- mod_fit$optim$convergence
      model_structure$df[jj] <- mod_fit$npar
      model_structure$mspe[jj] <- spmodel::loocv(mod_fit)
      
    }
  
  return(model_structure)
  
}


# Analysis
crs_by_region <- data.frame(region = c("AI", "GOA", "EBS"),
                            utmcrs = c("EPSG:32660", "EPSG:32605", "EPSG:32602"),
                            aeacrs = "EPSG:3338")

for(ii in 1:nrow(crs_by_region)) {
  
  aicc_table <- data.frame()
  
  bottom_dat <- readRDS(here::here("data", crs_by_region$region[ii], paste0("cv_data_", crs_by_region$region[ii], ".rds"))) |>
    sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), 
                 crs = crs_by_region$utmcrs[ii]) |>
    dplyr::filter(CAST == "bottom")
  
  unique_years <- sort(unique(bottom_dat$YEAR))
  
  for(jj in 1:length(unique_years)) {
    
    message("Region: ", crs_by_region$region[ii], ", Year: ", unique_years[jj])
    
    aicc_table <- fit_2d_spmod(data = dplyr::filter(bottom_dat, YEAR == unique_years[jj]),
                               spcov_type = c("exponential", "circular", "gaussian", "spherical", "matern"),
                               model_formula = c(TEMPERATURE ~ 1, 
                                                 TEMPERATURE ~ GEAR_DEPTH, 
                                                 TEMPERATURE ~ GEAR_DEPTH + I(GEAR_DEPTH^2), 
                                                 TEMPERATURE ~ I(log(GEAR_DEPTH))),
                               anisotropy = c(TRUE, FALSE), 
                               estmethod = "ml") |>
      dplyr::mutate(YEAR = unique_years[jj])
      dplyr::bind_rows(aicc_table)
      
      saveRDS(object = aicc_table,  
              file = here::here("output", crs_by_region$region[ii], paste0("2d_interp_aicc_", crs_by_region$region[ii], "_", unique_years[jj], ".rds")))
      
  }
  
}


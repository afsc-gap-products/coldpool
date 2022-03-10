#' Function to make loocv summary tables.
#'
#' Reads in output from leave-one-out cross-validation and makes tables summarizing mean, minimum, and maximum annual prediction error, and frequency of interpolation method being selected as the best.
#' 
#' @param sel_paths File paths to loocv output files
#' @param interp_variable Name of the variable to calculate summary statistics for (i.e., "gear_temperature")
#' @export

make_loocv_summary <- function(sel_paths = list.files("./output/loocv", full.names = TRUE),
                               interp_variable = "gear_temperature") {
  
  interp_variable <- tolower(interp_variable)
  
  sel_paths <- sel_paths[grep(interp_variable, sel_paths)]
  
  for(i in 1:length(sel_paths)) {
    if(i == 1) {
      sel_loocv <- read.csv(sel_paths[i], 
                            stringsAsFactors = FALSE)
    } else {
      sel_loocv <- rbind(sel_loocv, 
                         read.csv(sel_paths[i], stringsAsFactors = FALSE))
    }
  }
  
  names(sel_loocv) <- toupper(names(sel_loocv))
  
  sel_loocv <- sel_loocv %>%
    dplyr::mutate(YEAR = floor(CRUISE/100)) %>%
    dplyr::select(-CRUISE)
  
  id_cols <- c("YEAR", "STATIONID", "TRANSFORM.VARS")
  id_cols <- id_cols[which(id_cols %in% names(sel_loocv))]
  
  sel_loocv <- reshape2::melt(sel_loocv, 
                              id.vars = id_cols)
  
  all_rmse <- sel_loocv %>% 
    dplyr::group_by(YEAR, variable) %>%
    dplyr::summarise(rmse = mean(value))
  
  best_rmse <- inner_join(sel_loocv %>% 
                            dplyr::group_by(YEAR, variable) %>%
                            dplyr::summarise(rmse = mean(value)), 
                          sel_loocv %>% 
                            dplyr::group_by(YEAR, variable) %>%
                            dplyr::summarise(rmse = mean(value)) %>%
                            dplyr::ungroup() %>%
                            dplyr::group_by(YEAR) %>%
                            dplyr::summarise(rmse = min(rmse))) %>%
    dplyr::mutate(best = TRUE)
  
  
  best_freq <- as.data.frame(table(best_rmse$variable)) %>%
    dplyr::rename(variable = Var1)
  
  print(paste0("Writing results tables to ./output/method_rmse_", interp_variable, ".csv"))
  # write.csv(best_freq,
  #           file = paste0("./output/best_method_frequency_", interp_variable, ".csv"),
  #           row.names = FALSE)
  
  best_rmse <- sel_loocv %>% 
    dplyr::left_join(best_rmse)
  
  method_rmse <- all_rmse %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(mean = mean(rmse),
                     sd = sd(rmse),
                     min = min(rmse),
                     max = max(rmse))
  
  method_rmse <- method_rmse %>%
    dplyr::inner_join(all_rmse, 
                      by = c("variable" = "variable", 
                             "max" = "rmse")) %>%
    dplyr::rename(max_year = YEAR) %>%
    dplyr::inner_join(
  method_rmse %>%
    dplyr::inner_join(all_rmse, 
                      by = c("variable" = "variable", 
                             "min" = "rmse")) %>%
    dplyr::rename(min_year = YEAR))
  
  method_rmse <- method_rmse %>%
    dplyr::mutate(mean = round(mean, 3),
                  sd = round(sd, 3),
                  min = round(min, 3),
                  max = round(max, 3)) %>%
    dplyr::inner_join(best_freq)
  
  write.csv(method_rmse,
            file = paste0("./output/method_rmse_", interp_variable, ".csv"),
            row.names = FALSE)
  
}

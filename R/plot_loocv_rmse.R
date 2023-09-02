#' Function to make violin plots of MSE
#'
#' Reads in output from leave-one-out cross-validation and makes a violin plot of prediction error and RMSPE. If make_plot is TRUE, writes a png file. Returns a ggplot object.
#' 
#' @param sel_paths File paths to loocv output files
#' @param y_lab Y axis label as a character vector (i.e. "RSPE"). Default = NULL
#' @param make_plot Should a plot be generated
#' @param sel_var Character vector indicating what the variable name should be (used to name plot that is written to output)
#' @param by_cruise Logical. Should panels be divided by cruise? Default = FALSE plots panels by year.
#' @param suffix Text to append to a plot file name.
#' @param fig_res Resolution for the figure.
#' @export

plot_loocv_rmse <- function(sel_paths = dir("./output/loocv", full.names = TRUE),
                            y_lab = NULL,
                            make_plot = TRUE,
                            sel_var = NA,
                            by_cruise = FALSE,
                            suffix = "",
                            fig_res = 600) {
  
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
  
  # Plot by year instead of cruise
  if(!by_cruise) {
    sel_loocv$CRUISE <- floor(sel_loocv$CRUISE/100) 
  }
  
  id_cols <- c("CRUISE", "STATIONID", "TRANSFORM.VARS")
  id_cols <- id_cols[which(id_cols %in% names(sel_loocv))]
  
  sel_loocv <- reshape2::melt(sel_loocv, 
                              id.vars = id_cols)
  
  all_rmse <- sel_loocv |> 
    dplyr::group_by(CRUISE, variable) |>
    dplyr::summarise(rmse = mean(value))
  
  best_rmse <- inner_join(sel_loocv |> 
                            dplyr::group_by(CRUISE, variable) |>
                            dplyr::summarise(rmse = mean(value)), 
                          sel_loocv |> 
                            dplyr::group_by(CRUISE, variable) |>
                            dplyr::summarise(rmse = mean(value)) |>
                            dplyr::ungroup() |>
                            dplyr::group_by(CRUISE) |>
                            dplyr::summarise(rmse = min(rmse))) |>
    dplyr::mutate(best = TRUE)
  
  best_rmse <- sel_loocv |> 
    dplyr::left_join(best_rmse)
  
  best_rmse$best[is.na(best_rmse$best)] <- FALSE
  
    out_plot <- ggplot() + geom_violin(data = best_rmse, 
                                       aes(x = variable, 
                                           y = value, 
                                           fill = best)) + 
      geom_point(data = all_rmse, 
                 aes(x = variable, 
                     y = rmse), 
                 size = rel(1)) +
      scale_y_continuous(name = y_lab, 
                         limits = c(0, max(best_rmse$value)*1.02), 
                         expand = c(0,0)) +
      scale_x_discrete(name = "Interpolation method") +
      scale_fill_manual(values = c("white", "red")) +
      facet_wrap(~CRUISE) +
      ggthemes::theme_few() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, 
                                       vjust = 0.5, 
                                       hjust = 1))
  
  if(make_plot) {
    if(length(unique(all_rmse$CRUISE)) > 6) {
      fig_height <- 120
    } else if(length(unique(all_rmse$CRUISE)) <= 6 & length(unique(all_rmse$CRUISE)) > 3) {
      fig_height <- 90
    } else {
      fig_height <- 50
    }
    
    png(file = paste0("./plots/RSPE_violin_", sel_var, suffix, ".png"), 
        width = 190, 
        height = fig_height, 
        units = "mm", 
        res = fig_res)
    print(out_plot)
    dev.off()
  }
  
  return(out_plot)
  
}
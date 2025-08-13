#' Multi-panel map theme
#' @export

theme_multi_map <- function() {
  return(theme_base() %+replace%
           theme(panel.background = element_blank(),
                 panel.border = element_rect(color = "black", fill = NA),
                 panel.grid = element_blank(),
                 plot.title = element_text(size = 9, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                 axis.title = element_blank(),
                 axis.text = element_text(size = 9),
                 legend.position = "none",
                 plot.background = element_rect(fill = NA, color = NA)))
}



#' Tech memo figure without legend
#' @export

theme_tm_no_legend <- function() {
  theme_bw() %+replace%
    theme(legend.title = element_blank(),
          panel.border = element_rect(color = "black", fill = NA),
          legend.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"),
          axis.text = element_text(size = 8, color = "black"),
          plot.background = element_rect(fill = NA, color = NA))
}



#' Multi-panel map theme with blue strip
#' 
#' Theme for multipanel maps with blue strip
#' 
#' @export

theme_multi_map_blue_strip <- function() {
  theme_bw() %+replace%
    theme(axis.title = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 9,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = "#0055a4"))
}


#' Multi-panel map theme with blue strip
#' 
#' Theme for multipanel maps with blue strip
#' 
#' @export

theme_timeseries_blue_strip <- function() {
  theme_bw() %+replace%
    theme(axis.title = element_text(color = "black", face = "bold"),
          axis.text.y = element_text(color = "black"),
          axis.text.x = element_text(color = "black", angle = 90, vjust = 0.5),
          axis.ticks = element_line(color = "black"),
          panel.grid.major = element_line(color = "grey92"),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 9,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = "#0055a4"))
}
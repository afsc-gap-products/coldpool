#' Custom fermenter fill scale for ggplot2
#'
#' A fermenter scale that accepts custom color palettes.
#'
#' @param pal A character vector of colors to be used for the palette
#' @param na.value Color for NA values
#' @param guide Type of guide for ggplot2::binned_scale to use
#' @param aesthetics ggplot2 aesthetic type for the scale (e.g. "fill")
#' @param ... Addditional arguments passed to ggplot2::binned_scale
#' @export

scale_fill_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "fill", ...) {
  ggplot2::binned_scale("fill", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, guide = guide, ...)
}
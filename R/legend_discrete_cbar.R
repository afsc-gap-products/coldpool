#' Discrete continuous bar
#'
#' Generate a continuous bar plot using ggplot functions
#'
#' @param breaks Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
#' @param palette Character vector indicating the name of the RColorBrewer palette to use. Alternatively, can pass a vector of colors to the colors argument.
#' @param colors A vector of colors
#' @export

legend_discrete_cbar = function(
    breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
    palette = "Greys", #
    direction = 1, # Flip colors? Can be 1 or -1
    colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette),
    spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
    border_color = NA, # NA = no border color
    legend_title = NULL,
    legend_direction = "horizontal", # Can be "horizontal" or "vertical"
    font_size = 5,
    expand_size.x = 1,
    expand_size.y = 1,# Controls spacing around legend plot
    expand.y = 1,
    expand.x = 1,
    spacing_scaling = 1, # Multiplicative factor for label and legend title spacing
    width = 0.1, # Thickness of color bar
    triangle_size = 0.1, # Relative width of +-Inf triangles
    title_pos = NULL,
    text.angle = NULL,
    text.vjust = NULL,
    text.hjust = NULL,
    text.color = "black",
    neat.labels = FALSE,
    font.family = "serif"
) {
  require(ggplot2)
  if (!(spacing %in% c("natural", "constant"))) stop("spacing must be either 'natural' or 'constant'")
  if (!(direction %in% c(1, -1))) stop("direction must be either 1 or -1")
  if (!(legend_direction %in% c("horizontal", "vertical"))) stop("legend_direction must be either 'horizontal' or 'vertical'")
  breaks = as.numeric(breaks)
  new_breaks = sort(unique(breaks))
  if (any(new_breaks != breaks)) warning("Wrong order or duplicated breaks")
  breaks = new_breaks
  if (class(colors) == "function") colors = colors(length(breaks) - 1)
  if (length(colors) != length(breaks) - 1) stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
  if (!missing(colors)) warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")
  
  if (direction == -1) colors = rev(colors)
  
  inf_breaks = which(is.infinite(breaks))
  if (length(inf_breaks) != 0) breaks = breaks[-inf_breaks]
  plotcolors = colors
  
  n_breaks = length(breaks)
  
  labels = breaks
  
  if (spacing == "constant") {
    breaks = 1:n_breaks
  }
  
  r_breaks = range(breaks)
  
  cbar_df = data.frame(stringsAsFactors = FALSE,
                       y = breaks,
                       yend = c(breaks[-1], NA),
                       color = as.character(1:n_breaks)
  )[-n_breaks,]
  
  xmin = 1 - width/2
  xmax = 1 + width/2
  
  cbar_plot = ggplot(cbar_df, aes(xmin=xmin, xmax = xmax, ymin = y * expand.y, ymax = yend * expand.y, fill = factor(color, levels = 1:length(colors)))) +
    geom_rect(show.legend = FALSE,
              color=border_color)
  
  if (any(inf_breaks == 1)) { # Add < arrow for -Inf
    firstv = breaks[1]
    polystart = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-1]
    cbar_plot = cbar_plot +
      geom_polygon(data=polystart, aes(x=x, y=y * expand.y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[1],
                   color=border_color)
  }
  if (any(inf_breaks > 1)) { # Add > arrow for +Inf
    lastv = breaks[n_breaks]
    polyend = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-length(plotcolors)]
    cbar_plot = cbar_plot +
      geom_polygon(data=polyend, aes(x=x, y=y * expand.y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[length(colors)],
                   color=border_color)
  }
  
  if(is.null(text.angle)) {
    text.angle <- 0
  }
  
  if(is.null(text.hjust)) {
    text.hjust <- 0.5
  }
  
  if(is.null(text.vjust)) {
    text.vjust <- 0.5
  }
  
  if (legend_direction == "horizontal") { #horizontal legend
    mul = 1
    x = xmin
    xend = xmax
    cbar_plot = cbar_plot + coord_flip()
    angle = 0
    if(xmax > 0) {
      legend_position = xmax + 0.1 * spacing_scaling
    } else {
      legend_position = xmax + -0.1 * spacing_scaling
    }
    
  } else { # vertical legend
    mul = -1
    x = xmax
    xend = xmin
    angle = -90
    legend_position = xmax + 0.2 * spacing_scaling
  }
  
  if(neat.labels) {
    labels <- format(labels, nsmall = 1)
  }
  
  # print(c(min(x - 0.05 * mul * spacing_scaling), max(x * mul * spacing_scaling)))
  cbar_plot = cbar_plot +
    geom_segment(data = data.frame(y = breaks * expand.y,
                                   yend = breaks * expand.y),
                 aes(y=y,
                     yend=yend),
                 x = x - 0.05 * mul * spacing_scaling,
                 xend = xend,
                 inherit.aes = FALSE,
                 color = text.color) +
    annotate(geom = 'text',
             x = x - 0.1 * mul * spacing_scaling,
             y = breaks * expand.y,
             label = labels,
             angle = text.angle,
             hjust = text.hjust,
             vjust = text.vjust,
             size = font_size,
             color = text.color,
             family = font.family) +
    scale_x_continuous(expand = c(expand_size.x, expand_size.x)) +
    scale_y_continuous(expand = c(expand_size.y, expand_size.y)) +
    scale_fill_manual(values=plotcolors) +
    theme_void()
  
  if(is.null(title_pos)) {
    title_pos <- mean(r_breaks)
  }
  
  if (!is.null(legend_title)) { # Add legend title
    cbar_plot = cbar_plot +
      annotate(geom = 'text', x = legend_position, y = title_pos,
               label = legend_title,
               angle = angle,
               size = font_size,
               family = font.family)
  }
  
  return(cbar_plot)
}
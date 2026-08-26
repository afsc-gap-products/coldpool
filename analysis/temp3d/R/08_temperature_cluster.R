# Temperature EOF
library(coldpool)
library(cowplot)
library(akgfmaps)
library(ggdendro)
library(ggplot2)

goa_bt <- terra::unwrap(readRDS(file = here::here("output", "GOA_bt.rds")))
goa_bt <- goa_bt[[!(names(goa_bt) == "2001")]]

goa_sst <- terra::unwrap(readRDS(file = here::here("output", "GOA_sst.rds")))
goa_sst <- goa_sst[[!(names(goa_sst) == "2001")]]

ai_bt <- terra::unwrap(readRDS(file = here::here("output", "AI_bt.rds")))

ai_sst <- terra::unwrap(readRDS(file = here::here("output", "AI_sst.rds")))

make_temperature_dendro <- function(
    x, 
    scale_vars = TRUE, 
    dist_method  = "euclidean", 
    hclust = "average",
    legend_title = expression('BT'*'('*degree*'C)'),
    n_clust = NULL,
    y_min = -100) {
  
  mean_temp <- terra::global(x, mean, na.rm = TRUE)
  mean_temp$label <- rownames(mean_temp)
  
  clust_results <- coldpool::cluster_spatraster(x, hclust = hclust)
  
  clust_heights <- data.frame(
    n_clust = 1:length(clust_results$clust$height),
    height = sort(clust_results$clust$height, decreasing = TRUE)
  )
  
  cut_height <- NA
  
  if(!is.null(n_clust)) {
    clust_groups <- cutree(clust_results$clust, k = n_clust)
    cut_height <- mean(clust_heights$height[(n_clust-1):n_clust])
  }
  
  clust_results <- ggdendro::dendro_data(clust_results$clust)
  
  clust_results$labels <- 
    dplyr::inner_join(
      clust_results$labels,
      mean_temp,
      by = "label"
  )
  
  clust_results$labels$group <- as.factor(clust_groups[match(clust_results$labels$label, names(clust_groups))])
  
  p_dendro <- 
    ggplot() +
    geom_segment(data = ggdendro::segment(clust_results), 
                 aes(x = x, y = y, xend = xend, yend = yend)) +
    # geom_text(data = clust_results$labels, 
    #           aes(x = x, y = y, label = label, color = mean), 
    #           hjust = 1, angle = 90, size = 3.5) +
    geom_text(data = clust_results$labels, 
              aes(x = x, y = y, label = paste0("(", group, ") ", label), color = mean), 
              hjust = 1, angle = 90, size = 2.7) +
    geom_hline(linetype = 2, yintercept = cut_height) +
    ylim(y_min, max(ggdendro::segment(clust_results)$yend) + 10) +
    scale_color_viridis_c(name = legend_title, option = "H") +
    theme_dendro()
  
  p_scree <- 
    ggplot(data = clust_heights,
                    mapping = aes(x = n_clust, y = height)) +
    geom_path(color = "blue") +
    geom_point(color = "blue") +
    scale_x_continuous(name = "Clusters", breaks = clust_heights$n_clust) +
    scale_y_continuous(name = "Height") +
    ggtitle(legend_title) +
    theme_bw()

  
  return(list(p_dendro = p_dendro, p_scree = p_scree, clust_heights = clust_heights))
  
}

# Make dendrogram and scree plots

p_goa_sst_dendro <- 
  make_temperature_dendro(
    x = goa_sst,
    legend_title = expression('SST'*'('*degree*'C)'),
    hclust = "average",
    y_min = -250,
    n_clust = 3
  )

p_goa_bt_dendro <- 
  make_temperature_dendro(
    x = goa_bt,
    legend_title = expression('BT'*'('*degree*'C)'),
    hclust = "average",
    y_min = -200,
    n_clust = 6
  )

p_ai_sst_dendro <- 
  make_temperature_dendro(
    x = ai_sst,
    legend_title = expression('SST'*'('*degree*'C)'),
    hclust = "average",
    n_clust = 4
  )

p_ai_bt_dendro <- 
  make_temperature_dendro(
    x = ai_bt,
    legend_title = expression('BT'*'('*degree*'C)'),
    hclust = "average",
    n_clust = 4
  )



# Examine scree plots to determine the number of significant clusters for each layer

cowplot::plot_grid(
  p_goa_sst_dendro$p_scree, # 3 clusters for SST
  p_goa_bt_dendro$p_scree, # 6 clusters for BT
  nrow = 2
)

cowplot::plot_grid(
  p_ai_sst_dendro$p_scree, # 4 clusters for SST
  p_ai_bt_dendro$p_scree, # 4 clusters for BT
  nrow = 2
)


# Make plots
p_goa_dendro <- 
  cowplot::plot_grid(
    p_goa_sst_dendro$p_dendro +
       theme(legend.text = element_text(size = 9),
             legend.title = element_text(size = 9)),
    p_goa_bt_dendro$p_dendro +
      theme(legend.text = element_text(size = 9),
            legend.title = element_text(size = 9)),
    labels = c("A", "B"),
    nrow = 2
  )

p_ai_dendro <- 
  cowplot::plot_grid(
    p_ai_sst_dendro$p_dendro +
      theme(legend.text = element_text(size = 9),
            legend.title = element_text(size = 9)),
    p_ai_bt_dendro$p_dendro +
      theme(legend.text = element_text(size = 9),
            legend.title = element_text(size = 9)),
    labels = c("A", "B"),
    nrow = 2
  )

png(filename = here::here("plots", "GOA", "goa_cluster_dendro.png"), 
    width = 169,
    height = 120, units = "mm", res = 300)
print(p_goa_dendro)
dev.off()

png(filename = here::here("plots", "AI", "ai_cluster_dendro.png"), 
    width = 169,
    height = 120, units = "mm", res = 300)
print(p_ai_dendro)
dev.off()

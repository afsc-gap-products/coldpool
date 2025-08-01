#' Hierarchical clustering on SpatRaster objects
#'
#' @description
#' This function performs hierarchical clustering on a terra 'SpatRaster' object. It is intended for use on rasters where each layer represents an observation in time (e.g. annual temperature observations).
#'
#' @details
#' The analysis pipeline consists of several steps:
#' 1. The input `x` is converted to a matrix and any rows with `NA` values are removed.
#' 2. If `scale_vars = TRUE`, each row is standardized to have a mean of 0 and a
#'    standard deviation of 1. This focuses the analysis on the pattern of
#'    the values over time, rather than their magnitude.
#' 3. A distance matrix is calculated between the time points using the method
#'    specified by `dist_method`.
#' 4. Hierarchical clustering is performed using the method specified by `hclust_method`.
#' 5. A dendrogram of the clustering result is plotted.
#'
#' @param x A numeric data frame or matrix where rows represent individual
#'   observations (e.g., cities, subjects) and columns represent variables measured
#'   over time (e.g., months, days).
#' @param scale_vars A logical value. If `TRUE` (the default), the data for each
#'   row is standardized (scaled) before clustering.
#' @param dist_method The distance measure to be used by `dist()`. Defaults to
#'   `"euclidean"`. See help documentation for `dist()` for additional options.
#' @param hclust_method The agglomeration method to be used by `hclust()`.
#'   Defaults to `"complete"` for complete linkage clustering. See help documentation for `hclust()` for additional options.
#' @return A list containing three objects:
#'   \item{mat}{The processed (and possibly scaled and transposed) matrix used for the analysis.}
#'   \item{dist}{The distance object (`dist`) calculated from the processed matrix.}
#'   \item{clust}{The hierarchical clustering object (`hclust`) produced by the analysis.}
#'
#' @export
#'
#' @examples
#' # Hierarchical clustering on EBS shelf bottom temperature SpatRaster
#' 
#' ebs_bt <- terra::unwrap(coldpool::ebs_bottom_temperature)
#' 
#' cluster_spatraster(
#'   x = ebs_bt,
#'   scale_vars = TRUE,
#'   dist_method = "euclidean",
#'   hclust_method = "complete"
#' )

cluster_spatraster <- 
  function(x, scale_vars = TRUE, dist_method = "euclidean", hclust_method = "complete") {
    
    # Convert to matrix, remove NAs, apply row standardization
    x_mat <- 
      x |>
      as.matrix() |> 
      na.omit()
    
    if(scale_vars) {
      
      x_mat <- 
        x_mat |>
        apply(
          MARGIN = 1,
          FUN = scale
        )
      
      # Assign names
      rownames(x_mat) <- names(x)
      
    } else {
      
      x_mat <- t(x_mat)
      
    }
    
    # Calculate Euclidean distance matrix
    x_dist <- 
      x_mat |> 
      dist(method = dist_method)
    
    # Complete linkage clustering
    x_clust <- hclust(x_dist, method = hclust_method)
    
    plot(x_clust)
    
    return(list(mat = x_mat, dist = x_dist, clust = x_clust))
    
  }
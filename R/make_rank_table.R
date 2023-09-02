#' Make Kendall's tau ranking tables
#' 
#' Function to make Kendall's tau correlation tables for tech memo.
#' 
#' @param x Data frame contraining values.
#' @noRd

make_rank_table <- function(x) {
  
  temp_rank <- x |>
    dplyr::group_by(label) |>
    dplyr::summarise(year, rank = rank(value)) |>
    tidyr::pivot_wider(id_cols = "label", 
                       names_from = "year", 
                       values_from = "rank")
  
  pairs_df <- as.data.frame(t(combn(1:nrow(temp_rank), m = 2)))
  pairs_df$tau <- numeric(length = nrow(pairs_df))
  
  for(ii in 1:nrow(pairs_df)) {
    pairs_df$tau[ii] <- round(cor(as.numeric(temp_rank[pairs_df[ii,1],2:ncol(temp_rank)], use = "complete.obs"), 
                                  as.numeric(temp_rank[pairs_df[ii,2],2:ncol(temp_rank)],  use = "complete.obs"),
                                  method = "kendall"), 3)
  }
  
  pairs_df <- pairs_df |>
    dplyr::inner_join(data.frame(label_1 = temp_rank$label,
                                 V1 = 1:nrow(temp_rank))) |>
    dplyr::inner_join(data.frame(label_2 = temp_rank$label,
                                 V2 = 1:nrow(temp_rank))) |>
    dplyr::select(-V1, -V2) |>
    tidyr::pivot_wider(id_cols = "label_2", names_from = "label_1", values_from = "tau")
  
  return(pairs_df)
  
}
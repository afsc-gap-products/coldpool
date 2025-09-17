


region <- "goa"

model_summary <- readRDS(here::here("output", region, paste0(region, "_splm_interp_aicc.rds")))


best_model_tally <- 
  model_summary |>
  dplyr::filter(best == TRUE, layer == "bottom") |>
  dplyr::group_by(spcov_type, formula, anisotropy, layer) |>
  dplyr::summarise(n = n()) |>
  dplyr::arrange(-n) |>
  dplyr::mutate(region = toupper(region))


best_model_tally |>
  dplyr::inner_join(model_summary) |>
  dplyr::group_by(spcov_type, formula, anisotropy, layer, region) |>
  dplyr::summarise(mspe = mean(mspe))



ggplot() +
  geom_boxplot(data = 
                 best_model_tally |>
                 dplyr::inner_join(model_summary),
               mapping = aes(x = paste0(spcov_type, dplyr::if_else(anisotropy, "-anis.", "")),
                             y = rmspe)) +
  scale_x_discrete(name = "Model") +
  scale_y_continuous(name = "Annual MSPE") +
  theme_bw()

# Cold pool area comparison

# Load cold pool calculations from Bob Lauth ----
lauth_cpa_df <- read.csv(file = here::here("data", 
                                           "cpa_areas2019.csv"),
                         stringsAsFactors = FALSE) %>%
  dplyr::select(YEAR, 
                AREA_SUM_KM2_LTE2) %>%
  dplyr::rename(year = YEAR,
                `Old IDW` = AREA_SUM_KM2_LTE2)

annual_cpa_df <- read.csv(file = here::here("output", 
                                            "estimated_cpa", 
                                            "cpa_out.csv"),
                          stringsAsFactors = FALSE) %>%
  dplyr::filter(year > 1981)

annual_cpa_df <- annual_cpa_df[,1:11]

combined_cpa_df <- annual_cpa_df %>% 
  melt(id.vars = "year") %>%
  dplyr::bind_rows(lauth_cpa_df %>% 
                     melt(id.vars = "year"))

png(file = here::here("plots", "cpa_by_year.png"), width = 81, height = 81, units = "mm", res = fig_res)
print(ggplot(data = combined_cpa_df,
       aes(x = year, 
           y = value, 
           color = variable)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(name = "Year") +
  scale_color_tableau(palette = "Tableau 20") +
  scale_y_continuous(name = expression("Cold Pool Area"~(km^2))) +
  theme_bw())
dev.off()

cpa_change_df <- annual_cpa_df %>% 
  melt(id.vars = "year") %>%
  dplyr::inner_join(lauth_cpa_df) %>%
  dplyr::mutate(rel_diff = 100*(value - `Old IDW`)/`Old IDW`,
                abs_diff = value - `Old IDW`)

ggplot(data = cpa_change_df,
       aes(x = year,
           y = rel_diff,
           color = variable)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(name = "Relative Difference (%)") +
  scale_x_continuous(name = "Year") +
  scale_color_tableau()

ggplot(data = cpa_change_df,
       aes(x = year,
           y = abs_diff,
           color = variable)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(name = expression("Absolute Difference"~(km^2))) +
  scale_x_continuous(name = "Year") +
  scale_color_tableau()

cpa_change_df %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(mean_abs_diff = mean(abs_diff),
                   mean_rel_diff = mean (rel_diff),
                   median_rel_diff = median(rel_diff))

ggplot(data = cpa_change_df, 
       aes(x = variable,
           y = rel_diff)) + 
  geom_boxplot()

ggplot(data = cpa_change_df, 
       aes(x = variable,
           y = rel_diff)) + 
  geom_boxplot() +
  scale_y_continuous(name = "Relative Difference (%)")

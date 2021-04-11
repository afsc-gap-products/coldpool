# Cold pool area comparison

# Set up method labels 
method_labels_df <- data.frame(method = c("BES",
                                          "CIR",
                                          "EXP",
                                          "IDW",
                                          "IDW_NMAX4",
                                          "MAT",
                                          "NN",
                                          "SPH",
                                          "STE",
                                          "TPS",
                                          "OLD IDW"),
                               label = c("OK-BES",
                                         "OK-CIR",
                                         "OK-EXP",
                                         "IDW",
                                         "IDW (Max 4)",
                                         "OK-MAT",
                                         "NN",
                                         "OK-SPH",
                                         "OK-STE",
                                         "TPS",
                                         "Historical"))

# Set up Manual plot colors 
color_vec <- c("OK-BES" = "#4E79A7",
               "OK-CIR" = "#A0CBE8",
               "OK-EXP" = "#F28E2B",
               "IDW" = "#B6992D",
               "IDW (Max 4)" = "#F1CE63",
               "OK-MAT" = "#8CD17D",
               "NN" = "#499894",
               "OK-SPH" = "#FFBE7D",
               "OK-STE" = "#59A14F",
               "TPS" = "#E15759",
               "Historical" = "black")

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

# Select cold pool area calculations for less than or equal to to 2 degrees
annual_cpa_df <- annual_cpa_df[,c(1, which(stringr::str_detect(names(annual_cpa_df), "_lte2")))]

combined_cpa_df <- annual_cpa_df %>% 
  melt(id.vars = "year") %>%
  dplyr::bind_rows(lauth_cpa_df %>% 
                     melt(id.vars = "year")) %>%
  dplyr::mutate(method = stringr::str_remove(variable, "_lte2") %>% 
                  toupper()) %>%
  dplyr::inner_join(method_labels_df)

png(file = here::here("plots", "cpa_by_year.png"), width = 169, height = 81, units = "mm", res = fig_res)
print(ggplot() +
  geom_point(data = combined_cpa_df,
             aes(x = year, 
                 y = value, 
                 color = label)) +
  geom_line(data = combined_cpa_df,
            aes(x = year, 
                y = value, 
                color = label)) +
    geom_point(data = combined_cpa_df %>%
                 dplyr::filter(method == "OLD IDW"),
               aes(x = year, 
                   y = value)) +
    geom_line(data = combined_cpa_df %>%
                dplyr::filter(method == "OLD IDW"),
              aes(x = year, 
                  y = value)) +
  scale_x_continuous(name = "Year") +
  scale_color_manual(name = "Method", 
                     values = color_vec) +
  scale_y_continuous(name = expression("Cold Pool Area"~(km^2))) +
  theme_bw() +
    theme(legend.title = element_blank()))
dev.off()

cpa_change_df <- annual_cpa_df %>% 
  melt(id.vars = "year") %>%
  dplyr::inner_join(lauth_cpa_df) %>%
  dplyr::mutate(rel_diff = 100*(value - `Old IDW`)/`Old IDW`,
                abs_diff = value - `Old IDW`) %>%
  dplyr::mutate(method = stringr::str_remove(variable, "_lte2") %>% 
                  toupper()) %>%
  dplyr::inner_join(method_labels_df)

ggplot(data = cpa_change_df,
       aes(x = year,
           y = rel_diff,
           color = label)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(name = "Relative Difference (%)") +
  scale_x_continuous(name = "Year") +
  scale_color_manual(name = "Method",
                     values = color_vec) +
  theme_bw() +
  theme()

ggplot(data = cpa_change_df,
       aes(x = year,
           y = abs_diff,
           color = label)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous(name = expression("Absolute Difference"~(km^2))) +
  scale_x_continuous(name = "Year") +
  scale_color_manual(name = "Method",
                     values = color_vec) +
  theme_bw() +
  theme()

cpa_change_df %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(mean_abs_diff = mean(abs_diff),
                   mean_rel_diff = mean (rel_diff),
                   median_rel_diff = median(rel_diff))

ggplot(data = cpa_change_df, 
       aes(x = label,
           y = rel_diff)) + 
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  scale_y_continuous(name = "Relative Difference (%)") +
  scale_x_discrete(name = "Method") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))

ggplot(data = cpa_change_df, 
       aes(x = label,
           y = abs_diff)) + 
  geom_hline(yintercept = 0) +
  geom_boxplot() +
  scale_y_continuous(name = expression("Absolute Difference"~(km^2))) +
  scale_x_discrete(name = "Method") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1))


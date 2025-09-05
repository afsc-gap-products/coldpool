library(coldpool)
library(lubridate)

channel <- coldpool::get_connected(schema = "AFSC")

haul_dat <- RODBC::sqlQuery(
  channel = channel,
  query = "select h.*, c.survey_definition_id, c.year 
  from 
  gap_products.akfin_haul h,
  gap_products.akfin_cruise c 
  where c.survey_definition_id in (47, 52) 
  and h.cruisejoin = c.cruisejoin"
) |>
  dplyr::filter(!is.na(GEAR_TEMPERATURE_C))

sample_size_table <- haul_dat |> 
  dplyr::inner_join(
    data.frame(
      SURVEY_DEFINITION_ID = c(47, 52),
      region = c("GOA", "AI")
    ),
    by = "SURVEY_DEFINITION_ID"
  ) |>
  dplyr::group_by(region, YEAR) |>
  dplyr::summarise(
    n = n(),
    max_depth = max(DEPTH_M, na.rm = TRUE),
    min_depth = min(DEPTH_M, na.rm = TRUE),
    min_dt = min(DATE_TIME_START),
    max_dt = max(DATE_TIME_START)
  ) |>
  dplyr::arrange(region, YEAR) |>
  dplyr::mutate(`Depth range` = paste0(min_depth, "-", max_depth),
                Dates = paste0(
                  lubridate::month(min_dt, abbr = TRUE, label = TRUE), " ", lubridate::day(min_dt),
                  "-",
                  lubridate::month(max_dt, abbr = TRUE, label = TRUE), " ", lubridate::day(max_dt)
                )) |>
  dplyr::select(
    Region = region,
    Year = YEAR,
    Hauls = n,
    Dates
  )

sample_size_table

# Exclude 1990 GOA and 1991 AI surveys because of small and very spatially unbalanced sample sizes

sample_size_table <- dplyr::filter(sample_size_table, Year >= 1993)

write.csv(
  sample_size_table, 
  file = here::here("plots", "depth_sample_size.csv"),
          row.names = FALSE
  )



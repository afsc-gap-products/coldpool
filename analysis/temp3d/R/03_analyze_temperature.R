# Explore plotting options
library(tidyterra)
library(coldpool)

bt_layers <- readRDS(here::here("output", paste0(region, "_bt.rds")))

ggplot() +
  geom_spatraster(data = bt_layers) +
  scale_fill_viridis_c(option = "H", na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  geom_spatraster(data = c(bt_layers-mean(bt_layers))/stdev(bt_layers)) +
  scale_fill_viridis_c(option = "H", na.value = NA) +
  facet_wrap(~lyr)


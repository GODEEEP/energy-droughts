# install.packages('import')
xfun::pkg_attach2("ggplot2")
import::from(readr, read_csv, write_csv)
import::from(
  dplyr, bind_rows, rename, inner_join, mutate, group_by, summarise, filter, between,
  select, rename, pull, left_join, slice, case_when, row_number, full_join, everything,
  rename_all
)
import::from(tidyr, pivot_longer, pivot_wider)
import::from(lubridate, with_tz, year, years, hour, hours, leap_year, ymd_hms, force_tzs, month, day, hour)
import::from(ggthemes, colorblind_pal)

y1 <- 2017
y2 <- 2057

# metadata
message("Reading metadata")
wind_config <- read_csv("data/tgw-gen/wind/eia_wind_configs.csv", show = FALSE, progress = FALSE)
solar_config <- read_csv("data/tgw-gen/solar/eia_solar_configs.csv", show = FALSE, progress = FALSE)
configs <- bind_rows(
  wind_config |> mutate(type = "wind"),
  solar_config |> mutate(type = "solar")
)

# generation data
message("Reading scenario data")
scenario <- "historical"
solar_fn_y1 <- sprintf("data/tgw-gen/solar/historical/solar_gen_cf_%s.csv", y1)
solar_fn_y2 <- sprintf("../future-energy-droughts/data/wind-solar/baseline-future/solar_gen_cf_%s.csv", y2)
wind_fn_y1 <- sprintf("data/tgw-gen/wind/historical/wind_gen_cf_%s.csv", y1)
wind_fn_y2 <- sprintf("../future-energy-droughts/data/wind-solar/baseline-future/wind_gen_cf_%s.csv", y2)

solar_list <- wind_list <- load_list <- list()

solar_wide <- bind_rows(
  read_csv(solar_fn_y1, show = FALSE, progress = FALSE),
  read_csv(solar_fn_y2, show = FALSE, progress = FALSE)
)
wind_wide <- bind_rows(
  read_csv(wind_fn_y1, show = FALSE, progress = FALSE),
  read_csv(wind_fn_y2, show = FALSE, progress = FALSE)
)

message("Formatting data")

solar_count <- solar_config |>
  # filter(nerc_region == 'WECC') |> # & ba != 'SWPP') |>
  group_by(ba) |>
  summarise(solar_num_sites = length(unique(plant_code)))
wind_count <- wind_config |>
  # filter(nerc_region == 'WECC') |># & ba != 'SWPP') |>
  group_by(ba) |>
  summarise(wind_num_sites = length(unique(plant_code)))
plant_counts <- solar_count |>
  full_join(wind_count, by = "ba")

bas <- plant_counts |>
  pull(ba) |>
  unique() |>
  sort()

solar_wind_ba_ave_list <- list()
for (bai in bas) {
  message("\t", bai)

  solar_config_ba <- solar_config |> filter(ba == bai)
  n_solar_plants_ba <- nrow(solar_config_ba)
  if (n_solar_plants_ba > 0) {
    solar_wide_ba <- solar_wide |> select(datetime, solar_config_ba |> pull(plant_code_unique))
    solar_ba <- solar_wide_ba |>
      pivot_longer(-datetime, names_to = "plant_code_unique") |>
      rename(datetime_utc = datetime) |>
      # convert to hour beginning
      mutate(datetime_utc = datetime_utc - hours(1)) |>
      inner_join(solar_config_ba, by = "plant_code_unique") |>
      mutate(gen_mw = system_capacity * value)

    solar_ba_ave <- solar_ba |>
      group_by(datetime_utc, ba) |>
      summarise(
        solar_gen_mw = sum(gen_mw),
        solar_capacity = sum(system_capacity), .groups = "drop"
      ) |>
      mutate(solar_cf = solar_gen_mw / solar_capacity) |>
      inner_join(solar_count, by = "ba")
  }

  wind_config_ba <- wind_config |> filter(ba == bai)
  n_wind_plants_ba <- nrow(wind_config_ba)
  if (n_wind_plants_ba > 0) {
    wind_plant_codes <- wind_config_ba |> pull(plant_code_unique)
    wind_wide_ba <- wind_wide |> select(datetime, wind_plant_codes[wind_plant_codes %in% colnames(wind_wide)])
    wind_ba <- wind_wide_ba |>
      pivot_longer(-datetime, names_to = "plant_code_unique") |>
      rename(datetime_utc = datetime) |>
      # convert to hour beginning
      mutate(datetime_utc = datetime_utc - hours(1)) |>
      inner_join(wind_config_ba, by = "plant_code_unique") |>
      mutate(gen_mw = system_capacity * value)

    wind_ba_ave <- wind_ba |>
      group_by(datetime_utc, ba) |>
      summarise(
        wind_gen_mw = sum(gen_mw),
        wind_capacity = sum(system_capacity), .groups = "drop"
      ) |>
      mutate(wind_cf = wind_gen_mw / wind_capacity) |>
      inner_join(wind_count, by = "ba")
  }


  solar_wind_ba_ave_list[[bai]] <-
    if (n_solar_plants_ba > 0 & n_wind_plants_ba > 0) {
      solar_ba_ave |>
        inner_join(wind_ba_ave, by = c("datetime_utc", "ba"))
    } else if (n_solar_plants_ba > 0 & n_wind_plants_ba == 0) {
      solar_ba_ave
    } else if (n_solar_plants_ba == 0 & n_wind_plants_ba > 0) {
      wind_ba_ave
    } else {
      NULL
    }
}
solar_wind_ba_ave <- bind_rows(solar_wind_ba_ave_list)

scaling_factor <- solar_wind_ba_ave |>
  group_by(ba) |>
  select(datetime_utc, ba, solar_cf, wind_cf) |>
  mutate(
    year = year(datetime_utc),
    datetime_utc = case_when(
      year(datetime_utc) == y1 ~ datetime_utc,
      year(datetime_utc) == y2 ~ datetime_utc - years(40)
    )
  ) |>
  pivot_wider(id_cols = c(datetime_utc, ba), values_from = c(solar_cf, wind_cf), names_from = year, names_sep = "-") |>
  mutate(
    year = year(datetime_utc),
    month = month(datetime_utc),
    day = day(datetime_utc),
    hour = hour(datetime_utc)
  ) |>
  # select(-datetime_utc) |>
  select(month, day, hour, ba, everything())

write_csv(scaling_factor |> select(-datetime_utc), sprintf("data/ba-cf-%s-%s.csv", y1, y2))


# sf1 = scaling_factor |>
#   mutate(wind_sf=ifelse(`wind_cf-2058`==0,1,`wind_cf-2018`/`wind_cf-2058`),
#          solar_sf=ifelse(`solar_cf-2058`==0,1,`solar_cf-2018`/`solar_cf-2058`)) |>
#   select(c(ba, month, day, hour, wind_sf, solar_sf)) |>
#   pivot_longer(-c(ba, month, day, hour))
#
# ggplot(sf1)+
#   geom_boxplot(aes(factor(month),value,group=month))+
#   geom_hline(yintercept=1)+
#   facet_wrap(~name)+
#   scale_y_continuous(limits=c(0.9,2), breaks=seq(0.9,1.9,by=.1), minor_breaks=NULL)+
#   theme_bw()

sf_hourly <- scaling_factor |>
  mutate(
    wind_sf = ifelse(`wind_cf-2017` == 0, 1, `wind_cf-2057` / `wind_cf-2017`),
    solar_sf = ifelse(`solar_cf-2017` == 0, 1, `solar_cf-2057` / `solar_cf-2017`)
  ) |>
  mutate(
    wind_sf = ifelse(wind_sf > 3, NA, wind_sf),
    solar_sf = ifelse(solar_sf > 3, NA, solar_sf)
  )

sf_heatwave <- sf_hourly |>
  filter(datetime_utc >= ymd_hms("2018-07-06 12:00:00") - hours(24 * 3.5) &
    datetime_utc <= ymd_hms("2018-07-06 12:00:00") + hours(24 * 3.5)) |>
  group_by(ba) |>
  summarise(
    wind_sf = mean(wind_sf, na.rm = T),
    solar_sf = mean(solar_sf, na.rm = T)
  )

sf_monthly <-
  sf_hourly |>
  group_by(ba, month) |>
  summarise(
    `Wind` = median(wind_sf, na.rm = T),
    `Solar` = median(solar_sf[solar_sf != 1], na.rm = T),
    .groups = "drop"
  ) |>
  pivot_longer(-c(ba, month), names_to = "type")

sf_monthly_ave <- sf_monthly |>
  group_by(ba, type) |>
  summarise(value = mean(value, na.rm = T), .groups = "drop")

sf_hourly_ave <- sf_monthly |>
  group_by(ba, type) |>
  summarise(value = mean(value, na.rm = T), .groups = "drop")

ggplot(sf_monthly) +
  geom_boxplot(aes(factor(month), value, group = month)) +
  geom_hline(yintercept = 1) +
  facet_wrap(~type, scales = "free_y") +
  # scale_y_continuous(limits=c(0.9,2), breaks=seq(0.9,1.9,by=.1), minor_breaks=NULL)+
  theme_bw() +
  labs(x = "Month", y = "BA Ave Scaling Factor 2058/2018")



sf_monthly |>
  filter(month == 7) |>
  ggplot() +
  geom_bar(aes(ba, value, fill = type), stat = "identity", position = "dodge") +
  geom_hline(yintercept = 1.0) +
  facet_wrap(~type, ncol = 2, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  coord_cartesian(ylim = c(.9, 1.1)) +
  labs(y = "BA Ave Scaling Factor 2058/2018", x = "BA") +
  scale_fill_manual("", values = colorblind_pal()(3)[-1]) +
  labs(title = "July 2018 Heat Wave Scaling Factors")

## chacking data from casey

ratios_annual_mean_ba <- read_csv("data/Annual_Mean_Ratios.csv", na = "-999.0") |>
  rename_all(~ c("ba", "load", "solar", "wind")) |>
  pivot_longer(-ba)
ratios_monthly_mean_ba <- read_csv("data/Monthly_Mean_Ratios.csv", na = "-999.0")
ratios_mean_ba <- ratios_monthly_mean_ba |>
  rename(ba = BA) |>
  group_by(ba) |>
  summarise(
    load = mean(Mean_Load_Ratio),
    wind = mean(Mean_Wind_Ratio),
    solar = mean(Mean_Solar_Ratio)
  ) |>
  pivot_longer(-ba, names_to = "type") |>
  mutate(type = factor(type, levels = c("solar", "wind", "load")))


ratios_mean_ba |> ggplot() +
  geom_bar(aes(ba, value, fill = type), stat = "identity", position = "dodge") +
  # geom_point(aes(ba, value), data=ratios_annual_mean_ba) +
  geom_hline(yintercept = 1.0) +
  facet_wrap(~type, nrow = 1, scales = "free_x") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_cartesian(ylim = c(.8, 1.1)) +
  labs(y = "BA Ave Scaling Factor 2058/2018") +
  scale_fill_manual(values = colorblind_pal()(4)[-1])

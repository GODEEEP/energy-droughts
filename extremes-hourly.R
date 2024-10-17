library(tidyverse)
import::from(ggthemes, colorblind_pal)

# daily gen and load data
gen_load <- read_csv("data/ba_solar_wind_load_historical_1-hour.csv", progress = F, show = F) |>
  mutate(
    datetime_local = with_tz(datetime_utc, timezone),
    wind_type = "onshore"
  ) |>
  filter(ba %in% c("BPAT", "CISO", "IPCO", "NWMT", "PACE", "PACW", "PNM", "PSCO", "WACM"))

extend8760 <- function(l, fns) {
  years <- fns |>
    basename() |>
    tools::file_path_sans_ext() |>
    strsplit("_") |>
    sapply("[", 4) |>
    as.numeric()
  for (y in years) {
    yeari <- which(years == y)
    if (leap_year(y)) {
      last_day <- l[[yeari]] |>
        tail(24) |>
        mutate(datetime = datetime + hours(24))
      l[[yeari]] <- bind_rows(l[[yeari]], last_day)
    }
  }
  l
}

osw_cap <- c("41141" = 705000, "40629" = 1020000, "40895" = 705000, "45095" = 1020000)
osw_fns <- list.files("data/bpa-osw/", full.names = T)
osw <- osw_fns |>
  map(read_csv, progress = F, show = F) |>
  extend8760(osw_fns) |>
  bind_rows() |>
  mutate(
    BPAT_OSW = osw_cap[1] * `41141` + osw_cap[2] * `40629` + osw_cap[3] * `40895`,
    BPAT_OSW = BPAT_OSW / sum(osw_cap[1:3]),
    PACW_OSW = `45095`
  ) |>
  select(datetime, BPAT_OSW, PACW_OSW) |>
  pivot_longer(-datetime, names_to = "ba", values_to = "wind_cf") |>
  rename(datetime_utc = datetime) |>
  mutate(
    datetime_local = with_tz(datetime_utc, "US/Pacific"),
    year = year(datetime_local),
    wind_gen_mwh = ifelse(ba == "BPAT", wind_cf * sum(osw_cap[1:3]), wind_cf * osw_cap[4]),
    wind_type = "offshore",
    timezone = "US/Pacific"
  )

gen_load <- gen_load |> bind_rows(osw)
bas <- unique(gen_load$ba)
ba_tzs <- gen_load |>
  group_by(ba) |>
  summarise(tz = timezone[1])

# number of days +/- around the 1-day extreme event
window <- 3

# read in max/min dates for each ba
extreme_days_list <- list()
for (ba in bas) {
  bai <- ba
  ba_tz <- ba_tzs |>
    filter(ba == bai) |>
    pull(tz)
  ev_days <- read_csv(sprintf("data/ev_dates/%s_Extreme_Days_1981_to_2018.csv", ba), progress = F, show = F) |>
    mutate(
      min_rank = rank(Coldest_Temp_F),
      max_rank = rank(-Hottest_Temp_F),
      Coldest_Day = as.POSIXct(force_tz(Coldest_Day, tz = ba_tz)),
      Hottest_Day = as.POSIXct(force_tz(Hottest_Day, tz = ba_tz))
    )

  min_day_list <- max_day_list <- list()
  for (d in 1:nrow(ev_days)) {
    min_day <- ev_days$Coldest_Day[d]
    min_temp <- ev_days$Coldest_Temp_F[d]
    min_rank <- ev_days$min_rank[d]
    min_day_list[[d]] <- tibble(
      type = "min", ba = ba, year = year(min_day), ev_date = min_day, ev_temp = min_temp,
      rank = min_rank, hours_since_ev = -(window * 24):((window + 1) * 24),
      datetime_local = seq.POSIXt(min_day - days(window), min_day + days(window + 1), by = "hours")
    )
    max_day <- ev_days$Hottest_Day[d]
    max_temp <- ev_days$Hottest_Temp_F[d]
    max_rank <- ev_days$max_rank[d]
    max_day_list[[d]] <- tibble(
      type = "max", ba = ba, year = year(max_day), ev_date = max_day, ev_temp = max_temp,
      rank = max_rank, hours_since_ev = -(window * 24):((window + 1) * 24),
      datetime_local = seq.POSIXt(max_day - days(window), max_day + days(window + 1), by = "hours")
    )
  }

  extreme_days_list[[ba]] <- bind_rows(min_day_list, max_day_list)
}
extreme_days <- bind_rows(extreme_days_list)
gen_load_ev <- extreme_days |>
  inner_join(gen_load, by = c("datetime_local", "ba", "year"))
gen_load_ev_10yr <- gen_load_ev |> filter(rank <= 4)
gen_load_ev_20yr <- gen_load_ev |> filter(rank <= 2)
gen_load_ev_daily <- gen_load_ev |>
  mutate(
    day = day(datetime_local),
    month = month(datetime_local)
  ) |>
  group_by(ba, type, ev_date, day) |>
  reframe(
    solar_cf_max = max(solar_cf),
    solar_max_hour = hours_since_ev[which.max(solar_cf)],
    wind_cf = mean(wind_cf),
    solar_cf = mean(solar_cf),
    load_cf = mean(load_cf),
    temp = mean(ev_temp),
    datetime_local = min(datetime_local),
    .groups = "drop"
  ) |>
  mutate(days_since_ev = as.numeric(difftime(datetime_local, ev_date, units = "days")))

# solar_cf_max = max(solar_cf),
# solar_max_hour = hours_since_ev[which.max(solar_cf)],

gen_load_ev_mean <- gen_load_ev |>
  mutate(day = day(datetime_local)) |>
  group_by(hours_since_ev, type, ba) |>
  summarise(
    wind_cf_mean = mean(wind_cf, na.rm = T),
    solar_cf_mean = mean(solar_cf, na.rm = T),
    load_cf_mean = mean(load_cf, na.rm = T),
    wind_cf_q10 = quantile(wind_cf, .1, na.rm = T),
    solar_cf_q10 = quantile(solar_cf, .1, na.rm = T),
    load_cf_q10 = quantile(load_cf, .1, na.rm = T),
    wind_cf_q90 = quantile(wind_cf, .9, na.rm = T),
    solar_cf_q90 = quantile(solar_cf, .9, na.rm = T),
    load_cf_q90 = quantile(load_cf, .9, na.rm = T),
    temp_mean = mean(ev_temp),
    day = day[1],
    .groups = "drop"
  )
gen_load_ev_mean_2015 <- gen_load_ev |>
  mutate(day = day(datetime_local)) |>
  filter(year %in% 2015) |>
  group_by(hours_since_ev, type, ba, year) |>
  summarise(
    wind_cf_mean = wind_cf,
    solar_cf_mean = solar_cf,
    load_cf_mean = load_cf,
    temp_mean = ev_temp,
    day = day[1],
    .groups = "drop"
  )
gen_load_ev_mean_2018 <- gen_load_ev |>
  mutate(day = day(datetime_local)) |>
  filter(year %in% 2018) |>
  group_by(hours_since_ev, type, ba, year) |>
  summarise(
    wind_cf_mean = wind_cf,
    solar_cf_mean = solar_cf,
    load_cf_mean = load_cf,
    temp_mean = ev_temp,
    day = day[1],
    .groups = "drop"
  )

solar_ev_mean_max <- gen_load_ev_mean |>
  group_by(type, ba, day) |>
  reframe(
    solar_cf_max = max(solar_cf_mean),
    hours_since_ev = hours_since_ev[which.max(solar_cf_mean)]
  )
solar_ev_max <- gen_load_ev |>
  filter(hours_since_ev >= 0 & hours_since_ev <= 24) |>
  group_by(ba, type, ev_date) |>
  reframe(
    solar_cf_max = max(solar_cf),
    hours_since_ev = hours_since_ev[which.max(solar_cf)]
  )
wind_ev_daily_mean <- gen_load_ev |>
  filter(hours_since_ev >= 0 & hours_since_ev <= 24) |>
  group_by(ba, type, ev_date) |>
  summarise(
    wind_cf_mean = mean(wind_cf),
    .groups = "drop"
  )

gen_load_ev_mean_10yr <- gen_load_ev_10yr |>
  group_by(hours_since_ev, type, ba) |>
  summarise(
    wind_cf_mean = mean(wind_cf),
    solar_cf_mean = mean(solar_cf),
    load_cf_mean = mean(load_cf),
    temp_mean = mean(ev_temp),
    .groups = "drop"
  )
gen_load_ev_mean_20yr <- gen_load_ev_20yr |>
  group_by(hours_since_ev, type, ba) |>
  summarise(
    wind_cf_mean = mean(wind_cf),
    solar_cf_mean = mean(solar_cf),
    load_cf_mean = mean(load_cf),
    temp_mean = mean(ev_temp),
    .groups = "drop"
  )
gen_load_ev_daily_mean <- gen_load_ev_daily |>
  group_by(days_since_ev, type, ba) |>
  summarise(
    wind_cf_mean = mean(wind_cf),
    solar_cf_mean = mean(solar_cf),
    load_cf_mean = mean(load_cf),
    .groups = "drop"
  )
gen_load_ev_osw <- gen_load_ev |> filter(ba %in% c("BPAT", "BPAT_OSW", "PACW", "PACW_OSW"))
gen_load_ev <- gen_load_ev |> filter(!(ba %in% c("BPAT_OSW", "PACW_OSW")))
gen_load_ev_osw_mean <- gen_load_ev_mean |> filter(ba %in% c("BPAT", "BPAT_OSW", "PACW", "PACW_OSW"))
gen_load_ev_mean <- gen_load_ev_mean |> filter(!(ba %in% c("BPAT_OSW", "PACW_OSW")))

hourly_spaghetti_with_mean <- function(var, spaghetti, spaghetti_mean, ev_type, ylab,
                                       xlab = "Days since extreme temperature event", title) {
  spaghetti_mean <- spaghetti_mean |> filter(type == ev_type)

  ggplot(spaghetti |> filter(type == ev_type)) +
    geom_rect(xmin = 0, xmax = 1, ymin = -1, ymax = 1, fill = grey(.75, .5)) +
    # geom_line(aes(hours_since_ev/24, !!as.name(paste0(var,'_cf')), group=year),
    #          color=grey(.4,alpha=.4)) +
    facet_wrap(~ba) +
    theme_bw() +
    geom_vline(xintercept = 0, linetype = "dotted") +
    geom_vline(xintercept = 1, linetype = "dotted") +
    scale_x_continuous(breaks = -window:(window + 1)) +
    geom_line(aes(hours_since_ev / 24, !!as.name(paste0(var, "_cf_mean")), color = "mean", size = "mean"),
      data = spaghetti_mean
    ) +
    geom_line(aes(hours_since_ev / 24, !!as.name(paste0(var, "_cf_q10")), color = "q10/q90", size = "q10/q90"),
      data = spaghetti_mean
    ) +
    geom_line(aes(hours_since_ev / 24, !!as.name(paste0(var, "_cf_q90")), color = "q10/q90", size = "q10/q90"),
      data = spaghetti_mean
    ) +
    labs(x = xlab, y = ylab, title = title, color = "", size = "") +
    scale_color_manual(values = c("q10/q90" = "black", "mean" = "black")) +
    scale_size_manual(values = c("q10/q90" = .2, "mean" = .5)) +
    theme(legend.position = c(.85, 1.1), legend.direction = "horizontal")
}

load_min_hourly <- hourly_spaghetti_with_mean("load", gen_load_ev,
  gen_load_ev_mean, "min", "Normalized load",
  title = "Load during annual minimum\ntemperature event 1980-2019"
)
load_min_hourly
ggsave("extremes_plots/load_min_hourly.png", load_min_hourly, width = 6, height = 6, dpi = 300)

load_max_hourly <- hourly_spaghetti_with_mean("load", gen_load_ev, gen_load_ev_mean, "max", "Normalized load",
  title = "Load during annual maximum\ntemperature event 1980-2019"
)
load_max_hourly
ggsave("extremes_plots/load_max_hourly.png", load_max_hourly, width = 6, height = 6, dpi = 300)

wind_min_hourly <- hourly_spaghetti_with_mean("wind", gen_load_ev, gen_load_ev_mean, "min", "Wind capacity factor",
  title = "Wind generation during annual minimum\ntemperature event 1980-2019"
)
wind_min_hourly
ggsave("extremes_plots/wind_min_hourly.png", wind_min_hourly, width = 6, height = 6, dpi = 300)

wind_max_hourly <- hourly_spaghetti_with_mean("wind", gen_load_ev, gen_load_ev_mean, "max", "Wind capacity factor",
  title = "Wind generation during annual maximum\ntemperature event 1980-2019"
)
wind_max_hourly
ggsave("extremes_plots/wind_max_hourly.png", wind_max_hourly, width = 6, height = 6, dpi = 300)

# offshore
wind_min_hourly_osw <- hourly_spaghetti_with_mean("wind", gen_load_ev_osw, gen_load_ev_osw_mean, "min",
  "Wind capacity factor",
  title = "Wind generation during annual minimum\ntemperature event 1980-2019"
)
wind_min_hourly_osw
ggsave("extremes_plots/wind_min_hourly_osw.png", wind_min_hourly_osw, width = 6, height = 6, dpi = 300)

wind_max_hourly_osw <- hourly_spaghetti_with_mean("wind", gen_load_ev_osw, gen_load_ev_osw_mean, "max",
  "Wind capacity factor",
  title = "Wind generation during annual maximum\ntemperature event 1980-2019"
)
wind_max_hourly_osw
ggsave("extremes_plots/wind_max_hourly_osw.png", wind_max_hourly_osw, width = 6, height = 6, dpi = 300)

# wind_min_hourly_10yr =
#   hourly_spaghetti_with_mean('wind',gen_load_ev_10yr, gen_load_ev_mean_10yr, 'min', 'Wind capacity factor',
#                              title='Wind generation during 10yr minimum\ntemperature event')
# wind_min_hourly_10yr
# ggsave('extremes_plots/wind_min_hourly_10yr.png', wind_min_hourly_10yr, width=6, height=6, dpi=300)
#
# wind_max_hourly_10yr =
#   hourly_spaghetti_with_mean('wind',gen_load_ev_10yr, gen_load_ev_mean_10yr, 'max', 'Wind capacity factor',
#                              title='Wind generation during 10yr maximum\ntemperature event')
# wind_max_hourly_10yr
# ggsave('extremes_plots/wind_max_hourly_10yr.png', wind_max_hourly_10yr, width=6, height=6, dpi=300)
#
# wind_min_hourly_20yr =
#   hourly_spaghetti_with_mean('wind',gen_load_ev_20yr, gen_load_ev_mean_20yr, 'min', 'Wind capacity factor',
#                              title='Wind generation during 20yr minimum\ntemperature event')
# wind_min_hourly
# ggsave('extremes_plots/wind_min_hourly_20yr.png', wind_min_hourly_20yr, width=6, height=6, dpi=300)
#
# wind_max_hourly_20yr =
#   hourly_spaghetti_with_mean('wind',gen_load_ev_20yr, gen_load_ev_mean_20yr, 'max', 'Wind capacity factor',
#                              title='Wind generation during 20yr maximum\ntemperature event')
# wind_max_hourly
# ggsave('extremes_plots/wind_max_hourly_20yr.png', wind_max_hourly_20yr, width=6, height=6, dpi=300)

solar_min_hourly <- hourly_spaghetti_with_mean("solar", gen_load_ev, gen_load_ev_mean, "min", "Solar capacity factor",
  title = "Solar generation during annual minimum\ntemperature event 1980-2019"
) +
  geom_line(aes(hours_since_ev / 24, solar_cf_max),
    linetype = "dashed",
    data = solar_ev_mean_max |> filter(type == "min" & solar_cf_max > .5)
  )
solar_min_hourly
ggsave("extremes_plots/solar_min_hourly.png", solar_min_hourly, width = 6, height = 6, dpi = 300)


solar_min_hourly_bpa_pacw <- hourly_spaghetti_with_mean("solar", gen_load_ev |> filter(ba %in% c("BPAT", "PACW")),
  gen_load_ev_mean |> filter(ba %in% c("BPAT", "PACW")),
  "min", "Solar capacity factor",
  title = "Solar generation during annual minimum\ntemperature event 1980-2019"
) +
  geom_line(aes(hours_since_ev / 24, solar_cf_max),
    linetype = "dashed",
    data = solar_ev_mean_max |> filter(type == "min" & solar_cf_max > .25) |> filter(ba %in% c("BPAT", "PACW"))
  ) + facet_wrap(~ba, ncol = 1) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, .9))
solar_min_hourly_bpa_pacw
ggsave("extremes_plots/solar_min_hourly_bpa_pacw.png", solar_min_hourly_bpa_pacw, width = 4, height = 6, dpi = 300)

solar_max_hourly <- hourly_spaghetti_with_mean("solar", gen_load_ev |> filter(ba %in% c("BPAT", "PACW")),
  gen_load_ev_mean |> filter(ba %in% c("BPAT", "PACW")), "max",
  "Solar capacity factor",
  title = "Solar generation during annual maximum\ntemperature event 1980-2019"
) +
  geom_line(aes(hours_since_ev / 24, solar_cf_max),
    linetype = "dashed",
    data = solar_ev_mean_max |> filter(type == "max" & solar_cf_max > .5)
  )
solar_max_hourly
ggsave("extremes_plots/solar_max_hourly.png", solar_max_hourly, width = 6, height = 6, dpi = 300)

solar_max_hourly_bpa_pacw <- hourly_spaghetti_with_mean("solar", gen_load_ev |> filter(ba %in% c("BPAT", "PACW")),
  gen_load_ev_mean |> filter(ba %in% c("BPAT", "PACW")),
  "max", "Solar capacity factor",
  title = "Solar generation during annual maximum\ntemperature event 1980-2019"
) +
  geom_line(aes(hours_since_ev / 24, solar_cf_max),
    linetype = "dashed",
    data = solar_ev_mean_max |> filter(type == "max" & solar_cf_max > .25) |> filter(ba %in% c("BPAT", "PACW"))
  ) + facet_wrap(~ba, ncol = 1) +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0, .9))
solar_max_hourly_bpa_pacw
ggsave("extremes_plots/solar_max_hourly_bpa_pacw.png", solar_max_hourly_bpa_pacw, width = 4, height = 6, dpi = 300)

solar_mean_max_ba <- ggplot(solar_ev_mean_max |> filter(solar_cf_max > .5 & hours_since_ev > 0 & hours_since_ev <= 24)) +
  geom_line(aes(ba, solar_cf_max, group = type, color = type)) +
  theme_bw() +
  geom_hline(yintercept = 0.833, linetype = "dashed") +
  scale_color_manual("", values = colorblind_pal()(3)[-1]) +
  geom_text(aes(x, y, label = label), data = data.frame(x = 2, y = 0.833 + .01, label = "Maximum solar generation")) +
  labs(x = "BA", y = "Average daily max solar\ngeneration capacity factor")
solar_mean_max_ba
ggsave("extremes_plots/solar_mean_max_ba.png", solar_mean_max_ba, width = 8, height = 4, dpi = 300)


solar_max_ba <- ggplot(solar_ev_max) +
  geom_boxplot(aes(ba, solar_cf_max, fill = type), outlier.shape = NA) +
  theme_bw() +
  geom_hline(yintercept = 0.833, linetype = "dashed") +
  scale_fill_manual("", values = colorblind_pal()(3)[-1]) +
  geom_text(aes(x, y, label = label), data = data.frame(x = 2, y = 0.833 + .01, label = "Maximum solar generation")) +
  labs(
    x = "BA", y = "Daily max solar\ngeneration capacity factor",
    title = "Peak solar generation during the annual min/max temperature event"
  ) +
  coord_cartesian(ylim = c(0.64, 0.85))
solar_max_ba
ggsave("extremes_plots/solar_max_ba.png", solar_max_ba, width = 8, height = 3, dpi = 300)


combo_data <- gen_load_ev_osw_mean |>
  # filter(type == "max", ba %in% c("BPAT", "PACW")) |>
  mutate(
    wind_type = ifelse(substr(ba, 6, 8) == "OSW", "off_shore", "on_shore"),
    ba = substr(ba, 1, 4)
  ) |>
  select(hours_since_ev, ba, type, wind_type, wind_cf_mean, solar_cf_mean, load_cf_mean) |>
  pivot_longer(-c(hours_since_ev, ba, type, wind_type)) |>
  filter(type == "max") |>
  mutate(
    name = ifelse(name == "wind_cf_mean", paste0(name, "_", wind_type), name),
    name = ifelse(name == "wind_cf_mean_off_shore", "Wind Off Shore", name),
    name = ifelse(name == "wind_cf_mean_on_shore", "Wind On Shore", name),
    name = ifelse(name == "solar_cf_mean", "Solar", name),
    name = ifelse(name == "load_cf_mean", "Load", name)
  ) |>
  na.omit()

combo_plot <- combo_data |> ggplot() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  scale_x_continuous(breaks = -window:(window + 1)) +
  geom_rect(xmin = 0, xmax = 1, ymin = -1, ymax = 1, fill = grey(.75, .5)) +
  geom_line(aes(hours_since_ev / 24, value, color = name)) +
  facet_grid(~ba) +
  scale_color_colorblind("") +
  theme_bw() +
  labs(x = "Days since extreme temperature event", y = "Capacity Factor")
combo_plot
ggsave("extremes_plots/combo_plot.png", combo_plot, width = 8, height = 4, dpi = 300)

p_ntp_2015_2018 <- gen_load_ev_mean |>
  bind_rows(gen_load_ev_mean_2015) |>
  bind_rows(gen_load_ev_mean_2018) |>
  filter(ba %in% c("BPAT", "CISO", "PSCO", "PNM"), type == "max") |>
  mutate(year = ifelse(is.na(year), "1980-2019 Ave", year)) |>
  ggplot() +
  geom_rect(xmin = 0, xmax = 1, ymin = -1, ymax = 1, fill = grey(.75, .5)) +
  # geom_line(aes(hours_since_ev/24, !!as.name(paste0(var,'_cf')), group=year),
  #          color=grey(.4,alpha=.4)) +
  facet_wrap(~ba) +
  theme_bw() +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 1, linetype = "dotted") +
  scale_x_continuous(breaks = -window:(window + 1)) +
  geom_line(aes(hours_since_ev / 24, wind_cf_mean, color = factor(year))) +
  # geom_line(aes(hours_since_ev / 24, wind_cf, color = "q10/q90", size = "q10/q90")) +
  # geom_line(aes(hours_since_ev / 24, wind_cf_q90, color = "q10/q90", size = "q10/q90")) +
  labs(
    x = "Days since extreme temperature event", y = "Wind Capacity Factor",
    title = "Wind generation during annual maximum\ntemperature event", color = "", size = ""
  ) +
  scale_color_manual(values = c("1980-2019 Ave" = "black", "2015" = "steelblue", "2018" = "orange")) +
  # scale_size_manual(values = c("q10/q90" = .2, "mean" = .5, "2015" = .5, "2018" = .5)) +
  theme(legend.position = c(.75, 1.07), legend.direction = "horizontal")
ggsave("extremes_plots/p_ntp_2015_2018.png", p_ntp_2015_2018, width = 8, height = 8, dpi = 300)

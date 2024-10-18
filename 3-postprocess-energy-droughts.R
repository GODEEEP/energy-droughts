library(tidyverse)
import::from(ggthemes, colorblind_pal, scale_color_colorblind, scale_fill_colorblind)
import::from(ggpubr, ggarrange)
import::from(RColorBrewer, brewer.pal)
import::from(xtable, xtable, print.xtable)

do_iat <- FALSE

options("readr.show_progress" = FALSE, "readr.num_columns" = 0)

periods <- c(
  "1_1-hour" = 1,
  "2_4-hour" = 4,
  "3_12-hour" = 12,
  "4_1-day" = 24,
  "5_2-day" = 48,
  "6_3-day" = 72,
  "7_5-day" = 120
)

colorblind_ramp <- function(n) {
  colorRampPalette(colorblind_pal()(8)[-1])(n)
}

wind_config <- read_csv("/Volumes/data/tgw-gen-historical/wind/eia_wind_configs.csv", show = FALSE, progress = FALSE)
solar_config <- read_csv("/Volumes/data/tgw-gen-historical/solar/eia_solar_configs.csv", show = FALSE, progress = FALSE)
configs <- bind_rows(
  wind_config |> mutate(type = "wind"),
  solar_config |> mutate(type = "solar")
) |>
  mutate(ba = case_when(
    ba == "ERCO" ~ "ERCOT",
    ba == "ISNE" ~ "ISONE",
    ba == "NYIS" ~ "NYISO",
    .default = ba
  ))
ba_centroids <- configs |>
  group_by(ba) |>
  summarise(lon = mean(lon), lat = mean(lat))
#
period_names <- periods |>
  names() |>
  strsplit("_") |>
  sapply("[", 2)

read_droughts <- function(prefix) {
  droughts_list <- list()
  for (i in 1:length(periods)) {
    period <- periodi <- periods[i]
    period_name <- names(periods)[i]
    period_name_noindex <- strsplit(period_name, "_")[[1]][2]

    droughts_list[[i]] <- read_csv(sprintf("data/droughts/%s_%s.csv", prefix, period_name_noindex),
      show = FALSE, progress = FALSE
    ) |>
      mutate(
        period = period_name_noindex,
        period_length_hours = periodi,
        period = factor(period, levels = period_names)
      ) |>
      group_by(ba, period)
  }
  return(bind_rows(droughts_list))
}

ws_droughts <- read_droughts("ws_droughts") |>
  group_by(ba, period) |>
  left_join(ba_centroids, by = "ba") |>
  mutate(type = "")
lws_droughts <- read_droughts("lws_droughts") |>
  group_by(ba, period) |>
  left_join(ba_centroids, by = "ba")

ws_droughts_fixed <- read_droughts("ws_droughts_fixed") |>
  group_by(ba, period) |>
  left_join(ba_centroids, by = "ba") |>
  mutate(type = "")

wind_droughts_fixed <- read_droughts("wind_droughts_fixed") |>
  group_by(ba, period) |>
  left_join(ba_centroids, by = "ba") |>
  mutate(type = "")

solar_droughts_fixed <- read_droughts("solar_droughts_fixed") |>
  group_by(ba, period) |>
  left_join(ba_centroids, by = "ba") |>
  mutate(type = "")

n_years <- ws_droughts$year |>
  unique() |>
  length()
types <- c("Wind and Solar Drought", "Wind and Solar Drought + High Load")
ws_lws_droughts <- bind_rows(
  ws_droughts |> mutate(type = types[1]),
  lws_droughts |> mutate(type = types[2])
) |>
  mutate(
    type = factor(type, types),
    severity = ifelse(type == types[1], severity_ws, severity_lws)
  )

ws_droughts_fixed <- read_droughts("ws_droughts_fixed") |>
  group_by(ba, period) |>
  # mutate(severity = (severity_mwh)/sd(severity_mwh)) |>
  left_join(ba_centroids, by = "ba")

drought_stats <- function(d, sev_type = "ws") {
  d |>
    group_by(ba, period, lon, lat) |>
    summarise(
      median_dur = median(run_length_days),
      mean_dur = mean(run_length_days),
      p99_dur = quantile(run_length_days, .99),
      p90_dur = quantile(run_length_days, .90),
      max_dur = max(run_length_days),
      min_dur = min(run_length_days),
      mean_sev = mean(ifelse(sev_type == "ws", severity_ws, severity_lws)),
      mean_sev_mwh = mean(severity_mwh),
      p99_sev = quantile(ifelse(sev_type == "ws", severity_ws, severity_lws), .99, na.rm = T),
      p99_sev_mwh = quantile(severity_mwh, .99),
      p90_sev = quantile(ifelse(sev_type == "ws", severity_ws, severity_lws), .90, na.rm = T),
      p90_sev_mwh = quantile(severity_mwh, .90),
      p99_sev_pct_load = quantile(severity_mwh / load_mwh, .99),
      p90_sev_pct_load = quantile(severity_mwh / load_mwh, .90),
      max_sev = max(ifelse(sev_type == "ws", severity_ws, severity_lws)),
      max_sev_mwh = max(severity_mwh),
      min_sev = min(ifelse(sev_type == "ws", severity_ws, severity_lws)),
      min_sev_mwh = min(severity_mwh),
      frequency = length(run_id) / length(unique(year)),
      period_length_hours = period_length_hours[1],
      .groups = "drop"
    ) |>
    pivot_longer(-c(ba, period, period_length_hours, lon, lat), names_to = "stat", values_to = "value")
}

ws_drought_stats <- drought_stats(ws_droughts)
ws_max_dur_sum <- ws_drought_stats |>
  filter(stat == "max_dur") |>
  group_by(ba) |>
  summarise(max = sum(value))
ws_max_dur_order <- ws_max_dur_sum[order(ws_max_dur_sum$max), ]$ba

lws_drought_stats <- drought_stats(lws_droughts, sev_type = "lws")
lws_max_dur_sum <- lws_drought_stats |>
  filter(stat == "max_dur") |>
  group_by(ba) |>
  summarise(max = sum(value))
lws_max_dur_order <- lws_max_dur_sum[order(lws_max_dur_sum$max), ]$ba

ws_lws_drought_stats <- bind_rows(
  ws_drought_stats |> mutate(type = types[1]),
  lws_drought_stats |> mutate(type = types[2])
) |>
  mutate(type = factor(type, types))
ws_lws_drought_stats_wide <- ws_lws_drought_stats |>
  pivot_wider(id_cols = c(ba, period, period_length_hours, type, lon, lat), names_from = stat)


#######
# plots
#######

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_fill_gradientn("Drought\nLength", colours = myPalette(100), limits = c(0, 15), breaks = c(1, 5, 10, 15))

##########################
# Max Duration
##########################
p_ws_drought_max <-
  ws_drought_stats |>
  filter(stat == "max_dur") |>
  ggplot() +
  theme_minimal() +
  geom_tile(aes(period, factor(ba, levels = ws_max_dur_order), fill = value)) +
  sc
p_ws_drought_max
ggsave("plots-idf/drought_length_max.png", p_ws_drought_max, width = 10, height = 8, dpi = 600)

p_lws_drought_max <-
  lws_drought_stats |>
  filter(stat == "max_dur") |>
  ggplot() +
  theme_minimal() +
  geom_tile(aes(period, factor(ba, levels = lws_max_dur_order), fill = value)) +
  sc
p_lws_drought_max
ggsave("plots-idf/lws_drought_length_max.png", p_lws_drought_max, width = 10, height = 8, dpi = 600)

##########################
# Severity (Magnitude)
##########################

p_ws_lws_drought_sev <-
  ws_lws_droughts |>
  filter(period %in% c("1-hour", "1-day", "3-day")) |>
  ggplot() +
  geom_boxplot(aes(ba, severity, fill = type), outlier.shape = NA) +
  facet_grid(~period) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90), legend.position = "top") +
  scale_fill_manual("", values = colorblind_pal()(3)[-1]) +
  coord_cartesian(ylim = c(1.25, 2.35)) +
  labs(x = "", y = "Compound Drought Magnitude (CDM)")
p_ws_lws_drought_sev
ggsave("plots-idf/ws_lws_drought_magnitude.png", p_ws_lws_drought_sev, width = 8, height = 4, dpi = 600)

p_ws_lws_drought_sev2 <-
  ws_lws_droughts |>
  ggplot() +
  geom_boxplot(aes(period, severity, fill = type), outlier.shape = NA) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_manual("", values = colorblind_pal()(3)[-1]) +
  coord_cartesian(ylim = c(1.25, 2.35)) +
  labs(x = "", y = "Compound Drought\nMagnitude (CDM)")
p_ws_lws_drought_sev2
ggsave("plots-idf/ws_lws_drought_magnitude2.png", p_ws_lws_drought_sev2, width = 8, height = 3, dpi = 600)

p_ws_lws_drought_sev3 <-
  ws_lws_droughts |>
  ggplot() +
  geom_boxplot(aes(period, (severity_mwh) / (load_mwh) * 100, fill = type), outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 16)) +
  labs(x = "", y = "Wind and Solar Deficit\nBelow 10th Percentile as\nFraction of Total Load [%]") +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_manual("", values = colorblind_pal()(8)[c(4, 6)])
p_ws_lws_drought_sev3
ggsave("plots-idf/ws_lws_drought_magnitude3.png", p_ws_lws_drought_sev3, width = 8, height = 3, dpi = 600)

p_lws_magnitude_gwh <-
  ws_lws_drought_stats_wide |>
  filter(type == types[2]) |>
  arrange(period, frequency) |>
  mutate(ba = fct_reorder(ba, p99_sev_pct_load)) |>
  ggplot() +
  geom_bar(aes(period, p99_sev_pct_load * 100, fill = ba), stat = "identity", position = "dodge") +
  scale_fill_manual("BA", values = colorblind_ramp(ws_droughts$ba |> unique() |> length())) +
  scale_y_continuous(minor_breaks = NULL) +
  theme_bw() +
  labs(
    y = "99th Percentile Wind and Solar Deficit Below\n10th Percentile as Fraction of Total Load [%]", x = "",
    title = "Wind and Solar Droughts + High Load"
  )
p_lws_magnitude_gwh
ggsave("plots-idf/lws_drought_magnitude4.png", p_lws_magnitude_gwh, width = 8, height = 5, dpi = 600)

p_lws_magnitude_gwh2 <-
  ws_lws_drought_stats_wide |>
  filter(type == types[2], period %in% c("1-hour", "1-day")) |>
  arrange(period, frequency) |>
  mutate(ba = fct_reorder(ba, p99_sev_pct_load)) |>
  ggplot() +
  geom_bar(aes(period, p99_sev_pct_load * 100, fill = ba), stat = "identity", position = "dodge") +
  scale_fill_manual("BA", values = colorblind_ramp(ws_droughts$ba |> unique() |> length())) +
  scale_y_continuous(minor_breaks = NULL) +
  theme_bw() +
  labs(
    y = "99th Percentile Wind and Solar Deficit Below\n10th Percentile as Fraction of Total Load [%]", x = "",
    title = "Wind and Solar Droughts + High Load"
  )
p_lws_magnitude_gwh2
ggsave("plots-idf/lws_drought_magnitude4-1-hour-1-day.png", p_lws_magnitude_gwh, width = 8, height = 5, dpi = 600)

##########################
# Frequency
##########################
p_ws_frequency <-
  ws_droughts |>
  group_by(ba, period) |>
  summarise(frequency = n() / n_years, .groups = "drop") |>
  mutate(period = factor(period, levels = periods |> names() |> substr(3, 10))) |>
  arrange(period, frequency) |>
  mutate(ba = fct_reorder(ba, frequency, .desc = TRUE)) |>
  ggplot() +
  geom_bar(aes(period, frequency, fill = ba), stat = "identity", position = "dodge") +
  scale_fill_manual("BA", values = colorblind_ramp(ws_droughts$ba |> unique() |> length())) +
  scale_y_continuous(breaks = 0:15, limits = c(0, 15), minor_breaks = NULL) +
  theme_bw() +
  labs(y = "Number of Droughts per Year")
p_ws_frequency
ggsave("plots-idf/ws_drought_frequency.png", p_ws_frequency, width = 8, height = 5, dpi = 600)

# p_ws_frequency2 =
#   ggplot(ws_droughts |> group_by(ba,period) |> summarise(count=length(unique(run_id)))) +
#   geom_line(aes(factor(period), count/n_years, color=ba, group=ba)) +
#   geom_point(aes(factor(period), count/n_years, color=ba, group=ba)) +
#   scale_color_manual('BA',values=colorblind_ramp(ws_droughts$ba |> unique() |> length())) +
#   scale_y_continuous(breaks=0:15, limits=c(0,15),minor_breaks=NULL)+
#   theme_bw()+
#   labs(y='Number of Droughts per Year')
# p_ws_frequency2
# ggsave('plots-idf/ws_drought_frequency2.png', p_ws_frequency2, width=8, height=5, dpi=600)

p_lws_frequency <-
  lws_droughts |>
  group_by(ba, period) |>
  summarise(frequency = n() / n_years, .groups = "drop") |>
  mutate(period = factor(period, levels = periods |> names() |> substr(3, 10))) |>
  arrange(period, frequency) |>
  mutate(ba = fct_reorder(ba, frequency, .desc = TRUE)) |>
  ggplot() +
  geom_bar(aes(period, frequency, fill = ba), stat = "identity", position = "dodge") +
  scale_fill_manual("BA", values = colorblind_ramp(ws_droughts$ba |> unique() |> length())) +
  scale_y_continuous(breaks = 0:15, limits = c(0, 15), minor_breaks = NULL) +
  theme_bw() +
  labs(y = "Number of Droughts per Year")
p_lws_frequency
ggsave("plots-idf/lws_drought_frequency.png", p_lws_frequency, width = 8, height = 5, dpi = 600)

p_ws_lws_frequency <- ws_lws_droughts |>
  group_by(ba, period, type) |>
  summarise(frequency = n() / n_years, .groups = "drop") |>
  mutate(period = factor(period, levels = periods |> names() |> substr(3, 10))) |>
  arrange(period, frequency) |>
  mutate(ba = fct_reorder(ba, frequency, .desc = TRUE)) |>
  ggplot() +
  geom_bar(aes(period, frequency, fill = ba), stat = "identity", position = "dodge") +
  scale_fill_manual("BA", values = colorblind_ramp(ws_droughts$ba |> unique() |> length())) +
  scale_y_continuous(breaks = 0:15, limits = c(0, 15), minor_breaks = NULL) +
  theme_bw() +
  labs(y = "Number of Droughts per Year") +
  facet_wrap(~type)
p_ws_lws_frequency
ggsave("plots-idf/ws_lws_drought_frequency.png", p_ws_lws_frequency, width = 10, height = 5, dpi = 600)

p_lws_frequency2 <-
  lws_droughts |>
  group_by(ba, period) |>
  summarise(frequency = n() / n_years, .groups = "drop") |>
  mutate(period = factor(period, levels = periods |> names() |> substr(3, 10))) |>
  arrange(period, frequency) |>
  mutate(ba = fct_reorder(ba, frequency, .desc = TRUE)) |>
  ggplot() +
  geom_bar(aes(period, frequency, fill = ba), stat = "identity", position = "dodge") +
  scale_fill_manual("BA", values = colorblind_ramp(ws_droughts$ba |> unique() |> length())) +
  scale_y_continuous(breaks = 0:15) +
  theme_bw() +
  labs(y = "Number of Droughts per Year")
p_lws_frequency2
ggsave("plots-idf/lws_drought_frequency2.png", p_lws_frequency2, width = 8, height = 5, dpi = 600)
#
#
# p_lws_frequency2 =
#   ggplot(lws_droughts |> group_by(ba,period) |> summarise(count=length(unique(run_id)))) +
#   geom_line(aes(factor(period), count/n_years, color=ba, group=ba)) +
#   geom_point(aes(factor(period), count/n_years, color=ba, group=ba)) +
#   scale_color_manual('BA',values=colorblind_ramp(lws_droughts$ba |> unique() |> length())) +
#   scale_y_continuous(breaks=0:13, limits=c(0,13))+
#   theme_bw() +
#   labs(y='Number of Droughts per Year')
# p_lws_frequency2
# ggsave('plots-idf/lws_drought_frequency2.png', p_lws_frequency2, width=8, height=5, dpi=600)



####################################################
# IDF - Intensity Duration Frequency
####################################################
# fit_freq_dir_curve = function(data, x, y){
#   f = function(p, data){sqrt(sum(1/(data[[x]]-p[1])+p[2]-data[[y]])^2)}
#   fit = optim(c(1.55,.5), f, data=data)
#   curve = data.frame(data[[x]],
#                      1/(data[[x]]-fit$par[1])+fit$par[2],
#                      data$type)
#   names(curve) = c(x, y, 'type')
#   curve
# }
# curves = bind_rows(
#   fit_freq_dir_curve(ws_lws_drought_stats_wide |>
#                        filter(type=='Load, Wind, and Solar Droughts'), 'frequency', 'max_dur'),
#   fit_freq_dir_curve(ws_lws_drought_stats_wide |>
#                        filter(type=='Wind and Solar Droughts'), 'frequency', 'max_dur')
# )

max_freq_at_dur <- function(x, binsize = 2) {
  breaks <- seq(0, ceiling(max(x$max_dur)), by = binsize)
  line <- x |>
    mutate(
      dur_bin = cut(max_dur, breaks),
      lb = gsub("\\(", "", dur_bin) |> strsplit(",") |> sapply("[", 1) |> as.numeric(),
      ub = gsub("\\]", "", dur_bin) |> strsplit(",") |> sapply("[", 2) |> as.numeric()
    ) |>
    group_by(dur_bin) |>
    summarise(
      max_freq = max(frequency),
      lb = lb[1],
      ub = ub[1],
      mp = (lb + ub) / 2
    )
  line$type <- x$type[1]
  line
}
lines <- bind_rows(
  max_freq_at_dur(ws_lws_drought_stats_wide |>
    filter(type == types[1])),
  max_freq_at_dur(ws_lws_drought_stats_wide |>
    filter(type == types[2]))
)

a1 <- 2
b1 <- 18
c1 <- -0.6
d1 <- 2
cx1 <- seq(a1 + .55, 15, by = .1)
cy1 <- a1 + b1 * exp(c1 * (cx1 - d1))
a2 <- 1.5
b2 <- 15
c2 <- -0.7
d2 <- 1.5
cx2 <- seq(a2 + .75, 11, by = .1)
cy2 <- a2 + b2 * exp(c2 * (cx2 - d2))
ub_curves <- bind_rows(
  data.frame(x = cx1, y = cy1, type = types[1]),
  data.frame(x = cx2, y = cy2, type = types[2])
) |>
  mutate(type = factor(type, types))
max_dur_freq_max <- ws_lws_drought_stats_wide |>
  group_by(period, type) |>
  summarise(mean_freq = mean(frequency), mean_dur = mean(max_dur), .groups = "drop")
# summarise(max_freq = max(frequency), max_dur = max_dur[which.max(frequency)])
p_ws_lws_max_idf <- ws_lws_drought_stats_wide |>
  ggplot() +
  geom_line(aes(x, y), data = ub_curves) +
  geom_point(aes(frequency, max_dur, color = period), shape = 19, stroke = 2) +
  # geom_line(aes(mean_freq, mean_dur), data=max_dur_freq_max) +
  # geom_point(aes(mean_freq, mean_dur, fill=period, group=period), data=max_dur_freq_max, shape=24, size=1.5) +
  # geom_path(aes(max_freq, mp), data=lines) +
  scale_color_colorblind("Time\nScale") +
  scale_fill_colorblind("Time\nScale") +
  theme_bw() +
  facet_wrap(~type) +
  scale_size_continuous("Max Drought\nSeverity (GWH)", range = c(2, 10)) +
  # scale_x_continuous(breaks=seq(0,100,by=25), limits=c(0,100), minor_breaks=NULL)+
  scale_x_continuous(breaks = 1:13, limits = c(1, 13), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 2), minor_breaks = NULL) +
  labs(x = "Number of Events per Year", y = "Maximum Drought Duration (days)")
p_ws_lws_max_idf
ggsave("plots-idf/idf_ws_lws_max.png", p_ws_lws_max_idf, width = 10, height = 5, dpi = 600)


max_dur_freq_mean <- ws_lws_drought_stats_wide |>
  group_by(period, type) |>
  # summarise(mean_freq = mean(frequency), mean_dur = mean(mean_dur))
  summarise(
    max_freq = max(frequency), max_dur = mean_dur[which.max(frequency)],
    .groups = "drop"
  )
p_ws_lws_mean_idf <- ws_lws_drought_stats_wide |>
  ggplot() +
  geom_line(aes(x, y), data = ub_curves) +
  geom_errorbar(aes(frequency, ymin = min_dur, ymax = max_dur, color = period)) +
  geom_point(aes(frequency, mean_dur, color = period), shape = 19, stroke = 1.4) +
  # geom_line(aes(max_freq, max_dur), data=max_dur_freq_mean) +
  # geom_line(aes(mean_freq, mean_dur), data=max_dur_freq_mean) +
  # geom_point(aes(mean_freq, mean_dur, fill=period, group=period), data=max_dur_freq_mean, shape=24, size=1.5) +
  scale_color_colorblind("Time\nScale") +
  scale_fill_colorblind("Time\nScale") +
  theme_bw() +
  facet_wrap(~type) +
  # scale_size_continuous('Mean\nDrought\nMagnitude', range=c(1,10), limits=c(1.2,2.3), breaks=seq(1.2,2,by=.4)) +
  scale_x_continuous(breaks = 1:15, limits = c(1, 15), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 2), minor_breaks = NULL) +
  labs(x = "Number of Events per Year", y = "Drought Duration (days)")
p_ws_lws_mean_idf
ggsave("plots-idf/idf_ws_lws_mean_max.png", p_ws_lws_mean_idf, width = 10, height = 4.5, dpi = 600)

p_ws_lws_mean_idf <- ws_lws_droughts |>
  group_by(ba, period, type) |>
  mutate(frequency = length(run_id) / length(unique(year))) |>
  ggplot() +
  geom_line(aes(x, y), data = ub_curves) +
  # geom_errorbar(aes(frequency, ymin=min_dur, ymax=max_dur, color=period)) +
  geom_point(aes(frequency, run_length_days, color = period), shape = 19, stroke = 1.4) +
  # geom_line(aes(max_freq, max_dur), data=max_dur_freq_mean) +
  # geom_line(aes(mean_freq, mean_dur), data=max_dur_freq_mean) +
  # geom_point(aes(mean_freq, mean_dur, fill=period, group=period), data=max_dur_freq_mean, shape=24, size=1.5) +
  scale_color_colorblind("Time\nScale") +
  scale_fill_colorblind("Time\nScale") +
  theme_bw() +
  facet_wrap(~type) +
  # scale_size_continuous('Mean\nDrought\nMagnitude', range=c(1,10), limits=c(1.2,2.3), breaks=seq(1.2,2,by=.4)) +
  scale_x_continuous(breaks = 1:13, limits = c(1, 13), minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, 16), breaks = seq(0, 16, by = 2), minor_breaks = NULL) +
  labs(x = "Number of Events per Year", y = "Mean Drought Duration (days)")
p_ws_lws_mean_idf
ggsave("plots-idf/idf_ws_lws_mean_max2.png", p_ws_lws_mean_idf, width = 10, height = 4.5, dpi = 600)

# ws_lws_drought_stats_wide |>
#   ggplot() +
#   geom_point(aes(frequency, mean_dur, size=mean_sev, color=period), shape=16, alpha=.75) +
#   scale_color_colorblind('Period') +
#   theme_bw() +
#   facet_wrap(~type) +
#   scale_size_continuous('Average Drought\nMagnitude', range=c(1,10), limits=c(1.2,2.2), breaks=seq(1.2,2,by=.4)) +
#   scale_x_continuous(breaks=1:13, limits=c(1,13), minor_breaks=NULL) +
#   scale_y_continuous(limits=c(0,7),breaks=0:7, minor_breaks=NULL) +
#   labs(x='Number of Events per Year', y='Mean Drought Duration (days)')

p_ws_lws_p90_idf <- ws_lws_drought_stats_wide |>
  ggplot() +
  geom_point(aes(frequency, p90_dur, size = p90_sev, color = period), shape = 1, stroke = 2) +
  scale_color_colorblind("Period") +
  theme_bw() +
  facet_wrap(~type) +
  scale_size_continuous("Max Drought\nSeverity", range = c(2, 10)) +
  # scale_x_continuous(breaks=seq(0,100,by=25), limits=c(0,100), minor_breaks=NULL) +
  scale_y_continuous(limits = c(0, 10), breaks = 0:10, minor_breaks = NULL) +
  labs(x = "Number of Events per Year", y = "90th Percentile of Drought Duration (days)")
p_ws_lws_p90_idf
ggsave("plots-idf/idf_ws_lws_p90.png", p_ws_lws_p90_idf, width = 10, height = 5, dpi = 600)

# same graphs as above but with frequency the size and severity as the shape
# p_ws_max_idf2 = ws_drought_stats |>
#   pivot_wider(id_cols=c(ba, period, period_length_hours), names_from=stat) |>
#   ggplot() +
#   geom_point(aes(max_sev_mwh/1000, max_dur, size=frequency,color=period), shape=1, stroke=2) +
#   scale_color_colorblind('Period') +
#   theme_bw()
#   #scale_size_continuous('Max Drought\nSeverity (GWH)',breaks=c(10,50,150)) +
#   #scale_x_continuous(breaks=seq(0,100,by=25), limits=c(0,100), minor_breaks=NULL)+
#   #scale_y_continuous(limits=c(0,16),breaks=seq(0,16,by=2), minor_breaks=NULL) +
#   #labs(x='Number of Events per Year', y='Maximum Drought Duration (days)')
# p_ws_max_idf2
#
#
# p_ws_mean_idf2 = ws_drought_stats |>
#   pivot_wider(id_cols=c(ba, period, period_length_hours), names_from=stat) |>
#   ggplot() +
#   geom_point(aes(mean_sev_mwh/1000, mean_dur, size=frequency,color=period), shape=1, stroke=2) +
#   scale_color_colorblind('Period') +
#   theme_bw()
#   #scale_size_continuous('Max Drought\nSeverity (GWH)',breaks=c(10,50,150)) +
#   #scale_x_continuous(breaks=seq(0,100,by=25), limits=c(0,100), minor_breaks=NULL) +
#   #scale_y_continuous(limits=c(0,6),breaks=0:6, minor_breaks=NULL)
# p_ws_mean_idf2

states <- map_data("state")
world <- map_data("world")
ba_labels <- ba_centroids |> filter(ba %in% (ws_lws_drought_stats |> pull(ba) |> unique()))
a <- 2
b <- 0
ba_labels <- ba_labels |> mutate(
  lat = lat + c(
    BPAT = a, CISO = a + 1, ERCOT = a + 1, IPCO = a, ISONE = a, MISO = a, NWMT = a, NYISO = a,
    PACE = a + 1, PACW = -a, PJM = a, PNM = -a - 1, PSCO = -a + .2, SWPP = a + 1, WACM = a + 1
  ),
  lon = lon + c(
    BPAT = b, CISO = b, ERCOT = b, IPCO = b, ISONE = b + 3, MISO = b, NWMT = b, NYISO = b,
    PACE = b, PACW = b, PJM = b - 2.8, PNM = b, PSCO = b - 3.4, SWPP = b, WACM = b
  )
)
p_ws_freq_dur_1d <- ws_lws_drought_stats_wide |>
  filter(period == "1-day" & type == types[1]) |>
  # mutate(dur_binned = cut(p90_dur, breaks = 0:6)) |>
  ggplot(aes(lon, lat)) +
  geom_polygon(aes(long, lat, group = group), fill = "#E3DEBF", data = world) +
  geom_polygon(aes(long, lat, group = group), color = "white", fill = grey(.6), data = states, linewidth = .2) +
  geom_point(aes(size = frequency, color = max_dur), alpha = .8) +
  geom_point(aes(size = frequency), shape = 1, color = "black") +
  geom_label(aes(lon, lat, label = ba), size = 3, label.size = 0.1, data = ba_labels) +
  # facet_grid(~type)+
  scale_color_stepsn("Drought\nDuration\n(Days)", colors = colorblind_pal()(8)[1:8], limits = c(0, 8), n.breaks = 5) +
  # scale_color_stepsn('Drought\nDuration\n(Days)', colors=brewer.pal(6,'YlOrRd'), limits=c(0,5), n.breaks=6)+
  # scale_color_viridis_c(option='H')+
  theme_bw() +
  facet_wrap(~period) +
  theme(
    panel.background = element_rect(fill = "#A6CAE0"),
    panel.grid.minor = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.justification = "left",
    # legend.text.align=.5,
    # legend.title.align=.5,
    legend.margin = margin(0, 0, 0, 0),
    # legend.box.margin=margin(-10,-10,-10,-10)
  ) +
  scale_size_continuous("Events\nPer Year", range = c(1, 18), breaks = c(2, 4, 6), limits = c(1.2, 7)) +
  guides(size = guide_legend(order = 1, reverse = T)) +
  labs(x = "Longitude", y = "Latitude") + # , title = "1-day Coincident Wind and Solar Droughts (maximum duration)") +
  # coord_map()
  coord_cartesian(xlim = range(states$long) + c(2, -2), ylim = range(states$lat))
p_ws_freq_dur_1d
ggsave("plots-idf/map_ws_freq_dur_1d.png", p_ws_freq_dur_1d, width = 8, height = 4.2, dpi = 600)

p_ws_freq_dur_1h <- ws_lws_drought_stats_wide |>
  filter(period == "1-hour" & type == types[1]) |>
  # mutate(dur_binned = cut(p90_dur, breaks = 0:6)) |>
  ggplot(aes(lon, lat)) +
  # geom_polygon(aes(long, lat, group = group), fill = "#E3DEBF", data = world) +
  geom_polygon(aes(long, lat, group = group), fill = "#E3DEBF", data = world) +
  geom_polygon(aes(long, lat, group = group), color = "white", fill = grey(.6), data = states, size = .2) +
  geom_point(aes(size = frequency, color = max_dur), alpha = .8) +
  geom_point(aes(size = frequency), shape = 1, color = "black") +
  geom_label(aes(lon, lat, label = ba), size = 3, label.size = 0.1, data = ba_labels) +
  # facet_grid(~type)+
  scale_color_stepsn("Drought\nDuration\n(Days)",
    colors = colorblind_pal()(8)[1:8],
    limits = c(0.5, 1.5), n.breaks = 5
  ) +
  # scale_color_viridis_c(option='H')+
  scale_size_continuous("Events\nPer Year", range = c(1, 18), breaks = c(5, 9, 13), limits = c(4, 15)) +
  # scale_color_stepsn('Drought\nDuration\n(Days)', colors=brewer.pal(6,'YlOrRd'), limits=c(0,5), n.breaks=6)+
  # scale_fill_viridis(
  #   option="B",
  #   trans = "log",
  #   # breaks = c(1,7,54,403,3000),
  #   name='Drought\nDuration\n(Days)',
  #   guide = guide_legend( keyheight = unit(2.5, units = "mm"),
  #                         keywidth=unit(10, units = "mm"),
  #                         label.position = "bottom",
  #                         title.position = 'top', nrow=1)
  # )  +
  theme_bw() +
  facet_wrap(~period) +
  theme(
    panel.background = element_rect(fill = "#A6CAE0"),
    panel.grid.minor = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.justification = "left",
    # legend.text.align = .5,
    # legend.title.align = .5,
    legend.margin = margin(0, 0, 0, 0),
    # legend.box.margin=margin(-10,-10,-10,-10)
  ) +
  guides(size = guide_legend(order = 1, reverse = T)) +
  labs(x = "Longitude", y = "Latitude") + # , title='1-day Coincident Wind and Solar Droughts (maximum duration)')+
  # coord_map()
  coord_cartesian(xlim = range(states$long) + c(2, -2), ylim = range(states$lat))
p_ws_freq_dur_1h
ggsave("plots-idf/map_ws_freq_dur_1h.png", p_ws_freq_dur_1h, width = 9, height = 5, dpi = 600)


p_ws_freq_dur_1h_1d <- ggarrange(p_ws_freq_dur_1h, p_ws_freq_dur_1d, nrow = 1)
p_ws_freq_dur_1h_1d
ggsave("plots-idf/map_ws_freq_dur_1h_1d.png", width = 13, height = 5, dpi = 600)


p_ws_freq_dur_multiple <- ws_lws_drought_stats_wide |>
  filter(period %in% c("1-hour", "1-day") & type == types[1]) |>
  # mutate(dur_binned = cut(p90_dur, breaks = 0:6)) |>
  ggplot(aes(lon, lat)) +
  geom_polygon(aes(long, lat, group = group), fill = "#E3DEBF", data = world) +
  geom_polygon(aes(long, lat, group = group), color = "white", fill = grey(.6), data = states, size = .2) +
  geom_point(aes(size = frequency, color = max_dur), alpha = .8) +
  geom_point(aes(size = frequency), shape = 1, color = "black") +
  geom_label(aes(lon, lat, label = ba), size = 3, data = ba_labels) +
  # facet_grid(~type)+
  scale_color_stepsn("Drought\nDuration\n(Days)", colors = colorblind_pal()(8)[1:8], limits = c(0, 8), n.breaks = 6) +
  # scale_color_stepsn('Drought\nDuration\n(Days)', colors=brewer.pal(6,'YlOrRd'), limits=c(0,5), n.breaks=6)+
  theme_bw() +
  facet_wrap(~period) +
  theme(
    panel.background = element_rect(fill = "#A6CAE0"),
    panel.grid.minor = element_blank(),
    legend.box = "horizontal"
  ) +
  scale_size_area("Events\nPer Year", max_size = 19, breaks = c(1, 5, 9, 11), limits = c(0, 12)) +
  # scale_radius('Events\nPer Year', range=c(1,20), breaks=c(1,5,9,11), limits=c(0,12))+
  guides(size = guide_legend(order = 1, reverse = T)) +
  labs(x = "Longitude", y = "Latitude") + # , title='1-day Coincident Wind and Solar Droughts (maximum duration)')+
  # coord_map()
  coord_cartesian(xlim = range(states$long), ylim = range(states$lat))
p_ws_freq_dur_multiple
ggsave("plots-idf/map_ws_freq_dur_multiple.png", p_ws_freq_dur_multiple, width = 14, height = 4, dpi = 600)

# bigger points
# shapes for each number of events per year

p_lws_freq_dur <- ws_lws_drought_stats_wide |>
  filter(period == "1-day" & type == types[2]) |>
  ggplot(aes(lon, lat)) +
  geom_polygon(aes(long, lat, group = group), color = "black", fill = "white", data = states) +
  geom_label(aes(lon, lat, label = ba), size = 3, data = ba_labels) +
  geom_point(aes(size = frequency, color = p90_dur)) +
  geom_point(aes(size = frequency), shape = 1) +
  facet_grid(~type) +
  scale_color_distiller("Drought\nDuration\n(Days)", palette = "Spectral") +
  theme_bw() +
  scale_size_continuous("Events\nPer Year") +
  guides(size = guide_legend(order = 1))
p_lws_freq_dur
ggsave("plots-idf/map_lws_freq_dur.png", width = 8, height = 4, dpi = 600)

# ggarrange(p_ws_freq_dur, p_lws_freq_dur, ncol=2)


# ECDF Duration
bas <- ws_droughts$ba |> unique()
cols <- colorblind_ramp(length(bas))
cols[which(bas == "CISO")] <- "black"
p_ecdf <- ggplot(ws_droughts |> filter(period %in% c("1-hour", "1-day", "3-day"))) +
  stat_ecdf(aes(run_length_days, color = ba)) +
  facet_wrap(~period, scales = "free_x") +
  theme_bw() +
  scale_color_manual("BA", values = cols) +
  labs(y = "Cumulative Probability", x = "Drought Length (days)")
p_ecdf
ggsave(sprintf("plots-idf/drought_length_ecdf.png"), p_ecdf, width = 10, height = 4, dpi = 600)


# table showing min and max duration
ws_droughts |>
  group_by(ba, period) |>
  summarise(max = max(run_length_days)) |>
  group_by(period) |>
  summarise(
    ba_max_min = ba[which.min(max)],
    max_min = round(min(max), 2),
    ba_max_max = ba[which.max(max)],
    max_max = round(max(max), 2)
  ) |>
  xtable()

bas <- ws_droughts |>
  pull(ba) |>
  unique()
configs |>
  group_by(ba, type) |>
  summarise(n = length(unique(plant_code_unique)), cap = sum(system_capacity)) |>
  mutate(cap = cap / 1000) |>
  pivot_wider(id_cols = ba, values_from = c(n, cap), names_from = type) |>
  filter(ba %in% bas) |>
  left_join(read_csv("data/ba-names.csv"), by = "ba") |>
  select(ba, ba_name, n_solar, n_wind, cap_solar, cap_wind) |>
  mutate(cap_solar = round(cap_solar), cap_wind = round(cap_wind)) |>
  xtable(digits = 0) |>
  print.xtable(include.rownames = F)

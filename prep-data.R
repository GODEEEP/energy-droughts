# install.packages('import')
xfun::pkg_attach2('ggplot2')
import::from(readr, read_csv, write_csv)
import::from(dplyr, bind_rows, rename, inner_join, mutate, group_by, summarise, filter, between, 
             select, rename, pull, left_join, slice, case_when)
import::from(tidyr, pivot_longer)
import::from(rgdal, readOGR)
import::from(sf, read_sf, st_transform, st_as_sf, st_intersects)
import::from(jsonlite, read_json)
import::from(lubridate, with_tz, year, hour, hours, leap_year, ymd_hms, force_tzs)
import::from(data.table, melt.data.table, as.data.table, setnames, merge.data.table, setkey)
import::from(ggthemes, colorblind_pal)

source('lib.R')

periods = c('1-hour'=1,'4-hour'=4,'12-hour'=12,'1-day'=24,'2-day'=48,'3-day'=72,'5-day'=120)
min_sites_per_ba = 5
start_year = 1980

# metadata 
message('Reading metadata')
wind_config = read_csv('data/tgw-gen/wind/eia_wind_configs.csv', show=FALSE, progress=FALSE)
solar_config = read_csv('data/tgw-gen/solar/eia_solar_configs.csv', show=FALSE, progress=FALSE)


# generation data 
message('Reading scenario data')
scenario = 'historical'
solar_fns = list.files('data/tgw-gen/solar/historical_bc', '*solar*', full.names=TRUE)
wind_fns = list.files('data/tgw-gen/wind/historical', '*wind*', full.names=TRUE)
load_fns = list.files('data/tell/historic','TELL_Bal*',recursive=TRUE, full.names=TRUE)

solar_list = wind_list = load_list = list()

for(fn in solar_fns) solar_list[[fn]] = read_csv(fn, show=FALSE, progress=FALSE)
for(fn in wind_fns) wind_list[[fn]] = read_csv(fn, show=FALSE, progress=FALSE)
for(fn in load_fns) load_list[[fn]] = read_csv(fn, show=FALSE, progress=FALSE)

# add back in the last day of the year in leap days, just duplicate the day before
solar_years = names(solar_list) |> 
  basename() |> 
  tools::file_path_sans_ext() |> 
  strsplit('_') |> 
  sapply('[',4) |> 
  as.numeric()
for(y in solar_years){
  yeari = which(solar_years == y)
  if(leap_year(y)){
    last_day = solar_list[[yeari]] |> 
      tail(24) |> 
      mutate(datetime = datetime + hours(24))
    solar_list[[yeari]] = bind_rows(solar_list[[yeari]], last_day)
  }
}
wind_years = names(solar_list) |> 
  basename() |> 
  tools::file_path_sans_ext() |> 
  strsplit('_') |> 
  sapply('[',4) |> 
  as.numeric()
for(y in wind_years){
  yeari = which(wind_years == y)
  if(leap_year(y)){
    last_day = wind_list[[yeari]] |> 
      tail(24) |> 
      mutate(datetime = datetime + hours(24))
    wind_list[[yeari]] = bind_rows(wind_list[[yeari]], last_day)
  }
}


solar_wide = bind_rows(solar_list)
# temp fix for names, delete if the header data gets recomputed
#names(solar_wide)[2:ncol(solar_wide)] = paste0(names(solar_wide)[-1],'_',0:(ncol(solar_wide)-2))
wind_wide = bind_rows(wind_list) 
# load data is hour beginning
load_long = bind_rows(load_list) |>
  rename(datetime_utc = Time_UTC, 
         ba = BA_Code, 
         load_mwh = Scaled_TELL_BA_Load_MWh) |>
  group_by(ba) |>
  mutate(max_load = max(load_mwh),
         load_cf = load_mwh/max_load) |>
  select(datetime_utc, ba, load_cf, max_load)

rm(solar_list, wind_list)

message('Formatting data')
# filter out bas with less then the min number of both wind and solar plants
# also filter to WECC
solar_count = solar_config |> 
  #filter(nerc_region == 'WECC') |> # & ba != 'SWPP') |>
  group_by(ba) |> 
  summarise(solar_num_sites = length(unique(plant_code)))
wind_count = wind_config |> 
  #filter(nerc_region == 'WECC') |># & ba != 'SWPP') |>
  group_by(ba) |> 
  summarise(wind_num_sites = length(unique(plant_code)))
plant_counts = solar_count |> 
  inner_join(wind_count, by='ba') |>
  filter(solar_num_sites >= min_sites_per_ba & wind_num_sites >= min_sites_per_ba)

# add east and west split of large BAs
# bas = c(plant_counts |> filter(!(ba %in% c('MISO','PJM','WACM'))) |> pull(ba), 
#         'MISO-W', 'MISO-E', 'PJM-W', 'PJM-E')
bas = plant_counts$ba

# manually defined timezones
# timezones_ba = data.frame(ba=bas,
#                           timezone=c('BPAT'='US/Pacific', 'CISO'='US/Pacific','ERCO'='US/Central', 
#                                      'IPCO'='US/Mountain', 'ISNE'='US/Eastern', #'MISO'='US/Central',
#                                      'MISO-W'='US/Central', 'MISO-E'='US/Central',
#                                      'NWMT'='US/Mountain', 'NYIS'='US/Eastern', 'PACE'='US/Mountain', 
#                                      'PACW'='US/Pacific', #'PJM'='US/Eastern', 
#                                      'PJM-W'='US/Eastern','PJM-E'='US/Eastern', 'PNM'='US/Mountain', 
#                                      'PSCO'='US/Mountain', 'SWPP'='US/Central' #'WACM'='US/Mountain'
#                           ))
timezones_ba = data.frame(ba=bas,
                       timezone=c('BPAT'='US/Pacific', 'CISO'='US/Pacific','ERCO'='US/Central', 
                                  'IPCO'='US/Mountain', 'ISNE'='US/Eastern', 'MISO'='US/Central',
                                  #'MISO-W'='US/Central', 'MISO-E'='US/Central',
                                  'NWMT'='US/Mountain', 'NYIS'='US/Eastern', 'PACE'='US/Mountain', 
                                  'PACW'='US/Pacific', 'PJM'='US/Eastern', 
                                  #'PJM-W'='US/Eastern','PJM-E'='US/Eastern', 
                                  'PNM'='US/Mountain', 
                                  'PSCO'='US/Mountain', 'SWPP'='US/Central', 'WACM'='US/Mountain'
                                  ))

# filtering, ended up not using this
# wind_config = wind_config |>
#   # drop WACM, it overlaps almost entirely with other regions
#   #filter(ba != 'WACM') |>
#   # cut off northern SWPP since it overlaps with MISO
#   # and no solar in that area
#   filter(!(ba == 'SWPP' & lat > 42.5)) #|>
#   # # split MISO east/west
#   # # split PJM east west
#   # mutate(ba = case_when(ba =='MISO' & lon < -91 ~ 'MISO-W',
#   #                       ba =='MISO' & lon >= -91 ~ 'MISO-E',
#   #                       ba =='PJM' & lon < -80.5 ~ 'PJM-W',
#   #                       ba =='PJM' & lon >= -80.5 ~ 'PJM-E',
#   #                       .default = ba))
# 
# solar_config = solar_config |>
#   # drop WACM, it overlaps almost entirely with other regions
#   #filter(ba != 'WACM') |>
#   # exclude very southern MISO solar to limit geographic extent
#   filter(!(ba == 'MISO' & lat < 37)) #|>
#   # split MISO east/west
#   # split PJM east west
#   # mutate(ba = case_when(ba =='MISO' & lon > -91 ~ 'MISO-W',
#   #                       ba =='MISO' & lon >= -91 ~ 'MISO-E',
#   #                       ba =='PJM' & lon < -80.5 ~ 'PJM-W',
#   #                       ba =='PJM' & lon >= -80.5 ~ 'PJM-E',
#   #                       .default = ba))

chunk_size = 100
  
solar_wind_load_ba_ave_list = list()
for(bai in bas){
  
  message('\t',bai)
  
  load_ba = load_long |> filter(ba==bai)
  
  # solar 
  solar_config_ba = solar_config |> filter(ba==bai) 
  solar_wide_ba = solar_wide |> select(datetime, solar_config_ba |> pull(plant_code_unique))
  
  n_solar_plants = nrow(solar_config_ba)
  n_solar_chunks = ceiling(n_solar_plants/chunk_size)
  
  # split the data into chunks and compute the ba sum of each chunk 
  # otherwise my laptop runs out of memory -_- 
  solar_ba_ave_list = list()
  for(chunki in 1:n_solar_chunks){
    
    start = (chunki - 1) * chunk_size + 1
    end = min(chunki * chunk_size, n_solar_plants)
    
    solar_ba = solar_wide_ba |>
      select(datetime, start:end + 1) |>
      pivot_longer(-datetime, names_to='plant_code_unique') |>
      rename(datetime_utc = datetime) |>
      # convert to hour beginning
      mutate(datetime_utc = datetime_utc - hours(1)) |>
      inner_join(solar_config_ba, by='plant_code_unique') |>
      mutate(gen_mw = system_capacity*value)
    
    solar_ba_ave_list[[chunki]] = 
      solar_ba |>
      group_by(datetime_utc, ba) |>
      summarise(solar_gen_mw = sum(gen_mw),
                solar_capacity = sum(system_capacity), .groups='drop')
  }
  
  # add together all the chunks and compute the ba capacity factor 
  solar_ba_ave = solar_ba_ave_list |> 
    bind_rows() |> 
    group_by(datetime_utc, ba) |> 
    summarise(ba = ba[1], 
              solar_gen_mw = sum(solar_gen_mw), 
              solar_capacity = sum(solar_capacity),
              .groups='drop') |>
    mutate(solar_cf = solar_gen_mw/solar_capacity) |>
    inner_join(solar_count, by = 'ba')
    
  
  # wind 
  wind_config_ba = wind_config |> filter(ba==bai) 
  wind_wide_ba = wind_wide |> select(datetime, wind_config_ba |> pull(plant_code_unique))
    
  n_wind_plants = nrow(wind_config_ba)
  n_wind_chunks = ceiling(n_wind_plants/chunk_size)
  
  wind_ba_ave_list = list()
  for(chunki in 1:n_wind_chunks){
    
    start = (chunki - 1) * chunk_size + 1
    end = min(chunki * chunk_size, n_wind_plants)
    
    wind_ba = wind_wide_ba |>
      select(datetime, start:end + 1) |>
      pivot_longer(-datetime, names_to='plant_code_unique') |>
      rename(datetime_utc = datetime) |>
      # convert to hour beginning
      mutate(datetime_utc = datetime_utc - hours(1)) |>
      inner_join(wind_config_ba, by='plant_code_unique') |>
      mutate(gen_mw = system_capacity*value)
  
    
    wind_ba_ave_list[[chunki]] = wind_ba |>
      group_by(datetime_utc, ba) |>
      summarise(wind_gen_mw = sum(gen_mw),
                wind_capacity = sum(system_capacity), .groups='drop')
  }
  
  wind_ba_ave = wind_ba_ave_list |> 
    bind_rows() |> 
    group_by(datetime_utc, ba) |> 
    summarise(ba = ba[1], 
              wind_gen_mw = sum(wind_gen_mw), 
              wind_capacity = sum(wind_capacity),
              .groups='drop') |>
    mutate(wind_cf = wind_gen_mw/wind_capacity) |>
    inner_join(wind_count, by = 'ba')
  
  
  solar_wind_load_ba_ave_list[[bai]] = solar_ba_ave |> 
    inner_join(wind_ba_ave, by=c('datetime_utc','ba')) |>
    inner_join(load_ba, by=c('datetime_utc','ba'))
  
}
solar_wind_load_ba_ave = bind_rows(solar_wind_load_ba_ave_list) |>
  inner_join(timezones_ba, by='ba')

for(i in 1:length(periods)){
  period = periods[i]
  period_name = names(periods)[i]
  message(period_name)
  
  ba_gen = solar_wind_load_ba_ave |> 
    group_by(ba) |>
    mutate(datetime_local = with_tz(datetime_utc, timezone),
           year = year(datetime_utc)) |>
    # due to utc to local conversion, some hour occur before the beginning of the
    # first year, chop off this period to make the periods line up nicely with 
    # local time; the downside is that the last year will have a few missing 
    # hours at the end of the timeseries 
    filter(datetime_local >= force_tzs(ymd_hms(sprintf('%s-01-01 00:00:00',start_year)),timezone)) |>
    group_by(ba, year) |>
    # TODO correct for DST
    mutate(period = nhour_periods(hour(datetime_local), period)) |>
    group_by(ba, year, period) |>
    summarise(solar_gen_mwh = sum(solar_gen_mw)/1000,
              solar_capacity_mwh = sum(solar_capacity)/1000, 
              wind_gen_mwh = sum(wind_gen_mw)/1000,
              wind_capacity_mwh = sum(wind_capacity)/1000,
              load_mwh = sum(load_cf*max_load),
              load_max_mwh = sum(max_load),
              # rename for the output since write_csv automatically 
              datetime_utc = datetime_local[1],
              timezone=timezone[1],
              .groups='drop') |>
    mutate(wind_cf = wind_gen_mwh/wind_capacity_mwh,
           solar_cf = solar_gen_mwh/solar_capacity_mwh,
           load_cf = load_mwh/load_max_mwh)
  # will convert dates to utc 
  write_csv(ba_gen, sprintf('data/ba_solar_wind_load_%s_%s.csv', scenario, period_name), progress=FALSE)
}


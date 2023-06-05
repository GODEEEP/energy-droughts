# install.packages(c('import','xfun'))
xfun::pkg_attach2('ggplot2')
import::from(readr, read_csv, write_csv)
import::from(dplyr, bind_rows, rename, inner_join, mutate, group_by, summarise, filter, between, slice, pull)
import::from(lubridate, with_tz, year, month, day, hour)
import::from(sf, read_sf, st_transform, st_as_sf, st_intersects)
import::from(ggthemes, colorblind_pal)
import::from(tidyr, drop_na)
library(RColorBrewer)

periods = c('1-hour'=1,'4-hour'=4,'12-hour'=12,'1-day'=24,'2-day'=48,'3-day'=72,'5-day'=120)

dir.create('plots', showWarnings=FALSE)

ba_sf = read_sf('data/Elec_Control_Areas_BA')
states = map_data("state")
p_ba_map = ggplot(ba_sf |> filter(COMP_ABRV %in% c('BPA','CALISO','IDPC','PNM','WAPMON'))) +
  geom_polygon(aes(long,lat, group=group), color='black', fill='white',data=states)+
  geom_sf(aes(fill = COMP_ABRV)) +
  geom_sf_label(aes(label = COMP_ABRV))+
  scale_fill_manual('BA',values=colorblind_pal()(6)[-1])+
  coord_sf(xlim=c(-125,-100),ylim=c(32,50))+
  theme_bw()+
  labs(x='',y='')
p_ba_map
ggsave('plots/map_ba.png', p_ba_map, width=11, height=10, dpi=600)

wind_config = read_csv('data/tgw-gen/wind/eia_wind_configs.csv', show=FALSE, progress=FALSE)
solar_config = read_csv('data/tgw-gen/solar/eia_solar_configs.csv', show=FALSE, progress=FALSE)
hull_data = wind_config |> group_by(ba) |> slice(chull(lon, lat))

nerc_sf = read_sf('data/Elec_NERC_Regions_Subregions/')
nerc_conus_regions = c('NWPPUS','CAMX','AZNMSNV','ERCOTS','RMPA','MROUSS','SPPSR','RFC',
                       'MISO','GAT','CENTRAL','DELTA','SOUTHEA','FRCCS','VACAR','PJM',
                       'NYPP','NEPOOL')
nerc_sf_conus = nerc_sf |> filter(SUB_ABBREV %in% nerc_conus_regions)
p_nerc_subregion_map = ggplot(nerc_sf_conus) + 
  geom_polygon(aes(long,lat, group=group), color='black', fill='white',data=states) + 
  geom_sf(aes(fill = SUB_ABBREV)) + 
  geom_sf_label(aes(label = SUB_ABBREV),fill=grey(.9,.5),size=2.5,label.size=0) + 
  scale_fill_manual('',values=colorRampPalette(colorblind_pal()(8)[-1])(length(nerc_conus_regions)))+
  #coord_sf(expand = FALSE) + 
  theme_bw() + 
  theme(legend.position='none')
p_nerc_subregion_map
ggsave('plots/map_nerc_subregions.png', p_nerc_subregion_map, width=11, height=6, dpi=600)

n_nerc_regions = nerc_sf_conus$NERC_ABBRV |> unique() |> length()
p_nerc_map = ggplot(nerc_sf_conus) + 
  geom_polygon(aes(long,lat, group=group), color='black', fill='white',data=states) + 
  geom_sf(aes(fill = NERC_ABBRV)) + 
  geom_sf_label(aes(label = SUB_ABBREV),fill=grey(.9,.5),size=2.5,label.size=0) + 
  scale_fill_manual('',values=colorRampPalette(colorblind_pal()(8)[-1])(n_nerc_regions))+
  #coord_sf(expand = FALSE) + 
  theme_bw() 
  #theme(legend.position='none')
p_nerc_map
ggsave('plots/map_nerc.png', p_nerc_map, width=11, height=5, dpi=600)


# for(period in periods){
  # period_name = names(periods)[period == periods]
  period_name = '1-hour'
  # message(period_name)
  
  ba_gen = read_csv(sprintf('data/ba_solar_wind_load_%s.csv',period_name), show=FALSE, progress=FALSE) |> 
    group_by(timezone) |>
    mutate(datetime_local = with_tz(datetime_utc, timezone),
           month = month(datetime_local),
           day = day(datetime_local),
           hour = hour(datetime_local))
  
  hour_of_day_ave = ba_gen |>
    group_by(ba, hour) |>
    summarise(wind_cf_mean = mean(wind_cf),
              solar_cf_mean = mean(solar_cf),
              load_cf_mean = mean(load_cf),
              wind_cf_q10 = quantile(wind_cf,.1),
              solar_cf_q10 = quantile(solar_cf,.1),
              load_cf_q10 = quantile(load_cf,.1),
              wind_cf_q90 = quantile(wind_cf,.9),
              solar_cf_q90 = quantile(solar_cf,.9),
              load_cf_q90 = quantile(load_cf,.9),
              .groups='drop')
  
  colors = c("load" = "black", "solar" = "darkorange", "wind" = "steelblue")
  p_hour = ggplot(hour_of_day_ave) + 
    geom_line(aes(hour,load_cf_mean, color='load')) + 
    #geom_line(aes(hour,load_cf_q10), color='black', linetype='dashed') + 
    #geom_line(aes(hour,load_cf_q90), color='black', linetype='dashed') + 
    geom_ribbon(aes(hour,ymin=load_cf_q10,ymax=load_cf_q90,fill='load'), alpha=.2) + 
    geom_line(aes(hour,solar_cf_mean, color='solar')) + 
    #geom_line(aes(hour,solar_cf_q10), color='darkorange', linetype='dashed') + 
    #geom_line(aes(hour,solar_cf_q90), color='darkorange', linetype='dashed') + 
    geom_ribbon(aes(hour,ymin=solar_cf_q10,ymax=solar_cf_q90, fill='solar'), alpha=.3) + 
    geom_line(aes(hour,wind_cf_mean, color='wind')) + 
    #geom_line(aes(hour,wind_cf_q10), color='cornflowerblue', linetype='dashed') + 
    #geom_line(aes(hour,wind_cf_q90), color='cornflowerblue', linetype='dashed') + 
    geom_ribbon(aes(hour,ymin=wind_cf_q10,ymax=wind_cf_q90,fill='wind'), alpha=.3) + 
    scale_color_manual('',values=colors)+
    scale_fill_manual('',values=colors)+
    theme_bw()+
    labs(y='Capacity Factor', x = 'Hour') + 
    facet_wrap(~ba,nrow=3)+
    theme(legend.position='top')
  p_hour
  ggsave(sprintf('plots/ba_hour_ave_%s.png',period_name), p_hour, width=11, height=8, dpi=600)
  
  
  month_day_ave = ba_gen |>
    group_by(ba, year, month, day) |>
    summarise(wind_cf = mean(wind_cf),
              solar_cf = mean(solar_cf),
              load_cf = mean(load_cf),
              .groups='drop') |>
    group_by(ba, month, day) |>
    summarise(wind_cf_mean = median(wind_cf),
              solar_cf_mean = median(solar_cf),
              load_cf_mean = median(load_cf),
              wind_cf_q10 = quantile(wind_cf,.1),
              solar_cf_q10 = quantile(solar_cf,.1),
              load_cf_q10 = quantile(load_cf,.1),
              wind_cf_q90 = quantile(wind_cf,.9),
              solar_cf_q90 = quantile(solar_cf,.9),
              load_cf_q90 = quantile(load_cf,.9),
              month_day = as.POSIXct(sprintf('2000-%02d-%02d',month[1],day[1])),
              .groups='drop')
  
  p_month_day = ggplot(month_day_ave) + 
    geom_line(aes(month_day,load_cf_mean, color='load')) + 
    geom_ribbon(aes(month_day,ymin=load_cf_q10,ymax=load_cf_q90, fill='load'), alpha=.2) + 
    geom_line(aes(month_day,solar_cf_mean, color='solar'), color='darkorange') + 
    geom_ribbon(aes(month_day,ymin=solar_cf_q10,ymax=solar_cf_q90, fill='solar'), alpha=.3) + 
    geom_line(aes(month_day,wind_cf_mean, color='wind')) + 
    geom_ribbon(aes(month_day,ymin=wind_cf_q10,ymax=wind_cf_q90, fill='wind'), alpha=.3) + 
    scale_x_datetime(breaks='month', date_labels='%b') +
    facet_wrap(~ba, nrow=3)+
    theme_bw() +
    theme(axis.text.x=element_text(angle=90,vjust=.5)) +
    labs(y='Capacity Factor', x = 'Month')+
    scale_color_manual('',values=colors)+
    scale_fill_manual('',values=colors)+
    theme(legend.position='top')
  p_month_day
  ggsave(sprintf('plots/ba_month_day_%s.png',period_name), p_month_day, width=11, height=8, dpi=600)
  
  #cyclical plot no load 
  p_month_day_noload = ggplot(month_day_ave) + 
    #geom_line(aes(month_day,load_cf_mean, color='load')) + 
    #geom_ribbon(aes(month_day,ymin=load_cf_q10,ymax=load_cf_q90, fill='load'), alpha=.2) + 
    geom_line(aes(month_day,solar_cf_mean, color='solar')) + 
    geom_ribbon(aes(month_day,ymin=solar_cf_q10,ymax=solar_cf_q90, fill='solar'), alpha=.3) + 
    geom_line(aes(month_day,wind_cf_mean, color='wind')) + 
    geom_ribbon(aes(month_day,ymin=wind_cf_q10,ymax=wind_cf_q90, fill='wind'), alpha=.3) + 
    scale_x_datetime(breaks='month', date_labels='%b') +
    facet_wrap(~ba, nrow=3)+
    theme_bw() +
    theme(axis.text.x=element_text(angle=90,vjust=.5)) +
    labs(y='Capacity Factor', x = 'Month')+
    scale_color_manual('',values=colors)+
    scale_fill_manual('',values=colors)+
    theme(legend.position='top')
  p_month_day_noload
  ggsave(sprintf('plots/ba_month_day_noload_%s.png',period_name), p_month_day_noload, width=11, height=8, dpi=600)
# }
  
  # point maps 
  configs = bind_rows(wind_config |> mutate(type='wind'), 
                      solar_config |> mutate(type='solar'))
  nba <- configs |> pull(ba) |> unique() |> length()
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  #pie(rep(1,n), col=sample(col_vector, n))
  set.seed(999)
  p_wind_solar_point_map = ggplot(configs) + 
    geom_polygon(aes(long,lat, group=group), color=grey(.5), fill='white',data=states) + 
    geom_point(aes(lon,lat,fill=ba),stroke=0.5,size=3,shape=21,color='black') + 
    facet_wrap(~type,ncol=1)+
    scale_fill_manual('',values=sample(col_vector,nba))+
    #coord_sf(expand = FALSE) + 
    theme_bw() #+ 
    #theme(legend.position='none')
  p_wind_solar_point_map
  ggsave('plots/map_wind_solar_points.png', p_wind_solar_point_map, width=11, height=9, dpi=600)
  
  # plot points
  bas = c("BPAT", "CISO", "ERCO", "IPCO", "ISNE", "MISO", "NWMT", "NYIS", 
          "PACE", "PACW", "PJM", "PNM", "PSCO", "SWPP", "WACM")
  wind_config_min_sites = wind_config |> filter(ba %in% bas) |> mutate(ttype='EIA 2020 Wind')
  solar_config_min_sites = solar_config |> filter(ba %in% bas) |> mutate(ttype='EIA 2020 Solar')
  config_min_sites = bind_rows(wind_config_min_sites, solar_config_min_sites) |>
    mutate(ba = case_when(ba == 'ERCO' ~ "ERCOT",
                          ba == 'ISNE' ~ "ISONE",
                          ba == 'NYIS' ~ "NYISO",
                          .default = ba))
  cols = colorRampPalette(colorblind_pal()(8)[-1])(length(bas))
  chull_points = config_min_sites |> group_by(ba, ttype) |> slice(chull(lon, lat))
  chull_centroids = chull_points |> group_by(ba) |> summarise(lon=mean(lon),lat=mean(lat),.groups='drop')
  states = map_data("state")
  
  p_wind_solar_points = ggplot(config_min_sites) + 
    geom_polygon(aes(long,lat, group=group), color='black', fill='white',data=states) + 
    geom_point(aes(lon,lat,color=ba)) + 
    scale_color_manual('',values=cols)+
    scale_fill_manual('',values=cols)+
    theme_bw()+
    #geom_polygon(aes(lon,lat,color=ba,fill=ba),alpha=.3,data=chull_points) +
    geom_label(aes(x=lon,y=lat,label=ba), data=chull_centroids, alpha=.5, size=3)+
    facet_wrap(~ttype, ncol=1)+
    coord_equal()+
    labs(x='Longitude',y='Latitude')
  p_wind_solar_points
  ggsave('plots/ba_map_points.png',p_wind_solar_points, width=10, height=8)
  
  
  p_wind_solar_plant_map = ggplot(configs |> mutate(type=factor(type,levels=c('solar','wind')))) +
    geom_polygon(aes(long,lat, group=group), color='black', fill='white',data=states) + 
    geom_point(aes(lon, lat, color=type, shape=type), size=1) + 
    theme_bw() + 
    scale_color_manual('',values=colorblind_pal()(3)[-1])+
    coord_equal()+
    scale_shape_manual('',values=c(1,2))+labs(x='Longitude',y='Latitude')
  p_wind_solar_plant_map
  ggsave('plots/map_points.png',p_wind_solar_plant_map, width=8, height=3.5)
  
  
  

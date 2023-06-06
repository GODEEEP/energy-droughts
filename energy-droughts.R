# install.packages(c('import','xfun'))
xfun::pkg_attach2('ggplot2')
import::from(readr, read_csv, write_csv)
import::from(dplyr, bind_rows, rename, inner_join, mutate, group_by, summarise, filter, between, 
             lead, ungroup, select, pull, slice, case_when)
import::from(lubridate, with_tz, year, month, day, hour, week, yday)
import::from(ggthemes, colorblind_pal, scale_color_colorblind)
import::from(tidyr, pivot_longer)
import::from(RColorBrewer, brewer.pal)

source('lib.R')

colorblind_ramp = function(n){
  colorRampPalette(colorblind_pal()(8)[-1])(n)
}

periods = c('1_1-hour'=1,
            '2_4-hour'=4,
            '3_12-hour'=12,
            '4_1-day'=24,
            '5_2-day'=48,
            '6_3-day'=72,
            '7_5-day'=120
            )
# periods = c('1_1-hour'=1)
lower_thresh = c('1_1-hour'=0.1,
                 '2_4-hour'=0.1,
                 '3_12-hour'=0.1,
                 '4_1-day'=0.1,
                 '5_2-day'=0.1,
                 '6_3-day'=0.1,
                 '7_5-day'=0.1)
upper_thresh = 1-lower_thresh

# run the sensitivity analysis and produce plots (takes a while)
do_sensitivity = FALSE

for(i in 1:length(periods)){
  period = periodi = periods[i]
  period_name = names(periods)[i]
  period_name_noindex = strsplit(period_name,'_')[[1]][2]
  lt = lower_thresh[i]
  ut = upper_thresh[i]
  message('\n',period_name)
  
  message('Reading data')
  ba_gen = read_csv(sprintf('data/ba_solar_wind_load_historical_%s.csv', 
                            period_name_noindex), show=FALSE, progress=FALSE) |> 
    group_by(timezone) |>
    mutate(datetime_local = with_tz(datetime_utc, timezone[1]),
           year = year(datetime_local),
           month = month(datetime_local),
           day = day(datetime_local),
           jday = yday(datetime_local),
           hour = hour(datetime_local),
           week = week(datetime_local),
           residual_load_mwh = load_mwh-solar_gen_mwh-wind_gen_mwh) |>
    # normalize the laod data each year to account for increasing load signal
    group_by(ba,year) |>
    mutate(load_norm = (residual_load_mwh-mean(residual_load_mwh))/sd(residual_load_mwh),
           load_max_norm = (load_max_mwh-mean(load_mwh))/sd(load_mwh)) |>
    # something is wrong with the 1980 data
    filter(year > 1980) |>
    group_by(ba, week, hour) |>
    #group_by(ba, hour) |>
    mutate(zero_prob = length(which(solar_gen_mwh == 0))/length(solar_gen_mwh),
           srepi_solar = sdei(solar_gen_mwh),
           srepi_wind = sdei(wind_gen_mwh),
           srli_load = sdei(load_norm),
           solar_q = ecdf(solar_gen_mwh)(solar_gen_mwh),
           wind_q = ecdf(wind_gen_mwh)(wind_gen_mwh),
           load_q = ecdf(load_norm)(load_norm),
           residual_load_q = ecdf(residual_load_mwh)(residual_load_mwh),
           solar_q10 = quantile(solar_gen_mwh,lt),
           wind_q10 = quantile(wind_gen_mwh,lt),
           load_q90 = quantile(load_norm,ut),
           residual_load_q90 = quantile(residual_load_mwh,ut)) |>
    group_by(ba, hour) |>
    mutate(srepi_solar_fixed = sdei(solar_gen_mwh),
           srepi_wind_fixed = sdei(wind_gen_mwh),
           srli_load_fixed = sdei(load_norm)) |>
    ungroup() |>
    mutate(ba = case_when(ba == 'ERCO' ~ "ERCOT",
                          ba == 'ISNE' ~ "ISONE",
                          ba == 'NYIS' ~ "NYISO",
                          .default = ba)) 
  
  n_years = ba_gen$year |> unique() |> length()
  
  
  ####################################
  ####################################
  ####################################
  # Droughts
  ####################################
  ####################################
  ####################################
  message('Droughts')
  
  energy_drought <- function(gen, criteria){
    gen |> 
      # use night hours as a wild card for solar, let droughts continue overnight 
      mutate(solar_q = ifelse(zero_prob > .99, 0, solar_q)) |>
      #filter(wind_q < lt & solar_q <= lt) |>
      filter({{criteria}}) |>
      group_by(ba) |>
      mutate(run_length = run_length(datetime_utc, run_diff=periodi),
             run_id = run_length(datetime_utc, unique_id=TRUE, run_diff=periodi),
             run_length_days = run_length*periodi/24,
             duration_days = run_length_days,
             severity_mwh = ((solar_q10 - solar_gen_mwh) + (wind_q10 - wind_gen_mwh)),
             # standardize 
             #severity = (severity_mwh)/sd(severity_mwh),
             )
  }
  energy_drought_filter <- function(ed){
    ed |>
      group_by(ba, run_id) |>
      # change the wild card values so they dont contribute to the magnitude 
      # mutate(srepi_solar = ifelse(srepi_solar == -5, -1.28, srepi_solar)) |>
      summarise(datetime_local = datetime_local[1],
                timezone = timezone[1],
                run_length = run_length[1], 
                run_length_days = run_length_days[1],
                severity_ws = (mean(abs(srepi_solar[srepi_solar > -5]))+
                                 mean(abs(srepi_wind)))/2,
                severity_lws = (mean(abs(srepi_solar[srepi_solar > -5]))+
                                  mean(abs(srepi_wind))+
                                  mean(abs(srli_load)))/3,
                severity_mwh = sum(severity_mwh),
                zero_prob = mean(zero_prob),
                year = year[1],
                month = month[1],
                hour = hour[1],
                wind_cf = mean(wind_cf),
                solar_cf = mean(solar_cf),
                srepi_solar = mean(srepi_solar),
                srepi_wind = mean(srepi_solar),
                .groups='drop') |>
      # filter single timestep events that only occur at night
      filter(!(run_length == 1 & zero_prob == 1)) |>
      filter(!is.na(severity_ws) & !is.na(severity_lws))
  }
  
  # wind and solar
  # -1.28 corresponds to 10th percentile 
  droughts_q_all = ba_gen |> energy_drought(srepi_wind < -1.28 & srepi_solar < -1.28)
  # droughts_q_all = ba_gen |> energy_drought(wind_gen_mwh < wind_q10 & solar_gen_mwh <= solar_q10)
  #droughts_q_all = ba_gen |> energy_drought(wind_q < lt & solar_q <= lt)
  droughts_q = energy_drought_filter(droughts_q_all)
  
  # wind and solar fixed threshold
  # -1.28 corresponds to 10th percentile 
  droughts_fixed_all = ba_gen |> energy_drought(srepi_wind_fixed < -1.28 & srepi_solar_fixed < -1.28)
  # droughts_q_all = ba_gen |> energy_drought(wind_gen_mwh < wind_q10 & solar_gen_mwh <= solar_q10)
  #droughts_q_all = ba_gen |> energy_drought(wind_q < lt & solar_q <= lt)
  droughts_fixed = energy_drought_filter(droughts_fixed_all)
  
  # wind 
  # -1.28 corresponds to 10th percentile 
  # wind_droughts_q_all = ba_gen |> energy_drought(wind_gen_mwh < wind_q10)
  wind_droughts_q_all = ba_gen |> energy_drought(srepi_wind < -1.28)
  wind_droughts_q = energy_drought_filter(wind_droughts_q_all)
  
  wind_droughts_fixed_all = ba_gen |> energy_drought(srepi_wind_fixed < -1.28)
  wind_droughts_fixed = energy_drought_filter(wind_droughts_fixed_all)
  
  # solar
  # -1.28 corresponds to 10th percentile 
  # solar_droughts_q_all = ba_gen |> energy_drought(solar_gen_mwh < solar_q10)
  solar_droughts_q_all = ba_gen |> energy_drought(srepi_solar < -1.28)
  solar_droughts_q = energy_drought_filter(solar_droughts_q_all)
  
  solar_droughts_fixed_all = ba_gen |> energy_drought(srepi_solar_fixed < -1.28)
  solar_droughts_fixed = energy_drought_filter(solar_droughts_fixed_all)
  
  # load 
  # load_droughts_q_all = ba_gen |> energy_drought(load_norm > load_q90)
  # load_droughts_q = energy_drought_filter(load_droughts_q_all)
  
  # residual load 
  # 1.28 corresponds to 90th percentile
  # rl_droughts_q_all = ba_gen |> energy_drought(residual_load_mwh > residual_load_q90)
  rl_droughts_q_all = ba_gen |> energy_drought(srli_load > 1.28)
  rl_droughts_q = energy_drought_filter(rl_droughts_q_all)
  
  # wind and solar and load
  lws_droughts_q_all = ba_gen |> energy_drought(srepi_wind < -1.28 & srepi_solar < -1.28 & srli_load > 1.28)
  lws_droughts_q = energy_drought_filter(lws_droughts_q_all)
  
  
  # dont allow single hour droughts
  if(period_name_noindex == '1-hour'){
    droughts_q = droughts_q |> filter(run_length > 1)
    droughts_q_all = droughts_q_all |> filter(run_length > 1)
    
    wind_droughts_q = wind_droughts_q |> filter(run_length > 1)
    wind_droughts_q_all = wind_droughts_q_all |> filter(run_length > 1)
    
    solar_droughts_q = solar_droughts_q |> filter(run_length > 1)
    solar_droughts_q_all = solar_droughts_q_all |> filter(run_length > 1)
    
    rl_droughts_q = rl_droughts_q |> filter(run_length > 1)
    rl_droughts_q_all = rl_droughts_q_all |> filter(run_length > 1)
    
    lws_droughts_q = lws_droughts_q |> filter(run_length > 1)
    lws_droughts_q_all = lws_droughts_q_all |> filter(run_length > 1)
  }
  
  droughts_q |> rename(datetime_utc=datetime_local) |> 
    write_csv(sprintf('data/droughts/ws_droughts_%s.csv',period_name_noindex))
  droughts_fixed |> rename(datetime_utc=datetime_local) |> 
    write_csv(sprintf('data/droughts/ws_droughts_fixed_%s.csv',period_name_noindex))
  wind_droughts_q |> rename(datetime_utc=datetime_local)  |> 
    write_csv(sprintf('data/droughts/wind_droughts_%s.csv',period_name_noindex))
  wind_droughts_fixed |> rename(datetime_utc=datetime_local)  |> 
    write_csv(sprintf('data/droughts/wind_droughts_fixed_%s.csv',period_name_noindex))
  solar_droughts_q |> rename(datetime_utc=datetime_local)  |> 
    write_csv(sprintf('data/droughts/solar_droughts_%s.csv',period_name_noindex))
  solar_droughts_fixed |> rename(datetime_utc=datetime_local)  |> 
    write_csv(sprintf('data/droughts/solar_droughts_fixed_%s.csv',period_name_noindex))
  rl_droughts_q |> rename(datetime_utc=datetime_local)  |> 
    write_csv(sprintf('data/droughts/rl_droughts_%s.csv',period_name_noindex))
  lws_droughts_q |> rename(datetime_utc=datetime_local)  |> 
    write_csv(sprintf('data/droughts/lws_droughts_%s.csv',period_name_noindex))
  
  if(period >= 24){
    
    droughts_mwh_all = ba_gen |> energy_drought(wind_gen_mwh < wind_q10 & solar_gen_mwh <= solar_q10)
    droughts_mwh = energy_drought_filter(droughts_q_all)
    
    y = 1987
    ba_ = 'CISO'
    colors = c('wind'='steelblue','solar'='darkorange','load'=grey(0.3))
    
    p_threshold = ggplot(ba_gen |> filter(ba==ba_ & year ==y)) + 
      geom_vline(aes(xintercept=datetime_local), data=droughts_mwh_all |> filter(ba==ba_ & year ==y),color=grey(.5))+
      geom_line(aes(datetime_local, solar_gen_mwh/solar_capacity_mwh, color='solar'))+
      geom_line(aes(datetime_local, wind_gen_mwh/wind_capacity_mwh,color='wind'))+
      geom_line(aes(datetime_local, solar_q10/solar_capacity_mwh,color='solar'),linetype='dashed')+
      geom_line(aes(datetime_local, wind_q10/wind_capacity_mwh,color='wind'),linetype='dashed')+
      theme_bw()+
      scale_color_manual('',values=colors)+
      labs(y='Generation Capacity Factor', x = 'Date', title=period_name_noindex)
    p_threshold
    ggsave(sprintf('plots-ed/threshold_2019_%s.png',period_name), p_threshold, width=13, height=5, dpi=600)
    
    p_threshold_load = ggplot(ba_gen |> filter(ba==ba_ & year ==y)) + 
      geom_vline(aes(xintercept=datetime_local), data=lws_droughts_q_all |> filter(ba==ba_ & year ==y),color=grey(.5))+
      geom_line(aes(datetime_local, solar_gen_mwh/solar_capacity_mwh, color='solar'))+
      geom_line(aes(datetime_local, wind_gen_mwh/wind_capacity_mwh,color='wind'))+
      geom_line(aes(datetime_local, load_norm/load_max_norm,color='load'))+
      geom_line(aes(datetime_local, solar_q10/solar_capacity_mwh,color='solar'),linetype='dashed')+
      geom_line(aes(datetime_local, wind_q10/wind_capacity_mwh,color='wind'),linetype='dashed')+
      geom_line(aes(datetime_local, load_q90/load_max_norm,color='load'),linetype='dashed')+
      theme_bw()+
      scale_color_manual('',values=colors)+
      labs(y='Capacity Factor', x = 'Date', title=period_name_noindex)
    p_threshold_load
    ggsave(sprintf('plots-ed/threshold_load_2019_%s.png',period_name), 
           p_threshold_load, width=13, height=5, dpi=600)
  }
  
  plot_drought_length_histogram <- function(d){
    ggplot(d) + 
      geom_bar(aes(run_length_days, ..count../n_years), stat='count') + 
      facet_wrap(~ba, nrow=3) +
      theme_bw() +
      labs(x='Drought Length (days)', y='Droughts per year', title=period_name_noindex) #+ 
  }
  
  pd01 = plot_drought_length_histogram(droughts_q)
  pd01
  ggsave(sprintf('plots-ed/drought_length_%s.png',period_name), pd01, width=10, height=8, dpi=600)
  
  pd01wind = plot_drought_length_histogram(wind_droughts_q) 
  pd01wind
  ggsave(sprintf('plots-ed/wind_drought_length_%s.png',period_name), pd01wind, width=10, height=8, dpi=600)
  
  pd01solar = plot_drought_length_histogram(solar_droughts_q) 
  #theme(axis.text.x=element_text(angle=90))
  pd01solar
  ggsave(sprintf('plots-ed/solar_drought_length_%s.png',period_name), pd01solar, width=10, height=8, dpi=600)
  
  pd01lws = plot_drought_length_histogram(lws_droughts_q) 
  pd01lws
  ggsave(sprintf('plots-ed/lws_drought_length_%s.png',period_name), pd01lws, width=10, height=8, dpi=600)
  
  
  plot_drought_ecdf <- function(d){
    bas = ba_gen$ba |> unique()
    cols = colorblind_ramp(length(bas))
    cols[which(bas == 'CISO')] = 'black'
    ggplot(d) + 
      stat_ecdf(aes(run_length_days, color=ba)) + 
      theme_bw() + 
      scale_color_manual('BA',values=cols) + 
      labs(y='Cumulative Probability', x='Drought Length (days)', title=period_name_noindex)
  }
    
  p_ecdf = plot_drought_ecdf(droughts_q) 
  p_ecdf
  ggsave(sprintf('plots-ed/drought_length_ecdf_%s.png',period_name), p_ecdf, width=6, height=5, dpi=600)
  
  p_ecdf_wind = plot_drought_ecdf(wind_droughts_q)
  p_ecdf_wind
  ggsave(sprintf('plots-ed/wind_drought_length_ecdf_%s.png',period_name), p_ecdf_wind, width=6, height=5, dpi=600)
  
  p_ecdf_solar = plot_drought_ecdf(solar_droughts_q) 
  p_ecdf_solar
  ggsave(sprintf('plots-ed/solar_drought_length_ecdf_%s.png',period_name), p_ecdf_solar, width=6, height=5, dpi=600)
  
  p_ecdf_lws = plot_drought_ecdf(lws_droughts_q)
  p_ecdf_lws
  ggsave(sprintf('plots-ed/lws_drought_length_ecdf_%s.png',period_name), p_ecdf_lws, width=6, height=5, dpi=600)
  
  if(period < 24){
    pd03 = ggplot(droughts_q |> filter(zero_prob < 1)) + 
      geom_bar(aes(hour), stat='count') + 
      facet_wrap(~ba, nrow=3) +
      theme_bw() + 
      labs(x='Drought Hour', title=period_name_noindex)#+
    #theme(axis.text.x=element_text(angle=90,vjust=.5))
    pd03
    ggsave(sprintf('plots-ed/drought_occurance_hours_%s.png',period_name), pd03, width=11, height=8, dpi=600)
  }
  
  # pd04 = ggplot(droughts_q) + 
  #   geom_bar(aes(factor(month), fill=factor(run_length)), stat='count', position='dodge') + 
  #   scale_fill_manual('Drought\nRun\nLength', values=colorblind_pal()(8)[-1]) +
  #   facet_wrap(~ba, nrow=1) + 
  #   theme_bw() + 
  #   labs(x='Drought Month')+
  #   theme(axis.text.x=element_text(angle=90,vjust=.5))
  plot_drought_month <- function(d){
    ggplot(d |> filter(zero_prob < 1)) + 
      geom_bar(aes(factor(month), ..count../n_years, fill=factor(run_length_days |> round(2))), stat='count') + 
      scale_fill_manual('Drought\nLength\n(days)', values=colorblind_ramp(max(d$run_length))) +
      facet_wrap(~ba, nrow=3) + 
      theme_bw() + 
      labs(x='Drought Month', y='Droughts per Month (1980-2019)', title=period_name_noindex)+
      theme(axis.text.x=element_text(angle=90,vjust=.5))
  }
  pd04 = plot_drought_month(droughts_q)# |> filter(zero_prob < 1)) 
  pd04
  ggsave(sprintf('plots-ed/drought_occurance_month_%s.png',period_name), pd04, width=11, height=6, dpi=600)
  
  pd04_wind = plot_drought_month(wind_droughts_q) 
  pd04_wind
  ggsave(sprintf('plots-ed/wind_drought_occurance_month_%s.png',period_name), pd04_wind, width=11, height=6, dpi=600)
  
  pd04_solar = plot_drought_month(solar_droughts_q) 
  pd04_wind
  ggsave(sprintf('plots-ed/wind_drought_occurance_month_%s.png',period_name), pd04_wind, width=11, height=6, dpi=600)
  
  pd04_lws = plot_drought_month(lws_droughts_q) 
  pd04_lws
  ggsave(sprintf('plots-ed/lws_drought_occurance_month_%s.png',period_name), pd04_lws, width=11, height=6, dpi=600)
  
  
  
  plot_drought_month_binned <- function(d){
    
    drought_len_binned = d |> 
      group_by(ba) |> 
      filter(zero_prob<1) |>
      summarise(rl = run_length_days,
                run_length=cut(run_length_days, breaks=seq(0,15,by=.5)),
                month = month, .groups='drop')
    
    ggplot(drought_len_binned) + 
      geom_bar(aes(factor(month), ..count../40, fill=factor(run_length)), stat='count') + 
      scale_fill_manual('Drought\nLength\n(days) ', 
                        values=colorblind_ramp(length(unique(drought_len_binned$run_length)))) +
      facet_wrap(~ba, nrow=3) + 
      theme_bw() + 
      labs(x='Drought Month', y='Droughts per Month (1980-2019)', title=period_name_noindex)+
      theme(axis.text.x=element_text(angle=90,vjust=.5))
  }
  
  
  pd04_binned = plot_drought_month_binned(droughts_q) 
  pd04_binned
  ggsave(sprintf('plots-ed/drought_occurance_month_binned_%s.png',period_name), pd04_binned, 
         width=11, height=8, dpi=600)
  
  pd04_binned_fixed = plot_drought_month_binned(droughts_fixed) + facet_wrap(~ba, nrow=3, scales='free_y')
  pd04_binned_fixed
  ggsave(sprintf('plots-ed/drought_fixed_occurance_month_binned_%s.png',period_name), pd04_binned_fixed, 
         width=11, height=8, dpi=600)
  
  pd04_wind_binned_fixed = plot_drought_month_binned(wind_droughts_fixed) + facet_wrap(~ba, nrow=3, scales='free_y')
  pd04_wind_binned_fixed
  ggsave(sprintf('plots-ed/wind_drought_fixed_occurance_month_binned_%s.png',period_name), pd04_wind_binned_fixed, 
         width=11, height=8, dpi=600)
  
  pd04_solar_binned_fixed = plot_drought_month_binned(solar_droughts_fixed) + facet_wrap(~ba, nrow=3, scales='free_y')
  pd04_solar_binned_fixed
  ggsave(sprintf('plots-ed/solar_drought_fixed_occurance_month_binned_%s.png',period_name), pd04_solar_binned_fixed, 
         width=11, height=8, dpi=600)
  
  # pd04_bpat_caiso = plot_drought_month_binned(droughts_q |> filter(ba %in% c('BPAT','CISO'))) 
  # pd04_bpat_caiso
  # ggsave(sprintf('plots-ed/drought_occurance_month_caiso_bpat_%s.png',period_name), pd04_bpat_caiso, 
  #        width=8, height=4, dpi=600)
  # 
  # pd04_bpat_caiso_psco = plot_drought_month_binned(droughts_q |> filter(ba %in% c('BPAT','CISO','PSCO'))) 
  # pd04_bpat_caiso_psco
  # ggsave(sprintf('plots-ed/drought_occurance_month_caiso_bpat_psco_%s.png',period_name), 
  #        pd04_bpat_caiso_psco, width=3, height=8, dpi=600)
  
  
  pd04_binned_wind = plot_drought_month_binned(wind_droughts_q) 
  pd04_binned_wind
  ggsave(sprintf('plots-ed/wind_drought_occurance_month_binned_%s.png',period_name), pd04_binned_wind, 
         width=11, height=8, dpi=600)
  
  pd04_binned_solar = plot_drought_month_binned(solar_droughts_q) 
  pd04_binned_solar
  ggsave(sprintf('plots-ed/solar_drought_occurance_month_binned_%s.png',period_name), pd04_binned_solar, 
         width=11, height=8, dpi=600)
  
  pd04_binned_lws = plot_drought_month_binned(lws_droughts_q) 
  pd04_binned_lws
  ggsave(sprintf('plots-ed/lws_drought_occurance_month_binned_%s.png',period_name), pd04_binned_lws, 
         width=11, height=8, dpi=600)
  
  
  
  pd05 = ggplot(droughts_q) +
    geom_bar(aes(year), stat='count') +
    facet_wrap(~ba, nrow=3) +
    theme_bw() +
    labs(x='Drought Year', title=period_name_noindex)+
    theme(axis.text.x=element_text(angle=90,vjust=.5))
  pd05
  ggsave(sprintf('plots-ed/drought_occurance_year_%s.png',period_name), pd05, width=11, height=8, dpi=600)
  
  ####################################
  # drought length vs quantile limit
  ####################################
  if(do_sensitivity){
    drought_ave_len_list = list()
    pb = txtProgressBar(0.01,0.30,0.01,style=3)
    for(dq in seq(0.01,0.30,by=.01)){
      setTxtProgressBar(pb, dq)
      droughts_q_all = ba_gen |> energy_drought(srepi_wind < qnorm(dq) & srepi_solar < qnorm(dq))
      droughts_q = energy_drought_filter(droughts_q_all)
      
      drought_ave_len_list[[as.character(dq)]] = droughts_q |>
        group_by(ba) |>
        summarise(ave_run_len = mean(run_length_days),
                  max_run_len = max(run_length_days),
                  min_run_len = min(run_length_days),
                  ave_severity = mean(severity_ws),
                  max_severity = max(severity_ws),
                  min_severity = min(severity_ws)) |>
        mutate(q = dq)
    }
    close(pb)
    drought_ave_len = bind_rows(drought_ave_len_list)
    pd06 = ggplot(drought_ave_len)+
      geom_vline(aes(xintercept=0.1), linetype='dotted', color=grey(.2))+
      geom_line(aes(q,ave_run_len))+
      geom_line(aes(q,min_run_len),linetype='dashed')+
      geom_line(aes(q,max_run_len),linetype='dashed')+
      #scale_color_manual(values=colorblind_pal()(6)[-1])+
      theme_bw()+
      facet_wrap(~ba, nrow=3)+
      #scale_y_continuous(breaks=1:13,minor_breaks=1:13)+
      labs(y='Drought Length (days)', x='Quantile Threshold', title=period_name_noindex)
    pd06
    ggsave(sprintf('plots-ed/drought_run_vs_q_%s.png',period_name), pd06, width=11, height=8, dpi=600)
    
    pd06_severity = ggplot(drought_ave_len)+
      geom_vline(aes(xintercept=0.1), linetype='dotted', color=grey(.2))+
      geom_line(aes(q,ave_severity))+
      geom_line(aes(q,min_severity),linetype='dashed')+
      geom_line(aes(q,max_severity),linetype='dashed')+
      #scale_color_manual(values=colorblind_pal()(6)[-1])+
      theme_bw()+
      facet_wrap(~ba, nrow=3)+
      #scale_y_continuous(breaks=1:13,minor_breaks=1:13)+
      labs(y='Compound Drought Magnitude (CDM)', x='Quantile Threshold', title=period_name_noindex)
    pd06_severity
    ggsave(sprintf('plots-ed/drought_run_vs_q_severity_%s.png',period_name), pd06_severity, width=11, height=8, dpi=600)
  }
  
  ####################################
  # drought start hour
  ####################################
  if(period < 24){
    
    drought_start_hour <- function(d){
      drought_start_hour = d |> 
        group_by(ba, run_id) |>
        summarise(start_hour = hour[1],
                  end_hour = hour[length(hour)],
                  run_length = run_length[1],
                  .groups='drop')
    }
    
    plot_drought_start_hour <- function(d){
      dsh = drought_start_hour(d)
      ggplot(dsh) + 
        geom_bar(aes(start_hour,..count../40),stat='count') + #, fill=factor(run_length))) + 
        facet_wrap(~ba, nrow=3) + 
        #scale_fill_viridis_d('Drought Length (hours)')+#,colours=colorblind_pal()(8)) +
        #scale_fill_manual('Drought Length', values=colorblind_ramp(max(drought_start_hour$run_length))) +
        theme(legend.position='bottom') +
        theme_bw() + 
        labs(x='Drought Start Hour', title=period_name_noindex) + 
        theme(legend.position='top',
              legend.spacing.x=unit(0, 'cm'),
              legend.direction="vertical") +
        guides(fill = guide_legend(label.position = "left", nrow = 1, title.position='left'))
    }
    
    plot_drought_end_hour <- function(d){
      deh = drought_start_hour(d)
      ggplot(deh) + 
        geom_bar(aes(end_hour)) + #, fill=factor(run_length))) + 
        facet_wrap(~ba, nrow=3) + 
        #scale_fill_manual('Drought Length (hours)', values=colorblind_ramp(max(drought_start_hour$run_length))) +
        theme_bw() + 
        labs(x='Drought End Hour', title=period_name_noindex) + 
        theme(legend.position='top',
              legend.spacing.x=unit(0, 'cm'),
              legend.direction="vertical") +
        guides(fill = guide_legend(label.position = "left", nrow = 1, title.position='left'))
    }
    
    pd07 = plot_drought_start_hour(droughts_q_all) 
    pd07
    ggsave(sprintf('plots-ed/drought_start_hour_%s.png',period_name), pd07, width=10, height=8, dpi=600)
    
    pd07_wind = plot_drought_start_hour(wind_droughts_q_all) 
    pd07_wind
    ggsave(sprintf('plots-ed/drought_start_hour_%s.png',period_name), pd07_wind, width=10, height=8, dpi=600)
    
    pd07_solar = plot_drought_start_hour(solar_droughts_q_all) 
    pd07_solar
    ggsave(sprintf('plots-ed/drought_start_hour_%s.png',period_name), pd07_solar, width=10, height=8, dpi=600)
    
    pd07_lws = plot_drought_start_hour(lws_droughts_q_all) 
    pd07_lws
    ggsave(sprintf('plots-ed/drought_start_hour_%s.png',period_name), pd07_lws, width=10, height=8, dpi=600)
    
    pd08 = plot_drought_end_hour(droughts_q_all) 
    pd08
    ggsave(sprintf('plots-ed/drought_end_hour_%s.png',period_name), pd08, width=10, height=8, dpi=600)
    
    pd08_wind = plot_drought_end_hour(wind_droughts_q_all) 
    pd08_wind
    ggsave(sprintf('plots-ed/drought_end_hour_%s.png',period_name), pd08_wind, width=10, height=8, dpi=600)
    
    pd08_solar = plot_drought_end_hour(solar_droughts_q_all) 
    pd08_solar
    ggsave(sprintf('plots-ed/drought_end_hour_%s.png',period_name), pd08_solar, width=10, height=8, dpi=600)
    
    pd08_lws = plot_drought_end_hour(lws_droughts_q_all) 
    pd08_lws
    ggsave(sprintf('plots-ed/drought_end_hour_%s.png',period_name), pd08_lws, width=10, height=8, dpi=600)
  }
}


  if(FALSE){
  ###############################
  # droughts_cf = ba_gen |> 
  #   group_by(ba, month, hour) |>
  #   #filter(solar_cf>0) |>
  #   mutate(zero_prob = length(which(solar_cf == 0))/length(solar_cf)) |>
  #   filter(zero_prob < 1) |>
  #   ungroup() |>
  #   filter(wind_cf < .10 & solar_cf < .10) |>
  #   group_by(ba) |>
  #   mutate(run_length = run_length(datetime))
  #   
  # ggplot(droughts_cf) + geom_bar(aes(factor(run_length)), stat='count') + facet_wrap(~ba, nrow=1)
  # ggplot(droughts_cf |> filter(run_length==1)) + geom_point(aes(wind_cf, solar_cf)) + facet_wrap(~ba, nrow=1)
  # ggplot(droughts_cf) + geom_bar(aes(factor(hour)), stat='count') + facet_wrap(~ba, nrow=1)
  # ggplot(droughts_cf) + geom_bar(aes(factor(month)), stat='count') + facet_wrap(~ba, nrow=1)
  # ggplot(droughts_cf) + geom_bar(aes(factor(year)), stat='count') + facet_wrap(~ba, nrow=1)
  
  #############################
  # drought distributions
  #############################
  # pd09 = ggplot(droughts_q |> filter(zero_prob<1)) + 
  #   #geom_point(aes(wind_cf,solar_cf),bins=20) + 
  #   geom_bin2d(aes(wind_cf,solar_cf), bins=20) + 
  #   scale_fill_viridis_c() + 
  #   facet_wrap(~ba,nrow=3) +
  #   theme_bw() +
  #   labs(x='Wind Capacity Factor', y='Solar Capacity Factor', title=period_name_noindex)
  # pd09
  # ggsave(sprintf('plots-ed/drought_joint_%s.png',period_name), pd09, width=11, height=8, dpi=600)
  # 
  # pd09caliso = ggplot(droughts_q |> filter(zero_prob<1 & ba=='CISO')) + 
  #   #geom_point(aes(wind_cf,solar_cf),bins=20) + 
  #   geom_bin2d(aes(wind_cf,solar_cf), bins=20) + 
  #   scale_fill_viridis_c() + 
  #   facet_wrap(~ba,nrow=1) +
  #   theme_bw() +
  #   labs(x='Wind Capacity Factor', y='Solar Capacity Factor', title=period_name_noindex) +
  #   theme(legend.position='bottom')
  # pd09caliso
  # ggsave(sprintf('plots-ed/drought_joint_caliso_%s.png',period_name), pd09caliso, width=3, height=4, dpi=600)
  # 
  # pd10 = ggplot(droughts_q) + 
  #   geom_histogram(aes(wind_cf),bins=25) + 
  #   facet_wrap(~ba,nrow=3) +
  #   theme_bw() +
  #   labs(x='Wind Capacity Factor', title=period_name_noindex)
  # pd10
  # ggsave(sprintf('plots-ed/drought_marginal_wind_%s.png',period_name), pd10, width=11, height=8, dpi=600)
  # 
  # pd11 = ggplot(droughts_q |> filter(zero_prob<1)) + 
  #   geom_histogram(aes(solar_cf),bins=25) + 
  #   facet_wrap(~ba,nrow=3) +
  #   theme_bw() + 
  #   labs(x='Solar Capacity Factor', title=period_name_noindex)
  # pd11
  # ggsave(sprintf('plots-ed/drought_marginal_solar_%s.png',period_name), pd11, width=11, height=8, dpi=600)
  
  
  ####################################
  # surplus 
  ####################################
  # next
  # 
  # message('Surplus')
  # surplus_q = ba_gen |> 
  #   filter(wind_q > ut & solar_q > ut) |>
  #   group_by(ba) |>
  #   mutate(run_length = run_length(datetime_local, run_diff=periodi),
  #          run_id = run_length(datetime_local, unique_id=TRUE, run_diff=periodi)) 
  # 
  # surplus_q = surplus_q_all |>
  #   group_by(ba, run_id) |>
  #   summarise(datetime_local = datetime_local[1],
  #             timezone=timezone[1],
  #             run_length=run_length[1], 
  #             zero_prob=mean(zero_prob),
  #             year=year[1],
  #             month=month[1],
  #             hour=hour[1],
  #             wind_cf=mean(wind_cf),
  #             solar_cf=mean(solar_cf),
  #             .groups='drop')
  # 
  # if(period_name_noindex == '1-hour'){
  #   surplus_q = surplus_q |> filter(run_length > 1)
  #   surplus_q_all = surplus_q_all |> filter(run_length > 1)
  # }
  # #if(period_name_noindex == '1-hour')
  # #  surplus_q = surplus_q |> filter(run_length > 1)
  # 
  # ps01 = ggplot(surplus_q) + 
  #   geom_bar(aes(factor(run_length*periodi)), stat='count') + 
  #   facet_wrap(~ba, nrow=3) +
  #   theme_bw() +
  #   labs(x='Surplus Length (hours)', title=period_name_noindex) +
  #   theme(axis.text.x=element_text(angle=90,vjust=.5))
  # ps01
  # ggsave(sprintf('plots-ed/surplus_length_%s.png',period_name), ps01, width=11, height=8, dpi=600)
  # 
  # # ps02 = ggplot(surplus_q |> filter(run_length==2)) + 
  # #   geom_point(aes(wind_cf, solar_cf)) + 
  # #   facet_wrap(~ba, nrow=3) +
  # #   theme_bw() +
  # #   labs(title=sprintf('%s Conditional distributions (run length 2)', period_name_noindex),
  # #        x='Wind Capacity Factor', y='Solar Capacity Factor')
  # # ps02
  # # ggsave(sprintf('plots-ed/surplus_cond_dist_%s.png',period_name), ps02, width=11, height=8, dpi=600)
  # # 
  # # ps02w = ggplot(surplus_q |> filter(run_length==2)) + 
  # #   geom_histogram(aes(wind_cf), bins=10) + 
  # #   facet_wrap(~ba, nrow=3) +
  # #   theme_bw() +
  # #   labs(title=sprintf('%s Conditional distributions (run length 2)', period_name_noindex),
  # #        x='Wind Capacity Factor')
  # # ps02w
  # # ggsave(sprintf('plots-ed/surplus_cond_dist_wind_%s.png',period_name), ps02w, width=11, height=8, dpi=600)
  # # 
  # # ps02s = ggplot(surplus_q |> filter(run_length==2 & zero_prob < 1)) + 
  # #   geom_histogram(aes(solar_cf), bins=10) + 
  # #   facet_wrap(~ba, nrow=3) +
  # #   theme_bw() +
  # #   labs(title=sprintf('%s Conditional distributions (run length 2)', period_name_noindex),
  # #        x='Solar Capacity Factor')
  # # ps02s
  # # ggsave(sprintf('plots-ed/surplus_cond_dist_solar_%s.png',period_name), ps02s, width=11, height=8, dpi=600)
  # # 
  # psecdf = ggplot(surplus_q) + 
  #   stat_ecdf(aes(run_length * periodi/24, color=ba)) + 
  #   theme_bw() + 
  #   scale_color_manual('BA',values=colorblind_ramp(surplus_q$ba |> unique() |> length())) + 
  #   labs(y='Cumulative Probability', x='Surplus Length (days)')
  # psecdf
  # ggsave(sprintf('plots-ed/surplus_length_ecdf_%s.png',period_name), psecdf, width=4, height=3, dpi=600)
  # 
  # if(period < 24){
  #   ps03 = ggplot(surplus_q |> filter(zero_prob<1)) + 
  #     geom_bar(aes(hour), stat='count') + 
  #     facet_wrap(~ba, nrow=3) +
  #     theme_bw() + 
  #     labs(x='Surplus Hour')#+
  #   #theme(axis.text.x=element_text(angle=90,vjust=.5))
  #   ps03
  #   ggsave(sprintf('plots-ed/surplus_occurance_hours_%s.png',period_name), ps03, width=11, height=8, dpi=600)
  # }
  # 
  # ps04 = ggplot(surplus_q |> filter(zero_prob<1)) + 
  #   geom_bar(aes(factor(month), fill=factor(round(run_length * periodi/24,1))), stat='count') + 
  #   scale_fill_manual('Surplus Length (days)', values=colorblind_ramp(max(surplus_q$run_length))) +
  #   facet_wrap(~ba, nrow=3) + 
  #   theme_bw() + 
  #   labs(x='Surplus Month', title=period_name_noindex)+
  #   theme(axis.text.x=element_text(angle=90,vjust=.5))+
  #   theme(legend.position='top',
  #         legend.spacing.x=unit(0, 'cm'),
  #         legend.direction="vertical") +
  #   guides(fill = guide_legend(label.position = "left", nrow = 1, title.position='left'))
  # ps04
  # ggsave(sprintf('plots-ed/surplus_occurance_month_%s.png',period_name), ps04, width=11, height=8, dpi=600)
  # 
  # ps05 = ggplot(surplus_q) + 
  #   geom_bar(aes(year), stat='count') + 
  #   facet_wrap(~ba, nrow=3) + 
  #   theme_bw() + 
  #   labs(x='Surplus Year', title=period_name_noindex)+
  #   theme(axis.text.x=element_text(angle=90,vjust=.5))
  # ps05
  # ggsave(sprintf('plots-ed/surplus_occurance_year_%s.png',period_name), ps05, width=11, height=8, dpi=600)
  # 
  # ####################################
  # # surplus length vs quantile limit
  # ####################################
  # surplus_ave_len_list = list()
  # for(sq in seq(.7,.99,by=.01)){
  #   surplus_ave_len_list[[as.character(sq)]] = ba_gen |> 
  #     filter(wind_q > sq & solar_q > sq) |>
  #     group_by(ba) |>
  #     mutate(run_length = run_length(datetime_local, run_diff=periodi),
  #            run_id = run_length(datetime_utc, unique_id=TRUE, run_diff=periodi)) |>
  #   group_by(ba, run_id) |>
  #   summarise(run_length=run_length[1], .groups='drop') |>
  #   group_by(ba) |>
  #   summarise(ave_run_len = mean(run_length),
  #             max_run_len = max(run_length),
  #             min_run_len = min(run_length)) |>
  #   mutate(q = sq)
  # }
  # surplus_ave_len = bind_rows(surplus_ave_len_list)
  # ps06 = ggplot(surplus_ave_len)+
  #   geom_vline(aes(xintercept=0.9), linetype='dotted', color=grey(.2))+
  #   geom_line(aes(q,ave_run_len))+
  #   geom_line(aes(q,min_run_len),linetype='dashed')+
  #   geom_line(aes(q,max_run_len),linetype='dashed')+
  #   #scale_color_manual(values=colorblind_pal()(6)[-1])+
  #   theme_bw()+
  #   facet_wrap(~ba, nrow=3)+
  #   #scale_y_continuous(breaks=1:12,minor_breaks=1:12)+
  #   labs(y='Surplus Length', x='Quantile Threshold', title=period_name_noindex)
  # ps06
  # ggsave(sprintf('plots-ed/surplus_run_vs_q_%s.png',period_name), ps06, width=11, height=8, dpi=600)
  # 
  # ####################################
  # # surplus start hour
  # ####################################
  # if(period < 24){
  #   surplus_start_hour = surplus_q |> 
  #     group_by(ba, run_id) |>
  #     summarise(start_hour = hour[1],
  #               end_hour = hour[length(hour)],
  #               run_length = run_length[1],
  #               .groups='drop')
  #   
  #   ps07 = ggplot(surplus_start_hour) + 
  #     geom_bar(aes(start_hour)) +#,fill=factor(run_length))) + 
  #     facet_wrap(~ba, nrow=3) + 
  #     #scale_fill_manual('Surplus Length (hours)', values=colorblind_ramp(max(surplus_start_hour$run_length))) +
  #     theme_bw() + 
  #     labs(x='Surplus Start Hour', title=period_name_noindex) + 
  #     #theme(axis.text.x=element_text(angle=90,vjust=.5))
  #     theme(legend.position='top',
  #           legend.spacing.x=unit(0, 'cm'),
  #           legend.direction="vertical") +
  #     guides(fill = guide_legend(label.position = "left", nrow = 1, title.position='left'))
  #   ps07
  #   ggsave(sprintf('plots-ed/surplus_start_hour_%s.png',period_name), ps07, width=11, height=8, dpi=600)
  #   
  #   ps08 = ggplot(surplus_start_hour) + 
  #     geom_bar(aes(end_hour)) + #,fill=factor(run_length))) + 
  #     facet_wrap(~ba, nrow=3) + 
  #     #scale_fill_manual('Surplus Length (hours)', values=colorblind_ramp(max(surplus_start_hour$run_length))) +
  #     theme_bw() + 
  #     labs(x='Surplus End Hour', title=period_name_noindex) + 
  #     theme(legend.position='top',
  #           legend.spacing.x=unit(0, 'cm'),
  #           legend.direction="vertical") +
  #     guides(fill = guide_legend(label.position = "left", nrow = 1, title.position='left')) 
  #   ps08
  #   ggsave(sprintf('plots-ed/surplus_end_hour_%s.png',period_name), ps08, width=11, height=8, dpi=600)
  # }
  # 
  # #############################
  # # surplus distributions
  # #############################
  # ps09 = ggplot(surplus_q |> filter(zero_prob<1)) + 
  #   geom_bin2d(aes(wind_cf,solar_cf), bins=20) + 
  #   scale_fill_viridis_c() + 
  #   facet_wrap(~ba,nrow=3) +
  #   theme_bw() +
  #   labs(x='Wind Capacity Factor', y='Solar Capacity Factor', title=period_name_noindex)
  # ps09
  # ggsave(sprintf('plots-ed/surplus_joint_%s.png',period_name), ps09, width=11, height=8, dpi=600)
  # 
  # ps10 = ggplot(surplus_q) + 
  #   geom_histogram(aes(wind_cf),bins=25) + 
  #   facet_wrap(~ba,nrow=3) +
  #   theme_bw() + 
  #   labs(x='Wind Capacity Factor', title=period_name_noindex)
  # ps10
  # ggsave(sprintf('plots-ed/surplus_marginal_wind_%s.png',period_name), ps10, width=11, height=8, dpi=600)
  # 
  # ps11 = ggplot(surplus_q |> filter(zero_prob<1)) + 
  #   geom_histogram(aes(solar_cf),bins=25) + 
  #   facet_wrap(~ba,nrow=3) +
  #   theme_bw() +
  #   labs(x='Solar Capacity Factor', title=period_name_noindex)
  # ps11
  # ggsave(sprintf('plots-ed/surplus_marginal_solar_%s.png',period_name), ps11, width=11, height=8, dpi=600)
  # 
  ###############################
  }




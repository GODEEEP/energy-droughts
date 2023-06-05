#' Title
#'
#' @param time 
#' @param unique_id 
#' @param pad 
#' @param run_diff 
#'
#' @return
#' @export
#'
#' @examples
run_length = function(time, unique_id=FALSE, pad=3, run_diff=1){
  
  lt = length(time)
  if(unique_id){
    l = character(lt)
    run_length_count = list()
  }else{
    l = numeric(lt)
  }
  
  i = 1
  rl = 1 
  start_run = 1
  end_run = 1
  run_len = 1
  
  while(i <= lt){
    i = i + 1
    d = as.integer(difftime(time[i], time[i-1], units='hours'))
    #message(time[i], ' ', d)
    if(d == run_diff & i < lt){
      # still in a run
      end_run = i
      run_len = run_len + 1
    }else{
      # run ended, or we hit the end
      if(unique_id){
        rlc = as.character(run_len)
        if(is.null(run_length_count[[rlc]])){
          run_length_count[[rlc]] = 1
        }else{
          run_length_count[[rlc]] = run_length_count[[rlc]] + 1
        }
        l[start_run:end_run] = sprintf(paste0('%02d_%0',pad,'d'), run_len, run_length_count[[rlc]])
      }else{
        l[start_run:end_run] = run_len
      }
      run_len = 1
      start_run = i
      end_run = i
      run_len = 1
    }
  }
  l
}

#' Title
#'
#' @param hours 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
nhour_periods <- function(hours, n){
  # generate unique periods for aggregation based on a 
  # sequence of hours from hourly data. n is the number of hours 
  # in each aggregation period. The data is assumed to start at the 
  # beginning of a day. 
  nh = length(hours)
  np = floor(nh/n)
  periods = rep(1:np, each=n)
  # in case the periods dont divide the input evenly
  if(nh %% n != 0) periods = c(periods, rep(np+1, nh %% n))
  periods
}


#' Title
#'
#' @param names 
#'
#' @return
#' @export
#'
#' @examples
dedup_names <- function(names){
  # stolen from an old version of pandas
  # https://stackoverflow.com/questions/24685012/
  # pandas-dataframe-renaming-multiple-identically-named-columns
  
  counts = list()
  
  for(i in 1:length(names)){
    col = names[i]
    cur_count = counts[[col]]
    
    if(is.null(cur_count)) cur_count = 0
  
    if(cur_count > 0){
      names[i] = sprintf('%s_%d', col, cur_count)
    }
    
    counts[[col]] = cur_count + 1
  }
  
  return(names)
}


#' Starndardized Drought Energy Index
#'
#' @param x vector of values to compute the index for, they need not be continuous in time 
#'
#' @return index value for each data point
#' @export
#'
#' @examples
sdei = function(x){
  n = length(x)
  e = ecdf(x)
  p = (1 + e(x) * n)/(n + 2)
  if(all(x==0)){
    # hack for energy droughts, if all values are zero then the plotting position 
    # returns a value slightly less than 1. This breaks the continuity of the energy 
    # droughts over night. So in this case set the index to a large negative value
    # to allow the drought to continue over night. 
    rep(-5, n)
  }else{
    qnorm(p, 0, 1)
  }
    
}

downsample_using_mean <- function(df, col_names) {
  per_sec_df <- NULL
  if(nrow(df) <= 0) {
    return(NULL)
  }
  
  max_time <- floor(tail(df$Time, 1))
  min_time <- floor(head(df$Time, 1))
  
  if(!is.numeric(max_time) || length(max_time) == 0 || !is.numeric(min_time) || length(min_time) == 0) {
    return(NULL)
  }
  
  ## get rid of "Frame #" attribute 
  df <- df[ , (colnames(df) %in% c("Time", col_names))]
  
  # print(paste('max_time: ', max_time))
  # print(paste('min_time: ', min_time))
  
  for(cur_time in c(min_time : max_time)) {   #Use minTime instead of 0
    one_sec_data = df[df$Time >= cur_time & df$Time < cur_time+1, ]
    
    # print(nrow(one_sec_data))
    # print(str(one_sec_data))
    
    if(nrow(one_sec_data) > 0) {
      mean_values = colMeans(one_sec_data, na.rm = TRUE)
      data = data.frame("Time" = cur_time)
      for (column in col_names) {
        if(is.nan(mean_values[column])) {
          ################
          # val = NA/''
          ################
          val = ''
        } else {
          val = mean_values[column]
        }
        d = data.frame(column = val, row.names = NULL)
        colnames(d) = c(column)
        data = cbind(data, d)
      }
      per_sec_df = rbind(per_sec_df, data)
      # print(head(per_sec_df, 2))
    }
    else {
      data = data.frame("Time" = cur_time)
      for (column in col_names) {
        ################
        # val = NA/''
        ################
        val = ''
        d = data.frame(column = val, row.names = NULL)
        colnames(d) = c(column)
        data = cbind(data, d)
      }
      per_sec_df = rbind(per_sec_df, data) 
    }
    # data = NULL
  }
  return(per_sec_df)
}
library(tidyverse) 
library(lubridate) 

# @PARAM: A dataframe that contains columns Time, CovertedTime, and `col_names` 
# @RETURN: A dataframe with incrementing Time starting from 0s, one unique CovertedTime per row, and the average `col_names` per CovertedTime 
downsample_using_mean_with_date <- function(df, col_names) { 
  ## check if datafram has values 
  if(nrow(df) == 0) { 
    return(NULL) 
  } 
  
  ## get rid of "Frame #" attribute 
  df <- df[ , (colnames(df) %in% c("Time", "CovertedTime", col_names))] 
  
  ## convert CovertedTime to POSIX time values 
  df$CovertedTime <- as.POSIXct(strptime(df$CovertedTime, format="%m/%d/%Y %H:%M", tz = "GMT"), origin="1970-01-01")
  min_time <- format(as.POSIXct(head(df$CovertedTime, 1)), '%m/%d/%Y %H:%M')
  max_time <- format(as.POSIXct(tail(df$CovertedTime, 1)), '%m/%d/%Y %H:%M')
  
  print(min_time)
  print(max_time)
  
  print(class(min_time))
  print(class(max_time))
  
  if(!is.POSIXct(min_time) || !is.POSIXct(max_time) || length(min_time) == 0 || length(max_time) == 0) { 
    return(NULL) 
  } 
  
  
  ## this for loop goes through each CovertedTime and calculates the average `col_names` and adds it to the per_sec_df 
  per_sec_df <- data.frame() 
  for(cur_time in c(min_time : max_time)) { 
    # print(cur_time)
    
    one_sec_data = filter(df, CovertedTime == cur_time) 
    print(paste("nrow(one_sec_data): ", nrow(one_sec_data)))
    time <- as.POSIXct(cur_time, origin="1970-01-01") 
    
    # if(nrow(one_sec_data) > 0) {
    #   # print("if")
    #   one_sec_data$Time[1] <- as.numeric(time - min_time, units="secs")
    #   # one_sec_data$Time[1] <- as.numeric(time, units="secs") 
    #   for (column in col_names) { 
    #     if(all(is.na(one_sec_data[, column]))) { 
    #       ################ 
    #       # val = NA/'' 
    #       ################ 
    #       one_sec_data[1, column] <- '' 
    #     } else { 
    #       one_sec_data[1, column] <- mean(one_sec_data[[column]], na.rm = TRUE) 
    #     } 
    #   } 
    #   per_sec_df = rbind(per_sec_df, one_sec_data[1, ]) 
    # } else { 
    #   # print("else")
    #   ## if no `col_names` value for CovertedTime, it will add an '' value 
    #   data = data.frame("Time" = as.numeric(time - min_time, units="secs"), "CovertedTime" = time)
    #   # data = data.frame("Time" = as.numeric(time, units="secs"), "CovertedTime" = time) 
    #   for (column in col_names) { 
    #     ################ 
    #     # val = NA/'' 
    #     ################ 
    #     val = '' 
    #     d = data.frame(column = val, row.names = NULL) 
    #     colnames(d) = c(column) 
    #     data = cbind(data, d) 
    #   } 
    #   per_sec_df = rbind(per_sec_df, data) 
    # } 
  } 
  return(per_sec_df) 
} 

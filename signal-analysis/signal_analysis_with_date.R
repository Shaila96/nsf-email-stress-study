#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
# install.packages('XLConnect', dependencies=T)
# install.packages('data.table', dependencies=T)
library(XLConnect)
library(scales)
library(ggplot2)
library(dplyr)
library(readr)

require(xlsx)
library(readxl)
library(lubridate)



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
subject_name <- 'T003-New'

pp_file_pattern <- '.*_pp.csv'
pp_with_session_file_pattern <- '.*_pp_with_session.csv'
nr_pp_file_pattern <- '.*_pp_nr.csv'
# pp_with_session_file_pattern <- '.*_pp_with_session_test.csv'
marker_file_pattern <- '.*_sessionmarkers.csv'
subj_interface_file_pattern <- '.*.xlsx'

s_interface_date_format <- '%a %b %d %H:%M:%S'



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
getMatchedFileNames <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F))
}

convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

convert_date <- function(data, date_format) {
  return(as.POSIXct(data, format=date_format))
}

convert_s_interface_date <- function(data) {
  convert_date(data, s_interface_date_format)
}

getHourMinSec <- function(date_to_convert) {
  if (!is(date_to_convert, 'character')) {
    date_to_convert <- strftime(date_to_convert, format='%H:%M:%S')
  }
  
  return(hms(date_to_convert))
}

convertTimestampSessionMarkers <- function(df, intermittent_df, subj_name, timestamp=c('startTimestamp', 'EndTimestamp')) { 
  
  df[[timestamp[1]]] <- as.POSIXct(strptime(df[[timestamp[1]]], format=s_interface_date_format)) 
  df[[timestamp[2]]] <- as.POSIXct(strptime(df[[timestamp[2]]], format=s_interface_date_format))
  
  intermittent_time <- getHourMinSec(intermittent_df$Baseline.Stress.Timestamp)
  print(intermittent_time)
  print(class(intermittent_time))
  print(hour(intermittent_time))
  
  min_time <- head(df[[timestamp[1]]], 1) 
  max_time <- tail(df[[timestamp[2]]], 1) 
  
  if (min_time > max_time) { 
    for (i in 1:nrow(df)) { 
      if (df[i, timestamp[1]] < min_time) { 
        df[i, timestamp[1]] <- df[i, timestamp[1]] + (12 * one_hour_sec) 
      } 
      if (df[i, timestamp[2]] < min_time) { 
        df[i, timestamp[2]] <- df[i, timestamp[2]] + (12 * one_hour_sec) 
      } 
    } 
    message(paste0('Timestamps changed for subject ', subj_name, '. ')) 
    flush.console() 
    return(df) 
    
  } 
  else if (hour(intermittent_time) > hour(max_time)) {
    df[[timestamp[1]]] <- df[[timestamp[1]]] + (12 * one_hour_sec)
    df[[timestamp[2]]] <- df[[timestamp[2]]] + (12 * one_hour_sec)
    message(paste0('Timestamps changed for ', subj_name, '. '))
    flush.console()
    return(df)
  }
  
  return(df) 
}

convertTimestamp <- function(df, intermittent_df, timestamp='Timestamp') { 
  
  df[[timestamp]] <- as.POSIXct(strptime(df[[timestamp]], format=s_interface_date_format)) 
  intermittent_time <- getHourMinSec(intermittent_df$Baseline.Stress.Timestamp)
  
  min_time <- head(df[[timestamp]], 1) 
  max_time <- tail(df[[timestamp]], 1) 
  
  ## First check if time starts in AM then goes to PM - we need to convert later times to 24-hour format 
  if (min_time > max_time) { 
    df[df[[timestamp]] < min_time, ][[timestamp]] = df[df[[timestamp]] < min_time, ][[timestamp]] + (12 * one_hour_sec) 
    return(list(df, TRUE))
    
    ## Now check if time is all PM - we need to convert ALL times to 24-hour format  
  } else if (hour(intermittent_time) > hour(max_time)) { 
    df[[timestamp]] = df[[timestamp]] + (12 * one_hour_sec) 
    return(list(df, TRUE)) 
  } 
  
  return(list(df, FALSE)) 
}

getNextSessionRows <- function(session_name, df, start_time, end_time) {
  return(rbind(df, data.frame(Session=session_name, StartTime=start_time, EndTime=end_time)))
}

getSignalWithSession <- function(marker_time_df, signal_df) {
  signal_with_session_df <- data.frame()
  for(m_idx in 1 : nrow(marker_time_df)) {
    cur_row <- marker_time_df[m_idx, ]
    print(paste("class(cur_row$StartTime): ", class(cur_row$StartTime)))
    print(paste("class(signal_df$CovertedTime): ", class(signal_df$CovertedTime)))
    temp_df <- signal_df %>% filter(cur_row$StartTime<=CovertedTime & CovertedTime<=cur_row$EndTime)
    
    temp_df <- cbind(temp_df, Session=rep(cur_row$Session, nrow(temp_df)))
    signal_with_session_df <- rbind(signal_with_session_df, temp_df)
  }
  
  return(signal_with_session_df)
}


get_consecutive_df <- function(df, idx) {
  for(j in idx:(nrow(df)-1)) {
    # for(j in idx:(idx+4)) {
    # print(paste('j: ', j))
    # print(paste('df[j+1, ]$Time: ', df[j+1, ]$Time))
    # print(paste('df[j, ]$Time: ', df[j, ]$Time))
    # print(paste('df[j+1, ]$Time - df[j, ]$Time: ', df[j+1, ]$Time - df[j, ]$Time))
    if ((df[j+1, ]$Time - df[j, ]$Time) > 1) {
      return(df[idx:j, ])
    }
  }
  
  return(df[idx:nrow(df), ])
}


split_session <- function() {
  
  merged_nr_pp_df <- tibble()
  pp_file_name <- getMatchedFileNames(getwd(), pp_file_pattern)
  marker_file_name <- getMatchedFileNames(getwd(), marker_file_pattern)
  
  
  
  ###################################################################################################################################
  ###################################################################################################################################
  subj_interface_file_name <- getMatchedFileNames(getwd(), subj_interface_file_pattern)
  subj_interface_df <- readWorksheet(XLConnect::loadWorkbook(file.path(getwd(), subj_interface_file_name)), sheet = 'Sheet1')
  # print(str(subj_interface_df))
  subj_name <- subj_interface_df$Participant.ID
  
  marker_df <- read_csv(marker_file_name)
  # CHECK!!!
  # marker_df <- convertTimestampSessionMarkers(marker_df, subj_interface_df, subj_name)
  
  # rb_start_time <- convert_s_interface_date(marker_df$startTimestamp[1])
  # rb_end_time <- convert_s_interface_date(marker_df$EndTimestamp[1])
  # 
  # presentation_start_time <- convert_s_interface_date(marker_df$startTimestamp[2])
  # presentation_end_time <- convert_s_interface_date(marker_df$EndTimestamp[2])
  # 
  # baseline_essay_start_time <- convert_date(subj_interface_df$Baseline.Essay.Timestamp[1], subj_interface_date_format)
  # stress_cond_start_time <- convert_date(subj_interface_df$Stress.Condition.Timestamp[1], subj_interface_date_format)
  # dual_task_start_time <- convert_date(subj_interface_df$Dual.Essay.Timestamp[1], subj_interface_date_format)
  # 
  # date(baseline_essay_start_time) <- as.Date(rb_start_time)
  # date(stress_cond_start_time) <- as.Date(rb_start_time)
  # date(dual_task_start_time) <- as.Date(rb_start_time)
  # 
  # baseline_essay_end_time <- baseline_essay_start_time + 5*60
  # stress_cond_end_time <- stress_cond_start_time + 5*60
  # dual_task_end_time <- dual_task_start_time + 50*60
  # 
  # 
  # marker_time_df <- data.frame()
  # marker_time_df <- getNextSessionRows('RestingBaseline', marker_time_df, rb_start_time, rb_end_time)
  # marker_time_df <- getNextSessionRows('BaselineWriting', marker_time_df, baseline_essay_start_time, baseline_essay_end_time)
  # marker_time_df <- getNextSessionRows('StressCondition', marker_time_df, stress_cond_start_time, stress_cond_end_time)
  # marker_time_df <- getNextSessionRows('DualTask', marker_time_df, dual_task_start_time, dual_task_end_time)
  # marker_time_df <- getNextSessionRows('Presentation', marker_time_df, presentation_start_time, presentation_end_time)
  # 
  # # convert_to_csv(marker_time_df, file.path(getwd(), paste0(substr(pp_file_name, 1, 24), '_marker.csv')))
  # 
  # pp_with_session_file_name <- getMatchedFileNames(getwd(), pp_file_pattern)
  # 
  # pp_df <- read_csv(file.path(getwd(), pp_file_name))
  # names(pp_df) <- c('Frame#',	'Time',	'Timestamp', 'Perspiration')
  # 
  # pp_df <- data.frame(convertTimestamp(pp_df, subj_interface_df)[1])
  # print(str(pp_df))
  
  
  
  
  
  
  
  # ##-----------------------------------------------------------------------------------------------------------------------##
  # # pp_df$NR_Perspiration <- remove_noise(pp_df$Perspiration)
  # # downsampled_pp_df <- downsample_using_mean(pp_df, c('Perspiration', 'NR_Perspiration'))
  # #
  # # downsampled_pp_df <- downsampled_pp_df[, c(1, 4, 2, 3)]
  # # names(downsampled_pp_df)[names(downsampled_pp_df) == 'Timestamp'] <- 'CovertedTime'
  # # convert_to_csv(downsampled_pp_df, file.path(session_dir, paste0(substr(file_name, 1, nchar(file_name)-7), '_pp_nr.csv')))
  # ##-----------------------------------------------------------------------------------------------------------------------##
  # 
  # # names(pp_df)[names(pp_df) == 'Timestamp'] <- 'CovertedTime' 
  # pp_df$CovertedTime <- pp_df$Timestamp
  # 
  # pp_with_session_df <- getSignalWithSession(marker_time_df, pp_df)
  # pp_with_session_df <- pp_with_session_df[, c(2, 3, 5, 6, 4)]
  # 
  # convert_to_csv(pp_with_session_df, file.path(getwd(), paste0(substr(pp_file_name, 1, nchar(pp_file_name)-4), '_with_session.csv')))
  # ###################################################################################################################################
  # ###################################################################################################################################
  
  
  
  
  
  # ###################################################################################################################################
  # ###################################################################################################################################
  # pp_with_session_file_name <- getMatchedFileNames(getwd(), pp_with_session_file_pattern)
  # pp_with_session_df <- read_csv(file.path(getwd(), pp_with_session_file_name))
  # ###################################################################################################################################
  # ###################################################################################################################################
  # 
  # 
  # 
  # 
  # # read_csv makes everything as value not as factor. So, convert those as factor at first, then get level
  # # session_level_list <- as.list(levels(as.factor(pp_with_session_df$Session)))
  # # print(paste('session_level_list: ', session_level_list))
  # 
  # # Reading and factoring from dataframe change the order of the sessions
  # session_level_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'DualTask', 'Presentation')
  # 
  # for(session_level in session_level_list) {
  #   session_level_df <- pp_with_session_df %>% filter(Session == session_level)
  # 
  #   print(paste('Current Session: ', session_level))
  #   # print(paste('nrow(session_level_df): ', nrow(session_level_df)))
  #   # print(paste('session_level_df[nrow(session_level_df), ]: ', session_level_df[nrow(session_level_df), ]))
  #   # print(paste('session_level_df[nrow(session_level_df)+1, ]: ', session_level_df[nrow(session_level_df)+1, ]))
  #   # print(head(session_level_df, 2))
  # 
  #   start_index <- 1
  #   while(1) {
  #     if (start_index < nrow(session_level_df)) {
  #       consecutive_df <- get_consecutive_df(session_level_df, start_index)
  #       nr_df <- as.data.frame(remove_noise(consecutive_df$Perspiration))
  # 
  #       merged_nr_pp_df <- rbind(merged_nr_pp_df, nr_df)
  #       # print(paste('start_index: ', start_index))
  #       print(paste('Start Time: ', floor(session_level_df[start_index, 1]), '      ', 
  #                   'End Time: ', floor(session_level_df[start_index + nrow(consecutive_df) - 1, 1])))
  #       # print(paste('Check the row index: ', nrow(merged_nr_pp_df)+1))
  #       
  #       start_index <- start_index + nrow(consecutive_df)
  # 
  #     } else break # For break while loop
  # 
  #   }
  # 
  #   # break # For one session only
  # }
  # 
  # # pp_with_session_df$NR_Perspiration_By_Sesion <- merged_nr_pp_df
  # pp_with_session_df <- cbind(pp_with_session_df, merged_nr_pp_df)
  # colnames(pp_with_session_df)[6] <- 'NR_Perspiration_By_Session'
  # # print(head(pp_with_session_df, 2))
  # 
  # pp_with_session_df$NR_Perspiration <- remove_noise(pp_with_session_df$Perspiration)
  # convert_to_csv(pp_with_session_df, file.path(getwd(), paste0(substr(pp_file_name, 1, nchar(pp_file_name)-4), '_nr.csv')))
  
  
  
  
  # ###################################################################################################################################
  # ###################################################################################################################################
  # nr_pp_file_name <- getMatchedFileNames(getwd(), nr_pp_file_pattern)
  # pp_with_session_df <- read_csv(file.path(getwd(), nr_pp_file_name))
  # ###################################################################################################################################
  # ###################################################################################################################################
  # 
  # pp_with_session_df <- pp_with_session_df[c(1:8000),]
  # # pp_with_session_df <- as.data.frame(pp_with_session_df[c(1:4000),])
  # 
  # downsampled_pp_df <- downsample_using_mean(pp_with_session_df, c('Perspiration', 'NR_Perspiration', 'NR_Perspiration_By_Session'))
  # convert_to_csv(downsampled_pp_df, file.path(getwd(), paste0(substr(pp_file_name, 1, nchar(pp_file_name)-4), '_downsampled_1.csv')))
  # 
  # print(downsampled_pp_df)
  
  
  
  
  # downsampled_pp_df$NR_Diff <- as.numeric(downsampled_pp_df$NR_Perspiration) - as.numeric(downsampled_pp_df$NR_Perspiration_By_Sesion)
  # print(head(downsampled_pp_df, 2))
  # 
  # setnames(downsampled_pp_df,
  #          old = c('NR_Perspiration','NR_Perspiration_By_Session'),
  #          new = c('NR_Perspiration_Per_Sec','NR_Perspiration_By_Session_Per_Sec'))
  # convert_to_csv(downsampled_pp_df, file.path(getwd(), paste0(substr(pp_file_name, 1, nchar(pp_file_name)-4), '_downsampled_2.csv')))
  
}



#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(file.path(current_dir, subject_name))

source('../@Scripts/@RemoveNoise.R')
source('../@Scripts/@DownSampleTimeStamp.R')


split_session()


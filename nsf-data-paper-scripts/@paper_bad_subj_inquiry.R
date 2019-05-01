#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(directlabels)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

source(file.path(dirname(current_dir), "@common_functions.R"))

data_dir <- 'data'

filtered_file_name <- 'full_df_filtered.csv'
non_filtered_file_name <- 'full_df_non_filtered.csv'
good_subj_file_name <- 'subj_good_df.csv'
mean_file_name <- 'result_df.csv'
bad_moderate_signal_file_name <- 'bad_moderate_signal.csv'


session_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'DualTask', 'Presentation')
# session_list <- c('Presentation')




#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

get_subj_with_high_br <- function() {
  filtered_df <- read_csv(file.path(data_dir, filtered_file_name))
  non_filtered_df <- read_csv(file.path(data_dir, non_filtered_file_name))
  
  # #########################################################################################
  # #####                        Finding subjects for High BR                           #####
  # #########################################################################################
  non_filtered_df <- non_filtered_df[, c(seq(1, 5), 8)]
  non_filtered_df <- non_filtered_df[complete.cases(non_filtered_df), ]
  
  filtered_df <- filtered_df[, c(seq(1, 5), 8)]
  filtered_df <- filtered_df[complete.cases(filtered_df), ]
  
  conditions <- levels(factor((filtered_df$Condition)))
  
  for(session in session_list) {
    print(session)
    
    non_filtered_session_df <- non_filtered_df %>% filter(Session == session)
    filtered_session_df <- filtered_df %>% filter(Session == session)
    
    non_fil_subjects <- levels(factor((non_filtered_session_df$Subject)))
    fil_subjects <- levels(factor((filtered_session_df$Subject)))
    print(setdiff(non_fil_subjects, fil_subjects))
    
    for(condition in conditions) {
      filtered_condition_df <- filtered_session_df %>% filter(Condition == condition)
      subjects <- levels(factor((filtered_condition_df$Subject)))
      print(condition)
      print(length(subjects))
    }
    # filtered_condition_df <- filtered_session_df %>% filter(Condition == "IL")
    # subjects <- levels(factor((filtered_condition_df$Subject)))
    # print(length(subjects))
  }
}

all_other_functions <- function() {
  # filtered_df <- read_csv(file.path(data_dir, filtered_file_name))
  # non_filtered_df <- read_csv(file.path(data_dir, non_filtered_file_name))
  
  
  #########################################################################################
  #####             Finding filtered subjects for High HR (E4)                        #####
  #########################################################################################
  # pp_all_df <- non_filtered_df[, c(seq(1, 5), 12)]
  # pp_all_df <- pp_all_df[complete.cases(pp_all_df), ]
  # 
  # for(sess_idx in 1 : length(session_list)) {
  #   session_name <- session_list[sess_idx]
  #   session_df <- pp_all_df %>% filter(Session == session_name)
  # 
  #   # session_df <- session_df[session_df$N.HR.non.filtered > 150, ]
  # 
  #   bad_subjects <- levels(factor((session_df$Subject)))
  #   print(session_df)
  #   print(session_name)
  #   print(bad_subjects)
  # 
  # }
  ###########################################################################################
  
  ###########################################################################################
  #####                    Finding missing subjects for EDA                             #####
  ###########################################################################################  
  # filtered_df <- filtered_df[, c(seq(1, 5), 11)]
  # filtered_df <- filtered_df[complete.cases(filtered_df), ]
  # all_good_subjects <- levels(factor((filtered_df$Subject)))
  # print(all_good_subjects)
  
  # non_filtered_df <- non_filtered_df[, c(seq(1, 5), 11)]
  # non_filtered_df <- non_filtered_df[complete.cases(non_filtered_df), ]
  # e4_subjects <- levels(factor((non_filtered_df$Subject)))
  # 
  # print(setdiff(all_good_subjects, e4_subjects))
  # print(length(setdiff(all_good_subjects, e4_subjects)))
  
  
  
  # #########################################################################################
  # #####                        Finding subjects for High HR                           #####
  # #########################################################################################
  # non_filtered_df <- non_filtered_df[, c(seq(1, 5), 7)]  
  # non_filtered_df <- non_filtered_df[complete.cases(non_filtered_df), ]
  # 
  # for(sess_idx in 1 : length(session_list)) {
  #   session_name <- session_list[sess_idx]
  #   session_df <- non_filtered_df %>% filter(Session == session_name)
  # 
  #   bad_subjects <- levels(factor((session_df$Subject)))
  #   print(session_name)
  #   print(bad_subjects)
  # }
  # #########################################################################################
  
  
  #########################################################################################
  #####                Finding subjects for missing presentation                      #####
  #########################################################################################  
  # session_df <- pp_all_df %>% filter(Session == 'Presentation')
  
  
  
  #########################################################################################
  #####                Finding subjects for missing presentation                      #####
  #########################################################################################  
  # session_df <- pp_all_df %>% filter(Session == 'Presentation')
  # session_df <- session_df %>% group_by(Subject)
  # # session_df <- session_df %>% group_by(Subject) %>% filter(all(is.na(PP)))
  # session_df_no <- session_df %>% summarize(n())
  # # bad_subjects <- levels(factor((session_df$Subject)))
}

get_subj_with_high_eda <- function() {
  filtered_df <- read_csv(file.path(data_dir, filtered_file_name))
  filtered_df <- filtered_df[, c('Subject', 'Condition', 'Session', 'CovertedTime', 'TimeElapsed', 'N.EDA')]
  
  ###########################################################################################
  #####                           Finding subjects for EDA                              #####
  ###########################################################################################
  filtered_df <- filtered_df[complete.cases(filtered_df), ]

  for(session_name in session_list) {
    session_df <- filtered_df %>% filter(Session == session_name)
    session_df <- session_df[session_df$N.EDA > 5, ]
    bad_subjects <- levels(factor((session_df$Subject)))
    print(session_name)
    print(bad_subjects)
    print(length(bad_subjects))
  }
}

get_percentage_invalid_eda <- function() {
  non_filtered_df <- read_csv(file.path(data_dir, non_filtered_file_name))
  print(nrow(non_filtered_df))
  non_filtered_df <- non_filtered_df %>% 
    select('N.EDA.non.filtered') %>% 
    rename('EDA' = 'N.EDA.non.filtered') %>%
    filter(complete.cases(.))
  print(nrow(non_filtered_df))
  
  total_invalid_eda <- nrow(non_filtered_df[non_filtered_df$EDA<0.01 | non_filtered_df$EDA>100, ])
  total_eda <- nrow(non_filtered_df)
  print(paste0('Percentage of invalid EDA: ', (total_invalid_eda/total_eda)*100))
  
  
  plot <- non_filtered_df %>% 
    ggplot(aes(x=EDA)) +
    geom_histogram(aes(y=..density..),
                   alpha=0.8,
                   # stat="identity",
                   binwidth=20,
                   breaks=c(0, 0.01, 0.1, 1, 10))
  # +
    # geom_density(alpha=.2) 
  print(plot)
}

get_limit <- function(col_name) {
  if (is_match(col_name, 'HR')) {
    return(c(40, 150))
  } else if (is_match(col_name, 'BR')) {
    return(c(4, 40))
  } else if (is_match(col_name, 'EDA')) {
    return(c(0.01, 100))
  } 
}

specify_decimal <- function(num, decimal_points) {
  trimws(format(round(num, decimal_points), nsmall=decimal_points))
}

get_percentage_invalid_signal <- function() {
  non_filtered_df <- read_csv(file.path(data_dir, non_filtered_file_name))
  print(str(non_filtered_df))
  
  non_filtered_df <- non_filtered_df %>% 
    select(N.EDA.non.filtered, BR.non.filtered, HR.non.filtered, N.HR.non.filtered) %>% 
    rename(EDA=N.EDA.non.filtered,
           C_HR=HR.non.filtered,
           W_HR=N.HR.non.filtered,
           BR=BR.non.filtered)
  
  print(str(non_filtered_df))
  
  for(col_name in colnames(non_filtered_df)) {
    print(col_name)
    
    temp_df <- non_filtered_df %>% 
      select(col_name) %>% 
      filter(complete.cases(.))
    # print(nrow(temp_df))
    
    lower_bound_invalid_signal <- nrow(temp_df[temp_df[[col_name]]<get_limit(col_name)[1], ])
    upper_bound_invalid_signal <- nrow(temp_df[temp_df[[col_name]]>get_limit(col_name)[2], ])
    total_signal <- nrow(temp_df)
    print(paste0('Percentage of invalid ', col_name, ' lower than ',  get_limit(col_name)[1], ': ', specify_decimal((lower_bound_invalid_signal/total_signal)*100, 2), '%'))
    print(paste0('Percentage of invalid ', col_name, ' greater than ',  get_limit(col_name)[2], ': ', specify_decimal((upper_bound_invalid_signal/total_signal)*100, 2), '%'))
  }
}
get_qc2_bad_signal_count <- function() {
  qc2_df <- read_csv(file.path(data_dir, bad_moderate_signal_file_name))
  print(str(qc2_df))
  
  qc2_df <- qc2_df %>% 
    select(Signal, Condition) %>% 
    filter(is_match(Condition, 'Bad')) %>% 
    group_by(Signal) %>% 
    summarise(n=n())
  
  print(str(qc2_df))
  
}

check_hr_both_sensor <- function(subj_id) {
  filtered_df <- read_csv(file.path(data_dir, filtered_file_name))
  filtered_df <- filtered_df[, c('Subject', 'Condition', 'Session', 'CovertedTime', 'TimeElapsed', 'HR', 'N.HR')]
  
  ################################################################
  ## This following two procedure should represents the same df ##
  ################################################################
  
  ##------------------------------------------------------- Procedure 1
  mean_df <- read_csv(file.path(data_dir, mean_file_name))
  mean_df <- mean_df %>% filter(Subject == subj_id)
  print(mean_df)
 
  ##------------------------------------------------------- Procedure 2
  session_mean <- filtered_df %>% 
    filter(Subject == subj_id) %>%
    group_by(Session) %>%                       ## We want mean per session
    summarise_at(vars(HR, N.HR), mean) %>%      ## Mean for two columns  
    slice(match(session_list, Session))         ## Ordering as the session_list
  
  print(session_mean)

  convert_to_csv(mean_df, file.path(data_dir, subj_id, paste0(subj_id, '-all-signal-mean.csv')))
}

get_missing_subj_for_pp <- function() {
  
  filtered_df <- read_csv(file.path(data_dir, filtered_file_name))
  non_filtered_df <- read_csv(file.path(data_dir, non_filtered_file_name))
  good_subj_df <- read_csv(file.path(data_dir, good_subj_file_name))
  
  good_subj_list <- levels(factor((good_subj_df$Subject)))
  
  #########################################################################################
  #####              Finding filtered subjects for missing PP                         #####
  #########################################################################################
  pp_all_df <- filtered_df[, c('Subject', 'Condition', 'Session', 'CovertedTime', 'TimeElapsed', 'PP')]
  # pp_all_df <- pp_all_df[complete.cases(pp_all_df), ]

  for(sess_idx in 1 : length(session_list)) {
    session_name <- session_list[sess_idx]
    session_df <- pp_all_df %>% filter(Session == session_name)

    subjects <- levels(factor((session_df$Subject)))
    missing_subjects <- setdiff(good_subj_list, subjects)
    # print(length(subjects))
    print(paste0('Missing subjects for ', session_name, ': '))
    print(missing_subjects)
  }
}

get_subj_for_long_session <- function() {
  filtered_df <- read_csv(file.path(data_dir, filtered_file_name))
  pp_all_df <- filtered_df[, c('Subject', 'Condition', 'Session', 'CovertedTime', 'TimeElapsed', 'PP')]
  
  #########################################################################################
  #####                   Finding subjects for long presentation                      #####
  #########################################################################################
  pp_all_df <- pp_all_df[complete.cases(pp_all_df), ]

  for(sess_idx in 1 : length(session_list)) {
    session_name <- session_list[sess_idx]
    session_df <- pp_all_df %>% filter(Session == session_name)

    session_df <- session_df[session_df$TimeElapsed > 400, ]
    bad_subjects <- levels(factor((session_df$Subject)))
    print(bad_subjects)
  }
} 

get_subj_high_eda <- function() {
  
}

get_bad_subjects <- function() {
  # filtered_df <- read_csv(file.path(data_dir, filtered_file_name))
  # non_filtered_df <- read_csv(file.path(data_dir, non_filtered_file_name))
  
  # get_subj_high_br()
  # all_other_functions()
  
  # get_subj_with_high_eda()
  # check_hr_both_sensor('T011')
  # get_missing_subj_for_pp()
  
  # get_subj_for_long_session()
  
  # get_percentage_invalid_eda()
  get_percentage_invalid_signal()
  get_qc2_bad_signal_count()
}






#-------------------------#
#-------Main Program------#
#-------------------------#
get_bad_subjects()






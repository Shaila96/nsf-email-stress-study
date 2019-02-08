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

data_dir <- 'data'
plots_dir <- 'plots'

filtered_file_name <- 'full_df_filtered.csv'
non_filtered_file_name <- 'full_df_non_filtered.csv'
grid_plot_title <- bquote(paste('Perinasal Perspiration [',''^'o','C',''^2,'] valid signal sets'))
y_axis_label <- bquote(paste('PP [',''^'o','C',''^2,']'))

plot_list <- list()





session_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'DualTask', 'Presentation')


# session_list <- c('RestingBaseline')







#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
get_bad_subjects <- function() {
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
  # #########################################################################################
  
  #########################################################################################
  #####              Finding filtered subjects for missing PP                         #####
  #########################################################################################
  # pp_all_df <- non_filtered_df[, c(seq(1, 5), 6)]
  # # pp_all_df <- pp_all_df[complete.cases(pp_all_df), ]
  # # 
  # for(sess_idx in 1 : length(session_list)) {
  #   session_name <- session_list[sess_idx]
  #   session_df <- pp_all_df %>% filter(Session == session_name)
  # 
  #   # session_df <- session_df[session_df$N.HR.non.filtered > 150, ]
  # 
  #   subjects <- levels(factor((session_df$Subject)))
  #   # print(session_df)
  #   print(session_name)
  #   print(subjects)
  #   print(length(subjects))
  # 
  # }
  ###########################################################################################
  
  
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
  
  
  ###########################################################################################
  # #####                           Finding subjects for EDA                            #####
  # #########################################################################################
  # pp_all_df <- pp_all_df[complete.cases(pp_all_df), ]
  # 
  # for(sess_idx in 1 : length(session_list)) {
  #   session_name <- session_list[sess_idx]
  #   session_df <- pp_all_df %>% filter(Session == session_name)
  # 
  #   session_df <- session_df[session_df$N.EDA.non.filtered > 10, ]
  #   bad_subjects <- levels(factor((session_df$Subject)))
  #   print(session_name)
  #   print(bad_subjects)
  #   print(length(bad_subjects))
  # }
  # ##############
  
  
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

  
  
  #########################################################################################
  #####                   Finding subjects for long presentation                      #####
  #########################################################################################
  # pp_all_df <- pp_all_df[complete.cases(pp_all_df), ]
  # 
  # for(sess_idx in 1 : length(session_list)) {
  #   session_name <- session_list[sess_idx]
  #   session_df <- pp_all_df %>% filter(Session == session_name)
  #   
  #   session_df <- session_df[session_df$TimeElapsed > 300, ]
  #   bad_subjects <- levels(factor((session_df$Subject)))
  #   print(bad_subjects)
  # }
  #########################################################################################

}






#-------------------------#
#-------Main Program------#
#-------------------------#
get_bad_subjects()






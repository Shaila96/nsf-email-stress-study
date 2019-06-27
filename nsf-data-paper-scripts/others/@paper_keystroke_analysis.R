#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(directlabels)
library(scales)



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

source(file.path(dirname(current_dir), "@common_functions.R"))

data_dir <- 'data'
performance_dir <- 'performane-data'
tamu_dir <- 'data-from-tamu'
plots_dir <- 'plots'

ets_score_file_name <- 'ets_score_final.csv'


# version <- 'v1'
# key_data_file_name <- 'ks.csv'

version <- 'v2'
key_data_file_name <- 'ks4.csv'


# plot_list <- list()


# #-------------------------#
# #---FUNCTION DEFINITION---#
# #-------------------------#
print_msg <- function(df) {
  print(df)
  message(df)
}

read_data <- function() {
  ets_df <<- read_csv(file.path(data_dir, performance_dir, ets_score_file_name)) %>% 
    select(Subject, Condition, Session, WordCount) %>% 
    rename(Group=Condition,
           Treatment=Session)
    # distinct(Subject)
  key_str_df <<- read_csv(file.path(data_dir, tamu_dir, key_data_file_name)) %>% 
    rename(Subject=Participant) %>%
    filter(Subject %in% unique(ets_df$Subject))
  
  print(nlevels(factor(ets_df$Subject)))
  print(levels(factor(ets_df$Subject)))

  print(nlevels(factor(key_str_df$Subject)))
  print(levels(factor(key_str_df$Subject)))
}


make_error_correction_df <- function() {
  print(nrow(key_str_df))
  key_str_df <<- key_str_df %>% 
    filter(KeyUp==0,
           Key=='BACK' | Key=='DELETE') %>% 
    group_by(Subject, Group, Treatment, Key) %>% 
    summarise(Key_No=n())
  # print(str(key_str_df))
  # print(nrow(key_str_df))
  
  key_str_df <<- merge(key_str_df, ets_df, by=c('Subject', 'Group', 'Treatment')) %>% 
    mutate(Key_No_Norm=Key_No/WordCount) %>% 
    select(Subject, Group, Treatment, Key, WordCount, Key_No, Key_No_Norm)
  
  
  key_back_delt_combined_df <<- key_str_df %>% 
    group_by(Subject, Group, Treatment, WordCount) %>% 
    summarise(Key_No=sum(Key_No), 
              Key_No_Norm=sum(Key_No_Norm))
  
  convert_to_csv(key_str_df, file.path(data_dir, tamu_dir, 'ks_back_delete.csv'))
  convert_to_csv(key_back_delt_combined_df, file.path(data_dir, tamu_dir, 'ks_back_dlt_combined.csv'))
}

get_y_label <- function(col_name) {
  if (col_name=='Key_No') {
    return('Key Count')
  } else if (col_name=='Key_No_Norm') {
    return('Key Count / Word Count')
  }
}

give.n <- function(x) { 
  return(c(y=-Inf, vjust=-1, label=length(x))) 
} 

plot_back_dlt_no <- function(df, col_name, file_name) {
  # print(nlevels(factor(key_str_df$Subject)))
  # print(levels(factor(key_str_df$Subject)))
  
  
  plot_list <- list()
  for (treatment in levels(factor(key_str_df$Treatment))) {
    key_str_session_df <<- key_str_df %>% 
      filter(Treatment==treatment)
    
    
    # print(paste0(treatment, ': '))
    # print(nlevels(factor(key_str_session_df$Subject)))
    # print(levels(factor(key_str_session_df$Subject)))
    
    
    plot <- key_str_session_df %>% 
      ggplot(aes_string(x='Key', y=col_name, fill='Key')) +
      geom_boxplot() +
      xlab(treatment) +
      ylab(get_y_label(col_name)) +
      stat_summary(fun.data = give.n, geom = "text", size = 6) +
      scale_y_continuous(limits = c(min(key_str_df[[col_name]]), max(key_str_df[[col_name]])))
    
    plot_list[[length(plot_list)+1]] <- plot
  } 
  
  grid_plot <- grid.arrange(do.call('grid.arrange', c(plot_list, ncol=2)))
  save_plot(file_name, grid_plot)
}

plot_back_dlt_combined <- function(df, col_name, file_name) {
  plot <- df %>% 
    ggplot(aes_string(x='Treatment', y=col_name, fill='Treatment')) +
    geom_boxplot() +
    xlab('') +
    ylab(get_y_label(col_name)) +
    stat_summary(fun.data = give.n, geom = "text", size = 6) +
    scale_y_continuous(limits = c(min(key_str_df[[col_name]]), max(key_str_df[[col_name]])))
    
  save_plot(file_name, plot)
}


plot_error_correction <- function() {
  key_str_df <<- read_csv(file.path(data_dir, tamu_dir, 'ks_back_delete.csv'))
  key_back_delt_combined_df <<- read_csv(file.path(data_dir, tamu_dir, 'ks_back_dlt_combined.csv'))
  
  
  plot_back_dlt_no(key_str_df, 'Key_No', 'key_back_dlt_no')
  plot_back_dlt_no(key_str_df, 'Key_No_Norm', 'key_back_dlt_normalized')
  
  plot_back_dlt_combined(key_back_delt_combined_df, 'Key_No', 'key_back_dlt_combined_no')
  plot_back_dlt_combined(key_back_delt_combined_df, 'Key_No_Norm', 'key_back_dlt_combined_normalized')
}


analyze_error_correction <- function() {
  # make_error_correction_df()
  plot_error_correction()
}


analyze_key_stroke <- function() {
  analyze_error_correction()
}



#-------------------------#
#-------Main Program------#
#-------------------------#
# read_data()
analyze_key_stroke()






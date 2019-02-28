#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
library(tidyverse)
# library(ggplot2)
# library(grid)
# library(gridExtra)
# library(directlabels)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

data_dir <- 'data'
plots_dir <- 'plots'

data_file_name <- 'result_df.csv'

session_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'Presentation', 'DualTask')
col_list <- c('Subject', 'Condition', 'Session', 'PP', 'N.EDA', 'HR', 'N.HR')
new_col_list <- c('Subject', 'Condition', 'Signal', 'WB.RB', 'SC.RB', 'DT.RB', 'P.RB')

mean_diff_df <- tibble()
middle_mean_diff_df <- tibble()

#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_name) {
  write.table(df, file = file.path(data_dir, file_name), row.names=F, sep = ',')
}

generate_mean_diff_data <- function() {
  filtered_df <- read_csv(file.path(data_dir, data_file_name))[, col_list]
  
  ###############################################################
  ## If you need to test how the shape of the data is changing ##
  ###############################################################
  # mean_diff_df <- filtered_df %>%
  #   gather(Signal, Value, -Subject, -Condition, -Session)
  # print(str(mean_diff_df))
  # convert_to_csv(mean_diff_df, 'test_df.csv')
  ###############################################################
  
  middle_mean_diff_df <<- filtered_df %>% 
    gather(Signal, Value, -Subject, -Condition, -Session) %>%   ## making the data wide to long for all signals
    spread(Session, Value) %>%                                  ## Spreading the session, long to wide again
    mutate(WB.RB = BaselineWriting - RestingBaseline,           ## 4 new columns
           SC.RB = StressCondition - RestingBaseline, 
           DT.RB = DualTask - RestingBaseline, 
           P.RB = Presentation - RestingBaseline)
  
  mean_diff_df <<- middle_mean_diff_df %>%
    select(new_col_list) %>%                                    ## selecting neccessary columns
    gather(Session, Value, -Subject, -Condition, -Signal) %>%   ## making the data wide to long
    spread(Signal, Value)                                       ## Spreading the signal, long to wide again
  
  convert_to_csv(middle_mean_diff_df, 'middle_mean_diff_df.csv')  
  convert_to_csv(mean_diff_df, 'mean_diff_df.csv')
}

generate_eda_vs_pp_plot <- function() {
  ###############################################################
  ##                     NEED TO STUDY MORE                    ##
  ###############################################################
  plot(mean_diff_df$PP, mean_diff_df$N.EDA)
  abline(lm(mean_diff_df$PP~mean_diff_df$N.EDA))
  
  # Do boxplot
  
  ?boxplot()
  p <- ggplot(mean_diff_df, aes(x=dose, y=len)) + 
    geom_boxplot()
  
  # plot(mean_diff_df$HR, mean_diff_df$N.HR)
  # abline(lm(mean_diff_df$HR~mean_diff_df$N.HR))
 
  
  # Baseline Zephyr vs. Baseline E4
  # Scatter plot for absolute mean for each session
  # t.test for mean HR - Baseline
  # Check the normality, discard the outlier or log
  # Boxplot for all t.test
  # Pearson Correlation Test

}



#-------------------------#
#-------Main Program------#
#-------------------------#
generate_mean_diff_data()
generate_eda_vs_pp_plot()




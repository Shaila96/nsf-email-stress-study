#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

source(file.path(dirname(current_dir), "@common_functions.R"))

data_dir <- 'data'
performance_data_dir <- 'performane-data'
final_data_dir <- 'final-data-set'

qc2_filtered_file_name <- 'full_df_second_phase_filtered.csv'
performance_file_name <- 'ets_score_final.csv'


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

make_physiological_df <- function() {
  qc2_filtered_df <- read_csv(file.path(data_dir, qc2_filtered_file_name))
  print(str(qc2_filtered_df))
  
  final_physiological_df <- qc2_filtered_df %>% 
    select(-D.EDA, -D.HR, -Task) %>% 
    rename(EDA=N.EDA,
           C_HR=HR,
           W_HR=N.HR)
  
  print(str(final_physiological_df))
  convert_to_csv(final_physiological_df, file.path(data_dir, final_data_dir, 'physiological_data.csv'))
}

make_performance_df <- function() {
  performance_df <- read_csv(file.path(data_dir, performance_data_dir, performance_file_name))
  print(str(performance_df))
  convert_to_csv(performance_df, file.path(data_dir, final_data_dir, 'performance_data.csv'))
}

# make_psychometrics_df <- function() {
#   
# }



#-------------------------#
#-------Main Program------#
#-------------------------#
# make_physiological_df()
# make_performance_df()
# make_psychometrics_df()




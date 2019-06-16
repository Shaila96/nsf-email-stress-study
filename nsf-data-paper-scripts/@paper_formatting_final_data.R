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


# physiological_col_order <- c(1:5, 11, 6, 9, 8, 7, 10)
physiological_col_order <- c('Participant_ID',
                             'Group',
                             'Treatment',
                             'Time',
                             'Treatment_Time',
                             'Task',
                             'PP',
                             'EDA',
                             'BR',
                             'Chest_HR',
                             'Wrist_HR')

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
    select(-D.EDA, -D.HR) %>% 
    rename(Participant_ID=Subject,
           Group=Condition,
           Treatment=Session,
           Time=CovertedTime,
           TaskMarkers=Task,
           EDA=N.EDA,
           Chest_HR=HR,
           Wrist_HR=N.HR) %>% 
    mutate(Treatment = recode(Treatment,
                            'RestingBaseline' = 'RB',
                            'BaselineWriting' = 'ST',
                            'StressCondition' = 'PM',
                            'DualTask' = 'DT',
                            'Presentation' = 'PR'),
           TaskMarkers = recode(TaskMarkers,
                                'Essay' = 'Report'))
  
  # print(str(final_physiological_df[, physiological_col_order)
  convert_to_csv(final_physiological_df[, physiological_col_order], 
                 file.path(data_dir, final_data_dir, 'Physiological Data.csv'))
}

make_performance_df <- function() {
  performance_df <- read_csv(file.path(data_dir, performance_data_dir, performance_file_name)) %>% 
    rename(Participant_ID=Subject,
           Group=Condition)
  print(str(performance_df))
  convert_to_csv(performance_df, file.path(data_dir, final_data_dir, 'Performance Data.csv'))
}



#-------------------------#
#-------Main Program------#
#-------------------------#
make_physiological_df()
make_performance_df()




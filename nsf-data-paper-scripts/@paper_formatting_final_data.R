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
tamu_dir <- 'data-from-tamu'
survey_data_dir <- 'survey-data'
performance_data_dir <- 'performane-data'
final_data_dir <- 'final-data-set'
quantitative_data_dir <- 'Quantitative Data'

qc2_filtered_file_name <- 'full_df_second_phase_filtered.csv'
performance_file_name <- 'ets_score_final.csv'
key_str_file_name <- 'ks4.csv'



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

key_str_col_order <- c('Participant_ID',
                   'Group',
                   'Treatment',
                   'Time',
                   'Task',
                   'Is_Key_Up',
                   'Key')

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
           Treatment_Time=TimeElapsed,
           # TaskMarkers=Task,
           EDA=N.EDA,
           Chest_HR=HR,
           Wrist_HR=N.HR) %>% 
    mutate(Treatment = recode(Treatment,
                            'RestingBaseline' = 'RB',
                            'BaselineWriting' = 'ST',
                            'StressCondition' = 'PM',
                            'DualTask' = 'DT',
                            'Presentation' = 'PR'),
           Task = recode(Task, 'Essay' = 'Report'))
  
  # print(str(final_physiological_df[, physiological_col_order)
  convert_to_csv(final_physiological_df[, physiological_col_order], 
                 file.path(data_dir, final_data_dir, quantitative_data_dir, 'Physiological Data.csv'))
}

make_performance_df <- function() {
  performance_df <- read_csv(file.path(data_dir, performance_data_dir, performance_file_name)) %>% 
    rename(Participant_ID=Subject,
           Group=Condition)
  print(str(performance_df))
  convert_to_csv(performance_df, file.path(data_dir, final_data_dir, quantitative_data_dir, 'Performance Data.csv'))
}

make_keyboard_df <- function() {
  key_str_df <- read_csv(file.path(data_dir, tamu_dir,  key_str_file_name)) %>% 
    rename(Participant_ID=Participant,
           Is_Key_Up=KeyUp,
           Task='Task Markers')
  
  print(str(key_str_df))
  convert_to_csv(key_str_df[, key_str_col_order], 
                 file.path(data_dir, final_data_dir, quantitative_data_dir, 'Keyboard Data (Test).csv'))
}



#-------------------------#
#-------Main Program------#
#-------------------------#
# make_physiological_df()
# make_performance_df()
make_keyboard_df()      ## Occuring problem for Time and Task column
### make_questionnaire_df() ## This is done in questionnaire data analysis





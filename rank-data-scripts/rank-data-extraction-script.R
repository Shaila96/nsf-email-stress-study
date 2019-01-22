#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library('readr')
library('tibble')


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

data_path <- 'data'
db_data_path <- 'data-for-db'

pre_survey_file_name <- 'pre-survey.csv'

na_val <- 'NA'

bio_final_df <- tibble()
scale_score_df <- tibble()
pre_survey_df <- tibble()


#-------------------------#
#------- FUNCTIONS -------#
#-------------------------#

#---- Set the current directory and read the data file ----#
set_dir_and_read_data <- function() {
  setwd(current_dir)
  pre_survey_df <<- read_csv(file.path(data_path, pre_survey_file_name))
}

convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

is_invalid_data <- function(var) {
  return(is.null(var) || is.na(var))
}

get_gender <- function(gender_val) {
  if(is_invalid_data(gender_val)) 
    return(na_val)
  else
    return(switch(as.numeric(gender_val), "Male", "Female", "Do not want to disclose"))
}

get_nationality <- function(nationality_val) {
  if(is_invalid_data(nationality_val)) 
    return(na_val)
  else
    return(switch(as.numeric(nationality_val), "United States", "Other", "NA"))
}

get_education <- function(education_val) {
  if(is_invalid_data(education_val)) 
    return(na_val)
  else
    return(switch(as.numeric(education_val), "High school or below", "Undergraduate", "Master or equivalent", "PhD, JD, or equivalent"))
}

#---- extracting bio info for each row/subject ----#
extract_bio_info_each_subj <- function(current_subj) {
  bio_df_temp <- tibble("Subject" = current_subj['SubjectID'],
                             "Age" = current_subj['Age'],
                             "Gender" = get_gender(current_subj['Gender']),
                             "Nationality" = get_nationality(current_subj['Nationality']),
                             "Education" = get_education(current_subj['Education'])
                             )

  if (nrow(bio_final_df) == 0) {
    bio_final_df <<- bio_df_temp
  } else {
    bio_final_df <<- rbind(bio_final_df, bio_df_temp)
  }
}

extract_biographic_info <- function() {
  
  #---- extracting neccessary info ----#
  biographic_df <- pre_survey_df[, c(seq(22, 32))]
  colnames(biographic_df) <- c("SubjectID", "Age", "Gender", "Nationality", 
                               "NationalityOthers", "NativeLang", "NativeLangOthers",
                               "Education", "Occupation", "ProficiencyEnglish", "EmailUsingFreq")
  
  
  apply(biographic_df, 1, function(each_row) extract_bio_info_each_subj(each_row))
  convert_to_csv(bio_final_df, file.path(db_data_path, 'biographic.csv'))
}


#---- Perceived Stress Scale ----#
get_pss_score <- function(current_subj) {
  
}


extract_scale_score_each_row <- function(current_subj) {
  # extract_pss_score(current_subj) #---- Perceived Stress Scale ----#
  # extract_big_five_score(current_subj) #---- Big Five Questionnaire ----#
  
  scale_df_temp <- tibble("Subject" = current_subj['SubjectID'],
                          "Percieved Stress Scale" = get_pss_score(current_subj)
                          )
  
  if (nrow(scale_score_df) == 0) {
    scale_score_df <<- scale_df_temp
  } else {
    scale_score_df <<- rbind(scale_score_df, scale_df_temp)
  }
}


extract_scale_score <- function() {
  #---- NOTE: YOU HAVE TO CALCULATE ALL SCALES FOR ONE SUBJECT, THEN GO FOR NEXT RWOW/SUBJECT ----#
  
  scale_df <- pre_survey_df[, c(22, seq(33, 42))]
  # print(str(pre_survey_pss_df))
  colnames(scale_df) <- c("SubjectID", 
                           "PSS-Q1", "PSS-Q2", "PSS-Q3", "PSS-Q4", "PSS-Q5", 
                           "PSS-Q6", "PSS-Q7", "PSS-Q8", "PSS-Q9", "PSS-Q10")

  apply(scale_df, 1, function(each_row) extract_scale_score_each_row(each_row))
  # convert_to_csv(scale_score_df, file.path(db_data_path, 'rank_data_scale.csv'))
}





#-------------------------#
#-------Main Program------#
#-------------------------#
set_dir_and_read_data()

# extract_biographic_info()
# extract_group_data()

extract_scale_score()



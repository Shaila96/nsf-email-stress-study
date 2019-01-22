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

# PSS scores are obtained by reversing responses(e.g., 0 = 4, 1 = 3, 2 = 2, 3 = 1 & 4 = 0) 
# to the four positively stated items (items 4, 5, 7, & 8) 
# and then summing across all scale items.
get_reverse_val_pss <- function(ques_val) {
  # switch does not work for 0; So, adding 1 to start the number from 1 instead for 0
  return(switch(as.numeric(ques_val)+1, 4, 3, 2, 1, 0))
}

#---- Perceived Stress Scale ----#
get_pss_score <- function(subj) {
  # current_subj <- as.data.frame(t(current_subj))
  # cols.num <- c("PSS_Q1", "PSS_Q2", "PSS_Q3", "PSS_Q4", "PSS_Q5", 
  #               "PSS_Q6", "PSS_Q7", "PSS_Q8", "PSS_Q9", "PSS_Q10")
  # current_subj[cols.num] <- sapply(current_subj[cols.num], as.numeric)
  # sapply(current_subj, class)

  pss_score <- as.numeric(subj['PSS_Q1']) + as.numeric(subj['PSS_Q2']) + 
    as.numeric(subj['PSS_Q3']) + as.numeric(subj['PSS_Q6']) + 
    as.numeric(subj['PSS_Q9']) + as.numeric(subj['PSS_Q10']) +
    get_reverse_val_pss(subj['PSS_Q4']) + get_reverse_val_pss(subj['PSS_Q5']) +
    get_reverse_val_pss(subj['PSS_Q7']) + get_reverse_val_pss(subj['PSS_Q8'])
  
  return(pss_score)
}


extract_scale_score_each_row <- function(current_subj) {
  # extract_pss_score(current_subj) #---- Perceived Stress Scale ----#
  # extract_big_five_score(current_subj) #---- Big Five Questionnaire ----#

  scale_df_temp <- tibble("Subject" = current_subj['SubjectID'],
                          "GraphType" = "Scale",
                          "Division" = "Pre Exp",
                          "ScaleTitle" = "Percieved Stress Scale",
                          "ScaleValue" = get_pss_score(current_subj))
  
  # scale_df_temp <- tibble("Subject" = current_subj['SubjectID'],
  #                         "GraphType" = "Scale",
  #                         "Division" = "Pre Exp",
  #                         "ScaleTitle" = "Percieved Stress Scale",
  #                         "ScaleValue" = get_pss_score(current_subj))

  if (nrow(scale_score_df) == 0) {
    scale_score_df <<- scale_df_temp
  } else {
    scale_score_df <<- rbind(scale_score_df, scale_df_temp)
  }
}


extract_scale_score <- function() {
  #---- NOTE: YOU HAVE TO CALCULATE ALL SCALES FOR ONE SUBJECT, THEN GO FOR NEXT RWOW/SUBJECT ----#
  
  scale_df <- pre_survey_df[, c(22, seq(33, 42))]
  # print(str(scale_df))
  # print(tail(scale_df, 2))
  colnames(scale_df) <- c("SubjectID", 
                           "PSS_Q1", "PSS_Q2", "PSS_Q3", "PSS_Q4", "PSS_Q5", 
                           "PSS_Q6", "PSS_Q7", "PSS_Q8", "PSS_Q9", "PSS_Q10")

  apply(scale_df, 1, function(each_row) extract_scale_score_each_row(each_row))
  
  print(head(scale_score_df, 2))
  print(tail(scale_score_df, 2))
  
  convert_to_csv(scale_score_df, file.path(db_data_path, 'rank_data_scale.csv'))
}





#-------------------------#
#-------Main Program------#
#-------------------------#
set_dir_and_read_data()

# extract_biographic_info()
# extract_group_data()

extract_scale_score()



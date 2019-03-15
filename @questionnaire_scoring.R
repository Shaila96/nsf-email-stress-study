#--------------------------------------#
#--------Perceived Stress Scale--------#
#--------------------------------------#

# PSS scores are obtained by reversing responses(e.g., 0 = 4, 1 = 3, 2 = 2, 3 = 1 & 4 = 0) 
# to the four positively stated items (items 4, 5, 7, & 8) 
# and then summing across all scale items.
get_reverse_val_pss <- function(ques_val) {
  # switch does not work for 0; So, adding 1 to start the number from 1 instead for 0
  return(switch(as.numeric(ques_val)+1, 4, 3, 2, 1, 0))
}

get_pss_score <- function(df) {
  pss_score <- as.numeric(df['PSS_Q1']) + 
    as.numeric(df['PSS_Q2']) + 
    as.numeric(df['PSS_Q3']) + 
    as.numeric(df['PSS_Q6']) + 
    as.numeric(df['PSS_Q9']) + 
    as.numeric(df['PSS_Q10']) +
    get_reverse_val_pss(df['PSS_Q4']) + 
    get_reverse_val_pss(df['PSS_Q5']) +
    get_reverse_val_pss(df['PSS_Q7']) + 
    get_reverse_val_pss(df['PSS_Q8'])
  
  return(pss_score)
}








#------------------------------------------------#
#--------Emotion Regulation Questionnaire--------#
#------------------------------------------------#
get_erq_cog_score <- function(df) {
  erq_cog_score <- as.numeric(df['ERQ_Q1']) + 
    as.numeric(df['ERQ_Q3']) + 
    as.numeric(df['ERQ_Q5']) + 
    as.numeric(df['ERQ_Q7']) + 
    as.numeric(df['ERQ_Q8']) + 
    as.numeric(df['ERQ_Q10'])
  
  return(erq_cog_score)
}

get_erq_sup_score <- function(df) {
  erq_sup_score <- as.numeric(df['ERQ_Q2']) + 
    as.numeric(df['ERQ_Q4']) + 
    as.numeric(df['ERQ_Q6']) + 
    as.numeric(df['ERQ_Q9'])
  
  return(erq_sup_score)
}
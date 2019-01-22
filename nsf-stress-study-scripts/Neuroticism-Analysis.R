library(nlme)


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}




#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

all_data_df <- read.csv("@Datasets/data-uci.csv")[, c('pid', 'email_condition', 'stress_condition', 'big5_n')]
nlp_data_df <- read.csv("@Datasets/essay_nlp_results.csv")[, c('Subject', 'Condition', 'Essay', 'Negative')]
nlp_data_df <- nlp_data_df[nlp_data_df$Essay == "DT",]

# str(all_data_df)
# str(nlp_data_df)
# print(nlp_data_df)
# 
# print(all_data_df$big5_n)
hist(all_data_df$big5_n)

merged_df <- merge(nlp_data_df, all_data_df, by.x='Subject', by.y='pid')
convert_to_csv(merged_df, "@Datasets/sentiment_neuroticism.csv")

str(merged_df)
print(merged_df$Subject)
print(length(merged_df$Subject))
print(length(merged_df$Negative))
print(nlp_data_df$Subject)



# Mixed Model - Without Interaction
model1 = lme(Negative ~ 1 + stress_condition + big5_n,  data=merged_df,
             random=~1|Subject, na.action = na.omit)
summary(model1)


# Fixed Model
model2 = lm(Negative ~ 1 + stress_condition + big5_n,  data=merged_df, na.action = na.omit)
summary(model2)


# Mixed Model - With Interaction
model3 = lme(Negative ~ 1 + stress_condition + big5_n + stress_condition*big5_n,  data=merged_df,
             random=~1|Subject, na.action = na.omit)
summary(model3)


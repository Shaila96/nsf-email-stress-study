library(dplyr) 
library(ggplot2)
library(cowplot) 

#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

testing_df <- read.csv("@Datasets/testing_df.csv")[, c('Subject', 'Condition', 'Measurement')]
pre_survey_df <- read.csv("@Datasets/survey-reports/pre-survey.csv")[, c(19:32)]


str(testing_df)
str(pre_survey_df)


merged_df <- merge(testing_df, pre_survey_df, by.x='Subject', by.y='Your.participant.ID.')
# merged_df <- na.omit(merged_df)
str(merged_df)
nrow(merged_df)

min(na.omit(merged_df$Your.age.))
max(na.omit(merged_df$Your.age.))
range(na.omit(merged_df$Your.age.))
mean(na.omit(merged_df$Your.age.))
sd(na.omit(merged_df$Your.age.))

hist(na.omit(merged_df$Your.age.))
hist(na.omit(merged_df$Your.education.level.))

min(na.omit(merged_df$Your.education.level.))
max(na.omit(merged_df$Your.education.level.))
# 1 --> High school or below
# 2 --> Undergraduate
# 3 --> Master or equivalent
# 4 --> PhD, JD, or equivalent



nrow(filter(merged_df, Gender. == "1"))
nrow(filter(merged_df, Gender. == "2"))




gg <- ggplot(data=merged_df, aes(merged_df$Your.age.)) + 
  geom_histogram(color="black", fill="grey") +
  ggtitle("Age Distribution - Histogram") +
  labs(x="Age of Subjects", y="Frequency") 
save_plot("age_distribution.pdf", gg, base_height = 10, base_width = 12)



gg <- ggplot(data=merged_df, aes(merged_df$Your.education.level.)) + 
  geom_histogram(color="black", fill="grey") +
  # scale_x_discrete(limits = c('High school or below', 'Undergraduate', 'Master or equivalent', 'PhD, JD, or equivalent')) +
  ggtitle("Education Distribution - Histogram") +
  labs(x="Education of Subjects", y="Frequency") 
save_plot("education_distribution.pdf", gg, base_height = 10, base_width = 12)


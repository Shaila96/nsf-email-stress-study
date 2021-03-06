library(readr)


current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)



#---------------------- CHECK full_data.csv
full_df <- read_csv(file.path(current_dir, "@Datasets/full_df.csv"))
length(unique(full_df$Subject))


full_df_filtered <- read_csv(file.path(current_dir, "@Datasets/full_df_first_phase_filtered.csv"))
length(unique(full_df_filtered$Subject))


full_df_non_filtered <- read_csv(file.path(current_dir, "@Datasets/full_df_non_filtered.csv"))
length(unique(full_df_non_filtered$Subject))



unique(full_df$Subject) == unique(full_df_filtered$Subject)
unique(full_df$Subject) == unique(full_df_non_filtered$Subject)
#-------------------------------------------------------#
#---------------- MAKE SURE THIS IS  63 ----------------#
#-------------------------------------------------------#


 

#---------------------- EXPORT to csv
# good_subj_list <- list('T003', 'T005', 'T009', 'T011', 'T016', 'T019', 'T021', 'T031',
#                        'T032', 'T035', 'T037', 'T046', 'T047', 'T051', 'T061', 'T063',
#                        'T064', 'T065', 'T066', 'T068', 'T077', 'T078', 'T079', 'T082',
#                        'T083', 'T084', 'T085', 'T091', 'T092', 'T093', 'T094', 'T096', 
#                        'T098', 'T099', 'T106', 'T112', 'T113', 'T114', 'T121', 'T122', 
#                        'T124', 'T126', 'T128', 'T130', 'T132', 'T138', 'T139', 'T141', 
#                        'T144', 'T145', 'T151', 'T152', 'T154', 'T156', 'T157', 'T162', 
#                        'T166', 'T172', 'T173', 'T174', 'T175', 'T176', 'T178')
# 
# good_subj_df <- t(as.data.frame(good_subj_list))
# write.table(good_subj_df, file = "@Datasets/subj_good_df.csv", col.names=c("Subject"), row.names=F)







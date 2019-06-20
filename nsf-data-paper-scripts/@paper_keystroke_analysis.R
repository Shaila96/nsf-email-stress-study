#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(directlabels)
library(scales)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

source(file.path(dirname(current_dir), "@common_functions.R"))

data_dir <- 'data'
tamu_dir <- 'data-from-tamu'
plots_dir <- 'plots'

data_file_name <- 'df_hrv.csv'


plot_list <- list()


# #-------------------------#
# #---FUNCTION DEFINITION---#
# #-------------------------#
print_msg <- function(df) {
  print(df)
  message(df)
}

# #-------------------------#
# #-------Main Program------#
# #-------------------------#
generate_hrv_plot()






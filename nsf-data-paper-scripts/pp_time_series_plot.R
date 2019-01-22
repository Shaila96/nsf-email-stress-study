#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(directlabels)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

data_dir <- 'data'
plots_dir <- 'plots'

data_file_name <- 'full_df.csv'
grid_plot_title <- bquote(paste('Perinasal Perspiration [',''^'o','C',''^2,'] valid signal sets'))
y_axis_label <- bquote(paste('PP [',''^'o','C',''^2,']'))

plot_list <- list()






# session_atr <- 'all-session'
# session_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'DualTask', 'Presentation')

session_atr <- 'non-dual-session'
session_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'Presentation')

# session_atr <- 'dual-session'
# session_list <- c('DualTask')







#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
save_plot <- function(plot_name, plot) {
  plot_path <- file.path(current_dir, plot_name)
  ggsave(plot_path, plot, width=10, height=10)
}

get_session_name <- function(session_name) {
  return(gsub("([a-z])([A-Z])", "\\1 \\2", session_name))
}


generate_pp_plot <- function() {
  pp_all_df <- read_csv(file.path(data_dir, data_file_name))
  pp_all_df <- pp_all_df[, c(seq(1, 6))]
  # print(str(pp_all_df))
  
  subj_list <- levels(factor(pp_all_df$Subject))
  
  
  #################### CHECK THIS  ################### 
  pp_all_df <- pp_all_df[complete.cases(pp_all_df),] %>% 
    filter(Session %in% session_list)
  
  max_pp_val <- max(pp_all_df$PP)
  max_x_limit <- max(pp_all_df$TimeElapsed)
  # print(paste0('max x limit: ', max_x_limit))
  
  subj_no_label_x_pos = max_x_limit-max_x_limit/40
  subj_no_label_y_pos = max_pp_val-max_pp_val/8

  for(sess_idx in 1 : length(session_list)) {
    session_name <- session_list[sess_idx]
    session_df <- pp_all_df %>% filter(Session == session_name)
    # print(str(session_df))

    subj_no <- session_df %>% distinct(Subject) %>% summarize(n = n())
    subj_no_label <- paste("n =", subj_no)
    
    
    #---- SETUP FOR PLOTS ----#
    x_axis_label <- ''
    
    #---- PUTTING X-LABEL FOR THE LAST PLOT ONLY ----#
    if (sess_idx == length(session_list)) {
      x_axis_label <- 'Time [s]'
    }
    
    if (nrow(session_df) != 0) {
      session_plot <- 
        session_df %>%
        ggplot(aes(TimeElapsed, PP, group=Subject)) +
        geom_line(alpha = 0.7) +
        annotate("text", x=subj_no_label_x_pos, y=subj_no_label_y_pos, label=subj_no_label) +
        theme_bw() +
        theme(
          # panel.background = element_blank(),
          # panel.grid.major = element_blank(), 
          # panel.grid.minor = element_blank(),
          axis.text.y.right = element_blank(),
          axis.ticks.y.right = element_blank(),
          legend.position='none'
          ) +
        xlim(0, max_x_limit) +
        # ylim(0, max_pp_val) +
        xlab(x_axis_label) +
        ylab(y_axis_label) +
        scale_y_continuous(limits=c(0, max_pp_val), 
                           sec.axis=sec_axis(~.+1, name=get_session_name(session_name)))
        
      
      # print(session_plot)
   
      #---- SAVING THE PLOTS IN A LIST TO MAKE A GRID GRAPH ----#
      plot_list[[sess_idx]] <- session_plot
    }
  }
  
  
  #----------------------------------------------------------------#
  #---- MAKING GRID GRAPH WITH ALL THE PLOTS FROM EACH SESSION ----#
  #----------------------------------------------------------------#
  
  #---- THIS GRID PLOT CONTAINS ALL THE SINGLE PLOT----#
  grid_plot <- do.call('grid.arrange', c(plot_list, ncol=1))
  grid_plot <- grid.arrange(grid_plot, top=textGrob(grid_plot_title, gp=gpar(fontsize=10, font=1)))
  
  print(grid_plot)
  
  #---- SAVING GRID PLOT ----#
  plot_path <- file.path(plots_dir, paste0('pp-time-series-', session_atr, '-', format(Sys.Date(), format='%m-%d-%y'), '.pdf'))
  save_plot(plot_path, grid_plot)
}




#-------------------------#
#-------Main Program------#
#-------------------------#
generate_pp_plot()





